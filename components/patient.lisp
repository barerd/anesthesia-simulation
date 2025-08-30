;;;; components/patient.lisp - Standalone patient model

(in-package :anesthesia-sim)

(defclass patient (dynamic-mixin thermal-component gas-component)
  ((age-years :initarg :age-years :accessor age-years :initform 40)
   (body-weight-kg :initarg :body-weight-kg :accessor body-weight-kg :initform 70.0
                   :documentation "Body weight in kg")
   (tidal-volume :initarg :tidal-volume :accessor tidal-volume :initform 500.0
                 :documentation "Tidal volume in mL")
   (respiratory-rate :initarg :respiratory-rate :accessor respiratory-rate :initform 12.0
                     :documentation "Breaths per minute")
   (frc :initarg :frc :accessor frc :initform 2500.0
        :documentation "Functional residual capacity in mL")
   (co2-production :initarg :co2-production :accessor co2-production :initform 200.0
                   :documentation "CO2 production in mL/min")
   (o2-consumption :initarg :o2-consumption :accessor o2-consumption :initform 250.0
                   :documentation "O2 consumption in mL/min")
   (respiratory-phase :accessor respiratory-phase :initform 0.0
                      :documentation "Respiratory phase (0-1, 0=start inspiration)")
   (last-time :accessor last-time :initform 0.0)
   (alveolar-temp :accessor alveolar-temp :initform *body-temp*
                  :documentation "Alveolar temperature (°C)")
   (airway-humidity :accessor airway-humidity :initform 1.0
                    :documentation "Airway relative humidity")
   (anatomical-dead-space :initarg :anatomical-dead-space 
                          :accessor anatomical-dead-space :initform 150.0
                          :documentation "Anatomical dead space (mL)")
   (physiologic-deadspace-frac :initarg :vdphys-frac :accessor physiologic-deadspace-frac :initform 0.30)
   (shunt-fraction :initarg :shunt-frac :accessor shunt-fraction :initform 0.05)
   
   (out-fractions :accessor out-fractions :initform (make-hash-table :test 'equal)
                  :documentation "Gas fractions in expired air")
   (opioid-resp-sensitivity :initarg :opioid-resp-sensitivity :accessor opioid-resp-sensitivity :initform 1.0) ; 1.0 is normal
   (alveolar-contents :accessor alveolar-contents
                      :initform (make-hash-table :test 'equal)
                      :documentation "Alveolar gas contents by species (liters at STPD).")
   (alveolar-volume-l :accessor alveolar-volume-l
                      :initform 2.5
                      :documentation "Alveolar gas volume (L); usually FRC/1000.")
   ;; Optional: store last expired mix (for monitors/graphs)
   (last-expired-fractions :accessor last-expired-fractions
                           :initform (make-hash-table :test 'equal)
                           :documentation "Expired-gas fractions (last tick).")))

(defmethod initialize-instance :after ((patient patient) &key)
  "Initialize patient with default expired gas and alveolar contents."
  ;; Expired-gas defaults (what you already had)
  (let ((fractions (out-fractions patient)))
    (setf (gethash "O2"  fractions) 0.16)
    (setf (gethash "CO2" fractions) 0.04)
    (setf (gethash "N2"  fractions) 0.79)
    (setf (gethash "H2O" fractions) 0.01))
  ;; Alveolar volume from FRC
  (setf (alveolar-volume-l patient) (/ (frc patient) 1000.0))
  ;; Initialize alveolar contents to plausible alveolar mix (L at STPD)
  (let* ((va (max 0.5 (alveolar-volume-l patient))) ; guard
         (alv (alveolar-contents patient)))
    (clrhash alv)
    ;; ~14% O2, ~5% CO2, balance N2 (ignore H2O for STPD bookkeeping)
    (setf (gethash "O2"  alv) (* va 0.14))
    (setf (gethash "CO2" alv) (* va 0.05))
    (setf (gethash "N2"  alv) (* va 0.81))
    ;; Include known agents at 0 to ensure keys exist
    (dolist (g '("N2O" "sevoflurane" "H2O")) (setf (gethash g alv) 0.0)))
  ;; Also seed last-expired-fractions
  (let ((last (last-expired-fractions patient)))
    (clrhash last)
    (setf (gethash "O2"  last) 0.16)
    (setf (gethash "CO2" last) 0.04)
    (setf (gethash "N2"  last) 0.79)
    (setf (gethash "H2O" last) 0.01)))

(defmethod compute-target-fractions ((p controlled-patient) input-stream)
  "Mixed-expired composition: FECO2 = VCO2/VE, FEO2 = FiO2 - VO2/VE."
  (let* ((fi (if input-stream (clone-hash (fractions input-stream))
                 (let ((h (make-hash-table :test 'equal)))
                   (setf (gethash "O2" h) 0.21
                         (gethash "CO2" h) 0.0
                         (gethash "N2" h)  0.79)
                   h)))
         (fo     (clone-hash fi))
         (fiO2   (gethash "O2"  fi 0.21))
         (rr     (max 2.0 (respiratory-rate p)))
         (vtml   (max 100.0 (tidal-volume p)))   ; mL
         (ve     (max 0.1 (* rr vtml 0.001)))    ; L/min
         (vo2    (/ (o2-consumption p) 1000.0))  ; L/min
         (vco2   (/ (co2-production  p) 1000.0)) ; L/min
         (feCO2  (min 0.5 (max 0.0 (/ vco2 ve))))
         (feO2   (max 0.0 (- fiO2 (/ vo2 ve)))))
    (setf (gethash "CO2" fo) feCO2
          (gethash "O2"  fo) feO2)
    (renormalize-fractions! fo)))

;; (defmethod compute-target-fractions ((patient patient) input-stream)
;;   "Lung exchanges O2 and CO2. Exhaled fractions scale with minute ventilation (MV)."
;;   (let* ((input-fractions (fractions input-stream))
;;          (target (clone-hash input-fractions))

;;          ;; Inspired fractions (with safe fallbacks)
;;          (o2-in  (gethash "O2"  target 0.21))
;;          (co2-in (gethash "CO2" target 0.0))

;;          ;; Minute ventilation in L/min
;;          (mv (* (tidal-volume patient) (respiratory-rate patient) 0.001))
;;          (mv (max 0.1 mv))  ; avoid division by ~0

;;          ;; Convert VO2/VCO2 (mL/min) -> fractional change at given MV
;;          ;; Clamp to plausible bounds so fractions stay sane.
;;          (o2-uptake-fraction   (min 0.12 (/ (o2-consumption patient) 1000.0 mv)))
;;          (co2-addition-fraction (min 0.12 (/ (co2-production  patient) 1000.0 mv)))

;;          ;; Output fractions (O2 can fall, CO2 can rise) with guards
;;          (o2-out  (max 0.05 (- o2-in  o2-uptake-fraction)))
;;          (co2-out (min 0.12 (+ co2-in co2-addition-fraction))))

;;     (setf (gethash "O2"  target) o2-out
;;           (gethash "CO2" target) co2-out)

;;     ;; Let the rest of gases renormalize to 1.0
;;     (renormalize-fractions! target)))

(defmethod effective-tau ((patient patient) input-stream dt)
  "Return gas-composition time constant (s). Uses FRC/VA, with a mechanics floor."
  (declare (ignore input-stream dt))
  (let* ((frc (max 500.0 (frc patient))) ; mL
         ;; VA = RR*(VT - VD); use stored anatomic DS if available
         (rr  (max 2.0 (respiratory-rate patient)))
         (vt  (max 100.0 (tidal-volume patient)))
         (vd  (max 50.0 (ignore-errors (anatomical-dead-space patient))))
         (va-lpm (* rr (max 0.0 (- vt vd)) 0.001)) ; L/min
         ;; composition tau (s): (FRC[L]/VA[L/min]) * 60
         (tau-gas (* 60.0 (/ (/ frc 1000.0) (max 0.05 va-lpm))))
         ;; mechanics floor: τ_mech = R*C (s). Use simple defaults if not modeled.
         (r-aw (ignore-errors (airway-resistance patient)))
         (c-rs (/ (+ (tidal-volume patient) (frc patient)) 20.0)) ; your helper; mL/cmH2O
         (tau-mech (if (and r-aw c-rs) (* r-aw (/ c-rs 1000.0)) 0.2)))
    ;; Final tau: gas mixing dominates, but never faster than mechanics floor
    (max tau-mech (min 10.0 tau-gas))))

(defmethod process-gas :around ((patient patient) input-stream dt)
  "Condition inspired gas → alveolar mass balance → emit expired stream,
   and remember the patient's expired stream for EtCO2."
  ;; Advance respiratory phase
  (let* ((cycle-duration (/ 60.0 (max 2.0 (respiratory-rate patient))))
         (time-increment (/ dt cycle-duration)))
    (incf (respiratory-phase patient) time-increment)
    (when (>= (respiratory-phase patient) 1.0)
      (decf (respiratory-phase patient) 1.0)))

  ;; Condition inspired stream to alveolar T/RH
  (let* ((insp (if (typep input-stream 'gas-stream)
                   (make-instance 'gas-stream
                                  :flow-rate (flow-rate input-stream)
                                  :fractions (%clone-fractions (fractions input-stream))
                                  :temperature (alveolar-temp patient)
                                  :pressure    (pressure input-stream)
                                  :humidity    (airway-humidity patient))
                   (make-instance 'gas-stream
                                  :flow-rate (flow-rate input-stream)
                                  :fractions (%clone-fractions (fractions input-stream))
                                  :temperature (alveolar-temp patient)
                                  :pressure    *standard-pressure*
                                  :humidity    (airway-humidity patient))))
         ;; alveolar mass balance → expired fractions (end-tidal by default)
         (expired-fractions (nth-value 0 (step-alveoli-and-expired
                                          patient (fractions insp) dt
                                          :mix-deadspace-p nil)))
         ;; build expired stream
         (expired (make-instance 'gas-stream
                                 :flow-rate (flow-rate input-stream)
                                 :fractions (%clone-fractions expired-fractions)
                                 :temperature (alveolar-temp patient)
                                 :pressure (if (typep input-stream 'gas-stream)
                                               (pressure input-stream)
                                               *standard-pressure*)
                                 :humidity (saturated-rh-at (alveolar-temp patient)))))
    (%set-last-expired patient expired)
    expired))

(defmethod post-process-dynamics ((patient patient) input-stream output-stream dt)
  "Update patient physiological state after gas exchange."
  (declare (ignore input-stream output-stream dt))
  ;; Update thermal state based on metabolic activity
  (let* ((metabolic-heat (* (+ (o2-consumption patient) (co2-production patient)) 0.001))
         (temp-increase (* metabolic-heat dt 0.0001))) ; Very small effect
    (incf (operating-temp patient) temp-increase)
    ;; Keep temperature within physiological bounds
    (setf (operating-temp patient) 
          (max 36.0 (min 38.0 (operating-temp patient))))))

;;; Additional patient-specific methods for monitoring

(defgeneric respiratory-compliance (patient)
  (:documentation "Return patient's respiratory system compliance (mL/cmH2O)")
  (:method ((patient patient))
    ;; Simplified compliance model
    (/ (+ (tidal-volume patient) (frc patient)) 20.0)))

(defgeneric airway-resistance (patient)
  (:documentation "Return patient's airway resistance (cmH2O·s/L)")
  (:method ((patient patient))
    ;; Simplified resistance model
    2.0))

(defmethod calculate-alveolar-ventilation ((p patient))
  (let* ((vt (tidal-volume p))
         (rr (respiratory-rate p))
         (vd-anat (anatomical-dead-space p))
         (vdphys-frac (max 0.0 (min 0.9 (physiologic-deadspace-frac p))))
         ;; overall DS = max(anatomic, fraction of VT)
         (vd (max vd-anat (* vdphys-frac vt))))
    (max 0.05 (* rr (max 0.0 (- vt vd)) 0.001)))) ; L/min

(defun patient-vital-signs (patient)
  "Display current patient vital signs and gas exchange parameters."
  (format t "~&=== Patient Vital Signs ===~%")
  (format t "Respiratory Rate: ~A bpm~%" (respiratory-rate patient))
  (format t "Tidal Volume: ~A mL~%" (tidal-volume patient))
  (format t "Minute Ventilation: ~,1F L/min~%" 
          (* (tidal-volume patient) (respiratory-rate patient) 0.001))
  (format t "Alveolar Ventilation: ~,1F L/min~%" 
          (calculate-alveolar-ventilation patient))
  (format t "O2 Consumption: ~A mL/min~%" (o2-consumption patient))
  (format t "CO2 Production: ~A mL/min~%" (co2-production patient))
  (format t "Body Temperature: ~,1F°C~%" (alveolar-temp patient))
  (format t "Airway Humidity: ~,1F%~%" (* 100 (airway-humidity patient)))
  (format t "Respiratory Phase: ~,2F~%" (respiratory-phase patient)))

(defun adjust-patient-parameters (patient &key respiratory-rate tidal-volume 
                                 o2-consumption co2-production)
  "Adjust patient parameters for different clinical scenarios."
  (when respiratory-rate
    (setf (respiratory-rate patient) respiratory-rate))
  (when tidal-volume
    (setf (tidal-volume patient) tidal-volume))
  (when o2-consumption
    (setf (o2-consumption patient) o2-consumption))
  (when co2-production
    (setf (co2-production patient) co2-production))
  (format t "Patient parameters updated~%"))

(defun step-alveoli-and-expired (patient inspired-fractions dt
                                   &key (mix-deadspace-p nil))
  "Advance alveolar contents by dt using VA and VO2/VCO2. Return expired fractions.

Mass balance per dt:
  Alv_next = Alv_prev
             + V_in * Fin
             - V_out * F_alv(post-mix, post-metabolism)
             - VO2_dt (O2 only)
             + VCO2_dt (CO2 only)
with V_out chosen to keep alveolar volume constant:
  V_out = V_in - VO2_dt + VCO2_dt"
  (declare (type number dt))
  (let* ((alv (alveolar-contents patient))
         (va  (max 0.5 (alveolar-volume-l patient))) ; L
         ;; ventilation & metabolism in L over dt
         (va-lpm (calculate-alveolar-ventilation patient)) ; L/min
         (v-in   (* va-lpm (/ dt 60.0)))                  ; L this tick
         (vo2-lpm (/ (o2-consumption patient) 1000.0))    ; L/min
         (vco2-lpm (/ (co2-production  patient) 1000.0))  ; L/min
         (vo2-dt (* vo2-lpm (/ dt 60.0)))                 ; L this tick
         (vco2-dt (* vco2-lpm (/ dt 60.0)))               ; L this tick
         (v-out  (max 0.0 (- v-in vo2-dt (- vco2-dt)))))  ; ensure non-negative
    ;; 1) Inflow mixing: add inspired gases
    (maphash (lambda (gas frac)
               (incf (gethash gas alv 0.0) (* v-in (max 0.0 frac))))
             inspired-fractions)
    ;; Ensure we track common gases even if not present in inspiration
    (dolist (g '("O2" "CO2" "N2" "N2O" "sevoflurane" "H2O"))
      (gethash g alv 0.0)

      ;; also ensure inspired has key for mixing computation below
      (gethash g inspired-fractions 0.0))

    ;; 2) Metabolic exchange: -VO2, +VCO2
    (decf (gethash "O2"  alv) vo2-dt)
    (incf (gethash "CO2" alv) vco2-dt)
    ;; guard against negatives (rare with small dt, but safe)
    (dolist (g '("O2" "CO2" "N2" "N2O" "sevoflurane" "H2O"))
      (when (< (gethash g alv) 0.0) (setf (gethash g alv) 0.0)))

    ;; 3) Alveolar fractions *before* exhalation
    (let* ((sum-pre (%hash-sum alv))
           (f-alv  (make-hash-table :test 'equal)))
      (maphash (lambda (g amt)
                 (setf (gethash g f-alv) (if (> sum-pre 1e-12) (/ amt sum-pre) 0.0)))
               alv)

      ;; 4) Choose expired fractions
      (let* ((vt (tidal-volume patient))
             (vd (max (anatomical-dead-space patient)
                      (* (physiologic-deadspace-frac patient) vt)))
             (w-alv (if (or (<= vt 0.0) (>= vd vt)) 1.0
                        (max 0.0 (min 1.0 (/ (- vt vd) vt)))))
             (f-exp (make-hash-table :test 'equal)))
        (maphash
         (lambda (g _)
           (declare (ignore _))
           (let ((fin (gethash g inspired-fractions 0.0))
                 (fa  (gethash g f-alv 0.0)))
             (setf (gethash g f-exp)
                   (if mix-deadspace-p
                       (+ (* w-alv fa) (* (- 1.0 w-alv) fin))
                       fa))))
         f-alv)

        ;; 5) Exhalation: remove V_out of alveolar gas (using alveolar fractions)
        (maphash (lambda (g fa)
                   (decf (gethash g alv) (* v-out fa)))
                 f-alv)

        ;; 6) Keep alveolar volume exactly at VA by *construction*:
        ;; sum(alv) should remain constant; minor numeric drift may occur.
        (let ((sum-post (%hash-sum alv)))
          (when (and (> va 0.0) (> sum-post 1e-12)
                     (> (abs (- sum-post va)) 1e-6))
            (let ((scale (/ va sum-post)))
              (maphash (lambda (g amt)
                         (setf (gethash g alv) (* amt scale)))
                       alv))))

        ;; Stash for monitors
        (clrhash (out-fractions patient))
        (maphash (lambda (g v) (setf (gethash g (out-fractions patient)) v)) f-exp)
        (clrhash (last-expired-fractions patient))
        (maphash (lambda (g v) (setf (gethash g (last-expired-fractions patient)) v)) f-exp)

        ;; return expired fractions and alveolar fractions (handy for debugging)
        (values f-exp f-alv v-out)))))
