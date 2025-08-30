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
   (opioid-resp-sensitivity :initarg :opioid-resp-sensitivity :accessor opioid-resp-sensitivity :initform 1.0))) ; 1.0 is normal

(defmethod initialize-instance :after ((patient patient) &key)
  "Initialize patient with default gas fractions."
  (let ((fractions (out-fractions patient)))
    ;; Set default expired gas composition
    (setf (gethash "O2" fractions) 0.16)    ; Normal expired O2
    (setf (gethash "CO2" fractions) 0.04)   ; Normal expired CO2
    (setf (gethash "N2" fractions) 0.79)    ; Balance nitrogen
    (setf (gethash "H2O" fractions) 0.01))) ; Some water vapor

(defmethod compute-target-fractions ((patient patient) input-stream)
  "Lung exchanges O2 and CO2. Exhaled fractions scale with minute ventilation (MV)."
  (let* ((input-fractions (fractions input-stream))
         (target (clone-hash input-fractions))

         ;; Inspired fractions (with safe fallbacks)
         (o2-in  (gethash "O2"  target 0.21))
         (co2-in (gethash "CO2" target 0.0))

         ;; Minute ventilation in L/min
         (mv (* (tidal-volume patient) (respiratory-rate patient) 0.001))
         (mv (max 0.1 mv))  ; avoid division by ~0

         ;; Convert VO2/VCO2 (mL/min) -> fractional change at given MV
         ;; Clamp to plausible bounds so fractions stay sane.
         (o2-uptake-fraction   (min 0.12 (/ (o2-consumption patient) 1000.0 mv)))
         (co2-addition-fraction (min 0.12 (/ (co2-production  patient) 1000.0 mv)))

         ;; Output fractions (O2 can fall, CO2 can rise) with guards
         (o2-out  (max 0.05 (- o2-in  o2-uptake-fraction)))
         (co2-out (min 0.12 (+ co2-in co2-addition-fraction))))

    (setf (gethash "O2"  target) o2-out
          (gethash "CO2" target) co2-out)

    ;; Let the rest of gases renormalize to 1.0
    (renormalize-fractions! target)))

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
  "Add humidity and temperature conditioning plus respiratory mechanics."
  ;; Update respiratory phase
  (let* ((cycle-duration (/ 60.0 (respiratory-rate patient)))
         (time-increment (/ dt cycle-duration)))
    (incf (respiratory-phase patient) time-increment)
    (when (>= (respiratory-phase patient) 1.0)
      (decf (respiratory-phase patient) 1.0)))
  
  ;; Condition inspired gas to body temperature and full humidity
  (let ((conditioned-stream 
         (if (typep input-stream 'gas-stream)
             (make-instance 'gas-stream
                           :flow-rate (flow-rate input-stream)
                           :fractions (clone-hash (fractions input-stream))
                           :temperature (alveolar-temp patient)
                           :pressure (pressure input-stream)
                           :humidity (airway-humidity patient))
             ;; Convert basic gas-stream for internal processing
             (make-instance 'gas-stream
                           :flow-rate (flow-rate input-stream)
                           :fractions (clone-hash (fractions input-stream))
                           :temperature (alveolar-temp patient)
                           :pressure *standard-pressure*
                           :humidity (airway-humidity patient)))))
    
    ;; Process through lung gas exchange (calls compute-target-fractions)
    (let ((output (call-next-method patient conditioned-stream dt)))
      ;; Expired gas is fully saturated and at body temperature
      (make-instance 'gas-stream
                    :flow-rate (flow-rate output)
                    :fractions (clone-hash (fractions output))
                    :temperature (alveolar-temp patient)
                    :pressure (if (typep input-stream 'gas-stream)
                                 (pressure input-stream)
                                 *standard-pressure*)
                    :humidity (saturated-rh-at (alveolar-temp patient))))))

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

;; (defgeneric calculate-alveolar-ventilation (patient)
;;   (:documentation "Calculate effective alveolar ventilation (L/min)")
;;   (:method ((patient patient))
;;     (let* ((minute-vent (* (tidal-volume patient) (respiratory-rate patient) 0.001))
;;            (dead-space-vent (* (anatomical-dead-space patient) (respiratory-rate patient) 0.001)))
;;       (max 0.1 (- minute-vent dead-space-vent)))))

;;; Convenience functions for patient monitoring

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
