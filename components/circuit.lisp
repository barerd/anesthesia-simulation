;;; Gas mixer / inspiratory node (dynamic)

(in-package :anesthesia-sim)

(defclass gas-mixer (dynamic-mixin gas-component)
  ((volume :initarg :volume :accessor volume :initform 2.0 :documentation "Node volume (L)")))

;; default compute-target-fractions is pass-through (target = mixed upstream)
(defmethod effective-tau ((m gas-mixer) input-stream dt)
  (declare (ignore dt))
  (let* ((V (max 1e-6 (volume m)))
         (Q (max 1e-6 (flow-rate input-stream))))
    (max 1e-3 (* 60.0 (/ V Q)))))

;;; CO2 absorber (dynamic with capacity)

(defclass co2-absorber (dynamic-mixin gas-component)
  ((efficiency :initarg :efficiency :accessor efficiency :initform 0.95
               :documentation "Max instantaneous CO2 removal fraction (0..1).")
   (capacity  :initarg :capacity  :accessor capacity  :initform 1000.0
               :documentation "Total CO2 capacity (g).")
   (used      :initarg :used      :accessor used      :initform 0.0
               :documentation "CO2 absorbed so far (g).")
   (volume    :initarg :volume    :accessor volume    :initform 1.0
               :documentation "Effective absorber volume (L).")))

(defmethod compute-target-fractions ((abs co2-absorber) input-stream)
  "Compute the *instantaneous* target mixture after CO2 removal,
   limited by efficiency. Capacity is enforced in post-process-dynamics."
  (let* ((fin (fractions input-stream))
         (co2-in (gethash "CO2" fin 0.0))
         ;; target removal fraction, purely kinetic (capacity checked later)
         (rem-frac (min 1.0 (max 0.0 (efficiency abs))))
         (co2-out (* co2-in (- 1.0 rem-frac)))
         (target (clone-hash fin)))
    (setf (gethash "CO2" target) co2-out)
    ;; renormalize: freed fraction becomes N2 (inert placeholder here)
    (renormalize-fractions! target)))

(defmethod effective-tau ((abs co2-absorber) input-stream dt)
  (declare (ignore dt))
  "τ = 60 * V / Q (seconds)."
  (let* ((V (max 1e-6 (volume abs)))
         (Q (max 1e-6 (flow-rate input-stream))))
    (max 1e-3 (* 60.0 (/ V Q)))))

(defmethod post-process-dynamics ((abs co2-absorber) input-stream output-stream dt)
  "Update absorber capacity based on actual CO2 removed this step.
   If capacity is exhausted, reduce efficiency to 0 (i.e., no further removal)."
  (let* ((Qin   (flow-rate input-stream))          ; L/min
         (fin   (fractions input-stream))
         (fout  (fractions output-stream))
         (co2-in  (gethash "CO2" fin  0.0))
         (co2-out (gethash "CO2" fout 0.0))
         (delta-frac (max 0.0 (- co2-in co2-out)))
         ;; CO2 density ~1.977 g/L at STPD; convert removed fraction to g/min
         (co2-removed-g-per-min (* Qin delta-frac 1.977))
         (removed-this-step (* co2-removed-g-per-min (max 0.0 dt)))
         (remaining (- (capacity abs) (used abs))))
    (cond
      ((<= remaining 0.0)
       ;; fully exhausted: freeze efficiency at zero
       (setf (efficiency abs) 0.0)
       (setf (used abs) (capacity abs)))
      ((> removed-this-step remaining)
       ;; can only absorb what's left; push used to capacity and kill efficiency
       (setf (used abs) (capacity abs))
       (setf (efficiency abs) 0.0))
      (t
       (incf (used abs) removed-this-step)))))

;;; One-Way Valve Component

(defclass one-way-valve (gas-component)
  ((name :initarg :name :accessor name)
   ;; :inspiratory OR :expiratory
   (role :initarg :role :accessor role :initform :inspiratory)
   (cracking-pressure :initarg :cracking-pressure :accessor cracking-pressure 
                      :initform 0.1 :documentation "Minimum pressure to open (cmH2O)")
   (resistance :initarg :resistance :accessor resistance 
               :initform 0.1 :documentation "Flow resistance when open")
   (is-open :accessor is-open :initform nil :documentation "Current valve state")))

(defmethod process-gas ((valve one-way-valve) input-stream dt)
  "Allow flow only in forward direction, with pressure-dependent opening."
  (declare (ignore dt))
  (let ((flow (flow-rate input-stream))
	(Tin (temperature input-stream))
         (Pin (pressure input-stream))
         (Hin (humidity input-stream)))
    (cond
      ;; Reverse flow - block completely
      ((< flow 0)
       (setf (is-open valve) nil)
       (make-instance 'gas-stream :flow-rate 0.0))
      
      ;; Forward flow - check if enough pressure to open
      ((> flow 0)
       ;; Simplified: assume pressure proportional to flow
       ;; In reality, you'd calculate ΔP from flow and upstream pressure
       (let ((estimated-pressure (* flow 0.5))) ; rough approximation
         (if (>= estimated-pressure (cracking-pressure valve))
             (progn
               (setf (is-open valve) t)
               ;; Reduce flow slightly due to resistance
               (let ((reduced-flow (* flow (- 1.0 (resistance valve)))))
                 (make-instance 'gas-stream 
                               :flow-rate (max 0.0 reduced-flow)
                               :fractions (clone-hash (fractions input-stream))
			       :temperature Tin :pressure Pin :humidity Hin)))
             (progn
               (setf (is-open valve) nil)
               (make-instance 'gas-stream :flow-rate 0.0)))))
      
      ;; Zero flow
      (t input-stream))))

(defmethod process-gas :around ((valve one-way-valve) input-stream dt)
  "Hard gate by role + flow sign before the primary method runs."
  (declare (ignore dt))
  (let ((flow (and input-stream (flow-rate input-stream))))
    ;; For our simulator, 'forward' through both valves is flow >= 0.
    ;; Any negative flow trying to go back through the valve is blocked here.
    (when (and flow (< flow 0.0))
      (setf (is-open valve) nil)
      (return-from process-gas (make-instance 'gas-stream :flow-rate 0.0))))
  ;; If we didn’t return above, let your primary method do the pressure/ΔP logic.
  (call-next-method))

;;; Capnometer sensor

(defclass capnometer-port ()
  ((name :initarg :name :accessor name :initform "capnometer-exp-port")))

(defmethod process-gas ((cap capnometer-port) input-stream dt)
  (declare (ignore dt))
  (if (and input-stream (typep input-stream 'gas-stream))
      (make-instance 'gas-stream
        :flow-rate 0.0
        :fractions (clone-hash (fractions input-stream))
        :temperature (temperature input-stream)
        :pressure    (pressure    input-stream)
        :humidity    (humidity    input-stream))
      ;; No input? Still return a zero-flow stream so downstream code is happy.
      (make-instance 'gas-stream :flow-rate 0.0)))

;;; Dead Space Modeling

(defclass dead-space (dynamic-mixin gas-component)
  ((dead-volume :initarg :dead-volume :accessor dead-volume :initform 150.0
                :documentation "Dead space volume (mL)")
   (stored-gas :accessor stored-gas 
               :initform (make-hash-table :test 'equal)
               :documentation "Gas mixture stored in dead space")))

(defmethod compute-target-fractions ((ds dead-space) input-stream)
  "Dead space mixes current input with previously stored gas."
  (let* ((input-fractions (fractions input-stream))
         (stored-fractions (stored-gas ds))
         (target (make-hash-table :test 'equal))
         ;; Simple mixing ratio - could be more sophisticated
         (mix-ratio 0.3))
    
    ;; Mix stored and input fractions
    (maphash (lambda (gas input-frac)
               (let ((stored-frac (gethash gas stored-fractions 0.0)))
                 (setf (gethash gas target) 
                       (+ (* (- 1.0 mix-ratio) input-frac)
                          (* mix-ratio stored-frac)))))
             input-fractions)
    
    ;; Update stored gas for next cycle
    (setf (stored-gas ds) (clone-hash target))
    target))

(defmethod effective-tau ((ds dead-space) input-stream dt)
  "Very fast mixing in dead space."
  (declare (ignore input-stream dt))
  0.1)
