;;; Base protocol + classes

(in-package :anesthesia-sim)

(defgeneric process-gas (component input-stream dt)
  (:documentation "Process input gas stream through COMPONENT over timestep DT (s)."))

(defgeneric component-connections (component)
  (:documentation "Return list of connection points for this component"))

(defclass gas-component ()
  ((name    :initarg :name    :accessor name    :initform "unnamed")
   (enabled :initarg :enabled :accessor enabled :initform t)))

;; Default: pass-through (memoryless)
(defmethod process-gas ((component gas-component) input-stream dt)
  (declare (ignore component dt))
  input-stream)

;;; Dynamic mixin (τ dynamics + hooks)

(defclass dynamic-mixin ()
  ((tau           :initarg :tau :accessor tau :initform 3.0
                  :documentation "Base time constant (s). Often unused when effective-tau uses V/Q.")
   (out-fractions :accessor out-fractions
                  :initform (make-hash-table :test 'equal)
                  :documentation "Component's internal mixture state.")))

(defgeneric compute-target-fractions (component input-stream)
  (:documentation
   "Return a HASH-TABLE of fractions for the steady-state output given INPUT-STREAM."))

(defgeneric effective-tau (component input-stream dt)
  (:documentation
   "Return the time constant (s) to use this step. Default: slot TAU."))

(defgeneric post-process-dynamics (component input-stream output-stream dt)
  (:documentation
   "Optional post-step hook for components to update internal state (e.g., capacity)."))

;; defaults
(defmethod compute-target-fractions ((c dynamic-mixin) input-stream)
  (declare (ignore c))
  (clone-hash (fractions input-stream)))  ; pass-through target by default

(defmethod effective-tau ((c dynamic-mixin) input-stream dt)
  (declare (ignore input-stream dt))
  (max 1e-6 (tau c)))

(defmethod post-process-dynamics ((c dynamic-mixin) input-stream output-stream dt)
  (declare (ignore c input-stream output-stream dt))
  nil)

;; One around method to provide dynamics for *all* dynamic components
(defmethod process-gas :around ((c dynamic-mixin) input-stream dt)
  "Blend stored OUT-FRACTIONS toward steady-state target with τ dynamics, 
   preserving input stream temperature, pressure, and humidity."
  (let* ((flow   (flow-rate input-stream))
         (target (compute-target-fractions c input-stream))
         ;; Preserve input stream environmental properties
         (Tin (if (typep input-stream 'gas-stream)
                  (temperature input-stream)
                  *standard-temp*))
         (Pin (if (typep input-stream 'gas-stream)
                  (pressure input-stream)
                  *standard-pressure*))
         (Hin (if (typep input-stream 'gas-stream)
                  (humidity input-stream)
                  0.0)))
    
    (renormalize-fractions! target)
    
    ;; Initialize out-fractions if empty
    (when (zerop (hash-table-count (out-fractions c)))
      (setf (out-fractions c) (clone-hash target)))
    
    ;; Apply exponential approach to target
    (let* ((tau   (effective-tau c input-stream dt))
           (alpha (if (< (/ dt tau) 1e-6)
                      (/ dt tau)  ; linear approximation for small ratios
                      (- 1.0 (exp (/ (- dt) tau))))))
      (lerp-fractions! (out-fractions c) target alpha))
    
    ;; Create output stream with preserved environmental properties
    (let ((out (make-instance 'gas-stream
                              :flow-rate flow
                              :fractions (clone-hash (out-fractions c))
                              :temperature Tin
                              :pressure Pin
                              :humidity Hin)))
      (post-process-dynamics c input-stream out dt)
      out)))

;;; Temperature-Dependent Component Mixin

(defclass thermal-component ()
  ((operating-temp :initarg :operating-temp :accessor operating-temp 
                   :initform *standard-temp*
                   :documentation "Component operating temperature (°C)")
   (thermal-mass :initarg :thermal-mass :accessor thermal-mass :initform 1.0
                 :documentation "Thermal mass (relative units)")
   (heat-exchange-coeff :initarg :heat-exchange-coeff :accessor heat-exchange-coeff
                        :initform 0.1
                        :documentation "Heat exchange coefficient")))

(defmethod update-temperature ((comp thermal-component) input-stream dt)
  "Update component temperature based on input gas temperature."
  (let* ((input-temp (if (typep input-stream 'gas-stream)
                         (temperature input-stream)
                         *standard-temp*))
         (temp-diff (- input-temp (operating-temp comp)))
         (temp-change (* (heat-exchange-coeff comp) temp-diff dt)))
    (incf (operating-temp comp) temp-change)))
