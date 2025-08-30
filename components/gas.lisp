;;; Fresh Gas Inlet (memoryless source)

(in-package :anesthesia-sim)

(defclass fresh-gas-inlet (gas-component)
  ((o2-flow  :initarg :o2-flow  :accessor o2-flow  :initform 2.0)
   (air-flow :initarg :air-flow :accessor air-flow :initform 2.0)
   (n2o-flow :initarg :n2o-flow :accessor n2o-flow :initform 0.0)))

(defmethod process-gas ((inlet fresh-gas-inlet) input-stream dt)
  (declare (ignore input-stream dt))
  (let* ((o2 (o2-flow inlet))
         (air (air-flow inlet))
         (n2o (n2o-flow inlet))
         (total (+ o2 air n2o)))
    (if (<= total 0.0)
        ;; No flow at all
        (make-instance 'gas-stream :flow-rate 0.0 :humidity 0.0)
        (let* (;; Air is ~21% O2, ~79% N2
               (fio2  (/ (+ o2 (* 0.21 air)) total))
               (fin2o (/ n2o total))
               (fin2  (/ (* 0.79 air) total)))
          (make-gas-stream total           ; L/min
                           *standard-temp*
                           *standard-pressure*
                           0.0            ; medical supplies are dry
                           "O2"  fio2
                           "N2"  fin2
                           "N2O" fin2o)))))

;;; Vaporizer with Temperature Effects

(defclass vaporizer (dynamic-mixin thermal-component gas-component)
  ((agent   :initarg :agent   :accessor agent   :initform "sevoflurane")
   (setting :initarg :setting :accessor setting :initform 0.0 :documentation "Vol% dial")
   (open-p  :initarg :open-p  :accessor open-p  :initform nil)
   (volume  :initarg :volume  :accessor volume  :initform 0.3 :documentation "Chamber effective volume (L)")
   (vapor-pressure-coeffs :initarg :vapor-pressure-coeffs 
                          :accessor vapor-pressure-coeffs
                          :initform '(8.087 1750.286 235.0) ; Sevoflurane Antoine coeffs
                          :documentation "Antoine equation coefficients [A B C]")))

(defun sum-fractions (ht)
  (let ((s 0.0)) (maphash (lambda (_ v) (incf s v)) ht) s))

(defmethod compute-target-fractions ((vap vaporizer) input-stream)
  (let* ((fin   (fractions input-stream))
         (sum-in (sum-fractions fin))
         ;; dial is percent, store as fraction:
         (agent-frac (if (open-p vap) (/ (setting vap) 100.0) 0.0)))
    ;; If no incoming gas this step, do nothing (pass-through):
    (when (< sum-in 1e-9)
      (return-from compute-target-fractions (clone-hash fin)))

    (let ((target (make-hash-table :test 'equal)))
      ;; scale existing gases
      (maphash (lambda (gas fr)
                 (setf (gethash gas target) (* fr (- 1.0 agent-frac))))
               fin)
      ;; add agent
      (incf (gethash (agent vap) target 0.0) agent-frac)

      ;; IMPORTANT: return *without* forcing renormalization here.
      ;; The dynamic-mixin :around will renormalize only if sum > 0.
      target)))

(defmethod effective-tau ((vap vaporizer) input-stream dt)
  (declare (ignore dt))
  "τ = 60 * V / Q  (seconds) with guards."
  (let* ((V (max 1e-6 (volume vap)))
         (Q (max 1e-6 (flow-rate input-stream))))
    (max 1e-3 (* 60.0 (/ V Q)))))

(defmethod vapor-pressure-agent ((vap vaporizer) temp-c)
  "Calculate agent vapor pressure at given temperature using Antoine equation."
  (destructuring-bind (A B C) (vapor-pressure-coeffs vap)
    (let ((temp-k (celsius-to-kelvin temp-c)))
      (* 10.0 (expt 10 (- A (/ B (+ temp-k C))))))))

(defmethod process-gas :around ((vap vaporizer) input-stream dt)
  "Vaporizer with temperature-dependent vaporization."
  (update-temperature vap input-stream dt)
  
  (if (not (typep input-stream 'gas-stream))
      (call-next-method) ; Fallback to basic behavior
      
      (let* ((temp (operating-temp vap))
             (pressure (pressure input-stream))
             (sat-vapor-press (vapor-pressure-agent vap temp))
             ;; Temperature correction factor for vaporizer efficiency
             (temp-factor (/ sat-vapor-press (vapor-pressure-agent vap 20.0)))
             (effective-setting (* (setting vap) temp-factor))
             (output-stream (call-next-method)))
        
        ;; Create output stream
        (make-instance 'gas-stream
                      :flow-rate (flow-rate output-stream)
                      :fractions (clone-hash (fractions output-stream))
                      :temperature temp
                      :pressure pressure
                      :humidity (humidity input-stream)))))

