;;; Rebreather Systems

(in-package :anesthesia-sim)

(defclass volume-reflector (dynamic-mixin gas-component)
  ((reflector-volume :initarg :reflector-volume :accessor reflector-volume 
                     :initform 500.0 :documentation "Reflector bag volume (mL)")
   (compliance :initarg :compliance :accessor compliance :initform 0.02
               :documentation "Volume compliance (mL/cmH2O)")
   (current-volume :accessor current-volume :initform 0.0
                   :documentation "Current gas volume in bag (mL)")
   (pressure-relief :initarg :pressure-relief :accessor pressure-relief 
                    :initform 30.0 :documentation "Pressure relief valve (cmH2O)")))

(defmethod process-gas ((vr volume-reflector) input-stream dt)
  "Volume reflector acts as pressure buffer and gas reservoir."
  (let* ((input-flow (flow-rate input-stream)) ; L/min
         (input-vol (* input-flow dt (/ 1000.0 60.0))) ; convert to mL
         (new-volume (+ (current-volume vr) input-vol))
         (max-volume (reflector-volume vr))
         (excess-vol (max 0.0 (- new-volume max-volume)))
	 (Tin (temperature input-stream))
         (Pin (pressure input-stream))
         (Hin (humidity input-stream)))
    
    ;; Limit volume and calculate pressure
    (setf (current-volume vr) (min new-volume max-volume))
    (let* ((pressure (/ (current-volume vr) (compliance vr))) ; cmH2O
           (relief-flow (if (> pressure (pressure-relief vr))
                           (/ excess-vol dt (/ 1000.0 60.0)) ; back to L/min
                           0.0))
           (output-flow (- input-flow relief-flow)))
      
      (make-instance 'gas-stream
                    :flow-rate (max 0.0 output-flow)
                    :fractions (clone-hash (fractions input-stream))
		    :temperature Tin :pressure Pin :humidity Hin))))

(defclass volume-exchanger (dynamic-mixin gas-component)
  ((exchanger-volume :initarg :exchanger-volume :accessor exchanger-volume
                     :initform 800.0 :documentation "Exchanger volume (mL)")
   (exchange-efficiency :initarg :exchange-efficiency :accessor exchange-efficiency
                        :initform 0.85 :documentation "Gas exchange efficiency")
   (stored-mixture :accessor stored-mixture
                   :initform (make-hash-table :test 'equal)
                   :documentation "Stored gas mixture in exchanger")))

(defmethod compute-target-fractions ((ve volume-exchanger) input-stream)
  "Volume exchanger provides more complete gas mixing."
  (let* ((input-fractions (fractions input-stream))
         (stored-fractions (stored-mixture ve))
         (efficiency (exchange-efficiency ve))
         (target (make-hash-table :test 'equal)))
    
    ;; More complete mixing than simple dead space
    (maphash (lambda (gas input-frac)
               (let ((stored-frac (gethash gas stored-fractions 0.0)))
                 (setf (gethash gas target)
                       (+ (* (- 1.0 efficiency) stored-frac)
                          (* efficiency input-frac)))))
             input-fractions)
    
    ;; Handle gases only in stored mixture
    (maphash (lambda (gas stored-frac)
               (unless (gethash gas target)
                 (setf (gethash gas target) (* (- 1.0 efficiency) stored-frac))))
             stored-fractions)
    
    ;; Update stored mixture
    (setf (stored-mixture ve) (clone-hash target))
    (renormalize-fractions! target)))

(defmethod effective-tau ((ve volume-exchanger) input-stream dt)
  "Moderate mixing time in volume exchanger."
  (declare (ignore input-stream dt))
  1.5)
