;;; Environmental Contamination System

(in-package :anesthesia-sim)

;;; 1) Patient with Environmental Gas Absorption

(defclass environmentally-exposed-patient (patient)
  ((baseline-rr :accessor baseline-rr :initform 12.0)
   (co-absorption-rate :initarg :co-absorption-rate :accessor co-absorption-rate 
                       :initform 0.95 :documentation "CO absorption efficiency")
   (toxin-sensitivity :initarg :toxin-sensitivity :accessor toxin-sensitivity
                      :initform 1.0 :documentation "Sensitivity to environmental toxins")
   (carboxyhemoglobin :accessor carboxyhemoglobin :initform 0.0
                      :documentation "COHb percentage")
   (exposure-time :accessor exposure-time :initform 0.0)))


(defmethod initialize-instance :after ((p environmentally-exposed-patient) &key)
  (setf (baseline-rr p) (respiratory-rate p)))

;;; 2) Environmental Gas Source (simulates room air contamination)

(defclass environmental-gas-source (gas-component)
  ((co-level :initarg :co-level :accessor co-level :initform 0.0
             :documentation "CO concentration in room air (fraction)")
   (smoke-particles :initarg :smoke-particles :accessor smoke-particles :initform 0.0)
   (benzene-level :initarg :benzene-level :accessor benzene-level :initform 0.0)
   (h2s-level :initarg :h2s-level :accessor h2s-level :initform 0.0)
   (contamination-rate :initarg :contamination-rate :accessor contamination-rate :initform 0.0
                       :documentation "Rate of room air leaking into circuit (L/min)")
   (fire-intensity :initarg :fire-intensity :accessor fire-intensity :initform 0.0
                   :documentation "Fire intensity (0-1) affecting gas production")))

(defmethod process-gas ((env-source environmental-gas-source) input-stream dt)
  "Add environmental contamination to gas stream."
  (if (and (> (contamination-rate env-source) 0)
           (> (fire-intensity env-source) 0))
      ;; Create contaminated air stream
      (let* ((contaminated-flow (contamination-rate env-source))
             (contaminated-fractions (make-hash-table :test 'equal))
             ;; Base air composition
             (base-o2 0.21)
             (base-n2 0.79))
        
        ;; Set base gases
        (setf (gethash "O2" contaminated-fractions) base-o2)
        (setf (gethash "N2" contaminated-fractions) base-n2)
        
        ;; Add fire-related toxins based on intensity
        (let ((intensity (fire-intensity env-source)))
          (setf (gethash "CO" contaminated-fractions) 
                (* (co-level env-source) intensity))
          (setf (gethash "Benzene" contaminated-fractions)
                (* (benzene-level env-source) intensity))
          (setf (gethash "H2S" contaminated-fractions)
                (* (h2s-level env-source) intensity))
          
          ;; Reduce O2 due to combustion
          (setf (gethash "O2" contaminated-fractions)
                (max 0.10 (- base-o2 (* intensity 0.05))))
          
          ;; Add CO2 from combustion
          (setf (gethash "CO2" contaminated-fractions)
                (* intensity 0.02)))
        
        (renormalize-fractions! contaminated-fractions)
        
        ;; Create contaminated stream
        (let ((contam-stream (make-instance 'gas-stream
                                           :flow-rate contaminated-flow
                                           :fractions contaminated-fractions)))
          ;; Mix with input stream if present
          (if input-stream
              (mix-streams (list input-stream contam-stream))
              contam-stream)))
      ;; No contamination - pass through
      (or input-stream (make-instance 'gas-stream :flow-rate 0.0))))

;;; 3) Fresh Gas Inlet with leak mixing

(defclass contaminated-fresh-gas-inlet (fresh-gas-inlet)
  ((room-air-leak :initarg :room-air-leak :accessor room-air-leak :initform 0.0
                  :documentation "Room air leakage rate (L/min)")
   (environmental-source :initarg :environmental-source :accessor environmental-source
                         :initform nil :documentation "Environmental contamination source")))

(defmethod process-gas :around ((fgf contaminated-fresh-gas-inlet) input-stream dt)
  "Add environmental contamination to fresh gas flow."
  (let ((clean-output (call-next-method)))
    (if (and (environmental-source fgf) (> (room-air-leak fgf) 0))
        ;; Mix clean gas with contaminated room air
        (let* ((env-source (environmental-source fgf))
               (old-rate (contamination-rate env-source)))
          ;; Temporarily set contamination rate to room air leak rate
          (setf (contamination-rate env-source) (room-air-leak fgf))
          (let ((contaminated-air (process-gas env-source nil dt)))
            ;; Restore original rate
            (setf (contamination-rate env-source) old-rate)
            ;; Mix streams
            (if contaminated-air
                (mix-streams (list clean-output contaminated-air))
                clean-output)))
        clean-output)))

;;; 4) Methods specialized on environmentally-exposed-patient

(defmethod compute-target-fractions ((patient environmentally-exposed-patient) input-stream)
  "Enhanced gas exchange with environmental toxin handling."
  (let* ((input-fractions (fractions input-stream))
         (target (clone-hash input-fractions))
         ;; Standard gas exchange
         (o2-in (gethash "O2" target 0.21))
         (co2-in (gethash "CO2" target 0.0))
         (minute-ventilation (* (tidal-volume patient) (respiratory-rate patient) 0.001))
         (o2-uptake-fraction (min 0.05 (/ (o2-consumption patient) 1000.0 minute-ventilation)))
         (co2-addition-fraction (min 0.06 (/ (co2-production patient) 1000.0 minute-ventilation)))
         
         ;; Environmental toxin handling
         (co-in (gethash "CO" target 0.0))
         (benzene-in (gethash "Benzene" target 0.0))
         (h2s-in (gethash "H2S" target 0.0)))
    
    ;; Standard gas exchange (but impaired by CO)
    (let* ((co-impairment (min 0.8 (* (carboxyhemoglobin patient) 0.01)))
           (effective-o2-uptake (* o2-uptake-fraction (- 1.0 co-impairment)))
           (o2-out (max 0.12 (- o2-in effective-o2-uptake)))
           (co2-out (min 0.08 (+ co2-in co2-addition-fraction))))
      
      (setf (gethash "O2" target) o2-out)
      (setf (gethash "CO2" target) co2-out))
    
    ;; CO absorption and retention
    (when (> co-in 1e-6)
      (let* ((co-absorbed (* co-in (co-absorption-rate patient)))
             ;; CO has 200x affinity for Hb - very little eliminated
             (co-retained (* co-absorbed 0.98))
             (co-exhaled (* co-absorbed 0.02)))
        ;; Update carboxyhemoglobin level
        (incf (carboxyhemoglobin patient) (* co-retained 100 (toxin-sensitivity patient)))
        (setf (carboxyhemoglobin patient) (min 80.0 (carboxyhemoglobin patient)))
        ;; Very little CO is exhaled
        (setf (gethash "CO" target) co-exhaled)))
    
    ;; Benzene and H2S - partially absorbed, rest exhaled
    (when (> benzene-in 1e-6)
      (let ((benzene-absorbed (* benzene-in 0.7 (toxin-sensitivity patient))))
        (setf (gethash "Benzene" target) (- benzene-in benzene-absorbed))))
    
    (when (> h2s-in 1e-6)
      (let ((h2s-absorbed (* h2s-in 0.6 (toxin-sensitivity patient))))
        (setf (gethash "H2S" target) (- h2s-in h2s-absorbed))))
    
    (renormalize-fractions! target)))

(defmethod post-process-dynamics ((patient environmentally-exposed-patient) input-stream output-stream dt)
  "Update exposure time and physiological effects."
  (call-next-method)
  (incf (exposure-time patient) dt)
  
  ;; CO poisoning effects on respiratory rate
  (when (> (carboxyhemoglobin patient) 10.0)
    (let* ((co-effect (min 2.0 (/ (carboxyhemoglobin patient) 20.0)))
	   (target (* (baseline-rr patient) (+ 1.0 co-effect))))
      ;; Increase respiratory rate due to hypoxia
      (setf (respiratory-rate patient) 
            (min 30.0 target)))))

;;; Factory Function for Fire Emergency Scenario

(defun simulate-fire-emergency (circuit &key (fire-intensity 0.5) (room-air-leak 0.1))
  "Add fire emergency contamination to existing circuit."
  ;; Create environmental contamination source
  (let ((env-source (make-instance 'environmental-gas-source
                                   :name "room-fire-contamination"
                                   :co-level 0.005      ; 0.5% CO (very dangerous)
                                   :benzene-level 0.0001 ; 100 ppm benzene
                                   :h2s-level 0.00005   ; 50 ppm H2S
                                   :fire-intensity fire-intensity
                                   :contamination-rate room-air-leak)))
    
    ;; Find and replace fresh gas inlet with contaminated version
    (let ((old-fgf (find "FGF" (components circuit) :key #'name :test #'string=)))
      (when old-fgf
        (let ((new-fgf (make-instance 'contaminated-fresh-gas-inlet
                                      :name "FGF"
                                      :o2-flow (o2-flow old-fgf)
                                      :air-flow (air-flow old-fgf)
                                      :n2o-flow (n2o-flow old-fgf)
                                      :room-air-leak room-air-leak
                                      :environmental-source env-source)))
          ;; Replace in components list
          (setf (components circuit)
                (substitute new-fgf old-fgf (components circuit)))
          
          ;; Update connections
          (dolist (conn (connection-objects circuit))
            (when (eq (from-component conn) old-fgf)
              (setf (from-component conn) new-fgf))
            (when (eq (to-component conn) old-fgf)
              (setf (to-component conn) new-fgf))))))
    
    ;; Replace patient with environmentally-exposed version
    (let ((old-patient (find-if (lambda (comp) (typep comp 'patient))
                               (components circuit))))
      (when old-patient
        (let ((new-patient (make-instance 'environmentally-exposed-patient
                                          :name (name old-patient)
                                          :tidal-volume (tidal-volume old-patient)
                                          :respiratory-rate (respiratory-rate old-patient)
                                          :co2-production (co2-production old-patient)
                                          :o2-consumption (o2-consumption old-patient))))
          (setf (components circuit)
                (substitute new-patient old-patient (components circuit)))
          
          ;; Update connections
          (dolist (conn (connection-objects circuit))
            (when (eq (from-component conn) old-patient)
              (setf (from-component conn) new-patient))
            (when (eq (to-component conn) old-patient)
              (setf (to-component conn) new-patient))))))
    
    (format t "Fire emergency simulation activated:~%")
    (format t "  Fire intensity: ~A~%" fire-intensity)
    (format t "  Room air leak: ~A L/min~%" room-air-leak)
    (format t "  CO level: ~,3F%~%" (* (co-level env-source) fire-intensity 100))
    circuit))

;;; Monitoring Functions

(defun monitor-patient-toxin-exposure (circuit)
  "Monitor patient for environmental toxin exposure."
  (let ((patient (find-if (lambda (comp) (typep comp 'environmentally-exposed-patient))
                         (components circuit))))
    (if patient
        (progn
          (format t "~&=== Patient Toxin Exposure ===~%")
          (format t "Carboxyhemoglobin: ~,1F%~%" (carboxyhemoglobin patient))
          (format t "Exposure time: ~,1F seconds~%" (exposure-time patient))
          (format t "Current RR: ~,1F bpm (baseline: ~,1F)~%" 
                  (respiratory-rate patient) 12.0)
          (cond
            ((> (carboxyhemoglobin patient) 50) 
             (format t "STATUS: CRITICAL - Severe CO poisoning~%"))
            ((> (carboxyhemoglobin patient) 25) 
             (format t "STATUS: SEVERE - Significant CO poisoning~%"))
            ((> (carboxyhemoglobin patient) 10) 
             (format t "STATUS: MODERATE - CO poisoning detected~%"))
            ((> (carboxyhemoglobin patient) 3) 
             (format t "STATUS: MILD - CO exposure~%"))
            (t (format t "STATUS: Normal COHb levels~%"))))
        (format t "Patient not environmentally-exposed type~%"))))

;;; Usage Examples

(defun fire-emergency-example ()
  "Example of fire emergency simulation."
  (format t "~&=== Fire Emergency Simulation Example ===~%")
  (format t "1. Start with normal circuit:~%")
  (format t "   (setf circuit (make-circle-system :patient-type 'healthy))~%")
  (format t "~%2. Simulate fire emergency:~%")
  (format t "   (simulate-fire-emergency circuit :fire-intensity 0.7 :room-air-leak 0.2)~%")
  (format t "~%3. Start simulation and monitor:~%")
  (format t "   (start-simulation circuit :dt 1.0)~%")
  (format t "   (monitor-patient-toxin-exposure circuit)~%")
  (format t "   (monitor-all-gases circuit)~%")
  (format t "~%4. Check for toxins in expired air:~%")
  (format t "   (debug-all-gases circuit \"patient-name\")~%"))
