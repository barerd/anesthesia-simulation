;;; Pathological Patient Extensions

(in-package :anesthesia-sim)

;;; Enhanced Patient Class with Pathological Gas Exchange

(defclass pathological-patient (patient)
  ((co-production :initarg :co-production :accessor co-production :initform 0.0
                  :documentation "CO production rate (mL/min) - heavy smoker")
   (ethanol-elimination :initarg :ethanol-elimination :accessor ethanol-elimination :initform 0.0
                        :documentation "Ethanol elimination rate (fraction/min)")
   (acetone-production :initarg :acetone-production :accessor acetone-production :initform 0.0
                       :documentation "Acetone production rate (fraction/min) - ketosis")
   (liver-function :initarg :liver-function :accessor liver-function :initform 1.0
                   :documentation "Liver function efficiency (0-1) for ethanol metabolism")
   (smoking-history :initarg :smoking-history :accessor smoking-history :initform 0
                    :documentation "Pack-years smoking history")))

(defmethod compute-target-fractions ((patient pathological-patient) input-stream)
  "Enhanced gas exchange with pathological additions/consumptions."
  (let* ((input-fractions (fractions input-stream))
         (target (clone-hash input-fractions))
         ;; Standard gas exchange (from parent class)
         (o2-in (gethash "O2" target 0.21))
         (co2-in (gethash "CO2" target 0.0))
         (minute-ventilation (* (tidal-volume patient) (respiratory-rate patient) 0.001))
         (o2-uptake-fraction (min 0.05 (/ (o2-consumption patient) 1000.0 minute-ventilation)))
         (co2-addition-fraction (min 0.06 (/ (co2-production patient) 1000.0 minute-ventilation)))
         (o2-out (max 0.16 (- o2-in o2-uptake-fraction)))
         (co2-out (min 0.06 (+ co2-in co2-addition-fraction)))
         
         ;; Pathological gas handling
         (co-in (gethash "CO" target 0.0))
         (ethanol-in (gethash "EtOH" target 0.0))
         (acetone-in (gethash "Acetone" target 0.0)))
    
    ;; Standard gas exchange
    (setf (gethash "O2" target) o2-out)
    (setf (gethash "CO2" target) co2-out)
    
    ;; CO handling (production from smoking + reduced elimination due to high affinity)
    (when (> (co-production patient) 0)
      (let* ((co-addition-fraction (/ (co-production patient) 1000.0 minute-ventilation))
             ;; CO has ~200x affinity for Hb vs O2, so most stays in blood
             (co-retained-fraction 0.95) ; Very little eliminated via lungs
             (co-eliminated (* co-in (- 1.0 co-retained-fraction)))
             (co-final (+ co-eliminated co-addition-fraction)))
        (setf (gethash "CO" target) (min 0.01 co-final)))) ; Cap at 1% (lethal levels)
    
    ;; Ethanol elimination (primarily hepatic, some pulmonary)
    (when (and (> ethanol-in 1e-6) (> (ethanol-elimination patient) 0))
      (let* ((hepatic-elimination (* (ethanol-elimination patient) (liver-function patient)))
             ;; Small fraction eliminated through lungs (breath alcohol)
             (pulmonary-elimination-fraction 0.05)
             (total-elimination-rate (* hepatic-elimination pulmonary-elimination-fraction))
             (ethanol-out (* ethanol-in (- 1.0 total-elimination-rate))))
        (setf (gethash "EtOH" target) (max 0.0 ethanol-out))))
    
    ;; Acetone production (ketosis - diabetic ketoacidosis or starvation)
    (when (> (acetone-production patient) 0)
      (let* ((acetone-addition-fraction (/ (acetone-production patient) minute-ventilation))
             ;; Acetone is readily eliminated through lungs (fruity breath odor)
             (acetone-elimination-fraction 0.8) ; Efficiently eliminated
             (acetone-net (* acetone-in (- 1.0 acetone-elimination-fraction)))
             (acetone-final (+ acetone-net acetone-addition-fraction)))
        (setf (gethash "Acetone" target) (min 0.001 acetone-final)))) ; Cap at 0.1%
    
    (renormalize-fractions! target)))

;;; Factory Functions for Clinical Scenarios

(defun make-heavy-smoker-patient (&key (pack-years 20) (co-level 0.0005))
  "Create a patient with CO poisoning from heavy smoking."
  (let* ((co-production-rate (* pack-years 2.0))
         (patient (make-instance 'pathological-patient
                                 :name "heavy-smoker-patient"
                                 :co-production co-production-rate
                                 :smoking-history pack-years
                                 :o2-consumption 200.0
                                 :anatomical-dead-space 150.0)))
    ;; Initialize with elevated CO levels in expired air
    (setf (gethash "CO" (out-fractions patient)) co-level)
    patient))

(defun make-copd-patient
       (&key (severity :moderate) (pack-years 40) (co-level 0.0005)
             (bw-kg 70.0))
  "Create a COPD phenotype with ↑physiologic dead space, ↑FRC (hyperinflation), and optional CO exposure."
  (destructuring-bind (vdphys frc sh rr vt)
      (ecase severity
        (:mild     (list 0.35 3000 0.08 12 (round (* 7.0 bw-kg))))   ; ~7 mL/kg
        (:moderate (list 0.45 3500 0.10 12 (round (* 7.0 bw-kg))))
        (:severe   (list 0.55 4000 0.15 10 (round (* 6.5 bw-kg)))))
    (let* ((co-prod-rate (* pack-years 2.0))
           (p (make-instance 'pathological-patient
                             :name "copd-patient"
                             :age-years 65
                             :body-weight-kg bw-kg
                             :respiratory-rate rr
                             :tidal-volume vt
                             :o2-consumption 200.0
                             :co2-production 200.0
                             :anatomical-dead-space 150.0
                             :vdphys-frac vdphys
                             :frc frc
                             :shunt-frac sh
                             :co-production co-prod-rate
                             :smoking-history pack-years)))
      ;; baseline CO in expired air (optional)
      (setf (gethash "CO" (out-fractions p)) co-level)
      p)))

(defun make-ards-patient
       (&key (severity :moderate) (bw-kg 70.0))
  "Create an ARDS phenotype with ↑shunt, modest ↑VDphys, and ↓FRC; use lung-protective VT (~6 mL/kg)."
  (destructuring-bind (vdphys sh frc rr vt)
      (ecase severity
        (:mild     (list 0.30 0.10 1800 20 (round (* 6.0 bw-kg))))
        (:moderate (list 0.40 0.20 1500 24 (round (* 6.0 bw-kg))))
        (:severe   (list 0.50 0.35 1200 28 (round (* 5.5 bw-kg)))))
    (make-instance 'pathological-patient
                   :name "ards-patient"
                   :age-years 55
                   :body-weight-kg bw-kg
                   :respiratory-rate rr
                   :tidal-volume vt
                   :o2-consumption 220.0
                   :co2-production 180.0
                   :anatomical-dead-space 150.0
                   :vdphys-frac vdphys
                   :shunt-frac sh
                   :frc frc)))

(defun make-alcohol-intoxicated-patient (&key (blood-alcohol 0.15) (liver-damage 0.0))
  "Create an alcohol-intoxicated patient."
  (let* ((ethanol-fraction (* blood-alcohol 0.0001)) ; Convert to realistic breath levels
         (liver-efficiency (- 1.0 liver-damage))
         (patient (make-instance 'pathological-patient
                                 :name "alcohol-intoxicated-patient"
                                 :ethanol-elimination 0.02
                                 :liver-function liver-efficiency
                                 :respiratory-rate 10.0
                                 :tidal-volume 450.0)))
    ;; Initialize with ethanol in expired air
    (setf (gethash "EtOH" (out-fractions patient)) ethanol-fraction)
    patient))

(defun make-ketotic-patient (&key (ketosis-severity 'mild))
  "Create a patient in diabetic ketoacidosis or starvation ketosis."
  (let* ((acetone-rates '((mild . 5.0) (moderate . 15.0) (severe . 30.0)))
         (acetone-production (cdr (assoc ketosis-severity acetone-rates)))
         (patient (make-instance 'pathological-patient
                                 :name "ketotic-patient"
                                 :acetone-production acetone-production
                                 :respiratory-rate 20.0
                                 :tidal-volume 600.0
                                 :co2-production 250.0)))
    ;; Initialize with acetone in expired air
    (setf (gethash "Acetone" (out-fractions patient)) 0.0001) ; Start with detectable levels
    patient))

(defun make-healthy-patient ()
  "Create a healthy patient."
  (make-instance 'patient :name "healthy-patient")) 

;;; Diagnostic Functions

(defun diagnose-patient-condition (patient)
  "Analyze patient's expired gas for pathological indicators."
  (when (typep patient 'pathological-patient)
    (let ((expired-fractions (out-fractions patient)))
      (format t "~&=== Patient Condition Analysis ===~%")
      
      ;; CO poisoning check
      (let ((co-level (* 100 (gethash "CO" expired-fractions 0.0))))
        (cond
          ((> co-level 0.5) (format t "CRITICAL: Severe CO poisoning (~,2F%)~%" co-level))
          ((> co-level 0.1) (format t "WARNING: Moderate CO exposure (~,2F%)~%" co-level))
          ((> co-level 0.01) (format t "ALERT: Mild CO exposure (~,2F%)~%" co-level))
          (t (format t "CO levels: Normal (~,3F%)~%" co-level))))
      
      ;; Alcohol intoxication check
      (let ((ethanol-level (* 100 (gethash "EtOH" expired-fractions 0.0))))
        (cond
          ((> ethanol-level 0.08) (format t "ALERT: Legal intoxication detected (~,3F%)~%" ethanol-level))
          ((> ethanol-level 0.02) (format t "Alcohol detected (~,3F%)~%" ethanol-level))
          (t (format t "Ethanol levels: None detected~%"))))
      
      ;; Ketosis check  
      (let ((acetone-level (* 100 (gethash "Acetone" expired-fractions 0.0))))
        (cond
          ((> acetone-level 0.05) (format t "CRITICAL: Severe ketosis - fruity breath (~,3F%)~%" acetone-level))
          ((> acetone-level 0.01) (format t "WARNING: Ketosis detected (~,3F%)~%" acetone-level))
          (t (format t "Acetone levels: Normal (~,4F%)~%" acetone-level)))))))

;;; Clinical Monitoring

(defparameter *environmental-toxins*
  '(("CO" "Carbon Monoxide" 0.001)      ; 0.1% = dangerous
    ("Benzene" "Benzene" 0.00001)       ; 1 ppm = concern
    ("Toluene" "Toluene" 0.0001)        ; 100 ppm = limit
    ("Xylene" "Xylene" 0.0001)          ; 100 ppm = limit
    ("H2S" "Hydrogen Sulfide" 0.00001)  ; 10 ppm = dangerous
    ("SO2" "Sulfur Dioxide" 0.00001)    ; 10 ppm = limit
    ("NH3" "Ammonia" 0.00005))          ; 50 ppm = limit
  "List of (gas-key description danger-threshold)")

(defun monitor-all-gases (circuit &optional (component-name nil))
  "Monitor ALL gases in any component - environmental and pathological."
  (let* ((component (if component-name
                       (find component-name (components circuit) :key #'name :test #'string=)
                       (find-patient-in-circuit circuit)))
         (stream (when component (gethash component (gas-streams circuit)))))
    (if stream
        (let ((fractions (fractions stream)))
          (format t "~&=== Complete Gas Analysis: ~A ===~%" 
                  (if component (name component) "Unknown"))
          (format t "Flow Rate: ~,2F L/min~%" (flow-rate stream))
          
          ;; Standard respiratory gases
          (format t "~%Standard Gases:~%")
          (format t "  O2: ~,2F%~%" (* 100 (gethash "O2" fractions 0.21)))
          (format t "  CO2: ~,2F%~%" (* 100 (gethash "CO2" fractions 0.0)))
          (format t "  N2O: ~,2F%~%" (* 100 (gethash "N2O" fractions 0.0)))
          (format t "  N2: ~,2F%~%" (* 100 (gethash "N2" fractions 0.79)))
          
          ;; Volatile anesthetics
          (let ((agent (find-volatile-agent fractions)))
            (when (not (string= agent "None"))
              (format t "~%Anesthetic Agent:~%")
              (format t "  ~A: ~,2F%~%" agent (* 100 (gethash-volatile-agent fractions)))))
          
          ;; Environmental and pathological toxins
          (format t "~%Toxin Analysis:~%")
          (loop for (gas-key description threshold) in *environmental-toxins*
                for level = (gethash gas-key fractions 0.0)
                when (> level 1e-8) do ; Show anything detectable
                  (format t "  ~A (~A): ~,4F% ~A~%" 
                          description gas-key (* 100 level)
                          (if (> level threshold) "*** DANGER ***" "")))
          
          ;; Pathological metabolites
          (let ((etoh (gethash "EtOH" fractions 0.0))
                (acetone (gethash "Acetone" fractions 0.0)))
            (when (or (> etoh 1e-6) (> acetone 1e-6))
              (format t "~%Metabolic Indicators:~%")
              (when (> etoh 1e-6)
                (format t "  Ethanol: ~,4F% (~A)~%" 
                        (* 100 etoh)
                        (cond ((> etoh 0.001) "Severe intoxication")
                              ((> etoh 0.0005) "Legal intoxication")
                              (t "Alcohol detected"))))
              (when (> acetone 1e-6)
                (format t "  Acetone: ~,4F% (~A)~%" 
                        (* 100 acetone)
                        (cond ((> acetone 0.0005) "Severe ketosis")
                              ((> acetone 0.0001) "Moderate ketosis") 
                              (t "Mild ketosis"))))))
          
          ;; Show ANY other gases present
          (format t "~%Other Detected Gases:~%")
          (let ((known-gases '("O2" "CO2" "N2O" "N2" "H2O" "EtOH" "Acetone" 
                              "CO" "Benzene" "Toluene" "Xylene" "H2S" "SO2" "NH3"
                              "sevoflurane" "isoflurane" "desflurane" "halothane")))
            (maphash (lambda (gas fraction)
                       (when (and (> fraction 1e-8)
                                 (not (member gas known-gases :test #'string-equal)))
                         (format t "  Unknown gas \"~A\": ~,6F%~%" gas (* 100 fraction))))
                     fractions)))
        (format t "Component not found or no gas stream~%"))))

(defun available-patient-types ()
  "List all available patient types for make-circle-system."
  (format t "~&=== Available Patient Types ===~%")
  (format t "Primary types:~%")
  (format t "  'healthy~%")
  (format t "  'copd-mild, 'copd-moderate, 'copd-severe, 'copd~%")
  (format t "  'ards-mild, 'ards-moderate, 'ards-severe, 'ards~%")
  (format t "  'ketotic-mild, 'ketotic-moderate, 'ketotic-severe~%")
  (format t "  'smoker~%")
  (format t "  'alcoholic~%")
  (format t "~%Convenience aliases:~%")
  (format t "  'ketotic, 'ketosis -> moderate~%")
  (format t "  'smoking, 'heavy-smoker -> smoker~%")
  (format t "  'alcohol, 'drunk, 'intoxicated -> alcoholic~%")
  (format t "  'copd -> copd-moderate~%")
  (format t "  'ards -> ards-moderate~%")
  (format t "~%Usage: (make-circle-system :patient-type 'ards-severe)~%"))

(defun test-patient-types ()
  "Test all patient type creation."
  (format t "~&=== Testing Patient Type Creation ===~%")
  (dolist (ptype '(healthy ketotic-mild ketotic-moderate ketotic-severe 
                   smoker alcoholic ketotic smoking alcohol))
    (let ((circuit (make-circle-system :patient-type ptype)))
      (let ((patient (find-if (lambda (comp) (typep comp 'patient))
                             (components circuit))))
        (format t "~A -> ~A (~A)~%" ptype (name patient) (type-of patient))))))

;;; Usage Examples

(defun pathological-patient-examples ()
  "Examples of creating and using pathological patients."
  (format t "~&=== Pathological Patient Examples ===~%")
  (format t "1. Heavy smoker with CO poisoning:~%")
  (format t "   (make-heavy-smoker-patient :pack-years 30 :co-level 0.08)~%")
  (format t "~%2. Alcohol intoxication:~%")
  (format t "   (make-alcohol-intoxicated-patient :blood-alcohol 0.20 :liver-damage 0.3)~%")
  (format t "~%3. Diabetic ketoacidosis:~%")
  (format t "   (make-ketotic-patient :ketosis-severity 'severe)~%")
  (format t "~%4. Replace patient in circuit:~%")
  (format t "   (let ((circuit (make-circle-system)))~%")
  (format t "     (setf (components circuit) ~%")
  (format t "           (substitute (make-heavy-smoker-patient) patient ~%")
  (format t "                      (components circuit) :key #'name :test #'string=)))~%")
  (format t "~%5. Monitor during simulation:~%")
  (format t "   (monitor-pathological-gases circuit \"heavy-smoker\")~%")
  (format t "   (diagnose-patient-condition patient)~%"))

