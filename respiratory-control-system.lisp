;;; Centralized Respiratory Control System for Anesthesia Simulation

(in-package :anesthesia-sim)

;;; ====================================================================
;;; MAC CALCULATIONS
;;; ====================================================================

(defun hill (conc ec50 n)
  "Unitless effect in [0,1] for concentration 'conc'."
  (let ((x (max 0.0 conc)))
    (/ (expt x n) (+ (expt ec50 n) (expt x n)))))

;; Example EC50s and Hills (tunable)
(defparameter *propofol-ec50-μgml* 3.0)
(defparameter *propofol-hill* 2.0)

(defparameter *fentanyl-ec50-μgml* 1.2)
(defparameter *fentanyl-hill* 1.5)
(defparameter *k-opioid->ai* 0.8) ; map “full” fentanyl effect to ~0.8 MAC-eq worth of drive depression

(defparameter *k-propofol->mac* 1.0)        ; maps full propofol effect to ~1 MAC-eq
(defparameter *mac-awake* 0.3)              ; ceiling for MAC-sparing (fraction of MAC)
(defparameter *opioid-max-sparing* 0.7)     ; fraction reduction at high opioid effect (so floor ~0.3)

(defun propofol-mac-equivalent (controller)
  (let* ((mdl (find "propofol" (iv-drug-models controller) :key #'drug-name :test #'string=))
         (ce (if mdl (effect-site-conc mdl) 0.0))
         (eff (hill ce *propofol-ec50-μgml* *propofol-hill*)))
    (* *k-propofol->mac* eff)))

(defun mac-awake-floor (p)
  ;; tweak per age if you like; or return *mac-awake* unchanged
  *mac-awake*)

(defun opioid-mac-sparing (controller)
  (let* ((mdl (find "fentanyl" (iv-drug-models controller) :key #'drug-name :test #'string=))
         (ce (if mdl (effect-site-conc mdl) 0.0))
         (eff (hill ce *fentanyl-ec50-μgml* *fentanyl-hill*))
         (reduction (* *opioid-max-sparing* eff))
         (factor (- 1.0 reduction)))
    (max (mac-awake-floor (controller-patient controller)) factor)))

(defun compute-opioid-resp-sensitivity (p)
  (let* ((age (age-years p))
         (age-mult (cond ((< age 1) 1.6)
                         ((> age 70) 1.4)
                         (t 1.0)))
         ;; optional comorbidity multipliers (if you have flags)
         (mg-mult  (if (ignore-errors (slot-value p 'myasthenia-gravis)) 1.2 1.0))
         (copd-mult (if (ignore-errors (slot-value p 'copd)) 1.2 1.0))
         (hypothy-mult (if (ignore-errors (slot-value p 'hypothyroid)) 1.1 1.0)))
    (* age-mult mg-mult copd-mult hypothy-mult)))

(defun volatile-mac-multiple (controller)
  "Sum MAC multiples of all volatile agents after opioid MAC-sparing."
  (let* ((sparing (opioid-mac-sparing controller)))
    (reduce #'+ (volatile-models controller)
            :initial-value 0.0
            :key (lambda (vam)
                   (let* ((mac-mmHg (* (mac vam) 7.6))
                          (mmHg (brain-conc vam))
                          (raw (/ mmHg (max 1e-6 mac-mmHg))))
                     (/ raw (max 1e-6 sparing)))))))

(defun hypercapnia-cns-penalty (paco2 ph)
  "Returns extra MAC-eq depression from extreme hypercapnia/acidosis."
  (let* ((pc (max 0.0 paco2))
         ;; start penalizing above ~80; near-linear up to ~120 gives ~0.5 MAC-eq
         (pen (max 0.0 (/ (- pc 80.0) 80.0)))   ; 0 at 80, ~0.5 at 120
         ;; amplify if severely acidemic
         (acid (max 0.0 (- 7.35 (or ph 7.4)))))
    (+ pen (* 0.5 acid))))

(defun opioid-ai (controller)
  (let* ((mdl (find "fentanyl" (iv-drug-models controller) :key #'drug-name :test #'string=))
         (ce (if mdl (effect-site-conc mdl) 0.0))
         (eff (hill ce *fentanyl-ec50-μgml* *fentanyl-hill*)) ; 0..1
         (sens (opioid-resp-sensitivity (controller-patient controller))))
    (* *k-opioid->ai* eff sens)))

(defun total-anesthetic-index (controller)
  (let* ((mac-vol (volatile-mac-multiple controller))
         (mac-prop (propofol-mac-equivalent controller))
         (mac-opioid (opioid-ai controller))
         (mac-hyper (hypercapnia-cns-penalty (controller-paco2 controller)
                                             (controller-ph controller))))
    (+ mac-vol mac-prop mac-opioid mac-hyper)))

(defun ventilatory-depression-from-ai (ai)
  ;; 0 -> 1.0 ; ~1 -> ~0.6 ; ~2 -> ~0.36 ; floor 0.1
  (max 0.1 (exp (* -0.51 ai))))

;;; ====================================================================
;;; CENTRALIZED RESPIRATORY CONTROLLER
;;; ====================================================================

;; Fixed respiratory controller with renamed slots to avoid conflicts
(defclass respiratory-controller ()
  ((patient :initarg :patient :accessor controller-patient)
   (circuit :initarg :circuit :accessor controller-circuit)
   (current-mode :accessor current-ventilation-mode :initform nil)
   (available-modes :accessor available-ventilation-modes :initform nil)
   (respiratory-mechanics :accessor controller-respiratory-mechanics :initform nil)
   (respiratory-drive :accessor controller-respiratory-drive :initform nil)
   (support-active :accessor controller-support-active :initform nil)
   (ventilation-adequate :accessor controller-ventilation-adequate :initform nil)
   (iv-drug-models :accessor iv-drug-models :initform nil)  ; Separate IV drugs
   (volatile-models :accessor volatile-models :initform nil) ; Separate volatile agents
   
   ;; Monitoring parameters with bounds checking
   (minute-ventilation :accessor controller-minute-ventilation :initform 0.0)
   (alveolar-ventilation :accessor controller-alveolar-ventilation :initform 0.0)
   (paco2 :accessor controller-paco2 :initform 40.0)
   (pao2 :accessor controller-pao2 :initform 100.0)
   (ph :accessor controller-ph :initform 7.4)
   (respiratory-acidosis :accessor controller-respiratory-acidosis :initform nil)
   
   ;; Control parameters
   (spontaneous-breathing :accessor controller-spontaneous-breathing :initform t)
   (ventilatory-support-adequate :accessor controller-ventilatory-support-adequate :initform t)
   (anesthesiologist-intervention-needed :accessor controller-intervention-needed :initform nil)
   (last-ai :accessor last-ai :initform 0.0)
   (last-volatile-mac :accessor last-volatile-mac :initform 0.0)))

(defmethod initialize-instance :after ((controller respiratory-controller) &key)
  "Initialize respiratory controller with default modes and models."
  (setf (controller-respiratory-mechanics controller) (make-instance 'respiratory-mechanics))
  (setf (controller-respiratory-drive controller) (make-instance 'respiratory-drive-model))
  
  ;; Separate IV drugs from volatile agents
  (setf (iv-drug-models controller) (list (make-instance 'propofol-model)
                                          (make-instance 'fentanyl-model)))
  (setf (volatile-models controller) (list (make-instance 'sevoflurane-model)))
  
  (setf (available-ventilation-modes controller) (list (make-instance 'auxiliary-o2-mode)
                                                       (make-instance 'manual-ventilation-mode)
                                                       (make-instance 'controlled-ventilation-mode)))
  (setf (current-ventilation-mode controller) (first (available-ventilation-modes controller))))

;;; ====================================================================
;;; PHARMACOKINETIC/PHARMACODYNAMIC MODELS
;;; ====================================================================

;;; Drug Compartment Model Base Class
(defclass drug-compartment ()
  ((drug-name :initarg :drug-name :accessor drug-name)
   (central-volume :initarg :central-volume :accessor central-volume :initform 5.0)
   (peripheral-volume :initarg :peripheral-volume :accessor peripheral-volume :initform 20.0)
   (k12 :initarg :k12 :accessor k12 :initform 0.3) ; central to peripheral
   (k21 :initarg :k21 :accessor k21 :initform 0.1) ; peripheral to central
   (k10 :initarg :k10 :accessor k10 :initform 0.2) ; elimination from central
   (central-amount :accessor central-amount :initform 0.0)
   (peripheral-amount :accessor peripheral-amount :initform 0.0)
   (effect-site-conc :accessor effect-site-conc :initform 0.0)
   (keo :initarg :keo :accessor keo :initform 0.5))) ; effect site equilibration

;;; Specific Drug Models
(defclass propofol-model (drug-compartment)
  ((drug-name :initform "propofol")
   (central-volume :initform 4.27)
   (peripheral-volume :initform 18.9)
   (k12 :initform 0.302)
   (k21 :initform 0.066)
   (k10 :initform 0.119)
   (keo :initform 1.15)))

(defclass fentanyl-model (drug-compartment)
  ((drug-name :initform "fentanyl")
   (central-volume :initform 12.8)
   (peripheral-volume :initform 67.2)
   (k12 :initform 0.36)
   (k21 :initform 0.077)
   (k10 :initform 0.077)
   (keo :initform 0.147)))

(defclass volatile-agent-model ()
  ((drug-name :initarg :drug-name :accessor drug-name)
   (blood-gas-partition :initarg :blood-gas-partition :accessor blood-gas-partition)
   (brain-blood-partition :initarg :brain-blood-partition :accessor brain-blood-partition)
   (alveolar-conc :accessor alveolar-conc :initform 0.0)
   (arterial-conc :accessor arterial-conc :initform 0.0)
   (brain-conc :accessor brain-conc :initform 0.0)
   (mac :initarg :mac :accessor mac))) ; Minimum Alveolar Concentration

(defclass sevoflurane-model (volatile-agent-model)
  ((drug-name :initform "sevoflurane")
   (blood-gas-partition :initform 0.65)
   (brain-blood-partition :initform 1.7)
   (mac :initform 2.05)))

;;; Drug Administration and Kinetics
(defmethod administer-bolus ((model drug-compartment) dose-mg)
  "Administer IV bolus dose."
  (incf (central-amount model) dose-mg))

;;; Infusion: rate is mg/min; dt is seconds → convert to minutes
(defmethod administer-infusion ((model drug-compartment) rate-mg-per-min dt-seconds)
  (incf (central-amount model) (* rate-mg-per-min (/ (max 0.0 dt-seconds) 60.0))))

;; Updated drug administration method
(defmethod administer-drug ((controller respiratory-controller) drug-name dose-mg &key (route :iv-bolus) (rate nil) (duration-seconds nil))
  "Administer drug and update appropriate model."
  (let ((iv-model (find drug-name (iv-drug-models controller) 
			:key #'drug-name :test #'string=))
        (volatile-model (find drug-name (volatile-models controller) 
                              :key #'drug-name :test #'string=)))
    (cond
      (iv-model
       (case route
         (:iv-bolus (administer-bolus iv-model dose-mg))
         (:iv-infusion (when rate (administer-infusion iv-model rate duration-seconds)))))
      (volatile-model
       (format t "Volatile agents administered via vaporizer, not direct injection~%"))
      (t
       (format t "Unknown drug: ~A~%" drug-name)))))

;;; Pharmacokinetics — two-compartment + effect-site
(defmethod update-pk-model ((model drug-compartment) dt-seconds)
  "Advance two-compartment PK + effect site. dt-seconds is in seconds."
  (let* ((dt-min (max 0.0 (/ dt-seconds 60.0)))
         ;; pull state, enforce non-negativity
         (c (max 0.0 (central-amount model)))
         (p (max 0.0 (peripheral-amount model)))
         (v (max 1e-9 (central-volume model)))   ; avoid div-by-zero
         (k12 (k12 model))
         (k21 (k21 model))
         (k10 (k10 model))
         (keo (keo model)))
    ;; --- Substep to keep Euler stable and nonnegative ---
    ;; A conservative stability scale: dt_sub <= 1 / (k10+k12+k21), with margin
    (let* ((lambda (+ k10 k12 k21))
           (n (max 1 (ceiling (* dt-min (max 1e-9 lambda) 2.0)))) ; margin factor 2
           (h (/ dt-min n)))
      (dotimes (i n)
        (let* ((q12 (* k12 c h))
               (q21 (* k21 p h))
               (elim (* k10 c h)))
          ;; central
          (setf c (max 0.0 (- c q12 elim)))
          (incf c q21)
          ;; peripheral
          (incf p q12)
          (setf p (max 0.0 (- p q21)))
          ;; effect-site (first-order toward central concentration)
          (let* ((c-conc (/ (max 0.0 c) v))
                 (delta-es (* keo (- c-conc (effect-site-conc model)) h)))
            (setf (effect-site-conc model)
                  (max 0.0 (+ (effect-site-conc model) delta-es)))))))
    (setf (central-amount model)   c
          (peripheral-amount model) p)))

(defmethod update-volatile-uptake ((model volatile-agent-model)
                                   inspired-fraction cardiac-output-lpm dt-seconds)
  "Simple alveolar/arterial/brain update; dt in seconds."
  (let* ((dt-min  (/ (max 0.0 dt-seconds) 60.0))
         (pp-insp (* inspired-fraction 760.0))              ; mmHg
         ;; crude first-order rates per minute
         (k-alv (/ (max 1e-9 cardiac-output-lpm) (max 1e-9 (blood-gas-partition model))))
         (k-br  (/ (* 0.15 (max 1e-9 cardiac-output-lpm))
                   (max 1e-9 (brain-blood-partition model)))))
    ;; substep for stability
    (let* ((lambda (+ k-alv k-br))
           (n (max 1 (ceiling (* dt-min (max 1e-9 lambda) 2.0))))
           (h (/ dt-min n)))
      (dotimes (i n)
        ;; alveolar tracks inspired with first-order k-alv
        (setf (alveolar-conc model)
              (+ (* (alveolar-conc model) (- 1.0 (* k-alv h)))
                 (* pp-insp k-alv h)))
        ;; arterial equilibrated with alveolar / λ
        (setf (arterial-conc model)
              (/ (alveolar-conc model) (max 1e-9 (blood-gas-partition model))))
        ;; brain first-order toward arterial with rate k-br
        (incf (brain-conc model)
              (* k-br h (- (arterial-conc model) (brain-conc model))))
        (setf (brain-conc model) (max 0.0 (brain-conc model)))))))

;;; ====================================================================
;;; RESPIRATORY DRIVE AND DRUG EFFECTS
;;; ====================================================================

;;; Respiratory Drive Calculator
(defclass respiratory-drive-model ()
  ((baseline-drive :initarg :baseline-drive :accessor baseline-drive :initform 1.0)
   (co2-sensitivity :initarg :co2-sensitivity :accessor co2-sensitivity :initform 0.8)
   (hypoxic-drive :initarg :hypoxic-drive :accessor hypoxic-drive :initform 0.1)
   (current-drive :accessor current-drive :initform 1.0)))

(defmethod calculate-chemical-drive ((model respiratory-drive-model) paco2 pao2)
  "Calculate respiratory drive from blood gases."
  (let* ((co2-drive (* (co2-sensitivity model) (max 0.0 (- paco2 40.0)) 0.02))
         (hypoxic-drive (* (hypoxic-drive model) 
                           (if (< pao2 60.0) (/ (- 60.0 pao2) 60.0) 0.0)))
         (total-drive (+ (baseline-drive model) co2-drive hypoxic-drive)))
    (setf (current-drive model) (max 0.0 total-drive))))

;;; Drug Effect Models
(defgeneric respiratory-depression-effect (drug-model)
  (:documentation "Calculate respiratory depression from drug concentration."))

(defmethod respiratory-depression-effect ((model propofol-model))
  "Propofol causes dose-dependent respiratory depression."
  (let ((concentration (effect-site-conc model)))
    (cond
      ((< concentration 0.5) 1.0)      ; No effect
      ((< concentration 2.0) 0.9)      ; Mild depression  
      ((< concentration 4.0) 0.7)      ; Moderate depression
      ((< concentration 6.0) 0.4)      ; Severe depression
      (t 0.15))))                      ; Near apnea

(defmethod respiratory-depression-effect ((model fentanyl-model))
  "Fentanyl causes respiratory depression via opioid receptors."
  (let ((concentration (effect-site-conc model)))
    (cond
      ((< concentration 0.5) 1.0)      ; No effect
      ((< concentration 1.5) 0.95)     ; Minimal depression
      ((< concentration 3.0) 0.8)      ; Mild depression
      ((< concentration 5.0) 0.6)      ; Moderate depression
      (t 0.3))))                       ; Severe depression

(defmethod respiratory-depression-effect ((model volatile-agent-model))
  "Volatile agents depress respiratory drive (unit-correct)."
  (let* ((mac-vol% (mac model))
         (mac-mmHg (* mac-vol% 7.6))   ; 1 vol% ≈ 7.6 mmHg at sea level
         (mac-mmHg (max mac-mmHg 1e-6))
         (mac-multiple (/ (brain-conc model) mac-mmHg)))
    (cond
      ((< mac-multiple 0.5) 0.95)      ; Minimal effect
      ((< mac-multiple 1.0) 0.8)       ; Mild depression
      ((< mac-multiple 1.5) 0.6)       ; Moderate depression
      ((< mac-multiple 2.0) 0.4)       ; Severe depression
      (t 0.2))))                       ; Deep anesthesia

;;; ====================================================================
;;; RESPIRATORY MECHANICS
;;; ====================================================================

(defclass respiratory-mechanics ()
  ((lung-compliance :initarg :lung-compliance :accessor lung-compliance :initform 50.0) ; mL/cmH2O
   (chest-wall-compliance :initarg :chest-wall-compliance :accessor chest-wall-compliance :initform 200.0)
   (airway-resistance :initarg :airway-resistance :accessor airway-resistance :initform 2.0) ; cmH2O·s/L
   (functional-residual-capacity :initarg :frc :accessor frc :initform 2500.0) ; mL
   (current-lung-volume :accessor current-lung-volume :initform 2500.0)
   (airway-pressure :accessor airway-pressure :initform 0.0)
   (pleural-pressure :accessor pleural-pressure :initform -5.0)
   (work-of-breathing :accessor work-of-breathing :initform 0.0)))

(defmethod total-compliance ((mechanics respiratory-mechanics))
  "Calculate total respiratory system compliance."
  (/ 1.0 (+ (/ 1.0 (lung-compliance mechanics))
            (/ 1.0 (chest-wall-compliance mechanics)))))

(defmethod calculate-pressure-volume-relationship ((mechanics respiratory-mechanics) volume)
  "Calculate required pressure for given volume above FRC."
  (let ((volume-above-frc (- volume (frc mechanics))))
    (/ volume-above-frc (total-compliance mechanics))))

(defmethod calculate-work-of-breathing ((mechanics respiratory-mechanics) tidal-volume flow-rate)
  "Calculate work of breathing for given tidal volume and flow."
  (let* ((pressure-work (* tidal-volume (calculate-pressure-volume-relationship mechanics 
										(+ (frc mechanics) 
                                                                                   (/ tidal-volume 2)))))
         (flow-work (* (airway-resistance mechanics) flow-rate flow-rate 0.5)))
    (setf (work-of-breathing mechanics) (+ pressure-work flow-work))))

;; Updated switch-ventilation-mode method
(defmethod switch-ventilation-mode ((controller respiratory-controller) mode-name)
  "Switch to specified ventilation mode."
  (let ((new-mode (find mode-name (available-ventilation-modes controller) 
			:key #'mode-name :test #'string=)))
    (when new-mode
      (when (current-ventilation-mode controller)
        (setf (active (current-ventilation-mode controller)) nil))
      (setf (current-ventilation-mode controller) new-mode)
      (setf (active new-mode) t)
      (format t "Switched to ~A ventilation mode~%" mode-name))))

(defmethod update-respiratory-physiology ((controller respiratory-controller) dt-seconds)
  "Advance PK/PD, compute anesthetic index and respiratory drive,
   push settings to the patient via mode logic, and finalize flags."
  ;; --- Advance IV PK (central/peripheral/effect site) ---
  (dolist (model (iv-drug-models controller))
    (update-pk-model model dt-seconds))

  ;; --- Advance volatile agent uptake/distribution ---
  (let ((cardiac-output-lpm 5.0)) ; placeholder unless you model CO
    (dolist (vam (volatile-models controller))
      (update-volatile-uptake vam
                              (get-inspired-volatile-fraction controller vam)
                              cardiac-output-lpm
                              dt-seconds)))

  ;; --- Drive calculation (chemoreceptors × anesthetic index) ---
  (let* ((ai        (total-anesthetic-index controller))
         (vol-only  (volatile-mac-multiple controller))
         (drive-mult (ventilatory-depression-from-ai ai)))
    (calculate-chemical-drive (controller-respiratory-drive controller)
                              (controller-paco2 controller)
                              (controller-pao2 controller))
    (let* ((chem (current-drive (controller-respiratory-drive controller)))
           (effective-drive (max 0.05 (min 2.0 (* chem drive-mult))))
           (mode (current-ventilation-mode controller)))

      ;; record for monitors
      (setf (slot-value controller 'last-ai)           ai)
      (setf (slot-value controller 'last-volatile-mac) vol-only)

      ;; Spontaneous flag semantics:
      ;; - In controlled mode → no spontaneous breathing
      ;; - In other modes     → thresholded on effective drive
      (if (typep mode 'controlled-ventilation-mode)
          (setf (controller-spontaneous-breathing controller) nil)
          (setf (controller-spontaneous-breathing controller) (> effective-drive 0.3)))

      ;; Push settings to patient & update gases + adequacy flags
      (update-patient-from-controller controller effective-drive dt-seconds))))

(defun alveolar-ventilation-lpm (patient &key (deadspace-ml 150.0))
  "Return alveolar ventilation in L/min, using RR*(VT-VD)."
  (let* ((rr (max 0.0 (respiratory-rate patient)))
         (vt (max 0.0 (tidal-volume     patient))) ; mL
         (va-ml (max 0.0 (* rr (max 0.0 (- vt deadspace-ml))))))
    (/ va-ml 1000.0)))

(defun required-va-lpm (patient &key (target-paco2 40.0))
  "Return required alveolar ventilation (L/min) to hold target PaCO2."
  ;; V̇A_required = 0.863 * V̇CO2 / PaCO2_target
  (* 0.863 (/ (co2-production patient) (max 1e-6 target-paco2))))

(defun ventilation-adequate-p (controller
                               &key (target-paco2 40.0)
                                    (tolerance-frac 0.08)     ; 8% slack
                                    (deadspace-ml 150.0))
  "True if current V̇A is within tolerance of physiologic requirement to hold target PaCO2.
   If tolerance-frac=0.08, we accept VA >= 92% of requirement."
  (let* ((p   (controller-patient controller))
         (va  (alveolar-ventilation-lpm p :deadspace-ml deadspace-ml))
         (req (required-va-lpm p :target-paco2 target-paco2))
         (ok  (>= va (* (- 1.0 (max 0.0 (min 0.5 tolerance-frac))) req))))
    (setf (controller-alveolar-ventilation controller) va)
    ok))

(defmethod update-patient-from-controller ((controller respiratory-controller) effective-drive dt)
  "Update patient respiratory parameters based on controller state, then
   compute physiology-driven adequacy flags."
  (let* ((patient (controller-patient controller))
         (mode    (current-ventilation-mode controller))
         (min-rr  4.0)
         (min-tv  200.0))
    
    (typecase mode
      ;; ------------------------------
      (controlled-ventilation-mode
       ;; Machine is fully in control
       (setf (respiratory-rate patient) (set-respiratory-rate mode)
             (tidal-volume     patient) (set-tidal-volume   mode)
             (controller-support-active controller) t
             ;; explicitly no spontaneous effort in controlled mode
             (controller-spontaneous-breathing controller) nil))
      
      ;; ------------------------------
      (manual-ventilation-mode
       ;; Assisted/spontaneous with pressure support OR operator plan
       (setf (controller-support-active controller) t)
       (let* ((m (current-ventilation-mode controller))
              (op-rr (operator-rr m))
              (op-tv (operator-tv m)))
         (cond
           ;; If operator provided targets, enforce them (no blending)
           ((or op-rr op-tv)
            (when op-rr
              (setf (respiratory-rate patient) (max min-rr op-rr)))
            (when op-tv
              (setf (tidal-volume patient) (max min-tv op-tv))))
           ;; Otherwise use your existing “assisted” heuristics
           ((> effective-drive 0.2)
            (setf (respiratory-rate patient) (max min-rr (* 10.0 effective-drive)))
            (setf (tidal-volume     patient)
                  (max min-tv (+ (* 400.0 effective-drive)
                                 (* (pressure-support m) 20.0)))))
           (t
            (setf (respiratory-rate patient) 6.0
                  (tidal-volume     patient) 350.0)))))
      
      ;; ------------------------------
      (auxiliary-o2-mode
       ;; Purely spontaneous with O2 supplementation … unless AMBU (off-circle manual support)
       (let ((mode (current-ventilation-mode controller)))
	 (setf (controller-support-active controller)
               (eq (delivery-method mode) :ambu))
	 (if (> effective-drive 0.3)
	     (progn
               (setf (respiratory-rate patient) (max min-rr (* 12.0 effective-drive)))
               (setf (tidal-volume     patient) (max min-tv (* 500.0 effective-drive))))
	     (progn
               (setf (respiratory-rate patient) min-rr
		     (tidal-volume     patient) min-tv)
               (setf (controller-intervention-needed controller) t))))))
    
    ;; Update blood gases using whatever model you have (uses RR/VT just set)
    (update-blood-gases controller dt)
    
    ;; Objective adequacy: based on alveolar ventilation vs CO2 load
    (let ((ok (ventilation-adequate-p controller)))
      (setf (controller-ventilation-adequate controller) ok)
      ;; Legacy flag normalized to the same objective criterion
      (setf (controller-ventilatory-support-adequate controller) ok))
    
    ;; Unify the legacy flag with the new semantics:
    ;; If the machine is supporting, “adequate” means VA meets demand.
    ;; If it's not, we still judge adequacy by VA (objective and consistent).
    (setf (controller-ventilatory-support-adequate controller)
          (if (controller-support-active controller)
              (controller-ventilation-adequate controller)
              (controller-ventilation-adequate controller)))

    controller))

(defmethod update-blood-gases ((controller respiratory-controller) dt)
  "Update blood gases using alveolar ventilation (VA) with an exponential PaCO2 integrator,
   simple PaO2 model, and optional shunt mixing if the patient provides SHUNT-FRACTION.
   Also supports optional physiologic dead space via PHYSIOLOGIC-DEADSPACE-FRAC."
  (let* ((patient (controller-patient controller))

         ;; Tidal variables (mL) -> MV/VA (L/min)
         (rr (max 0.0 (respiratory-rate patient)))
         (vt (max 0.0 (tidal-volume     patient)))

         ;; --- Dead space handling ---
         ;; Anatomic DS: 2 mL/kg (50–250 mL bounds) if BW available, else 150 mL.
         (bw (ignore-errors (body-weight-kg patient)))
         (vd-anat (if bw (max 50.0 (min 250.0 (* 2.0 bw))) 150.0))
         ;; Optional physiologic dead space: fraction of VT, if patient provides it.
         (vdphys-frac (or (ignore-errors (physiologic-deadspace-frac patient)) 0.0))
         (vdphys (max 0.0 (min 0.9 vdphys-frac))) ; clamp 0..0.9
         (vd (max vd-anat (* vdphys vt)))         ; overall DS = max(anatomic, physio*VT)

         ;; Minute ventilation & alveolar ventilation
         (mv (* rr vt 0.001))                                ; L/min
         (va (* rr (max 0.0 (- vt vd)) 0.001))               ; L/min

         ;; Required VA to keep PaCO2 ≈ 40 (VCO2 * 0.863 / 40)
         (va-required (* (co2-production patient) 0.863 (/ 1.0 40.0)))
         (ventilation-ratio (/ (max 0.1 va) (max 0.1 va-required))))

    ;; Store for monitoring
    (setf (controller-minute-ventilation controller) mv)
    (when (slot-exists-p controller 'alveolar-ventilation)
      (setf (controller-alveolar-ventilation controller) va))

    ;; --- PaCO2 update with time constant (stable for any dt) ---
    (let* ((vr (max 0.2 (min 5.0 ventilation-ratio)))        ; guard rails
           (target-paco2 (max 20.0 (min 100.0 (/ 40.0 vr))))
           (tau 180.0)                                       ; seconds to ~63% toward target
           (alpha (- 1.0 (exp (/ (- (coerce dt 'double-float)) tau)))))
      (setf (controller-paco2 controller)
            (max 15.0 (min 120.0
                           (+ (controller-paco2 controller)
                              (* alpha (- target-paco2 (controller-paco2 controller))))))))

    ;; pH: simple Henderson-Hasselbalch-like slope vs PaCO2
    (setf (controller-ph controller)
          (max 6.8 (min 7.8 (- 7.4 (* (- (controller-paco2 controller) 40.0) 0.008)))))

    ;; Flag severe respiratory acidosis
    (setf (controller-respiratory-acidosis controller)
          (< (controller-ph controller) 7.35))

    ;; --- Provisional PaO2 (kept simple) ---
    (let ((pao2-ideal
            (typecase (current-ventilation-mode controller)
	      (auxiliary-o2-mode
	       (let* ((mode (current-ventilation-mode controller))
		      (fio2 (estimate-aux-fio2 mode))
		      ;; simple: PaO2 ≈ 500 * FiO2, clamped; room air (0.21) ≈ 105
		      (pao2-ideal (max 60.0 (min 500.0 (* 500.0 fio2)))))
		 (setf (controller-pao2 controller) pao2-ideal)))
              (t
               (max 50.0 (min 500.0 (* 100.0 (max 0.2 (min 5.0 ventilation-ratio)))))))))
      (setf (controller-pao2 controller) pao2-ideal))
    
    ;; --- Shunt mixing (optional; if patient has SHUNT-FRACTION) ---
    ;; Mix ideal alveolar PaO2 with venous surrogate based on shunt fraction.
    (let* ((sh (max 0.0 (min 0.6 (or (ignore-errors (shunt-fraction patient)) 0.0))))
           (pvo2 40.0) ; venous O2 surrogate (mmHg)
           (pao2-mixed (+ (* (- 1.0 sh) (controller-pao2 controller))
                          (* sh pvo2))))
      (setf (controller-pao2 controller) (max 35.0 (min 500.0 pao2-mixed))))))

(defun stream-by-name (circuit comp-name)
  "Return the gas stream for the named component, or NIL."
  (let ((comp (find-if (lambda (x) (string= (name x) comp-name))
                       (components circuit))))
    (and comp (gethash comp (gas-streams circuit)))))

;; read inspired volatile from the inspiratory limb, uses stream-by-name as helper
;; and ensures AMBU and all aux-O₂ devices do not see vaporizer output
(defmethod get-inspired-volatile-fraction ((controller respiratory-controller) volatile-model)
  (let* ((mode (current-ventilation-mode controller)))
    (cond
      ;; Controlled or manual (circle) → read inspiratory limb
      ((or (typep mode 'controlled-ventilation-mode)
           (typep mode 'manual-ventilation-mode))
       (let* ((c (controller-circuit controller))
              (stream (and c (stream-by-name c "inspiratory-limb"))))
         (if (and stream (> (flow-rate stream) 0.0))
             (or (gethash (drug-name volatile-model) (fractions stream)) 0.0)
             0.0)))
      ;; Auxiliary O2 (off circle or no volatile delivery) → 0
      (t 0.0))))

(defmethod check-intervention-criteria ((controller respiratory-controller))
  "Check if anesthesiologist intervention is needed."
  (let ((needs-intervention nil)
        (reasons nil)
        (previous-state (controller-intervention-needed controller)))
    
    ;; Inadequate ventilation with auxiliary O2
    (when (and (typep (current-ventilation-mode controller) 'auxiliary-o2-mode)
               (not (controller-ventilatory-support-adequate controller)))
      (setf needs-intervention t)
      (push "Inadequate spontaneous ventilation" reasons))
    
    ;; Severe respiratory acidosis
    (when (< (controller-ph controller) 7.25)
      (setf needs-intervention t)
      (push "Severe respiratory acidosis" reasons))
    
    ;; Severe hypercapnia
    (when (> (controller-paco2 controller) 60.0)
      (setf needs-intervention t)
      (push "Severe hypercapnia" reasons))
    
    (setf (controller-intervention-needed controller) needs-intervention)
    
    ;; Only print warning if intervention status changed
    (when (and needs-intervention (not previous-state))
      (format t "~&*** ANESTHESIOLOGIST INTERVENTION NEEDED: ~{~A~^, ~} ***~%" reasons))
    
    needs-intervention))

;;; ====================================================================
;;; ENHANCED PATIENT CLASS WITH RESPIRATORY CONTROLLER INTEGRATION
;;; ====================================================================

(defclass controlled-patient (patient)
  ((respiratory-controller :accessor respiratory-controller :initform nil)
   (baseline-respiratory-rate :accessor baseline-respiratory-rate :initform 12.0)
   (baseline-tidal-volume :accessor baseline-tidal-volume :initform 500.0)))

(defmethod initialize-instance :after ((patient controlled-patient) &key)
  "Initialize patient with respiratory controller."
  (setf (respiratory-controller patient) 
        (make-instance 'respiratory-controller :patient patient)))

(defmethod process-gas :around ((patient controlled-patient) input-stream dt)
  "Process gas with respiratory controller integration."
  ;; Only update respiratory physiology once per simulation step
  (when (and (respiratory-controller patient) (> dt 0))
    (update-respiratory-physiology (respiratory-controller patient) dt))
  
  ;; Call the standard gas processing without additional intervention checks
  (call-next-method))

(defmethod post-process-dynamics :after ((patient controlled-patient) input-stream output-stream dt)
  "Check intervention criteria after gas processing is complete."
  (when (respiratory-controller patient)
    (check-intervention-criteria (respiratory-controller patient))))

;;; ====================================================================
;;; FACTORY FUNCTIONS
;;; ====================================================================

(defun make-controlled-respiratory-system (&key (patient-type 'healthy) (initial-mode "auxiliary-o2"))
  "Create a complete system with respiratory control."
  (let* ((base-circuit (make-circle-system :patient-type patient-type))
         (controlled-patient (make-instance 'controlled-patient
                                            :name "controlled-patient"
                                            :tidal-volume 500.0
                                            :respiratory-rate 12.0))
         (machine (make-instance 'anesthesia-machine)))
    
    ;; Replace the basic patient with controlled patient
    (replace-patient base-circuit controlled-patient)

    (setf (controller-circuit (respiratory-controller controlled-patient)) base-circuit)
    
    ;; Set initial ventilation mode
    (switch-ventilation-mode (respiratory-controller controlled-patient) initial-mode)
    
    (format t "Created controlled respiratory system with ~A mode~%" initial-mode)
    (values base-circuit controlled-patient machine)))

;;; ====================================================================
;;; MONITORING AND INTERVENTION FUNCTIONS
;;; ====================================================================

(defun monitor-respiratory-status (patient &optional (detailed nil))
  "Monitor current respiratory status of controlled patient."
  (when (typep patient 'controlled-patient)
    (let* ((controller (respiratory-controller patient))
           (mode       (current-ventilation-mode controller)))
      (format t "~&=== Respiratory Status ===~%")
      (format t "Current Mode: ~A~%" (mode-name mode))
      (when (typep mode 'auxiliary-o2-mode)
	(let* ((fio2 (estimate-aux-fio2 mode))
               (meth (delivery-method mode))
               (flow (o2-flow-rate mode)))
	  (format t "Device: ~A @ ~,1F L/min → est FiO2 ~,0F%%~%"
		  meth flow (* 100 fio2))
	  (when (eq meth :ambu)
	    (format t "Note: AMBU off-circle (no absorber, no volatile delivery)~%"))))
      (when (typep mode 'manual-ventilation-mode)
	(if (operator-rr mode)
	    (format t "  (Operator targeted respiratory rate=~a)~%" (operator-rr mode))
	    (format t "Patient's respiratory rate: ~,1F bpm~%" (respiratory-rate patient)))
	(if (operator-tv mode)
	    (format t "  (Operator targeted tidal volume=~a mL)~%" (operator-tv mode))
	    (format t "Patient's tidal volume: ~,1F mL~%" (tidal-volume patient))))
      (when (typep mode 'controlled-ventilation-mode)
	(format t "Set frequency: ~,1F bpm~%" (set-respiratory-rate mode))
	(format t "Set tidal volume: ~,1F mL~%" (set-tidal-volume mode)))
      (format t "Minute Ventilation: ~,1F L/min~%"
              (controller-minute-ventilation controller))
      
      (let* ((p (respiratory-controller patient))
	     (va (controller-alveolar-ventilation p))
	     (req (required-va-lpm (controller-patient p))))
	(format t "Alveolar Ventilation: ~,2F L/min (required ~,2F)~%"
		va req))
      
      (format t "~%Blood Gases:~%")
      (format t "  PaCO2: ~,1F mmHg~%" (controller-paco2 controller))
      (format t "  PaO2: ~,1F mmHg~%" (controller-pao2 controller))
      (format t "  pH: ~,2F~%"        (controller-ph controller))

      (when detailed
        ;; IVs
        (format t "~%IV Drug Concentrations:~%")
        (dolist (drug (iv-drug-models controller))
          (format t "  ~A: ~,3F μg/mL (depression: ~,1F%)~%"
                  (slot-value drug 'drug-name)
                  (slot-value drug 'effect-site-conc)
                  (* 100 (- 1.0 (respiratory-depression-effect drug)))))

        ;; Volatiles
        (format t "~%Volatile agents (Brain partial pressure and concentrations in the inspiratory limb):~%")
	(dolist (agent (volatile-models controller))
	  (let* ((mac-vol% (mac agent))
		 (mac-mmHg (max 1e-6 (* mac-vol% 7.6)))
		 (mac-multiple (/ (brain-conc agent) mac-mmHg))
		 (insp-frac (get-inspired-volatile-fraction controller agent)))
	    (format t "  ~A: brain ~,2F mmHg (MAC=~,2F), inspired ~,2F%%~%"
		    (drug-name agent)
		    (brain-conc agent)
		    mac-multiple
		    (* 100 insp-frac))))
	
        ;; Depth summary
        (format t "~%Anesthetic Depth:~%")
        (format t "  Volatile MAC multiple: ~,2F (after opioid sparing)~%"
                (slot-value controller 'last-volatile-mac))
        (format t "  Propofol MAC-eq: ~,2F~%" (propofol-mac-equivalent controller))
        (format t "  Hypercapnia penalty: ~,2F~%"
                (hypercapnia-cns-penalty (controller-paco2 controller)
                                         (controller-ph controller)))
        (format t "  Total AI: ~,2F  -> Drive multiplier ~,2F~%"
                (slot-value controller 'last-ai)
                (ventilatory-depression-from-ai (slot-value controller 'last-ai))))

      ;; Clear, separate semantics for support vs adequacy
      (format t "~%Status Flags:~%")
      (format t "  Machine Support Active: ~A~%" (controller-support-active controller))
      (format t "  Ventilation Adequate:   ~A~%" (controller-ventilation-adequate controller))
      ;; (Optional) keep the legacy line for compatibility with older logs/tools:
      (format t "  Ventilatory Support Adequate: ~A~%"
              (controller-ventilatory-support-adequate controller))
      (format t "  Spontaneous Breathing: ~A~%"
              (controller-spontaneous-breathing controller))
      (format t "  Intervention Needed:   ~A~%"
              (controller-intervention-needed controller)))))

(defun intervene-ventilation (patient new-mode &rest mode-params)
  "Anesthesiologist intervention - switch ventilation mode."
  (when (typep patient 'controlled-patient)
    (let ((controller (respiratory-controller patient)))
      (switch-ventilation-mode controller new-mode)
      ;; Apply mode-specific parameters
      (when mode-params
        (let ((mode (current-ventilation-mode controller)))
          (typecase mode
            (controlled-ventilation-mode
             (when (getf mode-params :respiratory-rate)
               (setf (set-respiratory-rate mode)
                     (getf mode-params :respiratory-rate)))
             (when (getf mode-params :tidal-volume)
               (setf (set-tidal-volume mode)
                     (getf mode-params :tidal-volume)))
             (when (getf mode-params :peep)
               (setf (peep mode) (getf mode-params :peep))))
            (manual-ventilation-mode
             ;; NEW: bagging-to-a-plan setpoints
             (when (getf mode-params :respiratory-rate)
               (setf (operator-rr mode) (getf mode-params :respiratory-rate)))
             (when (getf mode-params :tidal-volume)
               (setf (operator-tv mode) (getf mode-params :tidal-volume)))
             ;; keep your existing pressure-support knob
             (when (getf mode-params :pressure-support)
               (setf (pressure-support mode)
                     (getf mode-params :pressure-support)))))))
      (format t "Intervention: Switched to ~A mode~%" new-mode)
      (monitor-respiratory-status patient))))

(defun administer-reversal-agent (patient agent-type)
  "Administer reversal agents for respiratory depression."
  (when (typep patient 'controlled-patient)
    (let ((controller (respiratory-controller patient)))
      (case agent-type
        (:naloxone
         ;; Reverse opioid effects
         (let ((fentanyl-model (find "fentanyl" (iv-drug-models controller) 
                                     :key #'drug-name :test #'string=)))
           (when fentanyl-model
             (setf (effect-site-conc fentanyl-model) 
                   (* (effect-site-conc fentanyl-model) 0.1))
             (format t "Naloxone administered - opioid effects reversed~%"))))
        (:flumazenil
         ;; Would reverse benzodiazepines if implemented
         (format t "Flumazenil administered~%"))
        (t 
         (format t "Unknown reversal agent: ~A~%" agent-type)))
      
      (update-respiratory-physiology controller 1.0)
      (monitor-respiratory-status patient))))

;;; Enhanced Clinical Scenario Functions
(defun safe-induction-scenario (circuit patient machine)
  "Demonstrate proper anesthesia induction with respiratory monitoring and intervention."
  (let ((controller (respiratory-controller patient)))
    
    (format t "~&=== Safe Anesthesia Induction Scenario ===~%")
    
    ;; Step 1: Pre-oxygenation
    (format t "~%Step 1: Pre-oxygenation with 100% O2~%")
    (switch-ventilation-mode controller "auxiliary-o2")
    (monitor-respiratory-status patient)
    
    ;; Step 2: Induction agent
    (format t "~%Step 2: Propofol 2 mg/kg induction~%")
    (administer-drug controller "propofol" 120.0)
    (update-respiratory-physiology controller 30.0) ; 30 seconds
    (monitor-respiratory-status patient)
    
    ;; Step 3: Check for adequate ventilation
    (if (controller-intervention-needed controller)
        (progn
          (format t "~%Step 3: Respiratory depression detected - switching to manual ventilation~%")
          (intervene-ventilation patient "manual" :pressure-support 15.0))
        (format t "~%Step 3: Spontaneous ventilation adequate~%"))
    
    ;; Step 4: Add opioid
    (format t "~%Step 4: Fentanyl 2 μg/kg~%")
    (administer-drug controller "fentanyl" 120.0)
    (update-respiratory-physiology controller 60.0) ; 1 minute
    (monitor-respiratory-status patient)
    
    ;; Step 5: Final assessment and mode adjustment
    (if (controller-intervention-needed controller)
        (progn
          (format t "~%Step 5: Significant respiratory depression - switching to controlled ventilation~%")
          (intervene-ventilation patient "controlled" 
                                 :respiratory-rate 12.0
                                 :tidal-volume 500.0
                                 :peep 5.0))
        (format t "~%Step 5: Manual ventilation adequate~%"))
    
    (format t "~%=== Induction Complete ===~%")
    (monitor-respiratory-status patient t))) ; detailed monitoring

(defun demonstrate-respiratory-control ()
  "Demonstrate the respiratory control system."
  (format t "~&=== Respiratory Control System Demonstration ===~%")
  (multiple-value-bind (circuit patient machine)
      (make-controlled-respiratory-system :patient-type 'healthy :initial-mode "auxiliary-o2")
    
    (format t "~%1. Initial state - Patient breathing spontaneously~%")
    (let ((controller (respiratory-controller patient)))
      (format t "   Respiratory rate: ~A bpm~%" (respiratory-rate patient))
      (format t "   Tidal volume: ~A mL~%" (tidal-volume patient))
      (format t "   Mode: ~A~%" (mode-name (current-ventilation-mode controller)))
      
      (format t "~%2. Administering propofol 2 mg/kg...~%")
      (administer-drug controller "propofol" 120.0)
      (update-respiratory-physiology controller 60.0)
      
      (format t "   Post-induction respiratory rate: ~A bpm~%" (respiratory-rate patient))
      (format t "   Intervention needed: ~A~%" (controller-intervention-needed controller))
      
      (when (controller-intervention-needed controller)
        (format t "~%3. Switching to controlled ventilation...~%")
        (switch-ventilation-mode controller "controlled")
        (update-respiratory-physiology controller 60.0)
        (format t "   New respiratory rate: ~A bpm~%" (respiratory-rate patient))
        (format t "   New tidal volume: ~A mL~%" (tidal-volume patient))))
    
    (values circuit patient machine)))

;;; Usage Examples
(defun respiratory-control-examples ()
  "Print usage examples for the respiratory control system."
  (format t "~&=== Respiratory Control System Usage ===~%")
  (format t "1. Create controlled system:~%")
  (format t "   (multiple-value-bind (circuit patient machine)~%")
  (format t "       (make-controlled-respiratory-system :patient-type 'healthy)~%")
  (format t "     ...)~%")
  (format t "~%2. Monitor patient status:~%")
  (format t "   (monitor-respiratory-status patient)~%")
  (format t "   (monitor-respiratory-status patient t) ; detailed~%")
  (format t "~%3. Switch ventilation modes:~%")
  (format t "   (intervene-ventilation patient \"controlled\" :respiratory-rate 14 :tidal-volume 550)~%")
  (format t "~%4. Administer drugs safely:~%")
  (format t "   (administer-drug (respiratory-controller patient) \"propofol\" 120.0)~%")
  (format t "   (update-respiratory-physiology (respiratory-controller patient) 30.0)~%")
  (format t "~%5. Use reversal agents:~%")
  (format t "   (administer-reversal-agent patient :naloxone)~%")
  (format t "~%6. Run safe induction scenario:~%")
  (format t "   (safe-induction-scenario circuit patient machine)~%")
  (format t "~%7. Proper workflow with simulation:~%")
  (format t "   ;; Create system~%")
  (format t "   (multiple-value-bind (circuit patient machine)~%")
  (format t "       (make-controlled-respiratory-system :patient-type 'healthy)~%")
  (format t "     ;; Start simulation~%")
  (format t "     (start-simulation circuit :dt 1.0)~%")
  (format t "     ;; Wait a moment, then intervene~%")
  (format t "     (sleep 2)~%")
  (format t "     (administer-drug (respiratory-controller patient) \"propofol\" 120.0)~%")
  (format t "     ;; Monitor and intervene as needed~%")
  (format t "     (monitor-respiratory-status patient)~%")
  (format t "     (when (anesthesiologist-intervention-needed (respiratory-controller patient))~%")
  (format t "       (intervene-ventilation patient \"controlled\"))~%")
  (format t "     (stop-simulation))~%"))
