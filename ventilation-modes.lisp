;;; Ventilation modes covering these cases:
;;; 1.  Auxiliary O2 Mode: Patient breathes room air + supplemental O2
;;;     (nasal cannula, simple face mask, face mask with rebreather, t-piece)
;;; 1.1 Auxiliary O2 Mode: Patient drive suppressed or only initiates breath.
;;;     Anesthesiologist manually supports via AMBU bag
;;; 2.  Manual/Assisted Ventilation: Patient drive suppressed or only initiates breath.
;;;     Anesthesiologist manually supports via circle system
;;; 2.1 Assisted Mechanical Ventilation: Patient drive suppressed or only initiates breath.
;;;     Machine supports via circle system (PSV, ASV, SIMV modes)
;;; 3.  Controlled Mechanical Ventilation: Patient drive suppressed so much that patient unable to even initiate breath.
;;;     Machine controls all parameters (SIMV, VCV, PCV)

(in-package :anesthesia-sim)

;;; ====================================================================
;;; ANESTHESIA MACHINE MODES
;;; ====================================================================

(defclass anesthesia-machine ()
  ((current-mode :accessor current-mode :initform :auxiliary-o2)
   (o2-flowmeter :accessor o2-flowmeter :initform 4.0) ; L/min
   (air-flowmeter :accessor air-flowmeter :initform 0.0)
   (ventilator-settings :accessor ventilator-settings :initform nil)
   (mode-selector :accessor mode-selector :initform :auxiliary-o2)))

(defmethod set-machine-mode ((machine anesthesia-machine) mode)
  "Set anesthesia machine to specified mode."
  (setf (current-mode machine) mode)
  (case mode
    (:auxiliary-o2 
     (format t "Machine set to Auxiliary O2 mode~%"))
    (:manual-ventilation 
     (format t "Machine set to Manual Ventilation mode~%"))
    (:controlled-ventilation 
     (format t "Machine set to Controlled Ventilation mode~%"))))

;;; ====================================================================
;;; VENTILATION MODES
;;; ====================================================================

(defclass ventilation-mode ()
  ((mode-name :initarg :mode-name :accessor mode-name)
   (active :accessor active :initform nil)
   (pressure-support :initarg :pressure-support :accessor pressure-support :initform 0.0)
   (trigger-sensitivity :initarg :trigger-sensitivity :accessor trigger-sensitivity :initform 2.0)))

(defclass auxiliary-o2-mode (ventilation-mode)
  ((mode-name :initform "auxiliary-o2")
   (o2-flow-rate :initarg :o2-flow-rate :accessor o2-flow-rate :initform 4.0) ; L/min
   ;; Delivery method:
   ;;   :nasal-cannula, :simple-mask, :nonrebreather, :t-piece, :ambu
   ;; AMBU here means *manual bagging disconnected from the circle*.
   (delivery-method :initarg :delivery-method :accessor delivery-method :initform :nasal-cannula)))

(defclass manual-ventilation-mode (ventilation-mode)
  ((mode-name :initform "manual")
   (pressure-support :initform 10.0) ; cmH2O
   (trigger-sensitivity :initform 2.0)
   ;; NEW: operator targets (nil means “not forcing”)
   (operator-rr :initarg :operator-rr :accessor operator-rr :initform nil)   ; bpm
   (operator-tv :initarg :operator-tv :accessor operator-tv :initform nil))) ; mL)) ; cmH2O or L/min

(defclass controlled-ventilation-mode (ventilation-mode)
  ((mode-name :initform "controlled")
   (set-respiratory-rate :initarg :set-rr :accessor set-respiratory-rate :initform 12.0)
   (set-tidal-volume :initarg :set-tv :accessor set-tidal-volume :initform 500.0)
   (peep :initarg :peep :accessor peep :initform 5.0)
   (peak-pressure-limit :initarg :pressure-limit :accessor peak-pressure-limit :initform 30.0)))

;; helper to estimate FiO₂ for common devices (simple empirical ranges)
(defun estimate-aux-fio2 (mode)
  "Very rough FiO2 estimate given delivery method and O2 flow."
  (let* ((flow (max 0.0 (o2-flow-rate mode)))
         (meth (delivery-method mode)))
    (ecase meth
      (:nasal-cannula  ; ~21% + ~4% per L/min up to ~45%
       (min 0.45 (+ 0.21 (* 0.04 flow))))
      (:simple-mask    ; ~0.35–0.60 depending on 5–10 L/min
       (cond ((< flow 5.0) 0.35)
             ((< flow 8.0) 0.45)
             (t            0.60)))
      (:nonrebreather  ; ~0.6–0.9 depending on 10–15 L/min and seal
       (cond ((< flow 8.0) 0.60)
             ((< flow 12.0) 0.75)
             (t             0.90)))
      (:t-piece        ; generally near source when sealed: assume 0.60 at ≥10 L/min
       (if (>= flow 10.0) 0.60 0.40))
      (:ambu           ; AMBU bag (off the circle): depends on reservoir & flow
       ;; with reservoir + 10–15 L/min you can approach ~0.85–0.95; be conservative
       (cond ((< flow 8.0) 0.50)
             ((< flow 12.0) 0.70)
             (t             0.85))))))

;; now we can do:
;; (set-aux-device patient :nonrebreather :flow 12)
;; (set-aux-device patient :ambu :flow 15)
(defun set-aux-device (patient method &key (flow 6.0))
  (let ((rc (and (slot-exists-p patient 'respiratory-controller)
                 (slot-value patient 'respiratory-controller))))
    (when rc
      (let ((m (current-ventilation-mode rc)))
        (when (typep m 'auxiliary-o2-mode)
          (setf (delivery-method m) method
                (o2-flow-rate m) flow))))))

(defun set-fgf-flows (circuit &key (o2 0.5) (air 0.5) (n2o 0.0))
  "Convenience setter for fresh gas flows (L/min) on the component named \"FGF\"."
  (let ((fgf (find "FGF" (components circuit) :key #'name :test #'string=)))
    (when fgf
      (setf (o2-flow fgf)  o2
            (air-flow fgf) air
            (n2o-flow fgf) n2o)
      (format t "FGF set: O2=~,1F L/min, Air=~,1F L/min, N2O=~,1F L/min~%" o2 air n2o))
    fgf))

