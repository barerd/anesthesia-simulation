;;; Capnogram

(in-package :anesthesia-sim)

;;; --- helpers ---

(defparameter *capno-default-fs* 100.0)      ; samples/sec for synthesized capnogram

(defun mmhg->frac (p-mmHg &optional (baro 760.0)) (/ p-mmHg baro))
(defun frac->mmhg (frac   &optional (baro 760.0)) (* frac baro))
(defun clamp (x lo hi) (max lo (min hi x)))

;;; --- stream sampling helpers ---

(defun %fraction-in-component (circuit comp-name gas)
  "Return two values: (fraction, present-p) for GAS in the OUT stream of COMP-NAME."
  (when circuit
    (let* ((stream (stream-by-name circuit comp-name))
           (fr     (and stream (ignore-errors (fractions stream)))))
      (when fr
        (multiple-value-bind (v presentp) (gethash gas fr)
          (values (or v 0.0) presentp))))))

;; Accepts either a PATIENT or a RESPIRATORY-CONTROLLER
(defun %ensure-patient (who)
  (etypecase who
    (patient who)
    (respiratory-controller (controller-patient who))))

(defun synthesize-capnogram-for-breath
    (who
     &key
       (fs *capno-default-fs*)
       (phase-ii-sharpness 4.0)
       (phase-iii-slope-mmHg-per-sec 0.5)
       (insp-fall-time 0.15))
  "Return two vectors (times, co2-mmHg) for one synthesized breath."
  (let* ((p (%ensure-patient who))
         (ctrl (respiratory-controller p))
         (rr (max 2.0 (respiratory-rate p)))
         (vt (max 100.0 (tidal-volume p)))
         (vd-anat (max 50.0 (anatomical-dead-space p)))
         (vdphys-frac (clamp (physiologic-deadspace-frac p) 0.0 0.9))
         (vd-phys (max vd-anat (* vdphys-frac vt)))
         (cycle-t (/ 60.0 rr))
         (airway-ds vd-anat)
         (alveolar-part (max 1.0 (- vt airway-ds)))
         (te (* 0.4 cycle-t))
         (ti (- cycle-t te))
         (t-phase-i (* te (/ airway-ds vt)))
         (t-phase-ii (* te (min 0.35 (/ (min alveolar-part (* 0.35 vt)) vt))))
         (t-phase-iii (max 0.05 (- te t-phase-i t-phase-ii)))
         ;; Use last expired (true EtCO2) if available; fall back to PaCO2 proxy:
         (pat-exp (last-expired-stream p))
         (etco2-mmHg (or (and pat-exp
                              (let* ((fr (fractions pat-exp))
                                     (f  (and fr (gethash "CO2" fr))))
                                (and f (frac->mmhg f))))
                         (controller-paco2 ctrl)))
         ;; FiCO2 from inspiratory limb if present:
         (circ (and ctrl (controller-circuit ctrl)))
         (fico2-frac (or (%fraction-in-component circ "inspiratory-limb" "CO2") 0.0))
         (fico2-mmHg (frac->mmhg fico2-frac))
         (n (max 1 (truncate (* fs cycle-t))))
         (times (make-array n :element-type 'single-float))
         (vals  (make-array n :element-type 'single-float)))
    (labels ((ease-sig (x s)
               (let ((x (clamp x 0.0 1.0)))
                 (/ 1.0 (+ 1.0 (expt (/ (- 1.0 x) (max 1e-6 x)) s))))))
      (loop for i from 0 below n
            for time = (/ i fs) do
              (setf (aref times i) (coerce time 'single-float))
              (cond
                ((< time ti)
                 (let* ((tau insp-fall-time)
                        (decay (exp (- (/ time (max 1e-3 tau)))))
                        (v (+ (* decay etco2-mmHg)
                              (* (- 1.0 decay) fico2-mmHg))))
                   (setf (aref vals i) (coerce v 'single-float))))
                ((< time (+ ti t-phase-i))
                 (setf (aref vals i) (coerce fico2-mmHg 'single-float)))
                ((< time (+ ti t-phase-i t-phase-ii))
                 (let* ((x (/ (- time ti t-phase-i) (max 1e-3 t-phase-ii)))
                        (w (ease-sig x phase-ii-sharpness))
                        (v (+ (* (- 1.0 w) fico2-mmHg)
                              (* w etco2-mmHg))))
                   (setf (aref vals i) (coerce v 'single-float))))
                ((< time (+ ti t-phase-i t-phase-ii t-phase-iii))
                 (let* ((x (/ (- time ti t-phase-i t-phase-ii)
                              (max 1e-3 t-phase-iii)))
                        (v (+ etco2-mmHg
                              (* x phase-iii-slope-mmHg-per-sec t-phase-iii))))
                   (setf (aref vals i) (coerce v 'single-float))))
                (t
                 (setf (aref vals i) (coerce etco2-mmHg 'single-float))))))
    (values times vals)))

(defun synthesize-capnogram (controller n-breaths &key (fs *capno-default-fs*))
  "Concatenate N synthesized breaths → (times, co2-mmHg)."
  (let* ((p (controller-patient controller))
         (rr (max 2.0 (respiratory-rate p)))
         (cycle-t (/ 60.0 rr))
         (nb (max 1 n-breaths))
         (times-list '())
         (vals-list  '())
         (t-offset 0.0))
    (dotimes (k nb)
      (multiple-value-bind (ts vs)
          (synthesize-capnogram-for-breath p :fs fs)   ;; <— pass p
        (let* ((len (length ts))
               (ts-shifted (make-array len :element-type 'single-float))
               (vs-copy    (make-array len :element-type 'single-float)))
          (dotimes (i len)
            (setf (aref ts-shifted i) (coerce (+ t-offset (aref ts i)) 'single-float))
            (setf (aref vs-copy    i) (aref vs i)))
          (push ts-shifted times-list)
          (push vs-copy    vals-list)
          (incf t-offset cycle-t))))
    (values (apply #'concatenate 'vector (nreverse times-list))
            (apply #'concatenate 'vector (nreverse vals-list)))))

(defun monitor-respiratory-status+capnogram (patient &optional (samples 16))
  "Normal monitor + capno numerics from the circuit. No PaCO2 fallback."
  (monitor-respiratory-status patient t)

  (let* ((ctrl (respiratory-controller patient))
	 (circ (and ctrl (controller-circuit ctrl)))
	 ;; read inspiratory-limb CO2
	 (fi-frac 0.0) (fi-present nil)
	 ;; read expiratory-limb CO2
	 (exp-frac 0.0) (exp-present nil)
	 ;; patient's true end-tidal sample (used for info only, not for 'exp limb')
	 (pat-exp (last-expired-stream patient))
	 (pat-et-frac (and pat-exp
			   (multiple-value-bind (v p) (gethash "CO2" (fractions pat-exp))
                             (when p v)))))
    
    (when circ
      (multiple-value-setq (fi-frac  fi-present)
	(%fraction-in-component circ "inspiratory-limb" "CO2"))
      (multiple-value-setq (exp-frac exp-present)
	(%fraction-in-component circ "capnometer-exp-port" "CO2")))
    
    (format t "~&~%-- Capnography --~%")
    (flet ((pp (present frac)
             (if present
		 (format nil "~,1F mmHg (~,2F%%)"
			 (* 760.0 frac) (* 100.0 frac))
		 "n/a")))
      (format t "FiCO2 (insp limb): ~a~%" (pp fi-present  fi-frac))
      (format t "EtCO2 (exp limb):  ~a~%" (pp exp-present exp-frac))
      (format t "EtCO2 (patient) : ~a~%"
              (if pat-et-frac
                  (format nil "~,1F mmHg (~,2F%%)"
                          (* 760.0 pat-et-frac) (* 100.0 pat-et-frac))
                  "n/a")))
    
    ;; Optional tiny preview waveform (still okay to synthesize; purely visual)
    (multiple-value-bind (ts vs)
	(synthesize-capnogram-for-breath patient :fs 100.0)
      (let* ((n (length ts)) (step (max 1 (truncate (/ n samples)))))
	(format t "~&Capnogram (mmHg)~%   ")
	(loop for i from 0 below n by step
              for k from 0 below samples do
		(format t "~,1F " (aref vs i)))
	(format t "~%")))))
