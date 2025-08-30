;;; Simulation with Circular Dependency Resolution

(in-package :anesthesia-sim)

;;; Core solver

(defun simulate-circuit-step (circuit dt &key (max-iterations 10) (convergence-threshold 1e-6) (verbose nil))
  "Silent version of simulate-circuit-with-loops - returns convergence status."
  (clrhash (gas-streams circuit))
  
  ;; Initialize with zero flows
  (dolist (comp (components circuit))
    (setf (gethash comp (gas-streams circuit))
          (make-instance 'gas-stream :flow-rate 0.0)))
  
  ;; Iterative solver without verbose output
  (loop for iteration from 1 to max-iterations
        do (let ((previous-streams (make-hash-table :test 'eq))
                 (max-change 0.0))
	     
	     (when verbose
	       (format t "~&=== ITERATION ~A ===~%" iteration))
             
             ;; Save previous iteration's results
             (maphash (lambda (comp stream)
			(setf (gethash comp previous-streams) 
                              (make-instance 'gas-stream 
                                             :flow-rate (flow-rate stream)
                                             :fractions (clone-hash (fractions stream)))))
                      (gas-streams circuit))
             
             ;; Process all components
             (dolist (comp (components circuit))
               (let* ((input-stream (get-mixed-input-for-component circuit comp))
                      (output-stream (process-gas comp input-stream dt)))
                 (setf (gethash comp (gas-streams circuit)) output-stream)))
             
             ;; Check convergence
             (maphash (lambda (comp current-stream)
			(let* ((prev-stream (gethash comp previous-streams))
                               (flow-change (abs (- (flow-rate current-stream)
                                                    (flow-rate prev-stream)))))
                          (setf max-change (max max-change flow-change))))
                      (gas-streams circuit))
	     
	     (when verbose
	       (format t "  Max flow change: ~,6F L/min~%" max-change))
	     
             (when (< max-change convergence-threshold)
	       (when verbose
		 (format t "  Converged after ~A iterations (Δ=~,6F)~%" iteration max-change))
               (return-from simulate-circuit-step t)))
           
        finally (return nil)))

(defun clone-gas-streams (circuit)
  "Create snapshot of current gas streams."
  (let ((snapshot (make-hash-table :test 'eq)))
    (maphash (lambda (comp stream)
               (setf (gethash comp snapshot)
                     (make-instance 'gas-stream
                                   :flow-rate (flow-rate stream)
                                   :fractions (clone-hash (fractions stream)))))
             (gas-streams circuit))
    snapshot))

(defun run-for-seconds (circuit total-time &key (dt 1.0) (output-interval nil))
  "Run simulation for specified duration with optional data collection."
  (let ((current-time 0.0)
        (results '()))
    (loop while (< current-time total-time)
          do (simulate-circuit-step circuit dt)
             (when (or (null output-interval)
                      (zerop (mod (round current-time) output-interval)))
               (push (list current-time (clone-gas-streams circuit)) results))
             (incf current-time dt))
    (reverse results)))

;;; Patient/Stream helpers

(defun find-patient-in-circuit (circuit &optional (patient-name nil))
  "Find patient in circuit - by name or by type. Fixed to handle partial name matching."
  (if patient-name
      ;; First try exact match
      (or (find patient-name (components circuit) :key #'name :test #'string=)
          ;; Then try partial match (for "patient" matching "*-patient")
          (find-if (lambda (comp)
                     (and (slot-boundp comp 'name)
                          (stringp (name comp))
                          (search patient-name (name comp) :test #'string-equal)))
                   (components circuit))
          ;; Finally try type-based search if name contains "patient"
          (when (search "patient" patient-name :test #'string-equal)
            (find-if (lambda (comp) (typep comp 'patient))
                     (components circuit))))
      ;; Find any component that's a patient (ends with "-patient" or is patient type)
      (find-if (lambda (comp)
                 (or (typep comp 'patient)
                     (and (slot-boundp comp 'name)
                          (stringp (name comp))
                          (alexandria:ends-with-subseq "-patient" (name comp)))))
               (components circuit))))

(defun find-volatile-agent (fractions)
  "Find the volatile anesthetic agent in gas fractions."
  ;; Check all possible agent names (including case variations)
  (dolist (agent '("sevoflurane" "Sevoflurane" "SEVOFLURANE" 
                   "isoflurane" "Isoflurane" "ISOFLURANE"
                   "desflurane" "Desflurane" "DESFLURANE" 
                   "halothane" "Halothane" "HALOTHANE"
                   "enflurane" "Enflurane" "ENFLURANE"))
    (when (> (gethash agent fractions 0.0) 1e-6)
      (return agent)))
  
  ;; If none found, check for any unknown volatile agent
  ;; (anything not in the standard gas list)
  (let ((standard-gases '("O2" "CO2" "N2O" "N2" "H2O" "EtOH" "Acetone" "CO")))
    (maphash (lambda (gas fraction)
               (when (and (> fraction 1e-6)
                          (not (member gas standard-gases :test #'string=)))
                 (return-from find-volatile-agent gas)))
             fractions))
  
  "None")

(defun gethash-volatile-agent (fractions)
  "Get the fraction of the volatile anesthetic agent."
  ;; Check all possible agent names (including case variations)
  (dolist (agent '("sevoflurane" "Sevoflurane" "SEVOFLURANE" 
                   "isoflurane" "Isoflurane" "ISOFLURANE"
                   "desflurane" "Desflurane" "DESFLURANE" 
                   "halothane" "Halothane" "HALOTHANE"
                   "enflurane" "Enflurane" "ENFLURANE"))
    (let ((frac (gethash agent fractions 0.0)))
      (when (> frac 1e-6)
        (return frac))))
  
  ;; If none found, check for any unknown volatile agent
  (let ((standard-gases '("O2" "CO2" "N2O" "N2" "H2O" "EtOH" "Acetone" "CO")))
    (maphash (lambda (gas fraction)
               (when (and (> fraction 1e-6)
                          (not (member gas standard-gases :test #'string=)))
                 (return-from gethash-volatile-agent fraction)))
             fractions))
  
  0.0)

(defun format-patient-gases (circuit &optional (patient-name nil))
  "Format patient gas composition - finds patient intelligently with better error handling."
  (let* ((patient (find-patient-in-circuit circuit patient-name))
         (stream (when patient (gethash patient (gas-streams circuit)))))
    (cond 
      ((null patient)
       (format nil "No patient found in circuit (searched for: ~A)" 
               (or patient-name "any patient")))
      ((null stream)
       (format nil "Patient ~A found but no gas stream available" 
               (name patient)))
      (t
       (let ((fractions (fractions stream))
             (flow (flow-rate stream)))
         (format nil "~A: Flow=~,1F | O2=~,1F% CO2=~,1F% N2O=~,1F% N2=~,1F% H2O=~,1F% ~A=~,1F%~@[ EtOH=~,1F%~]~@[ Acetone=~,1F%~]~@[ CO=~,1F%~]"
                 (name patient)
                 flow
                 (* 100 (gethash "O2" fractions 0.0))
                 (* 100 (gethash "CO2" fractions 0.0))
                 (* 100 (gethash "N2O" fractions 0.0))
                 (* 100 (gethash "N2" fractions 0.0))
		 (* 100 (gethash "H2O" fractions 0.0))
                 (or (find-volatile-agent fractions) "None")
                 (* 100 (gethash-volatile-agent fractions))
                 (let ((etoh (gethash "EtOH" fractions 0.0)))
                   (when (> etoh 1e-6) (* 100 etoh)))
                 (let ((acetone (gethash "Acetone" fractions 0.0)))
                   (when (> acetone 1e-6) (* 100 acetone)))
                 (let ((co (gethash "CO" fractions 0.0)))
                   (when (> co 1e-6) (* 100 co)))))))))

;; Alternative debug function to see exactly what's in the gas stream
(defun debug-all-gases (circuit component-name)
  "Show ALL gases present in a component's stream for debugging."
  (let ((comp (find-if (lambda (c) (string= (name c) component-name))
                       (components circuit))))
    (if comp
        (let ((stream (gethash comp (gas-streams circuit))))
          (if stream
              (progn
                (format t "~%=== All gases in ~A stream ===~%" component-name)
                (maphash (lambda (gas fraction)
                           (format t "  \"~A\": ~,4F (~,2F%)~%" 
                                   gas fraction (* fraction 100)))
                         (fractions stream))
                (format t "Total flow: ~,2F L/min~%" (flow-rate stream)))
              (format t "No stream found for ~A~%" component-name)))
        (format t "Component ~A not found~%" component-name))))

;;; Background controller for interactive and teaching runs

(defclass simulation-controller ()
  ((circuit :initarg :circuit :accessor circuit)
   (running :accessor running :initform nil)
   (paused :accessor paused :initform nil)
   (current-time :accessor current-time :initform 0.0)
   (dt :initarg :dt :accessor dt :initform 1.0)
   (history :accessor history :initform nil)
   (thread :accessor thread :initform nil)
   (lock :accessor lock :initform (bt:make-lock "sim-lock"))
   (stop-requested :accessor stop-requested :initform nil)))

(defparameter *sim-controller* nil "Global simulation controller")

(defun start-simulation (circuit &key (dt 1.0))
  "Start non-blocking simulation in separate thread."
  (labels ((thread-alive-p (th)
             (and th
                  ;; if BT has thread-alive-p use it; else ignore-errors wrapper
                  (ignore-errors (bt:thread-alive-p th)))))
    (when (and *sim-controller*
               (or (running *sim-controller*)
                   (thread-alive-p (thread *sim-controller*))))
      (format t "Stopping existing simulation...~%")
      (stop-simulation)))

  (setf *sim-controller* (make-instance 'simulation-controller :circuit circuit :dt dt)
        (running *sim-controller*) t
        (stop-requested *sim-controller*) nil
        (current-time *sim-controller*) 0.0)

  (setf (thread *sim-controller*)
        (bt:make-thread (lambda () (simulation-loop *sim-controller*))
                        :name "anesthesia-sim"))
  (format t "Simulation started (dt=~A s)~%" dt)
  *sim-controller*)

(defun pause-simulation ()
  (when (and *sim-controller* (running *sim-controller*))
    (bt:with-lock-held ((lock *sim-controller*))
      (setf (paused *sim-controller*) t))
    (format t "Simulation paused.~%")))

(defun resume-simulation ()
  (when (and *sim-controller* (running *sim-controller*))
    (bt:with-lock-held ((lock *sim-controller*))
      (setf (paused *sim-controller*) nil))
    (format t "Simulation resumed.~%")))

;;; --- robust stop ---
(defun stop-simulation ()
  "Stop the running simulation (robust to crashed/aborted threads)."
  (when *sim-controller*
    (setf (stop-requested *sim-controller*) t)
    (let ((th (thread *sim-controller*)))
      (when th
        (handler-case
            (bt:join-thread th)               ; join if it can
          (#-bordeaux-threads sb-thread:join-thread-error
           #+bordeaux-threads error          ; portable-ish fallback
           (e)
           (format t "Note: previous sim thread did not return normally (~A). Forcing cleanup.~%" e))
          (error (e)
            (format t "Note: error joining simulation thread: ~A~%" e)))))
    ;; cleanup state regardless
    (setf (running *sim-controller*) nil
          (thread *sim-controller*)  nil
          (stop-requested *sim-controller*) nil)
    (format t "Simulation stopped~%")))

(defun simulation-status ()
  "Show current simulation status."
  (if (and *sim-controller* (running *sim-controller*))
      (bt:with-lock-held ((lock *sim-controller*))
        (format t "~&=== Simulation Status ===~%")
        (format t "Running: YES~%")
        (format t "Time: ~,1F s~%" (current-time *sim-controller*))
        (format t "dt: ~A s~%" (dt *sim-controller*))
        (format t "History points: ~A~%" (length (history *sim-controller*))))
      (format t "Simulation not running~%")))

(defun simulation-loop (controller)
  "Main simulation loop running in a separate thread. Each tick advances
   circuit mixing and patient physiology (PK/PD, drive, push RR/VT)."
  (unwind-protect
       (handler-case
           (loop while (and (running controller)
                            (not (stop-requested controller)))
                 do (progn
                      (bt:with-lock-held ((lock controller))
                        (unless (paused controller)
                          (let* ((dt   (dt controller))
                                 (circ (circuit controller))
                                 ;; 1) advance circuit mixing for this dt
                                 (converged (simulate-circuit-step circ dt)))
                            ;; 2) advance physiology for the same dt
                            (let* ((patient (find-patient-in-circuit circ "patient"))
                                   (rc      (and patient (respiratory-controller patient))))
                              (when rc
                                (update-respiratory-physiology rc dt)))
                            ;; 3) advance time & log
                            (incf (current-time controller) dt)
                            (format t "t=~,1F: ~A~%"
                                    (current-time controller)
                                    (format-patient-gases circ "patient"))
                            (when (zerop (mod (round (current-time controller)) 5))
                              (push (list (current-time controller)
                                          (clone-gas-streams circ))
                                    (history controller)))
                            (unless converged
                              (format t "    WARNING: Simulation didn't converge at t=~,1F~%"
                                      (current-time controller))))))
                      ;; keep UI responsive (sleep *outside* the lock)
                      (sleep (min 0.1 (dt controller)))))
         (error (e)
           (format t "~&Simulation thread aborted due to error: ~A~%" e)))
    ;; always mark as stopped
    (setf (running controller) nil)
    (format t "~%Simulation stopped at t=~,1F s~%" (current-time controller))))

;;; Interactive Control Functions

(defun set-vaporizer (vap-name setting &key (open-p t))
  "Adjust vaporizer setting during simulation."
  (when (and *sim-controller* (running *sim-controller*))
    (bt:with-lock-held ((lock *sim-controller*))
      (let ((vap (find vap-name (components (circuit *sim-controller*))
                       :key #'name :test #'string=)))
        (when vap
          (setf (setting vap) setting)
          (setf (open-p vap) open-p)
          (format t "Vaporizer ~A: ~A% (open: ~A)~%" 
                  vap-name setting open-p)))))
  (unless (and *sim-controller* (running *sim-controller*))
    (format t "No simulation running~%")))

(defun set-fresh-gas-flows (fgf-name &key o2-flow air-flow n2o-flow)
  "Adjust fresh gas flows during simulation."
  (when (and *sim-controller* (running *sim-controller*))
    (bt:with-lock-held ((lock *sim-controller*))
      (let ((fgf (find fgf-name (components (circuit *sim-controller*))
                       :key #'name :test #'string=)))
        (when fgf
          (when o2-flow (setf (o2-flow fgf) o2-flow))
          (when air-flow (setf (air-flow fgf) air-flow))
          (when n2o-flow (setf (n2o-flow fgf) n2o-flow))
          (format t "FGF ~A: O2=~A Air=~A N2O=~A L/min~%" 
                  fgf-name 
                  (o2-flow fgf) (air-flow fgf) (n2o-flow fgf))))))
  (unless (and *sim-controller* (running *sim-controller*))
    (format t "No simulation running~%")))

(defun set-fgf-flows (circuit &key (o2 0.5) (air 0.5) (n2o 0.0))
  "Convenience setter for fresh gas flows (L/min) on the component named \"FGF\"."
  (let ((fgf (find "FGF" (components circuit) :key #'name :test #'string=)))
    (when fgf
      (setf (o2-flow fgf)  o2
            (air-flow fgf) air
            (n2o-flow fgf) n2o)
      (format t "FGF set: O2=~,1F L/min, Air=~,1F L/min, N2O=~,1F L/min~%" o2 air n2o))
    fgf))

(defun set-patient-ventilation (patient-name &key tidal-volume respiratory-rate)
  "Adjust patient ventilation during simulation."
  (when (and *sim-controller* (running *sim-controller*))
    (bt:with-lock-held ((lock *sim-controller*))
      (let ((patient (find patient-name (components (circuit *sim-controller*))
                           :key #'name :test #'string=)))
        (when patient
          (when tidal-volume (setf (tidal-volume patient) tidal-volume))
          (when respiratory-rate (setf (respiratory-rate patient) respiratory-rate))
          (format t "Patient ~A: TV=~A mL, RR=~A bpm~%" 
                  patient-name 
                  (tidal-volume patient) (respiratory-rate patient))))))
  (unless (and *sim-controller* (running *sim-controller*))
    (format t "No simulation running~%")))

(defun disable-component (comp-name)
  "Disable a component during simulation."
  (when (and *sim-controller* (running *sim-controller*))
    (bt:with-lock-held ((lock *sim-controller*))
      (let ((comp (find comp-name (components (circuit *sim-controller*))
                        :key #'name :test #'string=)))
        (when comp
          (setf (enabled comp) nil)
          (format t "Component ~A disabled~%" comp-name)))))
  (unless (and *sim-controller* (running *sim-controller*))
    (format t "No simulation running~%")))

(defun enable-component (comp-name)
  "Enable a component during simulation."
  (when (and *sim-controller* (running *sim-controller*))
    (bt:with-lock-held ((lock *sim-controller*))
      (let ((comp (find comp-name (components (circuit *sim-controller*))
                        :key #'name :test #'string=)))
        (when comp
          (setf (enabled comp) t)
          (format t "Component ~A enabled~%" comp-name)))))
  (unless (and *sim-controller* (running *sim-controller*))
    (format t "No simulation running~%")))

(defun simulation-snapshot ()
  "Take a snapshot of current simulation state."
  (when (and *sim-controller* (running *sim-controller*))
    (bt:with-lock-held ((lock *sim-controller*))
      (format t "~&=== Simulation Snapshot (t=~,1F s) ===~%" 
              (current-time *sim-controller*))
      (show-gas-streams (circuit *sim-controller*))
      (format t "~%"))))

(defun get-current-output (comp-name)
  "Get current output of a component during simulation."
  (when (and *sim-controller* (running *sim-controller*))
    (bt:with-lock-held ((lock *sim-controller*))
      (analyze-gas-composition (circuit *sim-controller*) comp-name))))

;;; Scripted mode (deterministic, single-thread)

(defun simulation-time (&optional (controller *sim-controller*))
  "Thread-safe read of current simulation time (seconds)."
  (if (and controller (running controller))
      (bt:with-lock-held ((lock controller))
        (current-time controller))
      0.0))

(defun advance-sim-seconds (seconds &key (poll 0.01))
  "Block the caller until the background sim's current-time advances by SECONDS."
  (unless (and *sim-controller* (running *sim-controller*))
    (error "No running simulation to advance."))
  (let* ((start (simulation-time))
         (goal (+ start (max 0.0 seconds))))
    (loop while (< (simulation-time) goal) do (sleep poll))
    (- (simulation-time) start)))

(defun step-simulation-once (circuit dt)
  (simulate-circuit-step circuit dt))

(defun run-scripted (&key (patient-type 'healthy) (dt 1.0) (tend 600.0) (script-fn nil))
  "Single-threaded run: executes SCRIPT-FN at specific sim times, stepping deterministically."
  (multiple-value-bind (circuit patient machine)
      (make-controlled-respiratory-system :patient-type patient-type)
    (declare (ignore machine))
    (let ((sim-time 0.0))
      (loop while (< sim-time tend)
            do (when script-fn
                 (funcall script-fn circuit patient sim-time))
               (step-simulation-once circuit dt)
               (incf sim-time dt))
      (values circuit patient))))

;;; Scenarios & Examples

(defun run-typical-induction ()
  "Simulate: preoxygenation (aux O2 via simple mask 6 L/min) → propofol 200 mg,
   fentanyl 100 mcg → manual mask ventilation on circle at FiO2≈0.8 (O2 3 L/min, Air 1 L/min)
   → rocuronium 50 mg (not modeled; we assume apnea risk handled by assisted/controlled modes)
   → after 2 min start controlled ventilation (RR 12, VT 500, PEEP 5) and open sevo at 3%
   → simulate 10 minutes and print status + key gas nodes."
  (multiple-value-bind (circuit patient machine)
      (make-controlled-respiratory-system :patient-type 'healthy :initial-mode "auxiliary-o2")
    (declare (ignore machine))
    ;; 0) Start stepping the circuit
    (start-simulation circuit :dt 1.0)

    ;; 1) Preoxygenation: simple face mask @ 6 L/min
    (set-aux-device patient :simple-mask :flow 6.0)
    (format t "~&-- Preoxygenation: simple mask @ 6 L/min --~%")
    (advance-sim-seconds 5.0)
    (monitor-respiratory-status patient)

    ;; 2) Induction boluses
    (administer-drug (respiratory-controller patient) "propofol" 200.0)
    (administer-drug (respiratory-controller patient) "fentanyl" 0.1) ; 100 mcg
    (format t "~&-- Induction: propofol 200 mg + fentanyl 100 mcg --~%")
    ;; 3) Manual mask ventilation on the circle, target FiO2 ~0.8 (O2 3, Air 1)
    (intervene-ventilation patient "manual" :pressure-support 12.0)
    (set-fgf-flows circuit :o2 3.0 :air 1.0 :n2o 0.0)
    (format t "~&-- Manual mask ventilation on circle, target FiO2≈0.8 --~%")
    ;; Let PK/PD equilibrate a bit (e.g., 15 s)
    (advance-sim-seconds 15.0)
    (monitor-respiratory-status patient)

    ;; 4) Rocuronium 50 mg (not modeled; we assume apnea risk handled by support)
    (format t "~&-- Rocuronium 50 mg (note: neuromuscular block not modeled) --~%")

    ;; 5) Wait ~2 minutes of manual ventilation
    (advance-sim-seconds 120.0)
    (monitor-respiratory-status patient)

    ;; 6) Switch to controlled ventilation; open sevo @ 3%
    (intervene-ventilation patient "controlled"
                           :respiratory-rate 12.0
                           :tidal-volume 500.0
                           :peep 5.0)
    (set-vaporizer "sevoflurane-vap" 3.0)
    (format t "~&-- Controlled ventilation started (RR=12, VT=500, PEEP=5); Sevo 3% --~%")

    ;; 7) Simulate the next 3 minutes
    (advance-sim-seconds 180.0)

    ;; 8) Final report
    (format t "~&== Final (10 min post-switch) ==~%")
    (monitor-respiratory-status patient t)
    (debug-all-gases circuit "controlled-patient")
    (debug-all-gases circuit "inspiratory-limb")
    (debug-all-gases circuit "expiratory-limb")
    (debug-all-gases circuit "FGF")

    ;; 9) Stop
    (stop-simulation)
    (values circuit patient)))

(defun usage-examples ()
  "Print usage examples for the anesthesia circuit simulation."
  (format t "~&=== Anesthesia Circuit Simulation Usage Examples ===~%")
  (format t "~%1. Test the system):~%")
  (format t "   (setf *circuit* (make-circle-system))~%")
  (format t "~%2. Run extended simulation:~%")
  (format t "   (run-for-seconds *circuit* 30 :dt 1.0)~%")
  (format t "~%3. Analyze results:~%")
  (format t "   (analyze-gas-composition *circuit* \"patient\")~%")
  (format t "~&=== Interactive Simulation Usage ===~%")
  (format t "1. Start simulation:~%")
  (format t "   (start-simulation (make-circle-system) :dt 1.0)~%")
  (format t "~%2. Control vaporizer:~%")
  (format t "   (set-vaporizer \"sevoflurane-vap\" 2.5 :open-p t)~%")
  (format t "~%3. Adjust fresh gas:~%")
  (format t "   (set-fresh-gas-flows \"FGF\" :o2-flow 2.0 :air-flow 3.0)~%")
  (format t "~%4. Adjust patient:~%")
  (format t "   (set-patient-ventilation \"patient\" :tidal-volume 600 :respiratory-rate 14)~%")
  (format t "~%5. Monitor:~%")
  (format t "   (simulation-status)~%")
  (format t "   (simulation-snapshot)~%")
  (format t "   (get-current-output \"patient\")~%")
  (format t "~%6. Component control:~%")
  (format t "   (disable-component \"CO2-absorber\")~%")
  (format t "   (enable-component \"CO2-absorber\")~%")
  (format t "~%7. Stop simulation:~%")
  (format t "   (stop-simulation)~%"))
