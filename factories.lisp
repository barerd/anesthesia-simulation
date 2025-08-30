;;; Factory Functions

(in-package :anesthesia-sim)

(defun make-complete-circle-system ()
  "Create a complete anesthesia circuit with one-way valves and patient."
  (let ((circuit (make-instance 'anesthesia-circuit))
        ;; Gas sources
        (fgf-inlet (make-instance 'fresh-gas-inlet :name "FGF"
                                  :o2-flow 1.0 :air-flow 1.0))
        (sevo-vap (make-instance 'vaporizer :name "sevoflurane-vap" 
                                 :agent "sevoflurane" :setting 2.0 :open-p t))
        
        ;; Circuit components
        (insp-valve (make-instance 'one-way-valve :name "inspiratory-valve"))
        (exp-valve (make-instance 'one-way-valve :name "expiratory-valve"))
        (absorber (make-instance 'co2-absorber :name "CO2-absorber"))
        (insp-limb (make-instance 'gas-mixer :name "inspiratory-limb" :volume 1.0))
        (exp-limb (make-instance 'gas-mixer :name "expiratory-limb" :volume 1.0))
        
        ;; Patient
        (patient (make-instance 'simple-lung :name "patient")))
    
    ;; Add all components
    (dolist (comp (list fgf-inlet sevo-vap insp-valve exp-valve 
                       absorber insp-limb exp-limb patient))
      (add-component circuit comp))
    
    ;; Create the circular topology using new interface
    ;; Fresh gas path: FGF -> Vaporizer -> Absorber -> Inspiratory limb
    (add-connection circuit fgf-inlet sevo-vap)
    (add-connection circuit sevo-vap absorber)
    (add-connection circuit absorber insp-limb)
    
    ;; To patient: Inspiratory limb -> Inspiratory valve -> Patient
    (add-connection circuit insp-limb insp-valve)
    (add-connection circuit insp-valve patient)
    
    ;; From patient: Patient -> Expiratory valve -> Expiratory limb -> Absorber
    (add-connection circuit patient exp-valve)
    (add-connection circuit exp-valve exp-limb)
    (add-connection circuit exp-limb absorber) ; This closes the circle!
    
    circuit))

(defun make-auxiliary-o2-system ()
  "Create a simple auxiliary O2 delivery system."
  (let ((circuit (make-instance 'anesthesia-circuit))
        (o2-source (make-instance 'fresh-gas-inlet :name "aux-O2"
                                                   :o2-flow 6.0
                                                   :air-flow 0.0
                                                   :n2o-flow 0.0)))
    (add-component circuit o2-source)
    circuit))

(defun make-circle-system (&key (patient-type 'healthy))
  "Create complete circle system with specified patient type."
  (let* (;; Create patient FIRST with improved type handling
         (patient (cond
                    ;; Exact matches
                    ((eq patient-type 'healthy) (make-healthy-patient))
                    ((eq patient-type 'ketotic-mild) (make-ketotic-patient :ketosis-severity 'mild))
                    ((eq patient-type 'ketotic-moderate) (make-ketotic-patient :ketosis-severity 'moderate))
                    ((eq patient-type 'ketotic-severe) (make-ketotic-patient :ketosis-severity 'severe))
                    ((eq patient-type 'smoker) (make-heavy-smoker-patient :pack-years 20 :co-level 0.0008))
                    ((eq patient-type 'alcoholic) (make-alcohol-intoxicated-patient :blood-alcohol 0.10))
                    ((eq patient-type 'copd-mild)     (make-copd-patient :severity :mild))
		    ((eq patient-type 'copd-moderate) (make-copd-patient :severity :moderate))
		    ((eq patient-type 'copd-severe)   (make-copd-patient :severity :severe))
		    ((eq patient-type 'ards-mild)     (make-ards-patient :severity :mild))
		    ((eq patient-type 'ards-moderate) (make-ards-patient :severity :moderate))
		    ((eq patient-type 'ards-severe)   (make-ards-patient :severity :severe))
	   
                    ;; Partial matches for convenience
                    ((member patient-type '(ketotic ketosis)) 
                     (make-ketotic-patient :ketosis-severity 'moderate)) ; default to moderate
                    ((member patient-type '(smoking heavy-smoker)) 
                     (make-heavy-smoker-patient :pack-years 20 :co-level 0.0008))
                    ((member patient-type '(alcohol drunk intoxicated)) 
                     (make-alcohol-intoxicated-patient :blood-alcohol 0.10))
		    ((eq patient-type 'copd) (make-copd-patient :severity :moderate))
		    ((eq patient-type 'ards) (make-ards-patient :severity :moderate))
                    
                    ;; Default case with warning
                    (t 
                     (format t "Warning: Unknown patient type '~A', using healthy patient~%" patient-type)
                     (make-healthy-patient))))
         
         ;; Then create circuit and other components
         (circuit (make-instance 'anesthesia-circuit))
         (fgf-inlet (make-instance 'fresh-gas-inlet :name "FGF"
                                   :o2-flow 0.5 :air-flow 0.5))
         (sevo-vap (make-instance 'vaporizer :name "sevoflurane-vap"
                                 :agent "sevoflurane" :setting 0.0 :open-p nil
                                 :operating-temp 22.0))
         
         ;; Circuit components
         (insp-valve (make-instance 'one-way-valve :name "inspiratory-valve"))
         (exp-valve (make-instance 'one-way-valve :name "expiratory-valve"))
         (absorber (make-instance 'co2-absorber :name "CO2-absorber"))
         (insp-limb (make-instance 'gas-mixer :name "inspiratory-limb" :volume 1.0))
         (exp-limb (make-instance 'gas-mixer :name "expiratory-limb" :volume 1.0))
         
         ;; Dead spaces
         (ett-dead-space (make-instance 'dead-space :name "ETT-dead-space"
                                        :dead-volume 15.0))
         (y-piece-dead (make-instance 'dead-space :name "Y-piece-dead"
                                      :dead-volume 10.0))
         
         ;; Rebreather component
         (volume-reflector (make-instance 'volume-reflector :name "vol-reflector"
                                          :reflector-volume 2000.0)))
    
    ;; Add all components INCLUDING the patient
    (dolist (comp (list fgf-inlet sevo-vap insp-valve exp-valve absorber
                       insp-limb exp-limb ett-dead-space y-piece-dead
                       volume-reflector patient))
      (add-component circuit comp))
    
    ;; Build the complete topology
    (add-connection circuit fgf-inlet sevo-vap)
    (add-connection circuit sevo-vap volume-reflector)
    (add-connection circuit volume-reflector absorber)
    (add-connection circuit absorber insp-limb)
    (add-connection circuit insp-limb insp-valve)
    (add-connection circuit insp-valve y-piece-dead)
    (add-connection circuit y-piece-dead ett-dead-space)
    (add-connection circuit ett-dead-space patient)
    
    ;; Return path
    (add-connection circuit patient ett-dead-space)
    (add-connection circuit ett-dead-space y-piece-dead) 
    (add-connection circuit y-piece-dead exp-valve)
    (add-connection circuit exp-valve exp-limb)
    (add-connection circuit exp-limb absorber) ; Close the circle
    
    ;; Report what patient was created
    (format t "Created circuit with ~A (~A)~%" (name patient) (type-of patient))
    
    circuit))

(defun replace-patient (circuit new-patient)
  "Replace the patient in an existing circuit without disrupting topology."
  (let ((old-patient (find-if (lambda (comp) (typep comp 'patient))
                             (components circuit))))
    
    (unless old-patient
      (format t "No existing patient found in circuit~%")
      (return-from replace-patient nil))
    
    ;; Update components list
    (setf (components circuit) 
          (substitute new-patient old-patient (components circuit)))
    
    ;; Update connection objects (new enhanced connections)
    (dolist (conn (connection-objects circuit))
      (when (eq (from-component conn) old-patient)
        (setf (from-component conn) new-patient))
      (when (eq (to-component conn) old-patient)
        (setf (to-component conn) new-patient)))
    
    ;; Update old-style connections for backward compatibility
    (setf (connections circuit)
          (mapcar (lambda (conn)
                   (cond
                     ((listp conn)
                      ;; Update list-based connection
                      (list (if (eq (first conn) old-patient) new-patient (first conn))
                            (second conn) ; port name
                            (if (eq (third conn) old-patient) new-patient (third conn))
                            (fourth conn))) ; port name
                     (t conn)))
                 (connections circuit)))
    
    (format t "Patient ~A replaced with ~A (~A)~%" 
            (name old-patient) (name new-patient) (type-of new-patient))
    new-patient))

(defun reconnect-patient-to-circuit (circuit new-patient old-patient)
  "Reconnect new patient to the circuit."
  ;; Find ETT dead space to connect to patient
  (let ((ett-dead (find "ETT-dead-space" (components circuit) :key #'name :test #'string=)))
    (when ett-dead
      ;; Add bidirectional connection between ETT and patient
      (add-connection circuit ett-dead new-patient)
      (add-connection circuit new-patient ett-dead))))
