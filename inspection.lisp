;;; Inspection helpers

(in-package :anesthesia-sim)

(defun list-circuit-components (circuit)
   "List all components in circuit for debugging."
  (format t "~&=== Circuit Components ===~%")
  (dolist (comp (components circuit))
    (format t "~A: ~A (type: ~A)~%" 
            (name comp) 
            comp
            (type-of comp)))
  (format t "Total components: ~A~%" (length (components circuit))))

(defun debug-component-order (circuit)
  (format t "~&=== Component processing order ===~%")
  (dolist (c (components circuit))
    (format t "  ~A~%" (name c))))

(defun inspect-component (circuit component-name)
  "Inspect a specific component by name."
  (let ((component (find component-name (components circuit)
                         :key #'name :test #'string=)))
    (if component
        (progn
          (format t "~&Component: ~A~%" (name component))
          (format t "Type: ~A~%" (type-of component))
          (format t "Enabled: ~A~%" (enabled component))
          (typecase component
            (fresh-gas-inlet
             (format t "O2 flow: ~A L/min~%" (o2-flow component))
             (format t "Air flow: ~A L/min~%" (air-flow component))
             (format t "N2O flow: ~A L/min~%" (n2o-flow component)))
            (vaporizer
             (format t "Agent: ~A~%" (agent component))
             (format t "Setting: ~A%%~%" (setting component))
             (format t "Open: ~A~%" (open-p component))
             (format t "Volume: ~A L~%" (volume component)))
            (gas-mixer
             (format t "Volume: ~A L~%" (volume component)))
            (co2-absorber
             (format t "Efficiency: ~A~%" (efficiency component))
             (format t "Capacity: ~A g~%" (capacity component))
             (format t "Used: ~A g~%" (used component))
             (format t "Volume: ~A L~%" (volume component)))
            (one-way-valve
             (format t "Cracking pressure: ~A cmH2O~%" (cracking-pressure component))
             (format t "Resistance: ~A~%" (resistance component))
             (format t "Currently open: ~A~%" (is-open component)))
            (simple-lung
             (format t "Tidal volume: ~A mL~%" (tidal-volume component))
             (format t "Respiratory rate: ~A bpm~%" (respiratory-rate component))
             (format t "FRC: ~A mL~%" (frc component))
             (format t "CO2 production: ~A mL/min~%" (co2-production component))
             (format t "O2 consumption: ~A mL/min~%" (o2-consumption component)))))
        (format t "Component '~A' not found~%" component-name))))

(defun debug-patient-search (circuit &optional search-term)
  "Debug patient finding logic."
  (format t "~&=== Patient Search Debug ===~%")
  (format t "Searching for: ~A~%" (or search-term "any patient"))
  
  ;; List all components
  (format t "~%All components:~%")
  (dolist (comp (components circuit))
    (format t "  ~A (~A) - Patient type? ~A~%" 
            (name comp) 
            (type-of comp)
            (typep comp 'patient)))
  
  ;; Test the search
  (let ((found-patient (find-patient-in-circuit circuit search-term)))
    (if found-patient
        (format t "~%Found patient: ~A (~A)~%" 
                (name found-patient) (type-of found-patient))
        (format t "~%No patient found~%")))
  
  ;; Check gas streams
  (format t "~%Gas streams available:~%")
  (maphash (lambda (comp stream)
             (format t "  ~A: ~A~%" (name comp) stream))
           (gas-streams circuit)))

(defun show-gas-streams (circuit)
  "Display current gas streams; keys are component objects."
  (format t "~&Current gas streams:~%")
  (maphash (lambda (comp stream)
             (format t "  ~A-output: Flow=~,1F L/min, Fractions: ~A~%"
                     (name comp)
                     (flow-rate stream)
                     (hash-table-alist (fractions stream))))
           (gas-streams circuit)))

(defun show-connections (circuit)
  "Display circuit connections."
  (format t "~&Circuit connections:~%")
  (if (connection-objects circuit)
      (dolist (conn (connection-objects circuit))
        (format t "  ~A~%" conn))
      (dolist (conn (connections circuit))
        (destructuring-bind (from-comp from-port to-comp to-port) conn
          (format t "  ~A[~A] -> ~A[~A]~%"
                  (name from-comp) from-port
                  (name to-comp) to-port)))))

(defun get-component-output (circuit component-name)
  "Get the output stream of the component named COMPONENT-NAME."
  (let ((comp (find component-name (components circuit) :key #'name :test #'string=)))
    (when comp
      (gethash comp (gas-streams circuit)))))

(defun component-output (circuit comp)
  "Return the output stream stored for component object COMP."
  (gethash comp (gas-streams circuit)))

;;; Analysis Functions

(defun analyze-gas-composition (circuit component-name)
  "Detailed analysis of gas composition at component output."
  (let* ((comp (find component-name (components circuit) :key #'name :test #'string=))
         (stream (when comp (gethash comp (gas-streams circuit)))))
    (when stream
      (format t "~&=== Gas Analysis: ~A ===~%" component-name)
      (format t "Flow rate: ~,2F L/min~%" (flow-rate stream))
      (when (typep stream 'gas-stream)
        (format t "Temperature: ~,1F°C~%" (temperature stream))
        (format t "Pressure: ~,1F mbar~%" (pressure stream))
        (format t "Humidity: ~,1F%~%" (* 100 (humidity stream))))
      (format t "Gas fractions:~%")
      (maphash (lambda (gas fraction)
                 (format t "  ~A: ~,4F (~,2F%)~%" gas fraction (* 100 fraction)))
               (fractions stream))
      stream)))

(defun pressure-drop-analysis (circuit)
  "Analyze pressure drops across circuit components."
  (format t "~&=== Pressure Drop Analysis ===~%")
  (dolist (comp (components circuit))
    (let ((stream (gethash comp (gas-streams circuit))))
      (when (and stream (typep stream 'gas-stream))
        (format t "~A: ~,2F mbar, Flow: ~,2F L/min~%" 
                (name comp) (pressure stream) (flow-rate stream))))))

(defun thermal-analysis (circuit)
  "Analyze temperature distribution in circuit."
  (format t "~&=== Thermal Analysis ===~%")
  (dolist (comp (components circuit))
    (when (typep comp 'thermal-component)
      (format t "~A: Operating temp ~,1F°C~%" 
              (name comp) (operating-temp comp)))
    (let ((stream (gethash comp (gas-streams circuit))))
      (when (and stream (typep stream 'gas-stream))
        (format t "  Output gas: ~,1F°C, ~,1F% RH~%" 
                (temperature stream) (* 100 (humidity stream)))))))
