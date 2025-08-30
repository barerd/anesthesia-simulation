;;; Gas Stream with Environmental Properties

(in-package :anesthesia-sim)

(defclass gas-stream ()
  ((flow-rate :initarg :flow-rate :accessor flow-rate :initform 0.0 :documentation "L/min")
   (fractions :initarg :fractions :accessor fractions :initform (make-hash-table :test 'equal)
              :documentation "Hash table of gas -> fraction")
   (temperature :initarg :temperature :accessor temperature :initform *standard-temp*
		:documentation "Gas temperature (°C)")
   (pressure :initarg :pressure :accessor pressure :initform *standard-pressure*
             :documentation "Gas pressure (mbar)")
   (humidity :initarg :humidity :accessor humidity :initform 0.0
             :documentation "Relative humidity (0.0-1.0)")
   (h2o-fraction :accessor h2o-fraction :initform 0.0
                 :documentation "Water vapor fraction")
   (auto-humidify :initarg :auto-humidify :accessor auto-humidify :initform nil
                  :documentation "Whether to automatically add humidity on initialization")))

(defun make-dry-gas-stream (flow-rate temp pressure &rest gas-fraction-pairs)
  "Create a dry gas stream (no automatic humidity)."
  (let ((stream (make-instance 'gas-stream 
                              :flow-rate flow-rate
                              :temperature temp
                              :pressure pressure
                              :humidity 0.0
                              :auto-humidify nil)))
    (loop for (gas fraction) on gas-fraction-pairs by #'cddr
          do (setf (gethash (string gas) (fractions stream)) fraction))
    ;; Only renormalize if we actually have gases
    (when gas-fraction-pairs
      (renormalize-fractions! (fractions stream)))
    stream))

(defun make-humidified-gas-stream (flow-rate temp pressure humidity &rest gas-fraction-pairs)
  "Create a humidified gas stream (for patient exhaled gases, etc.)."
  (let ((stream (make-instance 'gas-stream 
                              :flow-rate flow-rate
                              :temperature temp
                              :pressure pressure
                              :humidity humidity
                              :auto-humidify t)))
    ;; First set the dry gas fractions
    (loop for (gas fraction) on gas-fraction-pairs by #'cddr
          do (setf (gethash (string gas) (fractions stream)) fraction))
    
    ;; Only add humidity if we have actual gas content
    (when gas-fraction-pairs
      (renormalize-fractions! (fractions stream))
      (add-humidity-to-stream! stream))
    
    stream))

(defun add-humidity-to-stream! (stream)
  "Add water vapor to an existing stream and renormalize."
  (with-slots (temperature pressure humidity h2o-fraction fractions) stream
    (let* ((sat-vapor-pressure (vapor-pressure-water temperature))
           (actual-vapor-pressure (* humidity sat-vapor-pressure))
           (h2o-frac (min 0.1 (/ actual-vapor-pressure pressure))))  ; Cap at 10%
      (setf h2o-fraction h2o-frac)
      (setf (gethash "H2O" fractions) h2o-frac)
      (renormalize-fractions! fractions))))

(defun make-gas-stream (flow-rate temp pressure humidity &rest gas-fraction-pairs)
  "Create gas stream with environmental properties - backward compatibility."
  (if (zerop humidity)
      (apply #'make-dry-gas-stream flow-rate temp pressure gas-fraction-pairs)
      (apply #'make-humidified-gas-stream flow-rate temp pressure humidity gas-fraction-pairs)))

;;; Pressure-Aware Mixing

(defgeneric mix-streams (streams &key target-pressure target-temp)
  (:documentation "Mix streams with pressure and temperature considerations."))

;; Handle single streams
(defmethod mix-streams ((stream gas-stream) &key 
                                (target-pressure *standard-pressure*)
                                (target-temp *standard-temp*))
  "Handle single stream case - just return the stream with updated properties if needed."
  (if (and (= target-pressure (pressure stream))
           (= target-temp (temperature stream)))
      stream
      (make-instance 'gas-stream
                     :flow-rate (flow-rate stream)
                     :fractions (clone-hash (fractions stream))
                     :temperature target-temp
                     :pressure target-pressure
                     :humidity (humidity stream))))

(defun make-fresh-gas-stream (flow-rate o2-fraction air-fraction n2o-fraction)
  "Create a dry fresh gas stream (medical gases are dry)."
  (let* ((n2-from-air (* air-fraction 0.79)) ; Air is ~79% N2
         (o2-from-air (* air-fraction 0.21)) ; Air is ~21% O2
         (total-o2 (+ o2-fraction o2-from-air))
         (total-n2 n2-from-air))
    (make-dry-gas-stream flow-rate 
                        *standard-temp* 
                        *standard-pressure*
                        "O2" total-o2
                        "N2" total-n2
                        "N2O" n2o-fraction)))

(defun make-patient-exhaled-stream (flow-rate temp co2-fraction o2-fraction)
  "Create a humidified exhaled stream from patient."
  (let ((n2-fraction (- 1.0 co2-fraction o2-fraction))) ; Remainder is N2
    (make-humidified-gas-stream flow-rate 
                               temp 
                               *standard-pressure*
                               1.0  ; Exhaled gas is 100% humidified
                               "O2" o2-fraction
                               "CO2" co2-fraction
                               "N2" n2-fraction)))

(defun make-co2-absorber-stream (input-stream co2-removal-efficiency fresh-gas-flow)
  "Create stream from CO2 absorber with physiologically accurate humidity behavior.
   CO2 + H2O → H2CO3, 2NaOH + H2CO3 → Na2CO3 + 2H2O
   Absorber adds humidity unless high FGF or heating occurs."
  (let* ((input-flow (flow-rate input-stream))
         (input-fractions (clone-hash (fractions input-stream)))
         (co2-fraction (gethash "CO2" input-fractions 0.0))
         (removed-co2 (* co2-fraction co2-removal-efficiency))
         (remaining-co2 (- co2-fraction removed-co2))
         
         ;; Calculate humidity addition based on FGF and absorber activity
         ;; High FGF (>4 L/min) or very low CO2 load reduces humidity addition
         (fgf-dilution-factor (min 1.0 (/ 4.0 (max fresh-gas-flow 0.1))))
         (co2-load-factor (min 1.0 (* co2-fraction 20.0))) ; More CO2 = more reaction = more H2O
         (humidity-addition-factor (* fgf-dilution-factor co2-load-factor))
         
         ;; Absorber can add up to 3-5% humidity under normal conditions
         (added-humidity (* 0.04 humidity-addition-factor))
         (current-h2o (gethash "H2O" input-fractions 0.0))
         (new-h2o (min 0.06 (+ current-h2o added-humidity))) ; Cap at 6%
         
         ;; Temperature increase from exothermic reaction (affects humidity capacity)
         (reaction-heat (* removed-co2 input-flow 0.5)) ; Simplified heat generation
         (temp-increase (min 5.0 reaction-heat)) ; Cap temperature rise
         (output-temp (+ (temperature input-stream) temp-increase)))
    
    ;; Update gas fractions
    (setf (gethash "CO2" input-fractions) remaining-co2)
    (setf (gethash "H2O" input-fractions) new-h2o)
    
    ;; Renormalize to account for CO2 removal and H2O addition
    (renormalize-fractions! input-fractions)
    
    ;; Create output stream
    (make-instance 'gas-stream
                   :flow-rate input-flow
                   :fractions input-fractions
                   :temperature output-temp
                   :pressure (pressure input-stream)
                   :humidity (/ new-h2o 0.06)))) ; Convert back to relative humidity

;;; Enhanced mixing to preserve gas identity

(defmethod mix-streams ((streams list) &key 
                                (target-pressure *standard-pressure*)
                                (target-temp *standard-temp*))
  "Mix streams accounting for pressure and temperature changes.
   Fixed to not lose gas composition through premature renormalization."
  (when (null streams)
    (return-from mix-streams (make-instance 'gas-stream 
                                           :temperature target-temp
                                           :pressure target-pressure)))
    
  (let ((total-mass-flow 0.0)
        (result-fractions (make-hash-table :test 'equal))
        (weighted-temp 0.0)
        (weighted-humidity 0.0))
    
    ;; Convert flows to mass flows and mix
    (dolist (stream streams)
      (let* ((vol-flow (flow-rate stream))
             (density-factor (gas-density-correction (temperature stream) (pressure stream)))
             (mass-flow (* vol-flow density-factor)))
        (incf total-mass-flow mass-flow)
        (incf weighted-temp (* (temperature stream) mass-flow))
        (incf weighted-humidity (* (humidity stream) mass-flow))
        
        (maphash (lambda (gas fraction)
                   (incf (gethash gas result-fractions 0.0)
                         (* fraction mass-flow)))
                 (fractions stream))))
    
    (if (zerop total-mass-flow)
        (make-instance 'gas-stream 
                      :temperature target-temp
                      :pressure target-pressure)
        (let* ((avg-temp (/ weighted-temp total-mass-flow))
               (avg-humidity (/ weighted-humidity total-mass-flow))
               (final-density-factor (gas-density-correction target-temp target-pressure))
               (result-vol-flow (/ total-mass-flow final-density-factor))
               (result-stream (make-instance 'gas-stream
                                           :flow-rate result-vol-flow
                                           :temperature target-temp
                                           :pressure target-pressure  
                                           :humidity avg-humidity)))
          
          ;; Normalize fractions ONLY if we have content
          (maphash (lambda (gas mass-weighted-fraction)
                     (setf (gethash gas result-fractions)
                           (/ mass-weighted-fraction total-mass-flow)))
                   result-fractions)
          
          ;; Set fractions without triggering automatic renormalization
          (setf (fractions result-stream) result-fractions)
          
          result-stream))))

;;; Utility to check if a stream has meaningful gas content

(defun stream-has-gas-content-p (stream)
  "Check if stream has any non-water gas content."
  (let ((non-water-fraction 0.0))
    (maphash (lambda (gas fraction)
               (unless (string= gas "H2O")
                 (incf non-water-fraction fraction)))
             (fractions stream))
    (> non-water-fraction 1e-6)))
