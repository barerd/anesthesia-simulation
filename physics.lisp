;;; Temperature and Pressure Utilities

(in-package :anesthesia-sim)

(defparameter *standard-temp* 20.0 "Standard temperature (°C)")
(defparameter *standard-pressure* 1013.25 "Standard pressure (mbar)")
(defparameter *body-temp* 37.0 "Body temperature (°C)")
(defparameter *water-vapor-pressure-37c* 62.7 "Water vapor pressure at 37°C (mbar)")

(defun celsius-to-kelvin (celsius)
  "Convert Celsius to Kelvin."
  (+ celsius 273.15))

(defun vapor-pressure-water (temp-c)
  "Antoine equation for water vapor pressure (mbar) at °C input."
  (let* ((A 8.07131) (B 1730.63) (C 233.426)       ; mmHg, °C
         (log10-p (- A (/ B (+ temp-c C))))
         (mmhg (expt 10 log10-p))
         (mbar (* 1.33322 mmhg)))
    mbar))

;; (defun vapor-pressure-water (temp-c)
;;   "Antoine equation for water vapor pressure (mbar) at temperature (°C)."
;;   (let ((temp-k (celsius-to-kelvin temp-c)))
;;     (* 10.0 (expt 10 (- 8.07131 (/ 1730.63 (+ temp-k 233.426)))))))

(defun saturated-rh-at (temp-c)
  "Relative humidity at saturation (RH=1.0). Keep simple for now."
  (declare (ignore temp-c))
  1.0)

(defun ph2o-mmHg-at (temp-c)
  "Water vapour pressure (mmHg) at temp-c; 47 mmHg at 37°C."
  ;; You already have an Antoine-like function returning mbar; reuse it if preferred.
  (* 0.750062 (vapor-pressure-water temp-c))) ; mbar -> mmHg

(defun gas-density-correction (temp-c pressure-mbar)
  "Density correction factor for temperature and pressure vs STP."
  (/ (* *standard-pressure* (celsius-to-kelvin temp-c))
     (* pressure-mbar (celsius-to-kelvin *standard-temp*))))
