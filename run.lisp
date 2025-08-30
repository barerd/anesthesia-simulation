
;; 1. Start simulation
(setf *circuit* (make-circle-system))
(start-simulation *circuit* :dt 1.0)

;; 2. Make adjustments while running
(set-vaporizer "sevoflurane-vap" 2.5 :open-p t)
(set-fresh-gas-flows "FGF" :o2-flow 2.0 :air-flow 3.0 :n2o-flow 0.5)
(set-patient-ventilation "patient" :tidal-volume 600 :respiratory-rate 14)

;; 3. Monitor progress
(simulation-status)
(simulation-snapshot)
(get-current-output "patient")

;; 4. Test failure scenarios
(disable-component "CO2-absorber")
(enable-component "CO2-absorber")

;; 5. Stop when done
(stop-simulation)
