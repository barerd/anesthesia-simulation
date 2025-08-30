;;;; program.lisp - Manual loading version (alternative to ASDF)

(ql:quickload :bordeaux-threads)

(let ((kök-dizin "/Users/barerd/common-lisp/anesthesia-sim/"))
  ;; Load package definitions first
  (load (merge-pathnames #P"packages.lisp" kök-dizin))
  
  ;; Load utilities and physics
  (load (merge-pathnames #P"utilities.lisp" kök-dizin))
  (load (merge-pathnames #P"physics.lisp" kök-dizin))
  (load (merge-pathnames #P"gas-streams.lisp" kök-dizin))
  
  ;; Load components in dependency order
  (load (merge-pathnames #P"components/base.lisp" kök-dizin))
  (load (merge-pathnames #P"components/gas.lisp" kök-dizin))
  (load (merge-pathnames #P"components/circuit.lisp" kök-dizin))
  (load (merge-pathnames #P"components/patient.lisp" kök-dizin))
  (load (merge-pathnames #P"components/pathological-patients.lisp" kök-dizin))
  (load (merge-pathnames #P"components/rebreather.lisp" kök-dizin))
  
  ;; Load simulation system
  (load (merge-pathnames #P"circuit.lisp" kök-dizin))
  (load (merge-pathnames #P"simulation.lisp" kök-dizin))
  (load (merge-pathnames #P"interactive-simulation.lisp" kök-dizin))
  (load (merge-pathnames #P"inspection.lisp" kök-dizin))
  (load (merge-pathnames #P"factories.lisp" kök-dizin)))

;; Switch to the anesthesia-sim package and show usage
(in-package :anesthesia-sim)
(usage-examples)
(interactive-usage-examples)
