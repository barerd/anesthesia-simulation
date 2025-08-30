;;;; packages.lisp - Package definitions for anesthesia-sim

(in-package :cl-user)

(defpackage :anesthesia-sim
  (:use :cl :bordeaux-threads)
  (:export 
   ;; Physical constants
   #:*standard-temp*
   #:*standard-pressure*
   #:*body-temp*
   #:*water-vapor-pressure-37c*
   
   ;; Utility functions
   #:hash-table-alist
   #:clone-hash
   #:renormalize-fractions!
   #:lerp-fractions!
   
   ;; Physics functions
   #:celsius-to-kelvin
   #:vapor-pressure-water
   #:gas-density-correction
   
   ;; Gas stream classes and functions
   #:gas-stream
   #:gas-stream
   #:flow-rate
   #:fractions
   #:temperature
   #:pressure
   #:humidity
   #:make-gas-stream
   #:make-gas-stream
   #:mix-streams
   #:mix-streams
   
   ;; Base component classes
   #:gas-component
   #:dynamic-mixin
   #:thermal-component
   #:name
   #:enabled
   #:tau
   #:out-fractions
   #:operating-temp
   #:process-gas
   #:component-connections
   #:compute-target-fractions
   #:effective-tau
   #:post-process-dynamics
   #:update-temperature
   

   ;; Gas supply components
   #:fresh-gas-inlet
   #:o2-flow
   #:air-flow
   #:n2o-flow
   #:vaporizer
   #:vaporizer
   #:agent
   #:setting
   #:open-p
   #:volume
   #:vapor-pressure-coeffs
   #:vapor-pressure-agent
   
   ;; Circuit components
   #:gas-mixer
   #:co2-absorber
   #:efficiency
   #:capacity
   #:used
   #:one-way-valve
   #:cracking-pressure
   #:resistance
   #:is-open
   #:dead-space
   #:dead-volume
   #:stored-gas
   
   ;; Patient components
   #:simple-lung
   #:patient
   #:tidal-volume
   #:respiratory-rate
   #:frc
   #:co2-production
   #:o2-consumption
   #:respiratory-phase
   #:alveolar-temp
   #:airway-humidity
   #:anatomical-dead-space
   
   ;; Rebreather components
   #:volume-reflector
   #:reflector-volume
   #:compliance
   #:current-volume
   #:pressure-relief
   #:volume-exchanger
   #:exchanger-volume
   #:exchange-efficiency
   #:stored-mixture
   
   ;; Circuit and connections
   #:connection
   #:from-component
   #:to-component
   #:from-port
   #:to-port
   #:anesthesia-circuit
   #:components
   #:connections
   #:connection-objects
   #:gas-streams
   #:add-component
   #:add-connection
   #:disable-connection
   
   ;; Simulation functions
   #:simulate-circuit-with-loops
   #:run-for-seconds
   #:clone-gas-streams
   #:analyze-time-series
   #:export-time-series-csv
   #:steady-state-reached-p
   #:usage-examples
   
   ;; Inspection functions
   #:list-components
   #:inspect-component
   #:show-gas-streams
   #:show-connections
   #:get-component-output
   #:component-output
   #:analyze-gas-composition
   #:pressure-drop-analysis
   #:thermal-analysis
   
   ;; Factory functions
   #:make-complete-circle-system
   #:make-auxiliary-o2-system
   #:make-circle-system
   
   ;; Global variables
   #:*circuit*))
