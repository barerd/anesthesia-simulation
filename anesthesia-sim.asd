;;; Copyright (c) 2020-2030, Dr. Basar Erdivanlı.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;;; anesthesia-sim.asd - ASDF system definition

(in-package :cl-user)

(defvar *anesthesia-sim-version* "0.0.1"
  "A string denoting the current version. Used for diagnostic output.")

#-asdf3 (error "anesthesia-sim requires ASDF 3")

(asdf:defsystem :anesthesia-sim
  :description "anesthesia-sim: Anesthesia Circuit Simulation System"
  :version #.*anesthesia-sim-version*
  :author "Başar Erdivanlı <basar.erdivanli@erdogan.edu.tr>"
  :licence "MIT"
  :serial nil  ; Changed to nil for proper dependency handling
  :depends-on (:swank :bordeaux-threads)
  :components
  ((:file "packages")
   (:file "utilities" :depends-on ("packages"))
   (:file "physics" :depends-on ("packages" "utilities"))
   (:file "gas-streams" :depends-on ("packages" "utilities" "physics"))
   (:module "components"
    :depends-on ("packages" "utilities" "physics" "gas-streams")
    :components
    ((:file "base")
     (:file "gas" :depends-on ("base"))
     (:file "circuit" :depends-on ("base"))
     (:file "patient" :depends-on ("base"))
     (:file "pathological-patients" :depends-on ("base"))
     (:file "rebreather" :depends-on ("base"))))
   (:file "circuit" :depends-on ("packages" "components"))
   (:file "simulation" :depends-on ("packages" "circuit" "components"))
   (:file "inspection" :depends-on ("packages" "simulation" "components"))
      (:file "factories" :depends-on ("packages" "components" "circuit"))
   (:file "environmental-contamination-system" :depends-on ("packages" "components" "circuit"))
   (:file "ventilation-modes" :depends-on ("packages" "components" "circuit"))
   (:file "respiratory-control-system" :depends-on ("packages" "components" "circuit" "ventilation-modes"))
   (:static-file "LICENCE")
   (:static-file "TODO")))
