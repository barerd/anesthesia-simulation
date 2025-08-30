(defpackage :anesthesia-sim.server
  (:use :cl :hunchentoot :anesthesia-sim :yason))
(in-package :anesthesia-sim.server)

(defun json-response (data &key (code 200))
  (setf (hunchentoot:content-type*) "application/json")
  (setf (hunchentoot:return-code*) code)
  (yason:encode data))

(define-easy-handler (start :uri "/start") (dt)
  (let* ((dt-num (or (and dt (parse-number:parse-number dt)) 1.0))
         (circuit (make-circle-system :patient-type 'healthy)))
    (start-simulation circuit :dt dt-num)
    (json-response (list :ok t :dt dt-num))))

(define-easy-handler (pause :uri "/pause") ()
  (pause-simulation)
  (json-response (list :ok t)))

(define-easy-handler (resume :uri "/resume") ()
  (resume-simulation)
  (json-response (list :ok t)))

(define-easy-handler (advance :uri "/advance") (seconds)
  (let ((sec (or (and seconds (parse-number:parse-number seconds)) 0.0)))
    (advance-sim-seconds sec)
    (json-response (list :ok t :t (simulation-time)))))

(define-easy-handler (status :uri "/status") ()
  (if (and *sim-controller* (running *sim-controller*))
      (bt:with-lock-held ((lock *sim-controller*))
        (json-response
         (list :running t
               :t (current-time *sim-controller*)
               :dt (dt *sim-controller*)
               :flags (list :paused (paused *sim-controller*))
               :patient (format-patient-gases (circuit *sim-controller*) "patient"))))
      (json-response (list :running nil))))

(define-easy-handler (fgf :uri "/set/fgf") (o2 air n2o)
  (when (and *sim-controller* (running *sim-controller*))
    (bt:with-lock-held ((lock *sim-controller*))
      (set-fgf-flows (circuit *sim-controller*)
                     :o2 (parse-number:parse-number (or o2 "0"))
                     :air (parse-number:parse-number (or air "0"))
                     :n2o (parse-number:parse-number (or n2o "0")))))
  (json-response (list :ok t)))

(define-easy-handler (drug :uri "/drug") (name dose)
  (let* ((nm (or name "propofol"))
         (d  (parse-number:parse-number (or dose "0"))))
    (when (and *sim-controller* (running *sim-controller*))
      (bt:with-lock-held ((lock *sim-controller*))
        (let* ((p (find-patient-in-circuit (circuit *sim-controller*)))
               (rc (and p (respiratory-controller p))))
          (when rc (administer-drug rc nm d)))))
    (json-response (list :ok t :drug nm :dose d))))

(define-easy-handler (mode :uri "/intervene") (kind rr tv peep)
  (when (and *sim-controller* (running *sim-controller*))
    (bt:with-lock-held ((lock *sim-controller*))
      (let ((p (find-patient-in-circuit (circuit *sim-controller*))))
        (when p
          (cond
            ((string-equal kind "manual")
             (intervene-ventilation p "manual"
                                    :pressure-support 12.0))
            ((string-equal kind "controlled")
             (intervene-ventilation p "controlled"
                                    :respiratory-rate (or (and rr (parse-number:parse-number rr)) 12.0)
                                    :tidal-volume     (or (and tv (parse-number:parse-number tv)) 500.0)
                                    :peep             (or (and peep (parse-number:parse-number peep)) 5.0))))))))
  (json-response (list :ok t)))

(defun start-server (&key (port 8080))
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port))
  (format t "HTTP server listening on ~A~%" port))
