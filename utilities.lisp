;;; Small utilities

(in-package :anesthesia-sim)

(defun hash-table-alist (hash-table)
  "Convert hash table to association list for printing."
  (loop for key being the hash-keys of hash-table
          using (hash-value value)
        collect (cons key value)))

(defun clone-hash (ht)
  "Shallow copy of a hash table."
  (let ((out (make-hash-table :test (hash-table-test ht))))
    (maphash (lambda (k v) (setf (gethash k out) v)) ht)
    out))

(defun renormalize-fractions! (ht)
  "Normalize fractions to sum to 1.0, but only if we have positive content.
   Don't artificially add N2 to empty streams."
  (let ((sum 0.0))
    (maphash (lambda (k v) (declare (ignore k)) (incf sum v)) ht)
    (cond
      ((<= sum 0.0)
       ;; Empty stream - leave it empty, don't add artificial N2
       (clrhash ht))
      (t
       ;; Normalize existing fractions
       (maphash (lambda (k v) (setf (gethash k ht) (/ v sum))) ht))))
  ht)

(defun lerp-fractions! (current target alpha)
  "current := current + alpha * (target - current) for all keys."
  ;; ensure keys union
  (maphash (lambda (k v)
             (declare (ignore v))
             (unless (gethash k current) (setf (gethash k current) 0.0)))
           target)
  (maphash (lambda (k v)
             (declare (ignore v))
             (unless (gethash k target) (setf (gethash k target) 0.0)))
           current)
  ;; blend
  (maphash (lambda (k vcur)
             (let ((vtgt (gethash k target 0.0)))
               (setf (gethash k current)
                     (+ vcur (* alpha (- vtgt vcur))))))
           current)
  (renormalize-fractions! current))

(defun ≈ (a b tolerance)
  "Check if two numbers are approximately equal within tolerance."
  (< (abs (- a b)) tolerance))

(defun %hash-sum (ht)
  (let ((s 0.0)) (maphash (lambda (_ v) (declare (ignore _)) (incf s v)) ht) s))

(defun %clone-fractions (ht)
  (let ((h (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (setf (gethash k h) v)) ht) h))

(defvar *last-expired-stream* (make-hash-table :test #'eq))

(defun %set-last-expired (patient stream)
  (setf (gethash patient *last-expired-stream*) stream))

(defun last-expired-stream (patient)
  (gethash patient *last-expired-stream*))
