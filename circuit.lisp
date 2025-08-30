;;; Enhanced Connection System

(in-package :anesthesia-sim)

(defclass connection ()
  ((from-component :initarg :from-component :accessor from-component)
   (to-component :initarg :to-component :accessor to-component)
   (from-port :initarg :from-port :accessor from-port :initform "output")
   (to-port :initarg :to-port :accessor to-port :initform "input")
   (enabled :initarg :enabled :accessor enabled :initform t)
   (weight :initarg :weight :accessor weight :initform 1.0
           :documentation "Flow fraction weight (default 1/out-degree)")))

(defmethod print-object ((conn connection) stream)
  (format stream "#<CONNECTION ~A[~A] -> ~A[~A] w=~,3F~A>"
          (name (from-component conn)) (from-port conn)
          (name (to-component conn)) (to-port conn)
          (weight conn)
          (if (enabled conn) "" " [DISABLED]")))

;;; Enhanced Circuit Class (replaces original)

(defclass anesthesia-circuit ()
  ((components  :initarg :components  :accessor components  :initform nil)
   (connections :accessor connections :initform nil) ; old-style for backward compatibility
   (connection-objects :accessor connection-objects :initform nil) ; new enhanced connections
   (gas-streams :accessor gas-streams :initform (make-hash-table :test 'eq))))

(defmethod add-component ((circuit anesthesia-circuit) component)
  "Add a component to the end of the list (preserve intended processing order)."
  (setf (components circuit)
        (nconc (components circuit) (list component))))

(defmethod add-connection ((circuit anesthesia-circuit) from-comp to-comp 
                           &key (from-port "output") (to-port "input") 
                             (enabled t) (weight nil))
  "Add a connection object between components with optional weight.
   If weight is NIL, it will be computed by recompute-default-weights."
  (let ((conn (make-instance 'connection
                             :from-component from-comp
                             :to-component to-comp
                             :from-port from-port
                             :to-port to-port
                             :enabled enabled
                             :weight (or weight 1.0))))
    (push conn (connection-objects circuit))
    ;; Also add to old-style connections for compatibility
    (push (list from-comp from-port to-comp to-port) (connections circuit))
    
    ;; Recompute default weights if this connection didn't specify one
    (when (null weight)
      (recompute-default-weights circuit))
    
    conn))

(defmethod recompute-default-weights ((circuit anesthesia-circuit))
  "Recompute default weights for all connections.
   Each outgoing connection from a component gets weight = 1/out-degree."
  (let ((out-degree-table (make-hash-table :test 'eq)))
    
    ;; Count outgoing connections per component (only enabled ones)
    (dolist (conn (connection-objects circuit))
      (when (enabled conn)
        (let ((from-comp (from-component conn)))
          (incf (gethash from-comp out-degree-table 0)))))
    
    ;; Set weights for connections that weren't explicitly weighted
    (dolist (conn (connection-objects circuit))
      (when (enabled conn)
        (let* ((from-comp (from-component conn))
               (out-degree (gethash from-comp out-degree-table 1))) ; default to 1
          ;; Only update weight if it's currently 1.0 (likely default)
          ;; This preserves explicitly set weights
          (when (= (weight conn) 1.0)
            (setf (weight conn) (/ 1.0 out-degree))))))))

(defgeneric get-mixed-input-for-component (circuit component)
  (:documentation "Get mixed input streams for a component, respecting connection weights."))

(defmethod get-mixed-input-for-component ((circuit anesthesia-circuit) component)
  "Collect and mix all upstream gas streams with proper flow weighting."
  (let ((upstream-streams nil))
    
    ;; Collect all enabled upstream connections with their weights
    (dolist (conn (connection-objects circuit))
      (when (and (enabled conn)
                 (eq (to-component conn) component))
        (let* ((upstream-comp (from-component conn))
               (upstream-stream (gethash upstream-comp (gas-streams circuit)))
               (connection-weight (weight conn)))
          (when upstream-stream
            ;; Create a weighted stream by scaling the flow rate
            (let ((weighted-stream 
                    (make-instance 'gas-stream
                                   :flow-rate (* (flow-rate upstream-stream) connection-weight)
                                   :fractions (clone-hash (fractions upstream-stream))
                                   :temperature (temperature upstream-stream)
                                   :pressure (pressure upstream-stream)
                                   :humidity (humidity upstream-stream))))
              (push weighted-stream upstream-streams))))))
    
    ;; Mix all weighted streams
    (cond
      ((null upstream-streams)
       ;; No inputs - return zero flow stream
       (make-instance 'gas-stream :flow-rate 0.0))
      
      ((= (length upstream-streams) 1)
       ;; Single input - return as-is (already weighted)
       (first upstream-streams))
      
      (t
       ;; Multiple inputs - mix them using the enhanced mix-streams
       (mix-streams upstream-streams)))))

(defmethod validate-circuit-weights ((circuit anesthesia-circuit))
  "Validate and report on circuit connection weights."
  (format t "~&=== Circuit Connection Weights ===~%")
  (let ((component-out-weights (make-hash-table :test 'eq)))
    
    ;; Collect total outgoing weights per component
    (dolist (conn (connection-objects circuit))
      (when (enabled conn)
        (let ((from-comp (from-component conn)))
          (incf (gethash from-comp component-out-weights 0.0)
                (weight conn)))))
    
    ;; Report components and their total outgoing weights
    (maphash (lambda (comp total-weight)
               (format t "~A: total outgoing weight = ~,3F~A~%"
                       (name comp)
                       total-weight
                       (if (not (≈ total-weight 1.0 0.001))
                           " [WARNING: ≠ 1.0]"
                           "")))
             component-out-weights)
    
    ;; Report individual connections
    (format t "~%Individual connections:~%")
    (dolist (conn (connection-objects circuit))
      (when (enabled conn)
        (format t "  ~A~%" conn)))))

(defmethod disable-connection ((circuit anesthesia-circuit) from-comp to-comp)
  "Disable a connection (for valve failures, etc.)"
  (let ((conn (find-if (lambda (c) 
                         (and (eq (from-component c) from-comp)
                              (eq (to-component c) to-comp)))
                       (connection-objects circuit))))
    (when conn (setf (enabled conn) nil))))
