;;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GRAPH -*-

(in-package graph)

;;;
;;; Protokoll
;;;

(defgeneric make-graph (descr &key &allow-other-keys))

(defgeneric delete-graph (graph-spec &key &allow-other-keys))

(defgeneric make-node (graph-spec &key &allow-other-keys))

(defgeneric delete-node (graph-spec node-spec &key error-p satisfying &allow-other-keys))

(defgeneric make-edge (graph-spec from-node-spec to-node-spec &key &allow-other-keys))

(defgeneric delete-edge (graph-spec edge-spec &key error-p satisfying &allow-other-keys))

;;;
;;;
;;;

(defgeneric copy-graph (graph-spec &key &allow-other-keys))

(defgeneric copy-node (for-new-graph-spec node-spec &key from-graph-spec &allow-other-keys))

(defgeneric copy-edge (for-new-graph-spec edge-spec &key from-graph-spec &allow-other-keys))

(defgeneric copy-graph-item (for-new-graph-spec item &key &allow-other-keys))

;;;
;;;
;;;

(defgeneric find-graph (graph-spec &key error-p satisfying &allow-other-keys))

(defmethod find-graph :around ((graph-spec t) &key error-p &allow-other-keys)
  (or (call-next-method)
      (when error-p (error "Couldn't find graph ~A!" graph-spec))))

(defgeneric find-node (graph-spec node-spec &key error-p satisfying &allow-other-keys))

(defmethod find-node :around ((graph-spec t) (node-spec t) &key error-p &allow-other-keys)
  (or (call-next-method)
      (when error-p (error "Couldn't find node ~A!" node-spec))))

(defgeneric find-edge (graph-spec edge-spec &key error-p satisfying &allow-other-keys))

(defmethod find-edge :around ((graph-spec t) (edge-spec t) &key error-p &allow-other-keys)
  (or (call-next-method)
      (when error-p (error "Couldn't find edge ~A!" edge-spec))))

;;;
;;;
;;;

(defgeneric get-nodes (graph-spec &key error-p satisfying &allow-other-keys))

(defgeneric get-edges (graph-spec &key error-p satisfying &allow-other-keys))

(defgeneric no-of-nodes (graph-spec &key error-p satisfying &allow-other-keys))

(defgeneric no-of-edges (graph-spec &key error-p satisfying &allow-other-keys))

(defgeneric get-edges-between (graph-spec node-from-spec node-to-spec &key error-p satisfying &allow-other-keys))

;;;
;;;
;;;


(defgeneric get-node-position (graph-spec node-spec &key &allow-other-keys))

(defgeneric info (graph-or-graph-item &key for-graph-visualizer-p))

(defgeneric color (graph-item))

(defgeneric kind (graph-item))

(defgeneric mark (graph-item-or-items &optional val))

(defgeneric marked-p (graph-item))

(defgeneric unmark (graph-item-or-items))

;;;
;;;
;;;

(defgeneric visualize (graph-or-list-of-graphs &optional kill-p))

;;;
;;;
;;;

(defvar *graph-counter* 0) 


(defvar *bar*)

(defvar *resizer*)

;;;
;;;
;;;


(defpersistentclass graph ()
  ((name :reader name :initarg :name :initform nil)
   (id :reader id :initform nil :initarg :id)
   (node-counter :reader node-counter :initform 0 :initarg :node-counter)
   (edge-counter :reader edge-counter :initform 0 :initarg :edge-counter)))

(defmethod initialize-instance :after ((graph graph) &rest initargs)
  (with-slots (id) graph
    (unless id 
      (setf id (incf *graph-counter*)))))

(defmethod print-object ((graph graph) stream)
  (format stream "#<~A ~A/~A: ~A>"
          (type-of graph)
          (name graph)
          (id graph)
          (info graph)))

(defmethod describe-object ((object graph) stream)
  (print-object object stream)) 

(defmethod make-graph (type &key &allow-other-keys)
  (declare (ignorable type))
  (subclass-responsibility 'make-graph))

(defmethod find-graph (graph-spec &key &allow-other-keys)
  (declare (ignorable graph-spec))
  (subclass-responsibility 'find-graph))

(defmethod find-graph ((graph-spec graph) &key &allow-other-keys)
  graph-spec)

(defmethod delete-graph ((graph-spec graph) &key &allow-other-keys)
  (subclass-responsibility 'delete-graph))

(defmethod delete-graph (graph-spec &rest args &key (error-p t) all-p &allow-other-keys)
  (loop
   (let ((graph (apply #'find-graph graph-spec :error-p error-p args)))
     (unless graph
       (return))
     (setf error-p nil)
     (apply #'delete-graph graph args)
     (unless all-p  
       (return)))))


(defmethod get-nodes ((graph-spec graph) &rest args &key &allow-other-keys)
  (subclass-responsibility 'get-nodes))
  
(defmethod get-nodes ((graph-spec t) &rest args &key &allow-other-keys)
  (apply #'get-nodes (apply #'find-graph graph-spec :error-p t args) args))

(defmethod get-edges ((graph-spec graph) &rest args &key &allow-other-keys)
  (subclass-responsibility 'get-edges))  

(defmethod get-edges ((graph-spec t) &rest args &key &allow-other-keys)
  (apply #'get-edges (apply #'find-graph graph-spec :error-p t args) args))

(defmethod no-of-nodes ((graph graph) &rest args)
  (subclass-responsibility 'no-of-nodes))

(defmethod no-of-edges ((graph graph) &rest args)
  (subclass-responsibility 'no-of-edges))

(defmethod no-of-nodes ((graph-spec t) &rest args &key &allow-other-keys)
  (apply #'no-of-nodes (apply #'find-graph graph-spec :error-p t args) args))

(defmethod no-of-edges ((graph-spec t) &rest args &key &allow-other-keys)
  (apply #'no-of-edges (apply #'find-graph graph-spec :error-p t args) args))

(defmethod copy-graph ((graph-spec graph) &key &allow-other-keys)
  (subclass-responsibility 'copy-graph))

(defmethod copy-graph ((graph-spec t) &rest args &key &allow-other-keys)
  (apply #'copy-graph (apply #'find-graph graph-spec :error-p t args) args))

;;;
;;;
;;;

(defpersistentclass graph-item ()
  ((name :reader name :initarg :name :initform nil)

   (in-graph :reader in-graph :initarg :in-graph)
   (id :reader id :initform nil :initarg :id) ;;; :id -> vorsicht! wird eigentlich automatisch vergeben werden

   (marked-stack :reader marked-stack :initform nil)

   (compute-color-from-kind-p :accessor compute-color-from-kind-p :initarg :compute-color-from-kind-p :initform t)))

(defmethod print-object ((graph-item graph-item) stream)
  (format stream "#<~A ~A/~A: ~A>"
          (type-of graph-item)          
          (name graph-item)
          (id graph-item)
          (info graph-item)))

(defmethod describe-object ((object graph-item) stream)
  (print-object object stream)) 

;;;
;;;
;;;

(defmethod kind ((item graph-item))
  (subclass-responsibility 'kind))

(defmethod color ((item graph-item))
  (subclass-responsibility 'color))

(defmethod info ((item graph-item) &key &allow-other-keys)
  (subclass-responsibility 'info))

(defmethod info ((item graph)  &key &allow-other-keys)
  (subclass-responsibility 'info))

;;;
;;;
;;;

(defpersistentclass node (graph-item))

(defmethod make-node ((graph-spec graph) &key &allow-other-keys)                      
  (subclass-responsibility 'make-node))

(defmethod make-node ((graph-spec t) &rest args &key &allow-other-keys)                      
  (apply #'make-node (apply #'find-graph graph-spec :error-p t args) args))

(defmethod initialize-instance :after ((node node) &rest initargs)
  (with-slots (id in-graph) node
    (with-slots (node-counter) in-graph
      (incf node-counter)
      (unless id 
        (setf id node-counter)))))

(defmethod find-node ((graph-spec graph) (node-spec t) &key &allow-other-keys)
  (subclass-responsibility 'find-node))

(defmethod find-node ((graph-spec t) (node-spec t) &rest args &key &allow-other-keys)
  (apply #'find-node (apply #'find-graph graph-spec :error-p t args) node-spec args))

(defmethod delete-node ((graph-spec graph) (node-spec node) &key &allow-other-keys)
  (subclass-responsibility 'delete-node))

(defmethod delete-node ((graph-spec t) (node-spec t) &rest args &key (error-p t) all-p &allow-other-keys)
  (let ((graph (apply #'find-graph graph-spec :error-p t args)))
    (loop
     (let ((node (apply #'find-node graph node-spec :error-p error-p args)))
       (unless node
         (return))
       (setf error-p nil)
       (apply #'delete-node graph node args)
       (unless all-p  
         (return))))))

(defmethod copy-node ((graph-spec graph) (node-spec node) &key &allow-other-keys)
  (subclass-responsibility 'copy-node))
  
(defmethod copy-node ((new-graph-spec t) (node-spec t) &rest args &key from-graph-spec &allow-other-keys)
  (let ((new-graph (apply #'find-graph new-graph-spec :error-p t args)))
    (apply #'copy-node
           new-graph
           (apply #'find-node from-graph-spec node-spec :error-p t args)
           args)))

;;;
;;;
;;;

(defmethod get-node-position ((graph-spec t) (node-spec t) &rest args &key &allow-other-keys)
  (apply #'get-node-position 
         (apply #'find-graph graph-spec :error-p t args) 
         (apply #'find-node graph-spec node-spec :error-p t args)
         args))

(defmethod get-node-position ((graph-spec graph) (node-spec node) &rest args &key &allow-other-keys)
  (subclass-responsibility 'get-node-position))

;;;
;;;
;;;

(defpersistentclass edge (graph-item)
  ((from :reader from :initarg :from)
   (to   :reader   to :initarg :to)))

(defmethod make-edge ((graph-spec graph) (from-node-spec node) (to-node-spec node) &key &allow-other-keys)
  (subclass-responsibility 'make-edge))

(defmethod make-edge ((graph-spec t) (from-node-spec t) (to-node-spec t) &rest args &key &allow-other-keys)
  (let ((graph (apply #'find-graph graph-spec :error-p t args)))         
    (apply #'make-edge
           graph
           (apply #'find-node graph from-node-spec :error-p t args) 
           (apply #'find-node graph to-node-spec :error-p t args)
           args)))

(defmethod initialize-instance :after ((edge edge) &rest initargs)
  (with-slots (id from to in-graph) edge
    (unless (eq (in-graph from)
                (in-graph to))
      (error "Nodes must be in same graph!"))
    (with-slots (edge-counter) in-graph
      (incf edge-counter)
      (unless id 
        (setf id edge-counter)))))

(defmethod find-edge ((graph-spec graph) (edge-spec t) &key &allow-other-keys)
  (subclass-responsibility 'find-edge))

(defmethod find-edge ((graph-spec t) (edge-spec t) &rest args &key &allow-other-keys)
  (apply #'find-edge (apply #'find-graph graph-spec :error-p t args) edge-spec args))

(defmethod delete-edge ((graph-spec graph) (edge-spec edge) &key &allow-other-keys)
  (subclass-responsibility 'delete-edge))

(defmethod delete-edge ((graph-spec t) (edge-spec t) &rest args &key (error-p t) all-p &allow-other-keys)
  (let ((graph (apply #'find-graph graph-spec :error-p t args)))
    (loop
     (let ((edge (apply #'find-edge graph edge-spec :error-p error-p args)))
       (unless edge
         (return))
       (setf error-p nil)
       (apply #'delete-edge graph edge args)
       (unless all-p  
         (return))))))


(defmethod copy-edge ((graph-spec graph) (edge-spec edge) &key &allow-other-keys)
  (subclass-responsibility 'copy-edge))
  
(defmethod copy-edge ((new-graph-spec t) (edge-spec t) &rest args &key from-graph-spec &allow-other-keys)
  (let ((new-graph (apply #'find-graph new-graph-spec :error-p t args)))
    (apply #'copy-edge
           new-graph
           (apply #'find-edge from-graph-spec edge-spec :error-p t args)
           args)))

;;;
;;;
;;;

(defmethod copy-graph-item ((for-new-graph-spec t) (node-spec node) &rest args &key &allow-other-keys)
  (apply #'copy-node for-new-graph-spec node-spec args))

(defmethod copy-graph-item ((for-new-graph-spec t) (edge-spec edge) &rest args &key &allow-other-keys)
  (apply #'copy-edge for-new-graph-spec edge-spec args))

;;;
;;;
;;;

(defmethod get-edges-between ((graph-spec graph) (node-from-spec node) (node-to-spec node) &key &allow-other-keys)
  (subclass-responsibility 'get-edges-between))

(defmethod get-edges-between ((graph-spec t) (node-from-spec t) (node-to-spec t) &rest args &key &allow-other-keys)
  (let ((graph (apply #'find-graph graph-spec :error-p t args)))
    (apply #'get-edges-between
           graph
           (apply #'find-node graph node-from-spec :error-p t args)
           (apply #'find-node graph node-to-spec :error-p t args)
           args)))

;;;
;;;
;;;

(defvar *mark-id* 0)

(defmethod mark ((objects list) &optional (val *mark-id*))
  (mapc #'(lambda (x) (mark x val)) objects))

(defmethod unmark ((objects list))
  (mapc #'unmark objects))

(defmethod mark ((object graph-item) &optional (val *mark-id*))
  (unless (marked-p object)
    (push (or val t) (slot-value object 'marked-stack))))

(defmethod unmark ((object graph-item))
  (setf (slot-value object 'marked-stack)
        (rest (slot-value object 'marked-stack))))

(defmacro with-marked-objects ((objects &optional (val nil val-supplied-p)) &rest body)
  `(progn 
     (let ((*mark-id* ,(if (and val-supplied-p 
                                (not val)) ; NIL explizit angegeben!
                           '*mark-id* ; spezielle Semantik, für "with-marked-objects"-Schachtelungen
                                        '(1+ *mark-id*))))
       (mark ,objects (or ,val *mark-id*))
       (unwind-protect
           (progn ,@body)
         (unmark ,objects)))))

(defmacro with-new-marking-context (&body body)
  `(progn 
     (let ((*mark-id* (1+ *mark-id*)))
       ,@body)))
  
(defmethod marked-p ((item graph-item))
  (let ((top (first (marked-stack item))))
    (if (numberp top)
        (= top *mark-id*)
      top)))

;;;
;;;
;;;

#|

(defmethod (setf bound-by) (val (object graph-item))
  (if val
      (mark object val)
    (unmark object)))

(defmethod bound-by ((object graph-item))
  (marked-p object))

|#

#-:clim
(defmethod visualize ((graphs list) &optional kill-p)
  (declare (ignorable kill-p))
  (dolist (graph graphs)
    (visualize graph kill-p)))

#-:clim
(defmethod visualize (graph &optional (kill-p t))
  (declare (ignorable kill-p))
  (describe-object graph t))
