;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

(defconstant +secret-marker+ (gensym))

(defparameter *non-determinism-p* nil)

(defparameter *debug-p* nil)

(defvar *all-substrates* nil)

(defvar *cur-substrate* nil)

;;;
;;; Vorgesehenes Interface
;;;

(defgeneric create-substrate (name &rest args &key delete-if-exists-p type &allow-other-keys))

(defgeneric delete-substrate (substrate-specifier &key &allow-other-keys))

(defgeneric find-substrate (substrate-specifier &key &allow-other-keys))

(defgeneric consistent-p (substrate &key &allow-other-keys))

(defgeneric inconsistent-p (substrate &key &allow-other-keys))

;;;
;;;
;;;

(defgeneric get-missing-edges (substrate &key nodes loops-p inverses-p &allow-other-keys))

(defgeneric get-underspecified-edges (substrate &key satisfying use-aux-description-p &allow-other-keys))

;;;
;;;
;;;

(defgeneric simplify (substrate &key error-p &allow-other-keys))

(defgeneric scramble (substrate &key nodes))


;;;
;;; Für Macros "in-substrate" und "with-substrate"
;;;

(defgeneric establish-context-for (substrate continuation))

(defgeneric set-context-for (substrate)
  (:method-combination progn))

;;;
;;;
;;;

(defgeneric get-standard-node-class (substrate))

(defgeneric get-standard-edge-class (substrate))

(defgeneric get-standard-node-description-class (substrate))

(defgeneric get-standard-edge-description-class (substrate))

;;;
;;;
;;;

(defgeneric implies-p (substrate-object-a substrate-object-b
                                          &key use-aux-description-p
                                          &allow-other-keys))

(defgeneric equivalent-p (substrate-object-a substrate-object-b
                                             &key use-aux-description-p
                                             &allow-other-keys))

;;;
;;; Knoten
;;;

(defgeneric create-node (substrate name node-description
                                   &key aux-description error-p delete-if-exists-p type 
                                   &allow-other-keys))

(defgeneric add-to-node (substrate node-specifier delta-description 
                                   &key error-p
                                   &allow-other-keys))

;;;
;;; Kanten
;;;

(defgeneric create-edge (substrate node-specifier-from node-specifier-to edge-description
                                   &key aux-description error-p type
                                   &allow-other-keys))

(defgeneric equivalent-edge-present-p (substrate node-spec-from node-spec-to edge-description 
                                                 &key use-aux-description-p
                                                 &allow-other-keys))

(defgeneric reflexive-edge-p (edge))

(defgeneric underspecified-p (edge &key use-aux-description-p &allow-other-keys))

(defgeneric get-disjuncts (edge &key use-aux-description-p &allow-other-keys))

(defgeneric inverse-edge (edge))

;;;
;;; Various Stuff
;;;

(defgeneric subgraph-isomorphic-p (substratea substrateb
                                              &key nodes-a nodes-b permutations-p
                                              edges-equivalent-fn nodes-equivalent-fn))

(defgeneric isomorphic-p (substratea substrateb 
                                     &key nodes-a nodes-b permutations-p
                                     edges-equivalent-fn nodes-equivalent-fn))

(defgeneric has-next-configuration-p (substrate &key restart-from))

(defgeneric enumerate-atomic-configurations (substrate 
                                             &key restart-from how-many edges
                                             sym-gen selector-fn manager-fn 
                                             final-check-f construction-check-fn 
                                             fn
                                             &allow-other-keys))

(defgeneric get-all-atomic-configurations (substrate &key &allow-other-keys))

;;;
;;; Handling of the "aux description" 
;;;

(defgeneric save-current-description (substrate 
                                      &key edges nodes save-node-labels-p save-edge-labels-p
                                      &allow-other-keys))

(defgeneric pop-description-stack (substrate
                                   &key edges nodes 
                                   &allow-other-keys))

;;;
;;; Das Basis-Substrate verwaltet lediglich Knoten und Kanten; 
;;; Knoten müssen eindeutigen Namen tragen; beliebig viele Kanten 
;;; zwischen zwei Knoten werden unterstützt. Knoten kennen ihre
;;; Nachfolger- und Vorgängerknoten. 
;;; Kanten tragen "edge-descriptions", Knoten "node-descriptions"; 
;;; abstrakte Retrieval-Operation und eine "matches-p"-Operation 
;;; sind vorgesehen. Das Substrate weiß nichts über inverse und
;;; reflexive Kanten etc. S. Rolebox-Substrate-Unterklasse. 
;;; 

(defpersistentclass substrate (graph) 
  ((node-table :reader node-table 
               :initform 
               (let ((table (make-hash-table :size 1000 :rehash-size 1000)))
                 ;(hcl:set-hash-table-weak table :both)
                 table))
   (edge-table :reader edge-table 
               :initform
               (let ((table (make-hash-table :size 10000 :rehash-size 10000 :test #'equal)))
                 ;(hcl:set-hash-table-weak table :key)
                 table))
   
   (cached-nodes :reader cached-nodes :initform nil :not-persistent)
   (cached-edges :reader cached-edges :initform nil :not-persistent)   

   (max-counter :initform nil :accessor max-counter)
   (counter :initform nil :accessor counter)
   
   (reset-p :accessor reset-p :initform t)
   (stack :initform nil :accessor stack)
   (description-stack :initform nil :accessor description-stack)))


(defmethod get-nodes ((substrate substrate) &key (satisfying #'yes) (sort-p t) &allow-other-keys)
  (if (and (cached-nodes substrate)
           (equal (list satisfying sort-p)
                  (first (cached-nodes substrate))))
      (second (cached-nodes substrate))
    (let ((res nil))
      (with-new-marking-context
        (loop as node being the hash-value of (node-table substrate) 
              when (and (not (marked-p node))
                        (funcall satisfying node))
              do
              (progn 
                (mark node)
                (push node res)))
        (unmark res))
      (when sort-p (setf res (sort res #'< :key #'id)))
      (setf (slot-value substrate 'cached-nodes)
            (list (list satisfying sort-p) res))
      res)))

#|

(defmethod get-edges ((substrate substrate) &rest args)
  (loop as val being the hash-value of (edge-table substrate) 
        collect val))

|#

(defmethod get-edges ((substrate substrate)
                      &key exclude-loops-p exclude-inverses-p (satisfying #'yes)
                      &allow-other-keys)
  (if (and (cached-edges substrate)
           (equal (list exclude-loops-p exclude-inverses-p satisfying)
                  (first (cached-edges substrate))))
      (second (cached-edges substrate))      
    (let ((res nil))      
      (with-new-marking-context
        (loop as edge being the hash-value of (edge-table substrate)
              when (and                      
                    (not (consp edge))
                    (not (marked-p edge))
                    (=> (and exclude-inverses-p
                             (inverse-edge edge))
                        (< (id (from edge)) (id (to edge))))
                    ;;; notwendige Sonderbehandlung für
                    ;;; reflexive Kanten, die nicht selbst-invers
                    ;;; sind (wegen unterschiedlicher Beschreibungen z.B.) 
                    (=> (and exclude-inverses-p
                             (eq (from edge) (to edge))
                             (inverse-edge edge))
                        (not (marked-p (inverse-edge edge))))
                    (=> exclude-loops-p
                        (not (reflexive-edge-p edge)))
                    (funcall satisfying edge))
              do
              (progn 
                (mark edge)
                (push edge res)))
        (unmark res))
      (setf (slot-value substrate 'cached-edges)
            (list (list exclude-loops-p exclude-inverses-p satisfying)
                  res))
      res)))

(defmacro loop-over-nodes ((var substrate) &body body)
  `(maphash #'(lambda (key ,var)
                (declare (ignorable ,var))
                (when (integerp key)
                  ,@body))
            (node-table ,substrate)))


(defmacro loop-over-edges ((var substrate) &body body)
  `(maphash #'(lambda (key ,var)
                (declare (ignorable ,var))
                (when (integerp key)
                  ,@body))
            (edge-table ,substrate)))

;;;
;;;
;;;

(defmethod consistent-p ((substrate substrate) &key 
                         (check-node-labels-p t)
                         (check-edge-labels-p t)                         
                         &allow-other-keys)
  (and (=> check-node-labels-p (every #'consistent-p (get-nodes substrate)))
       (=> check-edge-labels-p (every #'consistent-p (get-edges substrate)) )))
       
(defmethod inconsistent-p ((obj substrate) &rest args)
  ;;; wegen Dreiwertigkeit!
  (eq (apply #'consistent-p obj args) nil))

;;;
;;;
;;;

(defmethod get-standard-node-class ((substrate substrate))
  'substrate-node)

(defmethod get-standard-edge-class ((substrate substrate))
  'substrate-edge)

(defmethod get-standard-node-description-class ((substrate substrate))
  'simple-node-description)

(defmethod get-standard-edge-description-class ((substrate substrate))
  'simple-edge-description)

;;;
;;;
;;;

(defun get-substrate-name (symbol-or-string) 
  (intern 
   (string-upcase (format nil "~A" symbol-or-string))))

(defmethod create-substrate ((name symbol) &rest args &key (type 'substrate) &allow-other-keys)
  (apply #'make-graph type :name name args))

(defmethod create-substrate ((name string) &rest args &key (type 'substrate) &allow-other-keys)
  (apply #'make-graph type :name (get-substrate-name name) args))

(defmethod make-graph ((type symbol) &rest args
                       &key (error-p t) delete-if-exists-p name copy-p 
                       &allow-other-keys)
  (when delete-if-exists-p 
    (delete-graph name :all-p t :error-p nil))
  (if (and (find-graph name :error-p nil)
           (not copy-p))
      (if error-p
          (error "Substrate named ~A already exists!" name)
        (return-from make-graph nil))
    (apply #'make-instance type :allow-other-keys t 
           args)))

(defmethod initialize-instance :after ((substrate substrate) &rest initargs)
  (push substrate *all-substrates*))

(defun all-substrates ()
  *all-substrates*)

;;;
;;;
;;;

(defmethod establish-context-for ((substrate substrate) continuation)
  (let ((*node-description-class* 
         (get-standard-node-description-class *cur-substrate*))
        (*edge-description-class* 
         (get-standard-edge-description-class *cur-substrate*)))

    (if (next-method-p)
        (call-next-method)
      (funcall continuation))))

(defmethod set-context-for progn ((substrate substrate))
  (setf *node-description-class* 
        (get-standard-node-description-class *cur-substrate*)
        *edge-description-class* 
        (get-standard-edge-description-class *cur-substrate*)))

;;;
;;;
;;;

(defmacro with-substrate* ((name &rest args &key all-args (delete-if-exists-p t) &allow-other-keys)
                           &rest body)
  `(let ((*cur-substrate* 
          (or (let ((found 
                     (find-substrate ,name)))
                (if found
                    (if ,delete-if-exists-p
                        nil
                      found)
                  nil))
              ,(if all-args
                   `(apply #'create-substrate ,name 
                           ,all-args)
                 `(create-substrate ,name 
                                    ,@args)))))

     (establish-context-for *cur-substrate*
                            #'(lambda () 
                                ,@body))))

(defmacro with-substrate ((name &rest args) &rest body)
  `(with-substrate* ( (quote ,name) ,@args)
     ,@body))

;;;
;;;
;;;

(defmacro in-substrate* (name &rest args &key all-args delete-if-exists-p &allow-other-keys)
  `(progn 
     (setf *cur-substrate* 
           (or (let ((found 
                      (find-substrate ,name)))
                 (if found
                     (if ,delete-if-exists-p
                         nil
                       found)
                   nil))
               ,(if all-args
                    `(apply #'create-substrate ,name ,all-args)
                  `(create-substrate ,name ,@args))))
     
     (set-context-for *cur-substrate*)

     *cur-substrate*))


(defmacro in-substrate (name &rest args)
  `(in-substrate* (quote ,name) ,@args))


(defun in-substrate-fn (name &rest args &key all-args delete-if-exists-p &allow-other-keys)
  (progn 
    (setf *cur-substrate* 
           (or (let ((found 
                      (find-substrate name)))
                 
                 (if found
                     (if delete-if-exists-p
                         nil
                       found)
                   nil))
               
               (if all-args
                   (apply #'create-substrate name all-args)
                 (apply #'create-substrate name args))))
     
    (set-context-for *cur-substrate*)

     *cur-substrate*))


;;;
;;;
;;;

(defmethod find-substrate ((substrate-spec t) &rest args)
  (apply #'find-graph substrate-spec args))

(defmethod find-graph ((graph-spec symbol) &rest args)
  (find graph-spec *all-substrates* :key #'name))

(defmethod find-graph ((graph-spec integer) &rest args)
  (find graph-spec *all-substrates* :key #'graph::id :test #'=))

;;;
;;;
;;;

(defmethod delete-substrate ((substrate-spec t) &rest args &key &allow-other-keys)
  (apply #'delete-graph substrate-spec args))

(defmethod delete-graph ((substrate substrate) &rest args &key &allow-other-keys)
  (with-slots (node-table edge-table cached-nodes cached-edges stack description-stack) substrate
    ;;; warum muss ich das tun? der GC hat anscheinend Probleme sonst...
    (when node-table
      (clrhash node-table))
    (when edge-table
      (clrhash edge-table))
    (setf cached-nodes nil
          cached-edges nil
          stack nil
          description-stack nil)
    ;;;(hcl::mark-and-sweep 3)
    #+:lispworks (hcl:gc-if-needed)
    (setf *all-substrates* 
          (delete substrate *all-substrates*))
    (when (eq substrate *cur-substrate*)
      (setf *cur-substrate* nil))))
  

;;;
;;;
;;;

(defun delete-all-substrates (&key (satisfying #'yes))
  (mapc #'delete-substrate 
        (get-all-substrates :satisfying satisfying)))


(defun get-all-substrates (&key (satisfying #'yes))
  (remove-if-not satisfying *all-substrates*))

;;;
;;;
;;;

(defmethod copy ((substrate substrate) &rest args)
  (apply #'copy-graph substrate args))

(defmethod copy-graph ((substrate substrate) &rest args &key name &allow-other-keys)
  (let ((copy (apply #'create-substrate 
                     (or name (format nil "Copy of ~A" (name substrate)))
                     :copy-p t
                     :type (type-of substrate)
                     args)))
    (with-slots (node-counter edge-counter
                              counter max-counter
                              reset-p stack description-stack) substrate

      (with-slots ((cnode-counter node-counter) 
                   (cnode-counter node-counter) 
                   (cedge-counter edge-counter)
                   (ccounter counter)
                   (cmax-counter max-counter)
                   (creset-p reset-p)
                   (cstack stack)
                   (cdescription-stack description-stack)) copy
        
        (dolist (node (sort (get-nodes substrate) #'< :key #'id))
          (copy-graph-item copy node))

        (dolist (edge (sort (get-edges substrate) #'< :key #'id))
          (copy-graph-item copy edge))

        (setf cnode-counter node-counter
              cedge-counter edge-counter
              ccounter counter
              cmax-counter max-counter
              creset-p reset-p
              cstack (copy-tree stack)
              cdescription-stack (copy-tree description-stack))))
    copy))

;;;
;;; 
;;;

(defpersistentclass substrate-object (graph-item) ; abstrakt
  ((description :accessor description :initarg :description :initform nil)
   (aux-description :accessor aux-description :initarg :aux-description :initform nil)

   (description-stack :accessor description-stack :initform nil))) 

;;;
;;;
;;;

(defmethod consistent-p ((obj substrate-object) &rest args  &key use-aux-description-p &allow-other-keys)
  (apply #'consistent-p 
         (if  use-aux-description-p 
             (aux-description obj)
           (description obj))
         args))

(defmethod inconsistent-p ((obj substrate-object) &rest args)
  (not (apply #'consistent-p obj args)))



(defmethod implies-p ((object-a substrate-object) (object-b substrate-object) 
                      &key use-aux-description-p &allow-other-keys)
  (if use-aux-description-p 
      (implies-p (aux-description object-a) 
                 (aux-description object-b))
    (implies-p (description object-a) 
               (description object-b))))

(defmethod equivalent-p ((object-a substrate-object) (object-b substrate-object) &rest args)
  (and (apply #'implies-p object-a object-b args)
       (apply #'implies-p object-b object-a args)))

;;;
;;;
;;;

(defpersistentclass substrate-node (substrate-object node)
  ((successors :reader successors :initarg :successors :initform nil)
   (predecessors :reader predecessors :initarg :predecessors :initform nil)

   (outgoing :reader outgoing :initarg :outgoing :initform nil)
   (incoming :reader incoming :initarg :incoming :initform nil)))
   
(defmethod initialize-instance :after ((object substrate-node) &rest initargs)
  (register-node object))

(defmethod register-node ((object substrate-node))
  (with-slots (id name in-graph) object
    (with-slots (node-table cached-nodes) in-graph      
      (setf cached-nodes nil)
      (setf (gethash id node-table) object)
      (when name 
        (setf (gethash name node-table) object)))))
  

(defmethod get-node-position ((substrate substrate) (node substrate-node) &rest args)
  (let ((nodes (sort (get-nodes substrate) #'< :key #'id)))
    (position node nodes)))

;;;
;;; 
;;;

(defmethod create-node ((substrate t) (name symbol) (description node-description) &rest args)
  (apply #'make-node substrate :name name :description description args))

(defmethod make-node ((substrate substrate)
                      &rest args 
                      &key 
                      name description aux-description
                      delete-if-exists-p id (error-p t) (type (get-standard-node-class substrate))
                      &allow-other-keys)
                        
  (if delete-if-exists-p 
      (delete-node substrate name :all-p t :error-p nil)
    (when (apply #'find-node substrate name :error-p nil args)
      (if error-p
          (error "Node ~A already exists in substrate ~A!" name substrate)
        (return-from make-node nil))))
  
  (apply #'make-instance type
         :id id
         :in-graph substrate
         :description description 
         :aux-description aux-description           
         :name name
         :allow-other-keys t 
         args))

(defmacro node (name description &rest args)
  `(create-node *cur-substrate* ',name ,description ,@args))

;;;
;;;
;;;

(defmethod add-to-node ((substrate t) (name t) (delta-description node-description)
                        &rest args &key error-p )
  (let* ((substrate (find-substrate substrate :error-p error-p))
         (node (find-node substrate name :error-p error-p)))
    (when (and node substrate)
      (apply #'add-to-node substrate node
             delta-description args))))

(defmethod add-to-node ((substrate substrate) (node substrate-node) (delta-description node-description)
                        &key (mode :and) &allow-other-keys)
  (change-description node
                      (ecase mode
                        (:and (make-and-description (description node)
                                                    :descriptions (list delta-description)))
                        (:or (make-or-description (description node)
                                                  :descriptions (list delta-description))))))


(defmacro add (name description &rest args)
  `(add-to-node *cur-substrate* ',name ,description ,@args))


;;;
;;;
;;;

(defmethod find-node ((substrate substrate) (node substrate-node) &rest args &key &allow-other-keys)
  (apply #'find-node substrate (id node) args))

(defmethod find-node ((substrate substrate) (id integer) &key &allow-other-keys)
  (when (node-table substrate)
    (gethash id (node-table substrate))))

(defmethod find-node ((substrate substrate) (name symbol) &key &allow-other-keys)
  (when (node-table substrate)
    (gethash name (node-table substrate))))

;;;
;;;
;;;

(defmethod delete-node ((substrate substrate) (node substrate-node) &key (delete-edges-p t) &allow-other-keys)
  (with-slots (node-table cached-nodes) substrate
      
    (setf cached-nodes nil)

    (remhash (id node) node-table)
    (remhash (name node) node-table)
    
    (when delete-edges-p 
      (dolist (edge (outgoing node))  
        (delete-edge substrate edge :error-p t))
      
      (dolist (edge (incoming node))  
        (delete-edge substrate edge :error-p t)))))

(defmacro del-node (node)
  `(delete-node *cur-substrate* ',node))

;;;
;;;
;;;

(defpersistentclass substrate-edge (substrate-object edge))

(defmethod initialize-instance :after ((object substrate-edge) &rest initargs)
  (register-edge object))

(defmethod register-edge ((object substrate-edge))
  (with-slots (id in-graph from to) object
    (with-slots (edge-table cached-edges) in-graph
      (setf cached-edges nil)

      (let* ((cons (cons (id from) (id to)))
             (item (gethash cons edge-table)))
        (if item 
            (setf (gethash cons edge-table)
                  (push object item))
          (setf (gethash cons edge-table)
                (list object))))

      (setf (gethash id edge-table) object)
      
      (push to (slot-value from 'successors))
      (push from (slot-value to 'predecessors))
      
      (push object (slot-value from 'outgoing))
      (push object (slot-value to 'incoming)))))

;;;
;;;
;;;

(defmethod get-edges-between ((substrate substrate) (from substrate-node) (to substrate-node) 
                              &rest args &key &allow-other-keys)
  (when (edge-table substrate)
    (gethash (cons (id from) (id to))
             (edge-table substrate))))
  
;;;
;;;
;;;

(defmethod find-edge ((substrate substrate) (edge substrate-edge) &rest args &key &allow-other-keys)
  (apply #'find-edge substrate (id edge) args))

(defmethod find-edge ((substrate substrate) (id integer) &key &allow-other-keys)
  (when (edge-table substrate)
    (gethash id (edge-table substrate))))

#|

(defmethod find-edge ((substrate substrate) (name symbol) &key &allow-other-keys)
  (gethash name (edge-table substrate)))

|#

;;;
;;;
;;;

(defmethod delete-edge ((substrate substrate) (edge substrate-edge) &key (error-p t) &allow-other-keys)
  (with-slots (edge-table cached-edges) substrate    
    (setf cached-edges nil)
    (with-slots (id from to) edge
      (if (not (gethash id edge-table))
          (progn
            (when error-p 
              (error "Edge ~A not present in ~A!" edge substrate))
            nil)
        (let ((cons (cons (id from) (id to))))
          (unless (setf (gethash cons edge-table)
                        (delete edge (gethash cons edge-table)))
            ;;; keine Kanten mehr zwischen "from" und "to"? 
            (remhash cons edge-table)
            (setf (slot-value from 'successors) (delete to (slot-value from 'successors)))
            (setf (slot-value to 'predecessors) (delete from (slot-value to 'predecessors))))
            
          (remhash id edge-table)
             
          (setf (slot-value  from 'outgoing) (delete edge (slot-value from 'outgoing)))
          (setf (slot-value to 'incoming) (delete edge (slot-value to 'incoming))))))))

(defmacro del-edge (from to &key (error-p t) (all-p t))
  (let ((var (gensym)))
    `(let ((,var (get-edges-between *cur-substrate* ',from ',to)))
       (if (and (cdr ,var) ,(not all-p))
           (when ,error-p 
             (error "More than one edge between ~A and ~A - not unique, can't delete!" ',from ',to))
         (dolist (edge ,var)
           (delete-edge *cur-substrate* edge))))))

;;;
;;;
;;;

(defmethod create-edge ((substrate t) (from t) (to t) (description edge-description)
                        &rest args)
  (apply #'make-edge substrate from to 
         :description description 
         args))

(defmethod make-edge ((substrate substrate) (from substrate-node) (to substrate-node) 
                      &rest args
                      &key description aux-description id (type (get-standard-edge-class substrate))
                      &allow-other-keys)
  (apply #'make-instance type
         :id id
         :name 'edge ;(intern (format nil "EDGE-~A-~A" (name from) (name to)))
         :description description
         :in-graph substrate
         :aux-description aux-description
         :from from
         :to to
         :allow-other-keys t 
         args))

(defmacro edge (from to description &rest args)
  `(create-edge *cur-substrate* ',from ',to ,description ,@args))


;;;
;;;
;;;

(defmethod reflexive-edge-p ((edge substrate-edge))
  (eq (from edge) (to edge)))

(defmethod underspecified-p ((edge substrate-edge) &key use-aux-description-p &allow-other-keys)
  (if use-aux-description-p 
      (underspecified-p (aux-description edge))
    (underspecified-p (description edge))))

(defmethod get-disjuncts ((edge substrate-edge) &key use-aux-description-p &allow-other-keys)
  (if use-aux-description-p 
      (get-disjuncts (aux-description edge))
    (get-disjuncts (description edge))))

(defmethod inverse-edge ((edge substrate-edge))
  (subclass-responsibility 'inverse-edge))

(defmethod equivalent-edge-present-p ((substrate substrate) nodea nodeb (description edge-description)
                                      &key aux-description-p &allow-other-keys)
  (find-if #'(lambda (edge)
               (equivalent-p (if aux-description-p 
                                 (aux-description edge)
                               (description edge))
                             description))
           (get-edges-between substrate nodea nodeb)))

(defmethod =>-edge-present-p ((substrate substrate) nodea nodeb (description edge-description)
                              &key aux-description-p)
  (find-if #'(lambda (edge)
               (implies-p (if aux-description-p 
                              (aux-description edge)
                            (description edge))
                          description))
           (get-edges-between substrate nodea nodeb)))

(defmethod <=-edge-present-p ((substrate substrate) nodea nodeb (description edge-description)
                              &key aux-description-p)
  (find-if #'(lambda (edge)
               (implies-p description
                          (if aux-description-p 
                              (aux-description edge)
                            (description edge))))
           (get-edges-between substrate nodea nodeb)))

;;;
;;; Various Stuff
;;;

(defmethod subgraph-isomorphic-p ((a substrate) (b substrate)
                                  &key
                                  (nodes-a (get-nodes a))
                                  (nodes-b (get-nodes b))
                                  (permutations-p t) 
                                  (edges-equivalent-fn #'equivalent-p)
                                  (nodes-equivalent-fn #'equivalent-p)
                                  iso)                                  
  ;;; is "a" an isomorphic subgraph of "b"? 
  (let* ((na nodes-a)
         (nb nodes-b)
         (na (if iso (intersection (mapcar #'first iso) na) na))
         (nb (if iso (intersection (mapcar #'second iso) nb) nb)))

    (with-marked-objects (na)
      (when (<= (length na) (length nb))
        (let* ((vec (make-array (1+ (apply #'max (mapcar #'id na)))))
               (perms-to-check 
                (if (or (not permutations-p) iso)
                    (list nb)
                  (perm nb (length na))))
               (res 
                (find-if #'(lambda (perm)
                             (mapc #'(lambda (a b)
                                       (setf (aref vec (id a)) b))
                                   na
                                   perm)   
                             (with-marked-objects (perm)
                               (and (every #'(lambda (node)              
                                               (funcall nodes-equivalent-fn
                                                        (description node)
                                                        (description (aref vec (id node)))))
                                           na)
                                    (every #'(lambda (edge)
                                               (with-slots (description current-label) edge
                                                 (some #'(lambda (edge)
                                                           (funcall edges-equivalent-fn (description edge) description))
                                                       (get-edges-between b
                                                                          (aref vec (id (from edge)))
                                                                          (aref vec (id (to edge)))
                                                                          :satisfying #'(lambda (x) 
                                                                                          (and (marked-p (from x))
                                                                                               (marked-p (to x))))))))
                                           (get-edges a)))))
                         perms-to-check)))
          (when res
            (mapcar #'(lambda (a x) 
                        (list a x))
                    na res)))))))
    
(defmethod isomorphic-p ((a substrate) (b substrate) 
                         &key (nodes-a (get-nodes a)) (nodes-b (get-nodes b)) (permutations-p t)
                         (edges-equivalent-fn #'equivalent-p)
                         (nodes-equivalent-fn #'equivalent-p)
                         iso)
  (let ((iso (subgraph-isomorphic-p a b
                                    :nodes-a nodes-a
                                    :nodes-b nodes-b 
                                    :permutations-p permutations-p 
                                    :nodes-equivalent-fn nodes-equivalent-fn
                                    :edges-equivalent-fn edges-equivalent-fn                                    
                                    :iso iso)))
    (when iso
      (subgraph-isomorphic-p b a 
                             :nodes-a nodes-b
                             :nodes-b nodes-a 
                             :permutations-p permutations-p
                             :nodes-equivalent-fn nodes-equivalent-fn
                             :edges-equivalent-fn edges-equivalent-fn
                             :iso (mapcar #'(lambda (x) 
                                              (reverse x))
                                          iso))
      iso)))

;;;
;;;
;;;


(defmethod copy-node ((substrate substrate) (node substrate-node) &rest args)
  (apply #'make-node substrate
         :name (name node)
         :description (copy (description node))                
         :copy-p t
         :id (id node)
         :type (type-of node)
         :aux-description (when (aux-description node)
                            (copy (aux-description node)))
         :description-stack (copy-tree (description-stack node))
         args))

(defmethod copy-edge ((substrate substrate) (edge substrate-edge) &rest args)
  (apply #'make-edge substrate 
         (find-node substrate (id (from edge)))
         (find-node substrate (id (to edge)))
         :description (copy (description edge))
         :aux-description (when (aux-description edge)
                            (copy (aux-description edge)))
         :copy-p t
         :id (id edge)
         :type (type-of edge)
         :description-stack (copy-tree (description-stack edge))
         args))

;;;
;;;
;;;

(defmethod get-missing-edges ((substrate substrate) &key
                              (nodes (get-nodes substrate))
                              loops-p inverses-p)
  (let ((res nil))
    (loop as n on nodes do
          (let ((i (first n)))
            (when (and loops-p
                       (not (get-edges-between substrate i i)))
              (push (list i i) res))
            (dolist (j (rest n))
              (if (get-edges-between substrate i j)
                  (if (not (get-edges-between substrate j i))
                      (when inverses-p 
                        (push (list j i) res)))
                (if (not (get-edges-between substrate j i))
                    (progn
                      (push (list i j) res)
                      (when inverses-p 
                        (push (list j i) res)))
                  (when inverses-p 
                    (push (list i j) res)))))))
    res))


(defmethod get-underspecified-edges ((substrate substrate) &rest args &key (satisfying #'yes) &allow-other-keys)
  (apply #'get-edges substrate
         :satisfying #'(lambda (x) 
                         (and (apply #'underspecified-p x args)
                              (funcall satisfying x)))
         args))

;;;
;;;
;;;

(defmethod save-current-description ((substrate substrate) &key 
                                     (save-node-labels-p t)
                                     (save-edge-labels-p t)                                     
                                     (edges (get-edges substrate))
                                     (nodes (get-nodes substrate))
                                     &allow-other-keys)
  
  (dolist (node nodes)
    (push (if save-node-labels-p
              (aux-description node) 
            +secret-marker+)
          (description-stack node))
    (when save-node-labels-p
      (setf (aux-description node)
            (copy (description node)))))
    
  
  (dolist (edge edges)
    (push (if save-edge-labels-p 
              (aux-description edge)
            +secret-marker+)
          (description-stack edge))
    (when save-edge-labels-p   
      (setf (aux-description edge)
            (copy (description edge)))))
    

  (push (copy-tree (stack substrate))
        (description-stack substrate))  
  
  substrate)

(defmethod pop-description-stack ((substrate substrate) &key 
                                  (edges (get-edges substrate))
                                  (nodes (get-nodes substrate))
                                  &allow-other-keys)
  (dolist (node nodes)
    (setf (aux-description node) 
          (let ((descr (pop (description-stack node))))
            (unless (eq descr +secret-marker+)
              descr))))
  
  (dolist (edge edges)
    (setf (aux-description edge)
          (let ((descr (pop (description-stack edge))))
            (unless (eq descr +secret-marker+)
              descr))))

  (setf (stack substrate)
        (pop (description-stack substrate)))
  
  substrate)

;;;
;;;
;;;      

(defmethod has-next-configuration-p ((substrate substrate) 
                                     &key restart-from)
  (with-slots (reset-p stack counter max-counter) substrate
    (or reset-p        
        (if restart-from
            (with-slots (stack) substrate
              (let ((stack-entry (find restart-from stack :key #'first)))
                (second stack-entry)))
          ;;; sonst: irgendeine Kante hat noch ausstehende Symbole!
          (some #'second stack)))))


(defmethod enumerate-atomic-configurations ((substrate substrate) 
                                            &rest args
                                            &key
                                            reset-p
                                            how-many 
                                            
                                            restart-from 

                                            auto-pop-p
                                            
                                            (edge-setter
                                             #'(lambda (edge sym &rest args) 
                                                 (declare (ignore args))
                                                 (change-textual-description (description edge) sym)))

                                            (edges 
                                             (if *non-determinism-p*
                                                 (reorder (get-underspecified-edges substrate 
                                                                                    :use-aux-description-p nil))
                                               (get-underspecified-edges substrate :use-aux-description-p nil)))
                                            
                                            (sym-gen #'(lambda (edge processed-edges &rest args)
                                                         (declare (ignore processed-edges args))
                                                         (if *non-determinism-p*
                                                             (reorder (get-disjuncts edge :use-aux-description-p t))
                                                           (get-disjuncts edge :use-aux-description-p t))))
                                            
                                            (selector-fn #'(lambda (rem-edges processed-edges  &rest args)
                                                             (declare (ignore processed-edges args))
                                                             (first rem-edges)))

                                            (manager-fn #'(lambda (sel-edge edges processed-edges  &rest args)
                                                            (declare (ignore args))
                                                            (values (remove sel-edge edges)
                                                                    (cons sel-edge processed-edges))))
                                                                      
                                            (save-node-labels-p t)
                                            (save-edge-labels-p t)
                                            
                                            (fn #'copy)
                                            (final-check-fn #'yes)
                                            (construction-check-fn #'yes)
                                            &allow-other-keys)

  (when reset-p 
    (apply #'reset-enumeration substrate args))  
  

  (labels ((do-it ()

             (with-slots (reset-p stack description-stack counter max-counter) substrate

               (if reset-p
                   (progn

                     (when *debug-p*
                       (format t "*** RESETING!~%"))

                     (save-current-description substrate 
                                               :save-node-labels-p save-node-labels-p
                                               :save-edge-labels-p save-edge-labels-p)
                     (setf reset-p nil)
                     (setf stack 
                           (list (list nil nil edges nil))) ; cur-edge, symbols, rem-edge, processed-edges
                     
                     (setf max-counter how-many)
                     (setf counter 0))
                 
                 (when (and max-counter 
                            (> counter max-counter))
                   (return-from enumerate-atomic-configurations nil)))

               (if (not stack)
                   (progn 
                     (unless auto-pop-p (break "No stack!"))
                     ;;; continue: 
                     (pop-description-stack substrate)
                     (if stack
                         (do-it)
                       (return-from enumerate-atomic-configurations nil)))
                 (let* ((top (first stack))
                        (edge (first top))
                        (syms (second top))
                        (rem (third top))
                        (pro (fourth top)))

                   (when *debug-p*
                     (format t "STACK: ~A~%" stack)                   
                     (format t "STACK -> EDGE: ~A SYMS: ~A~%         REM: ~A~%         PRO: ~A~%" edge syms rem pro))

                   (cond ((and (not edge) rem)                          
                          (let ((edge (apply selector-fn rem pro args)))
                            (multiple-value-bind (new-rem new-pro)
                                (apply manager-fn edge rem pro args)

                              (when *debug-p* (format t "Chosing edge ~A~%" edge))

                              (setf (first top) edge)
                              (setf (second top) (apply sym-gen edge pro args))
                              (setf (third top) new-rem)
                              (setf (fourth top) new-pro)
                              (do-it))))
                         (edge
                          (let ((sym (first (second top))))

                            (setf (second top) (rest (second top)))
                            
                            (cond (sym 
                                   (when *debug-p* (format t "Assigning edge ~A to ~A~%" edge sym))
                                   (apply edge-setter edge sym :remaining (second top) args)
                                   (when (apply construction-check-fn substrate pro args)
                                     (when *debug-p* (format t "Construction check passed~%"))
                                     (push (list nil nil rem pro) stack)))
                                  (t
                                   (when *debug-p* 
                                     (format t "No more alternatives for edge ~A; backtracking~%" edge ))
                                   (pop stack)))
                            (do-it)))
                         ((and (not edge) (not rem))
                          (pop stack)
                          (when *debug-p*
                            (format t "FINAL CHECK: ~A~%" (apply final-check-fn substrate args)))
                          (when (apply final-check-fn substrate args)
                            (when *debug-p* (format t "CONFIGURATION FOUND!~%" ))
                            (incf counter)
                            (return-from enumerate-atomic-configurations 
                              (apply fn substrate args)))
                          (do-it))))))))
                 
    (when restart-from
      (with-slots (stack) substrate
        (let ((pos (position restart-from stack :key #'first)))
          (when pos 
            (setf stack 
                  (subseq stack pos))))))

    (do-it)))



(defmethod reset-enumeration ((substrate substrate) &key &allow-other-keys)
  (setf (reset-p substrate) t))

(defmethod get-all-atomic-configurations ((substrate substrate) &rest args &key (fn #'copy))
  (apply #'reset-enumeration substrate args)
  (let ((Res nil))
    (loop 
     (let ((subs (apply #'enumerate-atomic-configurations substrate :fn fn :auto-pop-p t args)))
       (if subs
           (push subs res)
         (return t))))
    res))
        
;;;
;;;
;;;


(defmethod simplify ((substrate substrate) 
                     &key (error-p t))
  (let* ((edges (get-edges substrate))
         (new-edges nil)
         (del-edges nil))
    
    (loop while edges do
          (let* ((edge (first edges))
                 (from (from edge))
                 (to (to edge))
                 (supp (get-edges-between substrate from to)))
            
            (setf edges (set-difference edges supp))
              
            (let ((new-description
                   (apply #'make-and-description
                          (mapcar #'description supp))))

              (unless new-description
                (when error-p
                  (error "Unable to simplify ~A!" substrate))
                (return-from simplify substrate))
              
              (push (list (first supp) new-description) new-edges)
              (setf del-edges (append (rest supp) del-edges)))))
    
    (dolist (edge del-edges)
      (delete-edge substrate edge))

    (dolist (entry new-edges)
      (let ((edge (first entry))
            (new-descr (second entry)))
        (change-description edge new-descr)))    

    substrate))

;;;
;;;
;;;

(defmethod change-description ((node substrate-node) (description node-description))
  (setf (description node) description
        (aux-description node) nil
        (description-stack node) nil)
  (setf (reset-p (in-graph node)) t))

(defmethod change-description ((edge substrate-edge) (description edge-description))
  (setf (description edge) description
        (aux-description edge) nil
        (description-stack edge) nil)
  (setf (reset-p (in-graph edge)) t))

;;;
;;;
;;;

(defmethod scramble ((substrate substrate) &key (nodes (get-nodes substrate)) new-order)
  (let* ((reordered-nodes 
          (mapcar #'id (or new-order (reorder nodes))))
         (edges (get-edges substrate)))
    
    (with-slots (node-table edge-table
                            cached-nodes 
                            cached-edges) substrate
      (dolist (edge edges)
        (with-slots (from to) edge
          (remhash (cons (id from) (id to))
                   edge-table)))
      (dolist (node nodes)
        (remhash (id node) node-table))

      (dolist (node nodes)
        (let ((new-id (1+ (position (id node) 
                                    reordered-nodes))))

          (setf (slot-value node 'id) new-id)          
          (setf (gethash new-id node-table) node)))
      
      
      (dolist (edge edges)
        (with-slots (from to) edge
          (if (gethash (cons (id from) (id to)) edge-table)
              (push edge (gethash (cons (id from) (id to)) edge-table))
            (setf (gethash (cons (id from) (id to)) edge-table)
                  (list edge)))))

      (setf cached-nodes nil
            cached-edges nil)
      
      substrate)))

;;;
;;; Interface zum Graph-Visualizer
;;; 

(defmethod no-of-nodes ((graph substrate) &rest args)
  (let ((no 0))
    (loop-over-nodes (node graph)
      (incf no))
    no))

(defmethod no-of-edges ((graph substrate) &rest args)
  (let ((no 0))
    (loop-over-edges (node graph)
      (incf no))
    no))


;;;
;;;
;;;

(defmethod kind ((substrate substrate))
  (type-of substrate))

(defmethod kind ((node substrate-node))
  (or (textual-description (description node))
      'null))

(defmethod kind ((edge substrate-edge))
  (or (textual-description (description edge))
      'null))

;;;
;;;
;;;

(defmethod info ((substrate substrate) &key  &allow-other-keys)
  (format nil "Name: ~A, Type: ~A, Nodes: ~A, Edges: ~A, Consistent: ~A"
          (name substrate)
          (type-of substrate)
          (no-of-nodes substrate)
          (no-of-edges substrate)
          (consistent-p substrate)))

(defmethod info ((node substrate-node) &key  &allow-other-keys)
  (format nil "~A:~A/~A" 
          (name node)
          (get-textual-description (description node))
          (if (aux-description node)
              (get-textual-description (aux-description node))
            'none)))


(defmethod info ((edge substrate-edge) &key  &allow-other-keys)
  (format nil "~A:~A/~A"
          (name edge)
          (get-textual-description (description edge))
          (if (aux-description edge)
              (get-textual-description (aux-description edge))
            'none)))


#|

(defmethod info ((node substrate-node) &key  &allow-other-keys)
  (format nil "~A" 
          (name node)))


(defmethod info ((edge substrate-edge) &key  &allow-other-keys)
  (format nil "(~A,~A) : ~A"
          (name (from edge))
          (name (to edge))
          (get-textual-description (description edge))))


|# 


;;;
;;;
;;;

#|

(progn   
  (setf x (create-substrate "test" :delete-if-exists-p t))

  (create-node x 'a (make-node-description (get-standard-node-description-class x) 'a-descr) :delete-if-exists-p t)
  
  (create-node x 'b (make-node-description (get-standard-node-description-class x) 'b-descr) :delete-if-exists-p t)
  
  (princ (create-edge x 'a 'b (make-edge-description (get-standard-edge-description-class x) 'test) :error-p nil))

  (setf xx (list x (scramble (copy x)))))


(with-substrate (hello :delete-if-exists-p t)
  
  (node a (ncon a-descr))
 
  (visualize (list *cur-substrate*)))


(with-substrate (hello :delete-if-exists-p t)
  
  (node a (ncon a-descr))
  (node a (ncon a-descr) :delete-if-exists-p t)
    
  (node b (ncon b-descr))
  (del-node b)
  (node b (ncon b-descr))

  (edge a b (econ ab-edge-descr))

  (del-edge a b)

  (edge a b (econ ab-edge-descr))  
  (edge b a (econ ba-edge-descr))  
  (edge b b (econ bb-edge-descr))

  (del-edge b a)

  (visualize (list *cur-substrate*
                   (copy *cur-substrate* :delete-if-exists-p t))))


(progn 
  (delete-all-substrates)
  (in-substrate hello :delete-if-exists-p t)

  (node a (ncon a-descr))
  (node b (ncon b-descr))
  (node c (ncon c-descr))
  (node d (ncon d-descr))

  (edge a b (econ ab-edge-descr))  
  (edge b a (econ ba-edge-descr))  
  (edge b b (econ bb-edge-descr))
  (edge b c (econ bc-edge-descr))
  (edge b d (econ bd-edge-descr))
  (edge a d (econ ad-edge-descr))

  (setf *x* *cur-substrate*
        *y* (scramble (copy *cur-substrate*)))

  (visualize (list *x* *y*))
  (princ (isomorphic-p *x* *y*)))


(with-substrate (hello :type 'substrate :delete-if-exists-p t)
  
  (node a (ncon a-descr))
  (node b (ncon b-descr))
  (node c (ncon c-descr))
  
  
  (edge a b (econ (r s t)))
  (edge b c (econ (1 2 3)))
  
  (setf *x* *cur-substrate*)
  
  (visualize (remove nil 
                     (list (enumerate-atomic-configurations *x*)
                           (enumerate-atomic-configurations *x* :restart-from (first (get-edges-between *x* 'a 'b)))
                           (enumerate-atomic-configurations *x* :restart-from (first (get-edges-between *x* 'b 'c)))
                           (enumerate-atomic-configurations *x* :restart-from (first (get-edges-between *x* 'a 'b)))
                           (enumerate-atomic-configurations *x*)
                           (enumerate-atomic-configurations *x*)))))




(with-substrate (test :type 'substrate :delete-if-exists-p t)
  
  (node u (ncon a))
  (node v (ncon b))
  (node w (ncon c))
  
  (edge u v (econ (r s)))
  (edge v w (econ (x y)))

  (setf *x* *cur-substrate*)
  
  (visualize (cons (copy *x*) (get-all-atomic-configurations *x*))))


;;;
;;; Demo: geschachtelte Verwendung von "enumerate-atomic-configurations"
;;; Verwendung des Description Stacks
;;; 

(with-substrate (test :type 'substrate :delete-if-exists-p t)
  (setf *x* *cur-substrate*)
  
  (node a (ncon a))
  (node b (ncon b))
  
  (edge a b (econ (r s t)))
  
  (visualize (remove nil (append 
                          (list (enumerate-atomic-configurations *x* 
                                                                 :save-node-labels-p nil))
                          (progn 
                            (node c (ncon c))
                            (edge b c (econ (t u)))
                            (prog1 (list (enumerate-atomic-configurations *x* :reset-p t :save-node-labels-p nil)
                                         (enumerate-atomic-configurations *x*))                                       
                              (pop-description-stack *x*)))
                          (delete-node *x* 'c)
                          (when (find-node *x* 'c) (error "!"))
                          (list (enumerate-atomic-configurations *x* :save-node-labels-p nil))
                          (list (enumerate-atomic-configurations *x*))))))
                          
                          
  

(progn
  (setf *all-substrates* nil)
  (with-substrate (test :type 'substrate :delete-if-exists-p t)
    
    (setf *x* *cur-substrate*)
    
    (node c (ncon c))    
    (node a (ncon a))
    (node b (ncon b))
    
    (edge a b (econ r))
    (edge b c (econ s))
    
    (visualize (list *x*
                     (scramble (copy *x*))))

    (princ (get-missing-edges *x* :inverses-p nil :loops-p nil))))



(progn
  (setf *all-substrates* nil)
  (with-substrate (test :type 'substrate :delete-if-exists-p t)
    
    (setf *x* *cur-substrate*)
    
    (node a (ncon ax))
    (node b (ncon bx))
    (node c (ncon cx))
    (add c (ncon ccc) :mode :and)
    (edge a b (econ r))
    
    (scramble *x*) 

    (princ (get-nodes *x*))

    (visualize (list *x* (scramble (copy *x*))))))





(progn 
  (delete-all-substrates)
  (in-substrate hello :delete-if-exists-p t)

  (setf *x* *cur-substrate*)

  (node a (ncon a-descr))
  (node b (ncon b-descr))

  (edge a b (econ ab-edge-descr))
  (edge b a (econ ba-edge-descr))

  (del-edge a b)

  (princ (get-edges *x*))
  (terpri)
  (princ (get-edges-between *x* 'a 'b))
  (princ (get-edges-between *x* 'b 'a)))


|#

