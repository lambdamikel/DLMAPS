;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(defpersistentclass abox (substrate)
  ((tbox :reader tbox :initarg :tbox :initform *cur-tbox*)

   (abox-version :accessor abox-version :initform 0)
   (prepared-p :reader prepared-p :initform nil)

   (satisfiable :reader satisfiable :initform :not-tested)   
  
   ;;;
   ;;;
   ;;;

   (language :accessor language :initform nil :initarg :language)

   (ready-for-retrieval :initform nil)
   
   #+:use-membership-tables
   (unexpanded-table :accessor unexpanded-table 
                     :initform (when *create-membership-tables-p*
                                 (make-weak-hash-table :size 10000 :rehash-size 10000 :test #'equalp)))
   #+:use-membership-tables
   (expanded-table :accessor expanded-table 
                   :initform (when *create-membership-tables-p*
                               (make-weak-hash-table :size 10000 :rehash-size 10000 :test #'equalp)))

   (action-timestamp-counter :accessor action-timestamp-counter :initform 0)
   (current-action :accessor current-action :initform nil)
   (choice-point-counter :accessor choice-point-counter :initform 0)
   (cur-clash-node :accessor cur-clash-node :initform nil)))


(defmethod reset-abox ((abox abox))
  (with-slots (node-table
               edge-table
               tbox 
               abox-version
               prepared-p
               satisfiable
               language
               ready-for-retrieval
               unexpanded-table
               expanded-table
               action-timestamp-counter
               current-action
               choice-point-counter
               cur-clash-node) abox
    
    (setf satisfiable :not-tested 
          ready-for-retrieval nil
          action-timestamp-counter 0
          current-action nil
          choice-point-counter 0
          cur-clash-node nil)

    (when node-table
      (clrhash node-table))
    (when edge-table
      (clrhash edge-table))

    abox))
             

(defparameter *use-avl-trees-for-abox1-p* nil)

(defpersistentclass abox1 (abox)
  ((node-table :initform nil) 
   (edge-table :initform nil) 
   
   (all-nodes :reader all-nodes :initform nil)
   (all-edges :reader all-edges :initform nil)
  
   ;;;
   ;;;  plus optionale (!) spezialisierte Listen / Indexstrukture
   ;;; ACHTUNG! DIESE MUESSEN VERWALTET WERDEN, INSBESONDERE AUCH 
   ;;; BEIM ROLLBACK!!! AUFWENDIG!
   ;;; S. SET-STATE-VECTOR!
   ;;; 
   
   (old-nodes :initform nil)

   (cache-sat-nodes :initform nil)

   (deactivated-nodes :initform nil)

   (leaf-nodes :initform nil)

   (blocked-nodes :initform nil)
   
   (active-nodes :initform nil) 

   #|
   ;; ; f. determ. expansion 

   (nodes-with-alls-nodes :initform nil) ; AVL 
   (nodes-not-det-expanded :initform nil) ; 

 |# 

   ;;;
   ;;; plus optionale Heaps 
   ;;;


   ;;; diese sind fuer get-oldes-node-.... -> abox-saturation 
   (nodes-not-or-expanded1 :initform nil)
   (nodes-not-some-expanded1 :initform nil)
   (nodes-not-feature-expanded1 :initform nil)
   (nodes-not-at-least-expanded1 :initform nil)

   ;;;
   ;;;
   ;;;
   
   ;;; diese sind fuer trace strategy -> left-of-p ! 

   (nodes-not-and-expanded :initform nil)
   (nodes-not-atom-expanded :initform nil)
   (nodes-not-or-expanded :initform nil)
   (nodes-not-some-expanded :initform nil)
   (nodes-not-feature-expanded :initform nil)
   (nodes-not-at-least-expanded :initform nil)))


(defmethod reset-abox ((abox abox1))
  (with-slots (all-nodes
               all-edges
               old-nodes
               cache-sat-nodes
               deactivated-nodes
               leaf-nodes
               blocked-nodes
               active-nodes
               nodes-not-and-expanded
               nodes-not-atom-expanded
               nodes-not-or-expanded
               nodes-not-some-expanded
               nodes-not-feature-expanded 
               nodes-not-at-least-expanded) abox

    
    (setf all-nodes nil
          all-edges nil
          old-nodes nil
          cache-sat-nodes nil
          deactivated-nodes nil
          leaf-nodes nil
          blocked-nodes nil)

    (dolist (slot '(active-nodes
                    nodes-not-and-expanded
                    nodes-not-atom-expanded
                    nodes-not-or-expanded
                    nodes-not-some-expanded
                    nodes-not-feature-expanded 
                    nodes-not-at-least-expanded

                    nodes-not-or-expanded1
                    nodes-not-some-expanded1
                    nodes-not-feature-expanded1
                    nodes-not-at-least-expanded1))
      
      (heap-clear (slot-value abox slot))))

  (call-next-method))

;;;
;;;
;;;


(defmethod initialize-instance :after ((abox abox) &rest initargs)
  (push abox *all-aboxes*)

  (when (tbox abox)
    (pushnew abox (used-by-aboxes (tbox abox)))))

;;;
;;;
;;;


(defpersistentclass abox-item (substrate-object)
  ((old-p  :initarg :old-p :initform nil)
   (active-p  :reader active-p :initarg :active-p :initform nil)
   (deleted-p :reader deleted-p  :initform nil)

   (choice-points :initform nil :initarg :choice-points) ; initargs für Copy-Funktionen!
   (precondition-for-actions :initform nil :initarg :precondition-for-actions) 
   (postcondition-for-actions :initform nil :initarg :postcondition-for-actions) 
   
   (created-by :reader created-by
               :initform nil
               :initarg :created-by)))

(defmethod old-p ((item abox-item))
  (slot-value item 'old-p))

;;;
;;;
;;;

(defun get-standard-abox-class ()
  'abox1)

(defmethod get-standard-node-class ((abox abox))
  'abox-node)

(defmethod get-standard-edge-class ((abox abox))
  'abox-edge)

;;;
;;;
;;;

(defmethod get-standard-node-description-class ((abox abox))
  'node-label)

(defmethod get-standard-edge-description-class ((abox abox))
  'edge-label)

;;;
;;;
;;;

(defmethod unparse ((item symbol))
  item)

;;;
;;;
;;;

(defmacro representative (node)
  `(first (cluster-nodes ,node)))

(defmacro representative-p (node)
  `(eq ,node (representative ,node)))

;;;
;;; Iteratoren und mengenbasierte Schnittstelle! 
;;;


(defmacro loop-over-abox-nodes ((var abox &rest args) &body body)
  `(abox-nodes-iterator ,abox 
                        #'(lambda (,var)
                            ,@body)
                        ,@args))

(defmacro loop-over-abox-edges ((var abox &rest args) &body body)
  `(abox-edges-iterator ,abox 
                        #'(lambda (,var)
                            ,@body)
                        ,@args))

(defmacro loop-over-active-nodes ((var abox &rest args) &body body)
  `(abox-active-nodes-iterator ,abox 
                               #'(lambda (,var)
                                   ,@body)
                               ,@args))

(defmacro loop-over-leaf-nodes ((var abox &rest args) &body body)
  `(abox-leaf-nodes-iterator ,abox 
                             #'(lambda (,var)
                                 ,@body)
                             ,@args))

(defmacro loop-over-old-nodes ((var abox &rest args) &body body)
  `(abox-old-nodes-iterator ,abox 
                            #'(lambda (,var)
                                ,@body)
                            ,@args))

(defmacro loop-over-blocked-nodes ((var abox &rest args) &body body)
  `(abox-blocked-nodes-iterator ,abox 
                                #'(lambda (,var)
                                    ,@body)
                                ,@args))

(defmacro loop-over-deactivated-nodes ((var abox &rest args) &body body)
  `(abox-deactivated-nodes-iterator ,abox 
                                    #'(lambda (,var)
                                        ,@body)
                                    ,@args))

(defmacro loop-over-cache-sat-nodes ((var abox &rest args) &body body)
  `(abox-cache-sat-nodes-iterator ,abox 
                                  #'(lambda (,var)
                                      ,@body)
                                  ,@args))


;;;
;;;
;;;

(timed-defmethod abox-nodes-iterator ((abox abox) fn &optional (consider-cluster-p t))
  (maphash #'(lambda (key node)
               (when (and (integerp key)
                          (not (deleted-p node))
                          (=> consider-cluster-p 
                              (representative-p node)))
                 (funcall fn node)))
           (node-table abox)))

(timed-defmethod abox-nodes-iterator ((abox abox1) fn &optional (consider-cluster-p t))

  (if (not *use-avl-trees-for-abox1-p*)
           
      (dolist (node (all-nodes abox))
        (when (and (not (deleted-p node))
                   (=> consider-cluster-p 
                       (representative-p node)))
          (funcall fn node)))
    
    (loop-over-avl-tree (node (all-nodes abox))
                        (when (and (not (deleted-p node))
                                   (=> consider-cluster-p 
                                       (representative-p node)))
                          (funcall fn node)))))

;;;
;;;
;;:

(timed-defmethod abox-active-nodes-iterator ((abox abox) fn &optional (consider-cluster-p t))
  (abox-nodes-iterator abox #'(lambda (node)
                                (when (active-p node)
                                  (funcall fn node)))
                       consider-cluster-p))


(timed-defmethod abox-active-nodes-iterator ((abox abox1) fn &optional (consider-cluster-p t))

  (if *maintain-active-nodes-p* 
      
      (loop-over-heap-items (node (slot-value abox 'active-nodes))
                            
                            (when (=> consider-cluster-p 
                                      (representative-p node))

                              (funcall fn node)))
    (call-next-method)))
    
;;;
;;;
;;;

(timed-defmethod abox-old-nodes-iterator ((abox abox) fn &optional (consider-cluster-p t))
 (abox-nodes-iterator abox #'(lambda (node)
                               (when (old-p node)
                                  (funcall fn node)))
                      consider-cluster-p))


(timed-defmethod abox-old-nodes-iterator ((abox abox1) fn &optional (consider-cluster-p t))
  (if *maintain-old-nodes-p* 

      (dolist (node (slot-value abox 'old-nodes))
        (when (=> consider-cluster-p 
                  (representative-p node))
          (funcall fn node)))

    (call-next-method)))
    
;;;
;;;
;;;

(timed-defmethod abox-blocked-nodes-iterator ((abox abox) fn &optional (consider-cluster-p t))
 (abox-nodes-iterator abox #'(lambda (node)
                               (when (blocked-p node)
                                 (funcall fn node)))
                      consider-cluster-p))

(timed-defmethod abox-blocked-nodes-iterator ((abox abox1) fn &optional (consider-cluster-p t))
  (if *maintain-blocked-nodes-p* 

      (dolist (node (slot-value abox 'blocked-nodes))
        (when (=> consider-cluster-p 
                  (representative-p node))
          (funcall fn node)))
    
    (call-next-method)))    

;;;
;;;
;;;

(timed-defmethod abox-leaf-nodes-iterator ((abox abox) fn &optional (consider-cluster-p t))
  (abox-active-nodes-iterator abox #'(lambda (node)
                                       (when (every #'(lambda (out)
                                                        (not (active-p (to out))))
                                                    (outgoing node))
                                         (funcall fn node)))
                              consider-cluster-p))

(timed-defmethod abox-leaf-nodes-iterator ((abox abox1) fn &optional (consider-cluster-p t))
  
  (if *maintain-leaf-nodes-p* 
      
      (dolist (node (slot-value abox 'leaf-nodes))
        (when (=> consider-cluster-p 
                  (representative-p node))
          (funcall fn node)))

    (call-next-method)))
    
;;;
;;;
;;;


(timed-defmethod abox-deactivated-nodes-iterator ((abox abox) fn &optional (consider-cluster-p t))
  (abox-nodes-iterator abox #'(lambda (node) 
                                (unless (active-p node)
                                  (funcall fn node)))
                       consider-cluster-p))

(timed-defmethod abox-deactivated-nodes-iterator ((abox abox1) fn &optional (consider-cluster-p t))

  (if *maintain-deactivated-nodes-p* 

      (dolist (node (slot-value abox 'deactivated-nodes))
        (when (=> consider-cluster-p 
                  (representative-p node))
          (funcall fn node)))

    (call-next-method)))
    
;;;
;;;
;;;

(timed-defmethod abox-cache-sat-nodes-iterator ((abox abox) fn &optional (consider-cluster-p t))
  (abox-nodes-iterator abox #'(lambda (node) 
                                (when (cache-satisfiable-p node)
                                  (funcall fn node)))
                       consider-cluster-p))

(timed-defmethod abox-cache-sat-nodes-iterator ((abox abox1) fn &optional (consider-cluster-p t))

  (if *maintain-cache-sat-nodes-p* 
      
      (dolist (node (slot-value abox 'cache-sat-nodes))
        (when (=> consider-cluster-p 
                  (representative-p node))
          (funcall fn node)))

    (call-next-method)))

    
;;;
;;;
;;;

(timed-defmethod get-nodes ((abox abox) &rest args)
  (declare (ignorable args))
  (let ((nodes nil))
    (loop-over-abox-nodes (node abox)
      (push node nodes))
    nodes))


(timed-defmethod get-edges ((abox abox) &rest args &key (satisfying #'yes))
  (declare (ignorable args))
  (let ((edges nil))
    (loop-over-abox-edges (edge abox :satisfying satisfying)
      (push edge edges))
    edges))

(timed-defmethod get-active-nodes ((abox abox) &rest args)
  (declare (ignorable args))
  (let ((nodes nil))
    (loop-over-active-nodes (node abox)
                            (push node nodes))
    nodes))


(timed-defmethod get-old-nodes ((abox abox) &rest args)
  (declare (ignorable args))
  
  (let ((nodes nil))
    (loop-over-old-nodes (node abox)
                         (push node nodes))
    nodes))


(timed-defmethod get-blocked-nodes ((abox abox) &rest args)

  (let ((nodes nil))
    (loop-over-blocked-nodes (node abox)
                         (push node nodes))
    nodes))


(timed-defmethod get-leaf-nodes ((abox abox) &rest args)

  (let ((nodes nil))
    (loop-over-leaf-nodes (node abox)
                          (push node nodes))
    nodes))

(timed-defmethod get-deactivated-nodes ((abox abox) &rest args)

  (let ((nodes nil))
    (loop-over-deactivated-nodes (node abox)
                          (push node nodes))
    nodes))


(timed-defmethod get-cache-sat-nodes ((abox abox) &rest args)

  (let ((nodes nil))
    (loop-over-cache-sat-nodes (node abox)
                          (push node nodes))
    nodes))

;;;
;;;

(defmethod no-of-nodes ((abox abox) &rest args)
  (length (get-nodes abox)))

(defmethod no-of-active-nodes ((abox abox) &rest args)
  (length (get-active-nodes abox)))

(defmethod no-of-edges ((abox abox) &rest args)
  (length (get-edges abox)))

;;;
;;;
;;;

(timed-defun get-youngest-node-satisfying (abox fn)
  (let ((found nil))
    (loop-over-active-nodes (node abox)
      (when (and (or (not found)
                     (> (id node) (id found)))
                 (funcall fn node))
        (setf found node)))
    found))

(timed-defun get-youngest-node-satisfying1 (abox fn)
  (let ((found nil))
    (loop-over-active-nodes (node abox nil)
      (when (and (or (not found)
                     (> (id node) (id found)))
                 (funcall fn node))
        (setf found node)))
    found))

(timed-defun get-oldest-node-satisfying (abox fn)
  (let ((found nil))
    (loop-over-active-nodes (node abox)
      (when (and (or (not found)
                     (< (id node) (id found)))
                 (funcall fn node))
        (setf found node)))
    found))

(timed-defun get-oldest-node-satisfying1 (abox fn)
  (let ((found nil))
    (loop-over-active-nodes (node abox nil)
      (when (and (or (not found)
                     (< (id node) (id found)))
                 (funcall fn node))
        (setf found node)))
    found))

(timed-defun get-oldest-old-then-youngest-satisfying (abox fn)
  (or 
   (get-oldest-node-satisfying1 abox #'(lambda (x) (and (old-p x) (funcall fn x))))
   (get-youngest-node-satisfying1 abox fn)))


;;;
;;;
;;;

(timed-defmethod get-oldest-node-with-unexpanded-or-concepts ((abox abox))
  (get-oldest-node-satisfying abox #'has-unexpanded-or-concepts-p))

(timed-defmethod get-oldest-node-with-unexpanded-or-concepts ((abox abox1))
  (if *maintain-unexpanded-or-concepts1-heap-p* 
      (heap-peek (slot-value abox 'nodes-not-or-expanded1))
    (call-next-method)))

;;;
;;;
;;;

(timed-defmethod get-oldest-node-with-unexpanded-some-concepts ((abox abox))
    (get-oldest-node-satisfying abox #'has-unexpanded-some-concepts-p))

(timed-defmethod get-oldest-node-with-unexpanded-some-concepts ((abox abox1))
  (if *maintain-unexpanded-some-concepts1-heap-p* 
      (heap-peek (slot-value abox 'nodes-not-some-expanded1))
    (call-next-method)))

;;;
;;;
;;;

(timed-defmethod get-oldest-node-with-unexpanded-attribute-exists-concepts ((abox abox))
  (get-oldest-node-satisfying abox #'has-unexpanded-attribute-exists-concepts-p))

(timed-defmethod get-oldest-node-with-unexpanded-attribute-exists-concepts ((abox abox1))
  (if *maintain-unexpanded-attribute-exists-concepts1-heap-p*
      (heap-peek (slot-value abox 'nodes-not-feature-expanded1))
    (call-next-method)))

;;;
;;;
;;;

(timed-defmethod get-oldest-node-with-unexpanded-at-least-concepts ((abox abox))
  (get-oldest-node-satisfying abox #'has-unexpanded-at-least-concepts-p))

(timed-defmethod get-oldest-node-with-unexpanded-at-least-concepts ((abox abox1))
  (if *maintain-unexpanded-at-least-concepts1-heap-p*  
      (heap-peek (slot-value abox 'nodes-not-at-least-expanded1))
    (call-next-method)))


;;;
;;;
;;;

(timed-defmethod abox-edges-iterator ((abox abox) fn &key (satisfying #'yes))
  (maphash #'(lambda (key edge)
               (declare (ignorable edge))
               (when (and (integerp key)
                          (not (deleted-p edge))
                          (funcall satisfying edge))
                 (funcall fn edge)))
           (edge-table abox)))

(timed-defmethod abox-edges-iterator ((abox abox1) fn &key (satisfying #'yes))
  (if (not *use-avl-trees-for-abox1-p*)
      
      (dolist (edge (all-edges abox))
        (when (and (not (deleted-p edge))
                   (funcall satisfying edge))
          (funcall fn edge)))

    (loop-over-avl-tree (edge (all-edges abox))
                        (when (and (not (deleted-p edge))
                                   (funcall satisfying edge))
                          (funcall fn edge)))))

;;;
;;;
;;;


(defmethod establish-context-for ((abox abox) continuation)
  (with-slots (tbox) abox

    (let ((*cur-abox* abox)
          
          #+:use-membership-tables 
          (*unexpanded-table* (unexpanded-table abox))
          #+:use-membership-tables 
          (*expanded-table* (expanded-table abox)))

      (setf tbox (find-tbox (or tbox *cur-tbox*) :error-p t))

      (with-tbox* ( (tbox abox) )

        (if (next-method-p)
            (call-next-method)
          (funcall continuation))))))

(defmethod set-context-for progn ((abox abox))
  (with-slots (tbox) abox
    (setf *cur-abox* abox)

    #+:use-membership-tables 
    (setf *unexpanded-table* (unexpanded-table abox))
    #+:use-membership-tables 
    (setf *expanded-table* (expanded-table abox))
    
    (setf tbox (find-tbox (or tbox *cur-tbox*) :error-p t))
      
    (in-tbox* (tbox abox))))

;;;
;;;
;;;

(defun current-abox ()
  *cur-abox*)

(defun delete-abox (name &rest args)
  (apply #'delete-substrate name args))

(defmethod delete-graph :after ((abox abox) &rest args)
  ;;; wird von delete-substrate aufgerufen
  (setf *all-aboxes* (delete abox *all-aboxes*))
  (setf *cur-abox* nil)
  (when (tbox abox)
    (setf (used-by-aboxes (tbox abox))
          (delete abox (used-by-aboxes (tbox abox))))))

(defun find-abox (name &rest args)
  (apply #'find-substrate name args))

(defun delete-all-aboxes ()
  (delete-all-substrates :satisfying #'is-abox-p)
  (in-abox default-abox))

(defun all-aboxes ()
  (get-all-substrates :satisfying #'is-abox-p))

(defun associated-tbox (abox)
  (tbox-name (tbox (find-abox abox :error-p t))))

(defun clone-abox (abox)
  (copy-graph (find-abox abox :error-p t)))

;;;
;;;
;;;

(defmethod reset-sat-status ((abox abox)) 
  (incf (abox-version abox))
  (reset-cached-sat-result abox)
  (setf (slot-value abox 'prepared-p) nil)

  abox)

(defmethod reset-cached-sat-result ((abox abox))
  (setf (slot-value abox 'satisfiable) :not-tested))

(defmethod reset-sat-status :after ((item abox-item))
  (dolist (slot '(active-p 
                  deleted-p 
                  choice-points
                  precondition-for-actions
                  postcondition-for-actions))
   
    (setf (slot-value item slot) nil)))

;;;
;;;
;;;

(defmethod get-language ((abox abox) &optional recompute-p)
  (declare (ignorable recompute-p))
  ;;; durch das Parsen sind die Node-Assertions
  ;;; im concept-store gelandet! 
  
  (prepare abox +dl+)
  (slot-value abox 'language))


(defmethod note-abox-has-changed ((abox abox))
  (reset-sat-status abox))
 
(defmethod abox-realized-p ((abox abox))
  (loop-over-abox-nodes (node abox)
    (unless (realized-p node)
      (return-from abox-realized-p nil)))
  t)

;;;
;;;
;;;

#|

(defmethod copy ((abox abox) &rest args)
  (let ((copy (apply #'call-next-method abox
                     :name (name abox) 
                     ;;; prevent building "Copy of copy of copy of..." names!
                     args)))

    (with-slots (satisfiable language ready-for-retrieval tbox) abox
                      
      (setf (slot-value copy 'satisfiable) satisfiable
            (slot-value copy 'language) language
            (slot-value copy 'ready-for-retrieval) ready-for-retrieval
            (slot-value copy 'tbox) tbox))

    (loop-over-abox-nodes (node copy)
      (setf (slot-value node 'cluster-nodes)
            (mapcar #'(lambda (x) 
                        (find-node copy (id x) :error-p t))
                    (cluster-nodes node))))
    
    copy))


|#

