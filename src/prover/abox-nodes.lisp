;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(defpersistentclass abox-node (substrate-node abox-item)
  ((avl-key :initform (incf *avl-key-counter*) :reader avl-key)

   (address :reader address :initform '(0)) 
   (rev-address :reader rev-address :initform '(0))

   (succ-counter :reader succ-counter :initform 0)

   (initial-concept :reader initial-concept :initform nil :initarg :initial-concept)
   (told-concepts :reader told-concepts :initform nil :initarg :told-concepts)
   (cluster-nodes :reader cluster-nodes :initform nil :initarg :cluster-nodes) 
   ;; wird momentan nicht benutzt (s. merging2.lisp; auskommentiert) 

   (core-model :reader core-model :initform nil) ; entailed information (true in ALL models!) 
   (ind-models :reader ind-models :initform nil) ; model information (true in THIS model!); evtl. mehrere

   ;;;
   ;;; Die (anhand des Knotenlabels) abgeleiteten Attribute (zur Effizienzsteigerung) 
   ;;; werden stets fuer den Stellvertreter gesetzt, da die Iteratoren nur den  
   ;;; Stellvertreter als Knoten zuruckliefern, dessen Label ist aber die Ver-
   ;;; einigung der Knoten des Clusters!
   ;;; 
   ;;; Diese Markierungsattribute werden von den Tabregeln gesetzt.  
   ;;;

   (deterministically-expanded-p :accessor deterministically-expanded-p :initform nil)
   (checked-p :accessor checked-p :initform nil)   
   (really-satisfiable-p :accessor really-satisfiable-p :initform nil)
   (cache-satisfiable-p :accessor cache-satisfiable-p :initform nil)

   (realized-p :accessor realized-p :initform nil) 
   ;; noch nicht verwendet
   
   (stable-p :accessor stable-p :initform nil :initarg :stable-p)
   ;;; determinsitically-expanded-p iff BCP etc. abgeschlossen

   ;;;
   ;;; (private) Verwaltungsinfo; nur ueber Kernel bzw. "komplexe" Methoden zugreifbar 
   ;;; (um konsistente Verwaltung sicherzustellen!) 
   ;;; 

   (blocked-by   :initform nil :initarg :blocked-by)
   (blocking-for :initform nil :initarg :blocking-for)))

;;;
;;;
;;;

(defmethod root-or-old-p ((node abox-node))
  (or (old-p node)
      (root-p node)))

(defmethod root-p ((node abox-node))
  (= (id node) 1))

;;;
;;;
;;;

#|

(defmethod left-of-p ((a abox-node) (b abox-node))
  (if (old-p a)
      (if (old-p b)
          (< (id a) (id b))
        t)
    (if (old-p b)
        nil
      (let ((aa (rev-address a))
            (ba (rev-address b)))
        
        (some #'(lambda (a b) 
                  (< a b))
              aa ba)))))

|#


(defmethod left-of-p ((a abox-node) (b abox-node))
  (if (old-p a)
      (if (old-p b)
          (< (id a) (id b))
        t)
    (if (old-p b)
        nil
      (> (id a) (id b))))) 

(defmethod older-p ((a abox-node) (b abox-node))
  (< (id a) (id b)))


;;;
;;;
;;;

(defmethod copy-node ((abox abox) (node abox-node) &rest args)
  (declare (ignorable args))

  (let ((copy (call-next-method))
        (state (get-state-vector node)))

    (set-state-vector copy state nil)

    (dolist (slot '(old-p
                    active-p
                    deleted-p

                    address 
                    rev-address 
                    succ-counter
                    
                    choice-points
                    ;;; mit denen kann man zwar nicht weiterrechnen (da Referenzen auf
                    ;;; alte Objekte enthalten!), aber gut fuer Inspektionszwecke!
                    precondition-for-actions
                    postcondition-for-actions
                    
                    created-by
                    
                    ;;;
                    
                    initial-concept
                    told-concepts 
                    cluster-nodes
                    core-model
                    ind-models 
                    deterministically-expanded-p 
                    checked-p
                    really-satisfiable-p
                    cache-satisfiable-p
                    realized-p
                    stable-p
                    blocked-by
                    blocking-for))

      (setf (slot-value copy slot) (copy-tree (slot-value node slot))))
  
    copy))

;;;
;;;
;;;

(defmethod unparse ((item abox-node))
  (name item))

(defmethod get-abox ((node abox-node))
  (in-graph node))

(defmethod reset-sat-status ((node abox-node))
  (dolist (slot '(initial-concept
                  choice-points
                  core-model
                  ind-models
                  really-satisfiable-p 
                  cache-satisfiable-p
                  realized-p
                  stable-p
                  blocked-by
                  blocking-for))
    
    (setf (slot-value node slot) nil))
  
  (reset-label (description node))
  (reset-sat-status (in-graph node)))

;;;
;;; Macros fuer Label-Verwaltung
;;;

(defmacro loop-over-node-slot ((var node slot) &rest body)  
  #+:use-avl-trees-for-labels
  `(loop-over-avl-tree (,var (slot-value (description ,node) ',slot))
                       ,@body)
  #-:use-avl-trees-for-labels
  `(dolist (,var (slot-value (description ,node) ',slot))
     ,@body))

;;;
;;;
;;;

(defmacro loop-over-cluster-nodes ((var node &optional (consider-cluster-p t)) &body body)
  (if consider-cluster-p 
      `(dolist (,var (cluster-nodes ,node))
         ,@body)
    `(let ((,var ,node))
       ,@body)))

(defmacro loop-over-node-unexpanded-atomic-concepts ((var node &optional (consider-cluster-p t)) &rest body)  
  `(loop-over-cluster-nodes (equi ,node ,consider-cluster-p)
                            (loop-over-node-slot (,var equi unexpanded-atomic-concepts) ,@body)))

(defmacro loop-over-node-unexpanded-some-concepts ((var node &optional (consider-cluster-p t)) &rest body)  
  `(loop-over-cluster-nodes (equi ,node ,consider-cluster-p)
                            (loop-over-node-slot (,var equi unexpanded-some-concepts) ,@body)))

(defmacro loop-over-node-unexpanded-attribute-exists-concepts ((var node &optional (consider-cluster-p t)) &rest body)  
  `(loop-over-cluster-nodes (equi ,node ,consider-cluster-p)
                            (loop-over-node-slot (,var equi unexpanded-attribute-exists-concepts) ,@body)))

(defmacro loop-over-node-unexpanded-at-least-concepts ((var node &optional (consider-cluster-p t)) &rest body)  
  `(loop-over-cluster-nodes (equi ,node ,consider-cluster-p)
                            (loop-over-node-slot (,var equi unexpanded-at-least-concepts) ,@body)))

(defmacro loop-over-node-unexpanded-at-most-concepts ((var node &optional (consider-cluster-p t)) &rest body)  
  `(loop-over-cluster-nodes (equi ,node ,consider-cluster-p)
                            (loop-over-node-slot (,var equi unexpanded-at-most-concepts) ,@body)))

(defmacro loop-over-node-unexpanded-all-concepts ((var node &optional (consider-cluster-p t)) &rest body)  
  `(loop-over-cluster-nodes (equi ,node ,consider-cluster-p)
                            (loop-over-node-slot (,var equi unexpanded-all-concepts) ,@body)))

(defmacro loop-over-node-unexpanded-and-concepts ((var node &optional (consider-cluster-p t)) &rest body)  
  `(loop-over-cluster-nodes (equi ,node ,consider-cluster-p)
                            (loop-over-node-slot (,var equi unexpanded-and-concepts) ,@body)))

(defmacro loop-over-node-unexpanded-or-concepts ((var node &optional (consider-cluster-p t)) &rest body)  
  `(loop-over-cluster-nodes (equi ,node ,consider-cluster-p) 
                            (loop-over-node-slot (,var equi unexpanded-or-concepts) ,@body)))

;;;
;;;
;;;

(defmacro loop-over-node-unexpanded-concepts ((var node &optional (consider-cluster-p t)) &rest body)  
  (let ((label (gensym)))
    `(labels ((,label (,var ,node)
                (declare (ignorable ,var ,node))
                ,@body))
       
       (loop-over-cluster-nodes (equi ,node ,consider-cluster-p)

                                (loop-over-node-slot (,var equi unexpanded-atomic-concepts) (,label ,var ,node))
                                (loop-over-node-slot (,var equi unexpanded-and-concepts) (,label ,var ,node))
                                (loop-over-node-slot (,var equi unexpanded-or-concepts) (,label ,var ,node))
                                (loop-over-node-slot (,var equi unexpanded-all-concepts) (,label ,var ,node))
                                (loop-over-node-slot (,var equi unexpanded-some-concepts) (,label ,var ,node))
                                (loop-over-node-slot (,var equi unexpanded-attribute-exists-concepts) (,label ,var ,node))
                                (loop-over-node-slot (,var equi unexpanded-at-least-concepts) (,label ,var ,node))
                                (loop-over-node-slot (,var equi unexpanded-at-most-concepts) (,label ,var ,node))))))

;;; 
;;;
;;;

(defmacro loop-over-node-expanded-atomic-concepts ((var node &optional (consider-cluster-p t)) &rest body)  
  `(loop-over-cluster-nodes (equi ,node ,consider-cluster-p)
                            (loop-over-node-slot (,var equi expanded-atomic-concepts) ,@body)))

(defmacro loop-over-node-expanded-some-concepts ((var node &optional (consider-cluster-p t)) &rest body)  
  `(loop-over-cluster-nodes (equi ,node ,consider-cluster-p)
                            (loop-over-node-slot (,var equi expanded-some-concepts) ,@body)))

(defmacro loop-over-node-expanded-attribute-exists-concepts ((var node &optional (consider-cluster-p t)) &rest body)  
  `(loop-over-cluster-nodes (equi ,node ,consider-cluster-p)
                            (loop-over-node-slot (,var equi expanded-attribute-exists-concepts) ,@body)))

(defmacro loop-over-node-expanded-at-least-concepts ((var node &optional (consider-cluster-p t)) &rest body)  
  `(loop-over-cluster-nodes (equi ,node ,consider-cluster-p)
                            (loop-over-node-slot (,var equi expanded-at-least-concepts) ,@body)))

(defmacro loop-over-node-expanded-at-most-concepts ((var node &optional (consider-cluster-p t)) &rest body)  
  `(loop-over-cluster-nodes (equi ,node ,consider-cluster-p)
                            (loop-over-node-slot (,var equi expanded-at-most-concepts) ,@body)))

(defmacro loop-over-node-expanded-all-concepts ((var node &optional (consider-cluster-p t)) &rest body)  
  `(loop-over-cluster-nodes (equi ,node ,consider-cluster-p)
                            (loop-over-node-slot (,var equi expanded-all-concepts) ,@body)))

(defmacro loop-over-node-expanded-and-concepts ((var node &optional (consider-cluster-p t)) &rest body)  
  `(loop-over-cluster-nodes (equi ,node ,consider-cluster-p)
                            (loop-over-node-slot (,var equi expanded-and-concepts) ,@body)))

(defmacro loop-over-node-expanded-or-concepts ((var node &optional (consider-cluster-p t)) &rest body)  
  `(loop-over-cluster-nodes (equi ,node ,consider-cluster-p)
                            (loop-over-node-slot (,var equi expanded-or-concepts) ,@body)))

(defmacro loop-over-node-expanded-concepts ((var node &optional (consider-cluster-p t)) &rest body)  
  (let ((label (gensym)))
    `(labels ((,label (,var ,node)
                (declare (ignorable ,var ,node))
                ,@body))

       (loop-over-cluster-nodes (equi ,node ,consider-cluster-p)
                                (loop-over-node-slot (,var equi expanded-atomic-concepts) (,label ,var ,node))
                                (loop-over-node-slot (,var equi expanded-and-concepts) (,label ,var ,node))
                                (loop-over-node-slot (,var equi expanded-or-concepts) (,label ,var ,node))
                                (loop-over-node-slot (,var equi expanded-all-concepts) (,label ,var ,node))
                                (loop-over-node-slot (,var equi expanded-some-concepts) (,label ,var ,node))
                                (loop-over-node-slot (,var equi expanded-attribute-exists-concepts) (,label ,var ,node))
                                (loop-over-node-slot (,var equi expanded-at-least-concepts) (,label ,var ,node))
                                (loop-over-node-slot (,var equi expanded-at-most-concepts) (,label ,var ,node))))))

;;;
;;;
;;;

(defmacro loop-over-node-concepts ((var node &optional (consider-cluster-p t)) &rest body)
  (let ((label (gensym)))
    `(labels ((,label (,var ,node)
                (declare (ignorable ,var ,node))
                ,@body))
       (loop-over-node-unexpanded-concepts (,var ,node ,consider-cluster-p)  (,label ,var ,node))
       (loop-over-node-expanded-concepts (,var ,node ,consider-cluster-p)  (,label ,var ,node)))))

(defmacro loop-over-node-all-concepts ((var node &optional (consider-cluster-p t)) &rest body)  
  (let ((label (gensym)))
    `(labels ((,label (,var ,node)
                (declare (ignorable ,var ,node))
                ,@body))
       (loop-over-node-unexpanded-all-concepts (,var ,node ,consider-cluster-p) (,label ,var ,node))
       (loop-over-node-expanded-all-concepts (,var ,node ,consider-cluster-p) (,label ,var ,node)))))

;;;
;;;
;;;

(defmacro every-unexpanded-concept-satisfies (node predicate &optional (consider-cluster-p t))
  (let ((blockname (gensym)))
    `(block ,blockname
       (progn 
         (loop-over-node-unexpanded-concepts (x ,node ,consider-cluster-p)
           (unless (,predicate x)
             (return-from ,blockname nil)))
         t))))

(defmacro some-unexpanded-concept-satisfies (node predicate &optional (consider-cluster-p))
  (let ((blockname (gensym)))
    `(block ,blockname
       (progn 
         (loop-over-node-unexpanded-concepts (x ,node ,consider-cluster-p)
           (when (,predicate x)
             (return-from ,blockname t)))
         nil))))

;;;
;;; 
;;;

(defmethod has-unexpanded-concepts-p ((node abox-node) &optional (consider-cluster-p t))
  (some #'(lambda (node) 
            (has-unexpanded-concepts-p (description node)))
        (if consider-cluster-p
            (cluster-nodes node)
          (list node))))

(defmethod has-expanded-concepts-p ((node abox-node)  &optional (consider-cluster-p t))
  (some #'(lambda (node) 
            (has-expanded-concepts-p (description node)))
        (if consider-cluster-p
            (cluster-nodes node)
          (list node))))


(defmethod has-unexpanded-some-concepts-p ((node abox-node)  &optional (consider-cluster-p t))
  (some #'(lambda (node) 
            (has-unexpanded-some-concepts-p (description node)))
        (if consider-cluster-p
            (cluster-nodes node)
          (list node))))

(defmethod has-unexpanded-all-concepts-p ((node abox-node)  &optional (consider-cluster-p t))
  (some #'(lambda (node) 
            (has-unexpanded-all-concepts-p (description node)))
        (if consider-cluster-p
            (cluster-nodes node)
          (list node))))

(defmethod has-unexpanded-and-concepts-p ((node abox-node)  &optional (consider-cluster-p t))
  (some #'(lambda (node) 
            (has-unexpanded-and-concepts-p (description node)))
        (if consider-cluster-p
            (cluster-nodes node)
          (list node))))


(defmethod has-unexpanded-atomic-concepts-p ((node abox-node)  &optional (consider-cluster-p t))
  (some #'(lambda (node) 
            (has-unexpanded-atomic-concepts-p (description node)))
        (if consider-cluster-p
            (cluster-nodes node)
          (list node))))

(defmethod has-unexpanded-or-concepts-p ((node abox-node)  &optional (consider-cluster-p t))
  (some #'(lambda (node) 
            (has-unexpanded-or-concepts-p (description node)))
        (if consider-cluster-p
            (cluster-nodes node)
          (list node))))

(defmethod has-unexpanded-attribute-exists-concepts-p ((node abox-node)  &optional (consider-cluster-p t))
  (some #'(lambda (node) 
            (has-unexpanded-attribute-exists-concepts-p (description node)))
        (if consider-cluster-p
            (cluster-nodes node)
          (list node))))

(defmethod has-unexpanded-at-least-concepts-p ((node abox-node)  &optional (consider-cluster-p t))
  (some #'(lambda (node)
            (has-unexpanded-at-least-concepts-p (description node)))
        (if consider-cluster-p
            (cluster-nodes node)
          (list node))))

(defmethod has-unexpanded-at-most-concepts-p ((node abox-node)  &optional (consider-cluster-p t))
  (some #'(lambda (node)
            (has-unexpanded-at-most-concepts-p (description node)))
        (if consider-cluster-p
            (cluster-nodes node)
          (list node))))

;;;
;;;
;;;

(defmethod has-unexpanded-generating-concepts-p ((node abox-node)  &rest args)
  (or (apply #'has-unexpanded-some-concepts-p node args)
      (apply #'has-unexpanded-attribute-exists-concepts-p node args)
      (apply #'has-unexpanded-at-least-concepts-p node args)))

;;;
;;;
;;;

(defmacro on-tableau-p (node concept &optional (consider-cluster-p t))
  `(some #'(lambda (equi)
             (or (concept-member (description equi) ,concept expanded)
                 (concept-member (description equi) ,concept unexpanded)))
         ,(if consider-cluster-p
              `(cluster-nodes ,node)
            `(list ,node))))

(defmacro expanded-p (node concept &optional (consider-cluster-p t))
  `(some #'(lambda (equi) 
             (concept-member (description equi) ,concept expanded))
         ,(if consider-cluster-p
              `(cluster-nodes ,node)
            `(list ,node))))

(defmacro unexpanded-p (node concept &optional (consider-cluster-p t))
  `(some #'(lambda (equi) 
             (concept-member (description equi) ,concept unexpanded))
         ,(if consider-cluster-p
              `(cluster-nodes ,node)
            `(list ,node))))

;;;
;;;
;;; 

(defmethod complete-p ((node abox-node)  &optional (consider-cluster-p t))
  (every #'(lambda (node) (complete-p (description node)))
         (if consider-cluster-p
             (cluster-nodes node)
           (list node))))

;;;
;;;
;;;

(defmethod already-known-to-be-inconsistent-p ((abox-node abox-node))
  (when (initial-concept abox-node)
    (already-known-to-be-inconsistent-p (initial-concept abox-node))))

(defmethod already-known-to-be-tautological-p ((abox-node abox-node))
  (when (initial-concept abox-node)
    (already-known-to-be-tautological-p (initial-concept abox-node))))

(defmethod already-known-to-be-satisfiable-p ((abox-node abox-node))
  (when (initial-concept abox-node)
    (already-known-to-be-satisfiable-p (initial-concept abox-node))))

;;;
;;;
;;;

(defmethod compute-virtual-successors ((abox abox) (node abox-node) &optional role) 
  (declare (ignorable role))
  nil)

(defmethod materialize-virtual-edge ((abox abox) (node abox-node) (succ abox-node) role) 
  (declare (ignorable role))
  nil)
