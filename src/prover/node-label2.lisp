;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(defpersistentclass node-label (node-description)
  ((textual-description :initform "Node-Label")

   (of-node :reader of-node :initarg :of-node)   

   (unexpanded-atomic-concepts :initform nil :initarg :unexpanded-atomic-concepts)
   (unexpanded-all-concepts :initform nil :initarg :unexpanded-all-concepts)
   (unexpanded-and-concepts :initform nil :initarg :unexpanded-and-concepts)
   (unexpanded-or-concepts :initform nil :initarg :unexpanded-or-concepts)
   
   (unexpanded-some-concepts :initform nil :initarg :unexpanded-some-concepts)      
   (unexpanded-attribute-exists-concepts :initform nil :initarg :unexpanded-attribute-exists-concepts)   
   (unexpanded-at-least-concepts :initform nil :initarg :unexpanded-at-least-concepts)   
   (unexpanded-at-most-concepts :initform nil :initarg :unexpanded-at-most-concepts)   
      
   (expanded-atomic-concepts :initform nil :initarg :expanded-atomic-concepts)
   (expanded-all-concepts :initform nil :initarg :expanded-all-concepts)
   (expanded-and-concepts :initform nil :initarg :expanded-and-concepts)
   (expanded-or-concepts :initform nil :initarg :expanded-or-concepts)

   (expanded-some-concepts :initform nil :initarg :expanded-some-concepts)      
   (expanded-attribute-exists-concepts :initform nil :initarg :expanded-attribute-exists-concepts)   
   (expanded-at-least-concepts :initform nil :initarg :expanded-at-least-concepts)   
   (expanded-at-most-concepts :initform nil :initarg :expanded-at-most-concepts)))



(defmethod copy ((label node-label) &rest args)
  (declare (ignorable args))
  (let ((copy 

         (make-node-description 'node-label 
                                (textual-description label)
                                
                                :type 
                                (type-of label))))

    (with-slots (of-node) copy

      (setf of-node 
            (find-node copy (of-node label) :error-p t)))

    (dolist (slot '(unexpanded-atomic-concepts
                    unexpanded-all-concepts
                    unexpanded-and-concepts
                    unexpanded-or-concepts 
   
                    unexpanded-some-concepts
                    unexpanded-attribute-exists-concepts
                    unexpanded-at-least-concepts 
                    unexpanded-at-most-concepts 
      
                    expanded-atomic-concepts 
                    expanded-all-concepts 
                    expanded-and-concepts 
                    expanded-or-concepts 
   
                    expanded-some-concepts 
                    expanded-attribute-exists-concepts
                    expanded-at-least-concepts
                    expanded-at-most-concepts))

      (setf (slot-value copy slot)
            #+:use-avl-trees-for-labels
            (copy-tree (slot-value label slot))
            #-:use-avl-trees-for-labels
            (copy-list (slot-value label slot))))

    label))


;;;
;;;
;;;


(defmethod reset-label ((node-label node-label))
  (with-slots (expanded-atomic-concepts
               expanded-some-concepts
               expanded-attribute-exists-concepts
               expanded-at-least-concepts
               expanded-at-most-concepts
               expanded-all-concepts
               expanded-and-concepts
               expanded-or-concepts

               unexpanded-atomic-concepts
               unexpanded-some-concepts
               unexpanded-attribute-exists-concepts
               unexpanded-at-least-concepts
               unexpanded-at-most-concepts
               
               unexpanded-all-concepts
               unexpanded-and-concepts
               unexpanded-or-concepts) node-label 
    
    #+:use-membership-tables
    (dolist (slot '(expanded-atomic-concepts
                    expanded-some-concepts
                    expanded-attribute-exists-concepts
                    expanded-at-least-concepts
                    expanded-at-most-concepts
                    expanded-all-concepts
                    expanded-and-concepts
                    expanded-or-concepts))

      (dolist (concept (slot-value node-label slot))
        (remhash (list concept node-label) *expanded-table*)))

    #+:use-membership-tables
    (dolist (slot '(unexpanded-atomic-concepts
                    unexpanded-some-concepts
                    unexpanded-attribute-exists-concepts
                    unexpanded-at-least-concepts
                    unexpanded-at-most-concepts
                    unexpanded-all-concepts
                    unexpanded-and-concepts
                    unexpanded-or-concepts))

      (dolist (concept (slot-value node-label slot))
        (remhash (list concept node-label) *unexpanded-table*)))
    
    
    (setf expanded-atomic-concepts nil
          expanded-some-concepts nil
          expanded-attribute-exists-concepts nil
          expanded-at-least-concepts nil
          expanded-at-most-concepts nil
          
          expanded-all-concepts nil
          expanded-and-concepts nil
          expanded-or-concepts nil

          unexpanded-atomic-concepts nil
          unexpanded-some-concepts nil
          unexpanded-attribute-exists-concepts nil
          unexpanded-at-least-concepts nil
          unexpanded-at-most-concepts nil
               
          unexpanded-all-concepts nil
          unexpanded-and-concepts nil
          unexpanded-or-concepts nil)))

;;;
;;;
;;;

(defmacro get-slot-for (concept slot-type)
  (ecase slot-type
    (expanded
     `(etypecase ,concept
        (atomic-concept 'expanded-atomic-concepts)
        (and-concept 'expanded-and-concepts)
        (or-concept 'expanded-or-concepts)
        (attribute-exists-concept 'expanded-attribute-exists-concepts)
        (some-concept 'expanded-some-concepts)
        (all-concept 'expanded-all-concepts)
        (at-least-concept 'expanded-at-least-concepts)
        (at-most-concept 'expanded-at-most-concepts)))
    (unexpanded
     `(etypecase ,concept
        (atomic-concept 'unexpanded-atomic-concepts)
        (and-concept 'unexpanded-and-concepts)
        (or-concept 'unexpanded-or-concepts)
        (attribute-exists-concept 'unexpanded-attribute-exists-concepts)
        (some-concept 'unexpanded-some-concepts)
        (all-concept 'unexpanded-all-concepts)
        (at-least-concept 'unexpanded-at-least-concepts)
        (at-most-concept 'unexpanded-at-most-concepts)))))

;;;
;;;
;;;     

#-:use-membership-tables
(defmacro concept-member (label concept slot)
  `(let ((slot (get-slot-for ,concept ,slot)))
     #+:use-avl-trees-for-labels
     (find-in-avl-tree ,concept (slot-value ,label slot) :key #'id)
     #-:use-avl-trees-for-labels     
     (member ,concept (slot-value ,label slot))))


#+:use-membership-tables
(defmacro concept-member (label concept slot)  
  `(ecase ',slot 
     (expanded
      (gethash (list ,concept  ,label) *expanded-table*))
     (unexpanded
      (gethash (list ,concept  ,label) *unexpanded-table*))))

;;;
;;;
;;;

(defmethod make-node-description ((class (eql 'node-label)) descr &rest args &key type &allow-other-keys)
  (apply #'make-instance (or type class) 
         :textual-description descr 
         :constructor-sym 'node-label 
         :allow-other-keys t 
         args))

;;;
;;;
;;;

(defmethod has-unexpanded-concepts-p ((label node-label) &optional (consider-cluster-p t))
  (declare (ignorable consider-cluster-p))
  (some #'(lambda (slot) 
            (slot-value label slot))
        '(unexpanded-atomic-concepts 
          unexpanded-some-concepts 
          unexpanded-all-concepts 
          unexpanded-and-concepts 
          unexpanded-or-concepts
          unexpanded-attribute-exists-concepts
          unexpanded-at-least-concepts
          unexpanded-at-most-concepts)))

(defmethod has-expanded-concepts-p ((label node-label) &optional (consider-cluster-p t))
  (declare (ignorable consider-cluster-p))
  (some #'(lambda (slot) 
            (slot-value label slot))
        '(expanded-atomic-concepts 
          expanded-some-concepts 
          expanded-all-concepts 
          expanded-and-concepts 
          expanded-or-concepts 
          expanded-attribute-exists-concepts
          expanded-at-least-concepts
          expanded-at-most-concepts)))

;;;
;;;
;;;

(defmethod has-unexpanded-and-concepts-p ((label node-label) &optional (consider-cluster-p t))
  (declare (ignorable consider-cluster-p))
  (slot-value label 'unexpanded-and-concepts))

(defmethod has-unexpanded-atomic-concepts-p ((label node-label) &optional (consider-cluster-p t))
  (declare (ignorable consider-cluster-p))
  (slot-value label 'unexpanded-atomic-concepts))

(defmethod has-unexpanded-some-concepts-p ((label node-label) &optional (consider-cluster-p t))
  (declare (ignorable consider-cluster-p))
  (slot-value label 'unexpanded-some-concepts))

(defmethod has-unexpanded-all-concepts-p ((label node-label) &optional (consider-cluster-p t))
  (declare (ignorable consider-cluster-p))
  (slot-value label 'unexpanded-all-concepts))

(defmethod has-unexpanded-or-concepts-p ((label node-label) &optional (consider-cluster-p t))
  (declare (ignorable consider-cluster-p))
  (slot-value label 'unexpanded-or-concepts))

(defmethod has-unexpanded-attribute-exists-concepts-p ((label node-label) &optional (consider-cluster-p t))
  (declare (ignorable consider-cluster-p))
  (slot-value label 'unexpanded-attribute-exists-concepts))

(defmethod has-unexpanded-at-least-concepts-p ((label node-label) &optional (consider-cluster-p t))
  (declare (ignorable consider-cluster-p))
  (slot-value label 'unexpanded-at-least-concepts))

(defmethod has-unexpanded-at-most-concepts-p ((label node-label) &optional (consider-cluster-p t))
  (declare (ignorable consider-cluster-p))
  (slot-value label 'unexpanded-at-most-concepts))

;;;
;;;
;;;

(defmethod complete-p ((label node-label)  &optional (consider-cluster-p t))
  (declare (ignorable consider-cluster-p))
  
  ;;; all und at-most bewusst ausgespart!!

  (with-slots (unexpanded-atomic-concepts 
               unexpanded-some-concepts 
               unexpanded-and-concepts 
               unexpanded-or-concepts
               unexpanded-attribute-exists-concepts 
               unexpanded-at-least-concepts) label
    
    (not (or unexpanded-atomic-concepts 
             unexpanded-some-concepts 
             unexpanded-and-concepts 
             unexpanded-or-concepts
             unexpanded-attribute-exists-concepts 
             unexpanded-at-least-concepts))))

;;;
;;; Label Verwaltung
;;;

(timed-defmethod add-to-unexpanded ((label node-label) (concept concept))

  ;;; Position wichtig!
  (let* ((node (of-node label))
         (abox (in-graph node)))

    (add-to-heaps abox node concept) 
  
    #+:use-membership-tables
    (setf (gethash (list concept label) *unexpanded-table*) t)
    
    (let ((slot (get-slot-for concept unexpanded)))
      #+:use-avl-trees-for-labels
      (insert-into-avl-tree concept (slot-value label slot) :key #'id)        
      #-:use-avl-trees-for-labels
      (push concept (slot-value label slot)))

    (unless (active-p node) 
      (activate-node abox node))

    (unregister-deterministically-expanded abox node)

    (adjust-blocking-dependencies abox node)))


(timed-defmethod add-to-unexpanded ((label node-label) (list list))
  (dolist (concept list)
    (add-to-unexpanded label concept)))

;;;
;;;
;;;

(timed-defmethod add-to-expanded ((label node-label) (concept concept))

  #+:use-membership-tables
  (setf (gethash (list concept label) *expanded-table*) t)

  (let ((slot (get-slot-for concept expanded)))
    #+:use-avl-trees-for-labels
    (insert-into-avl-tree concept (slot-value label slot) :key #'id)
    #-:use-avl-trees-for-labels
    (push concept (slot-value label slot))))

(timed-defmethod add-to-expanded ((label node-label) (list list))
  (dolist (concept list)
    (add-to-expanded label concept)))

;;;
;;;
;;;

(timed-defmethod delete-from-unexpanded ((label node-label) (concept concept))
  #+:use-membership-tables
  (remhash (list concept label) *unexpanded-table*)
  
  (let ((slot (get-slot-for concept unexpanded)))
    #+:use-avl-trees-for-labels 
    (delete-from-avl-tree concept (slot-value label slot) :key #'id)
    #-:use-avl-trees-for-labels 
    (setf (slot-value label slot)
          (delete concept (slot-value label slot)))

    ;;; Position wichtig!
    (let* ((node (of-node label))
           (abox (in-graph node)))

      (delete-from-heaps abox node concept))))
    


(timed-defmethod delete-from-unexpanded ((label node-label) (list list))
  (dolist (concept list)
    (delete-from-unexpanded label concept)))

;;;
;;;
;;;

(timed-defmethod delete-from-expanded ((label node-label) (concept concept))
  #+:use-membership-tables
  (remhash (list concept label) *expanded-table*)
    
  (let ((slot (get-slot-for concept expanded)))
    #+:use-avl-trees-for-labels
    (delete-from-avl-tree concept (slot-value label slot) :key #'id)
    #-:use-avl-trees-for-labels
    (setf (slot-value label slot)
          (delete concept (slot-value label slot)))))

(timed-defmethod delete-from-expanded ((label node-label) (list list))
  (dolist (concept list)
    (delete-from-expanded label concept)))

;;;
;;;
;;;

(timed-defmethod put-from-expanded-to-unexpanded ((concept concept) (label node-label))
  (delete-from-expanded label concept)
  (add-to-unexpanded label concept))

(timed-defmethod put-from-unexpanded-to-expanded ((concept concept) (label node-label))
  (delete-from-unexpanded label concept)
  (add-to-expanded label concept))

