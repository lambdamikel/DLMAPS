;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;
;;;

(defmethod node-instance ((ind abox-node) (concept concept) &rest args)
  (declare (ignorable args))
  
  (unless *dont-invalidate-store-p*
    (note-abox-has-changed (in-graph ind))
    (reset-sat-status ind))

  (push concept (slot-value ind 'told-concepts)))

(defmethod node-forget ((ind abox-node) (concept concept) &rest args)
  (declare (ignorable args))

  (unless *dont-invalidate-store-p*
    (note-abox-has-changed (in-graph ind))
    (reset-sat-status ind))

  (setf (slot-value ind 'told-concepts)
        (delete concept (slot-value ind 'told-concepts))))

;;;
;;;
;;;

(defmethod node-instance ((ind abox-node) concept &rest args)
  (apply #'node-instance ind 
         (with-protected-concept-store
           (parse-concept (or concept 'top)))
         args))

(defmethod node-forget ((ind abox-node) concept &rest args)
  (apply #'node-forget ind
         (with-protected-concept-store
           (parse-concept (or concept 'top)))
         args))

;;;
;;;
;;;
  
(defmethod node-instance ((ind symbol) concept &rest args)
  (apply #'node-instance (or (apply #'find-node *cur-abox* ind :error-p nil args)
                             (apply #'create-abox-node *cur-abox* ind args))
         concept
         args))

(defmethod node-forget ((ind symbol) concept &rest args)
  (apply #'node-forget (apply #'find-node *cur-abox* ind :error-p t args)
         concept
         args))

;;;
;;;
;;;

(defmethod prepare :around ((abox abox) (language dl))
  (unless (prepared-p abox)
    (call-next-method)))
    

(defmethod prepare ((abox abox) (language dl))
  
  (with-slots (prepared-p 
               satisfiable
               ready-for-retrieval 
               action-timestamp-counter 
               current-action 
               choice-point-counter 
               cur-clash-node 
               language
               ) abox

    (setf satisfiable :not-tested
          language nil
          ready-for-retrieval nil
            
          action-timestamp-counter 0
          current-action nil
          choice-point-counter 0
          cur-clash-node nil)

    #+:use-membership-tables
    (progn
      (clrhash (unexpanded-table abox))
      (clrhash (expanded-table abox)))


    (loop-over-abox-nodes (node abox)
      (dolist (slot '(initial-concept
                      choice-points
                      really-satisfiable-p 
                      cache-satisfiable-p
                      stable-p))
        (setf (slot-value node slot) nil))
      (reset-label (description node))
      (reset-sat-status (in-graph node)))

    
    (loop-over-abox-edges (edge abox)
      (reset-label (description edge))
      (with-protected-concept-store
       (parse-concept `(some ,(role edge) top))))

    ;;; Sprache muss hier schon berechnet werden, 
    ;;; wegen register-as-unexpadned -> adjust-blocking-dependencies -> language! 

    (setf language (get-language 
                    (tbox abox)))

    (register-told-concepts abox +dl+)

    (prepare-index-structures abox)


    (setf prepared-p t)

    abox))

;;;
;;;
;;;

(defmethod prepare-index-structures ((abox abox))
  t)

(defmethod prepare-index-structures ((abox abox1))
  ;;; initial werden nun alle Indexstrukturen vorbereitet, 
  ;;; sie muessen ja spaeter nicht genutzt werden (beim Beweis!) 

  (let ((*maintain-old-nodes-p* nil)
        (*maintain-leaf-nodes-p* nil)
        (*maintain-deactivated-nodes-p* nil)
        (*maintain-active-nodes-p* nil)
        (*maintain-cache-sat-nodes-p* nil)
        (*maintain-blocked-nodes-p* nil))

    (with-slots (old-nodes
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
                 nodes-not-at-least-expanded

                 nodes-not-or-expanded1
                 nodes-not-some-expanded1
                 nodes-not-feature-expanded1 
                 nodes-not-at-least-expanded1) abox

    (setf nodes-not-or-expanded1 (create-heap #'older-p)
          nodes-not-some-expanded1 (create-heap #'older-p)
          nodes-not-feature-expanded1 (create-heap #'older-p)
          nodes-not-at-least-expanded1  (create-heap #'older-p))

    (setf nodes-not-and-expanded (create-heap #'left-of-p)
          nodes-not-atom-expanded  (create-heap #'left-of-p)
          nodes-not-or-expanded (create-heap #'left-of-p)
          nodes-not-some-expanded (create-heap #'left-of-p)
          nodes-not-feature-expanded (create-heap #'left-of-p)
          nodes-not-at-least-expanded (create-heap #'left-of-p))
    
    (loop-over-blocked-nodes (node abox)
                               (deactivate-node abox node))

      (setf active-nodes (create-heap #'left-of-p))
      
      (dolist (node (get-active-nodes abox))
        (heap-insert active-nodes node))

      (setf old-nodes (get-old-nodes abox)
            cache-sat-nodes (get-cache-sat-nodes abox)
            deactivated-nodes (get-deactivated-nodes abox)
            leaf-nodes (get-leaf-nodes abox)
            blocked-nodes (get-blocked-nodes abox))

      (loop-over-abox-nodes (node abox)

        (when (has-unexpanded-or-concepts-p node)
          (heap-insert nodes-not-or-expanded node)
          (heap-insert nodes-not-or-expanded1 node))

        (when (has-unexpanded-attribute-exists-concepts-p node)
          (heap-insert nodes-not-feature-expanded node)
          (heap-insert nodes-not-feature-expanded1 node))
      
        (when (has-unexpanded-some-concepts-p node)
          (heap-insert nodes-not-some-expanded node)
          (heap-insert nodes-not-some-expanded1 node))

        (when (has-unexpanded-at-least-concepts-p node)
          (heap-insert nodes-not-at-least-expanded node)
          (heap-insert nodes-not-at-least-expanded1 node))))))
      
;;;
;;;
;;;

(defmethod register-told-concepts :around ((abox abox1) (language dl))
  (let ((*maintain-unexpanded-atomic-concepts-heap-p* nil)
        (*maintain-unexpanded-atomic-concepts1-heap-p* nil)
        (*maintain-unexpanded-and-concepts-heap-p* nil)
        (*maintain-unexpanded-and-concepts-heap1-p* nil)
        (*maintain-unexpanded-or-concepts-heap-p* nil)
        (*maintain-unexpanded-or-concepts1-heap-p* nil)
        (*maintain-unexpanded-some-concepts-heap-p* nil)
        (*maintain-unexpanded-some-concepts1-heap-p* nil)
        (*maintain-unexpanded-at-least-concepts-heap-p* nil)
        (*maintain-unexpanded-at-least-concepts1-heap-p* nil)
        (*maintain-unexpanded-attribute-exists-concepts-heap-p* nil)
        (*maintain-unexpanded-attribute-exists-concepts1-heap-p* nil))

    (call-next-method)))

(defmethod register-told-concepts ((abox abox) (language dl))

  (loop-over-abox-nodes (node abox)
    (dolist (constraint *meta-constraints*)
      (pushnew constraint 
               (slot-value node 'told-concepts))))


  (loop-over-abox-edges (edge abox)
    (let* ((role (role edge))
           (domain (role-domain role))
           (range (role-range role))
           (from (from edge))
           (to (to edge)))

      (when domain
        (pushnew domain (slot-value from 'told-concepts)))

      (when range
        (pushnew range (slot-value to 'told-concepts))))

    (register-as-unexpanded edge 
                            :comment 'initialize-present-edge-for-new-abox  
                            ;; choice points nur für 
                            ;; nicht-disjunktive vergeben; 
                            :error-p nil
                            :new-choice-point 
                            (unless (underspecified-p edge) 0)))

  (let ((*adjust-blocking-dependencies-p* nil))
    ;;; Blockierungen beibehalten!
    
    (loop-over-abox-nodes (node abox)
    (dolist (concept (told-concepts node))
      (if (on-tableau-p node (get-negated-concept concept))
          (return-from register-told-concepts nil)
        (unless (on-tableau-p node concept)
          (register-as-unexpanded concept
                                  :node node 
                                  :new-choice-point 0)))))))



(defmethod register-told-concepts-as-expanded ((abox abox) (language dl))

  (let ((*reflexive-checks-p* nil))

    ;;; keine History-Objekte 
    (loop-over-abox-edges (edge abox)
      (register-as-unexpanded edge 
                            :comment 'initialize-present-edge-for-new-abox  
                            ;; choice points nur für 
                            ;; nicht-disjunktive vergeben; 
                            :error-p nil
                            :new-choice-point 
                            (unless (underspecified-p edge) 0)))

    (let ((*adjust-blocking-dependencies-p* nil))
      ;;; Blockierungen beibehalten!
    
      (loop-over-abox-nodes (node abox)
        (dolist (concept (told-concepts node))
          (register-as-expanded concept
                                :node node))))))

;;;
;;;
;;;

(defmethod abox-sat-p :around ((abox abox) &rest args &key recompute-p &allow-other-keys)                 
  (if (or (eq (slot-value abox 'satisfiable) :not-tested)
          recompute-p)
      (setf (slot-value abox 'satisfiable)
            (progn 
              (prepare abox +dl+)
              (call-next-method)))
    (slot-value abox 'satisfiable)))


(defmethod abox-sat-p :before ((abox abox) &rest args)
  (prepare (tbox abox) +dl+))

(defmethod abox-sat-p ((abox abox) &rest args &key (language (get-language abox)) &allow-other-keys)
  (apply #'prover-init
         'abox-sat
         (make-language language)
         abox
         args))    

(defmethod abox-sat-p (abox &rest args)
  (apply #'abox-sat-p 
         (find-abox abox :error-p t)
         args))

;;;
;;; Eigene Macros
;;;

(defmacro abox-sat? (abox &rest args)
  `(abox-sat-p (quote ,abox) ,@args))

(defmacro abox-sat*? (abox &rest args)
  `(abox-sat-p ,abox ,@args))

;;;
;;;
;;;

(defmacro abox-realized*? (abox &rest args)
  `(abox-realized-p ,abox ,@args))
 
;;;
;;;
;;;

(defmacro ins (name def &rest args)
  `(node-instance ',name ',def ,@args))

(defmacro for (name def &rest args)
  `(node-forget ',name ',def ,@args))

(defmacro rel (a b role &rest args)
  `(relate ',a ',b ',role ,@args :error-p nil))

(defmacro unrel (a b role &rest args)
  `(unrelate ',a ',b ',role ,@args))

(defmacro del  (name &rest args)
  `(delete-node *cur-abox* ',name ,@args))
