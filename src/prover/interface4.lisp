;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)
   
;;;
;;; Interface: Reduktion aller Inferenzen auf ABox-Sat! 
;;; Achtung: Consistent-p ist von descriptions.lisp geerbt
;;; 


(defmethod consistent-p ((concept and-concept) &rest args
                         &key 
                         subsumption-test-p
                         (use-cached-models-p *use-cached-models-p*)
                         &allow-other-keys)
    
  (if (and use-cached-models-p
           (register-concept-mm-query)
           (models-of-concepts-mergeable-p   
            (get-language concept)
            (arguments concept)))
      
      (progn 
        (register-concept-mm-hit) 
        (=> subsumption-test-p 
            (register-subsumption-mm-hit))
        (register-concept-is-satisfiable concept)
        t)

    (progn 

      (apply #'call-next-method concept args))))


;;;
;;;
;;;

#+:use-membership-tables
(defconstant +uex-table+
  (make-weak-hash-table :size 10000 :rehash-size 10000 :test #'equalp))

#+:use-membership-tables
(defconstant +ex-table+
  (make-weak-hash-table :size 10000 :rehash-size 10000 :test #'equalp))


(defmethod consistent-p :before ((concept concept) &rest args)
  (declare (ignorable args))
  (register-true-concept-query))

(defmethod consistent-p ((concept concept) &rest args 
                         &key
                         (maintain-prover-state-p *maintain-prover-state-p* mps-specified-p)
                         subsumption-test-p
                         type
                         tbox
                         &allow-other-keys)

  (let* ((tbox 
          (or tbox *cur-tbox*))
         (type  (or type
                    (get-abox-type-for-tbox (tbox concept))))
         (name 'temp-abox)
         (*dont-invalidate-store-p* t)
         (*create-membership-tables-p* nil)
         (*store-ind-models-p* nil))
    

    (register-abox-consistency-test)

    (when subsumption-test-p 
      (register-abox-subsumption-test))
    
    (with-abox* (name
                 :tbox tbox
                 :type type
                 :language (get-language concept)
                 :delete-if-exists-p t)

      #+:use-membership-tables
      (progn
        (clrhash +uex-table+)
        (clrhash +ex-table+)
        (setf (unexpanded-table *cur-abox*) +uex-table+)
        (setf (expanded-table *cur-abox*) +ex-table+))

      (let ((concept-sat-node (create-abox-node *cur-abox* 'temp :old-p nil))) 

        ;; :old-p nil ist wichtig! sonst bringt das die Strategie durcheinander!!!

        (node-instance concept-sat-node concept)
                  
        (let ((res (apply #'abox-sat-p *cur-abox* 
                          :maintain-prover-state-p 
                          (if mps-specified-p 
                              maintain-prover-state-p 
                            t)
                          ;;; ist doch eine Einmal-ABox! 
                          ;;; warum soll ich ein Rollback machen? 
                          ;;; ist teuer!
                          ;;;
                          ;;; doch es gibt doch einen Grund: 
                          ;;; die Unsat-Caches werden dann gefuellt!!!
                          args)))

          (delete-abox *cur-abox*)
                    
          (return-from consistent-p res))))))

;;;
;;;
;;;

(defmacro with-new-temp-abox-name (&body body)
  `(let ((*temp-abox-name* (1+ *temp-abox-name*)))
     ,@body))

;;;
;;;
;;;

(defmethod inconsistent-p ((concept concept) &rest args)
  (not (apply #'consistent-p concept args)))

(defmethod tautological-p ((concept concept) &rest args)
  (apply #'inconsistent-p (get-negated-concept concept) args))

;;;
;;;
;;;

(defmethod sat-p ((concept concept) &rest args &key recompute-p &allow-other-keys)

  (when recompute-p 
    (reset-sat-status concept))

  (register-concept-query)
  
  (apply #'consistent-p concept args))

(defmethod sat-p (concept &rest args &key tbox &allow-other-keys)
  (let ((*cur-store* (if tbox
                         (concept-store tbox)
                       (concept-store *cur-tbox*))))
    (apply #'sat-p (with-protected-concept-store 
                     (parse-concept concept))
           args)))

(defmethod sat-p ((abox abox) &rest args &key &allow-other-keys)
  (apply #'abox-sat-p abox args))

;;;
;;; Implies-p ist von descriptions2.lisp geerbt!
;;;



(defmethod implies-p ((concept1 concept) (concept2 concept) &rest args
                      &key
                      (use-cached-models-p *use-cached-models-p*)
                      (cache-models-p *cache-models-p*)
                      (use-told-subsumers-p *use-told-subsumers-p*)
                      &allow-other-keys)

  
  (register-subsumption-query)
           
  (cond

   ;;; die Klauseln sind beim Klassifizieren nicht wirksam, 
   ;;; da compute-node-children / parents schon optimiert sind 

   ((and use-told-subsumers-p (member concept2 (told-subsumers concept1)))
    (register-subsumption-ts-hit)
    t)
  
   ((and use-told-subsumers-p
         (member (get-negated-concept concept1) (told-subsumers (get-negated-concept concept2))))
    (register-subsumption-ts-hit)
    t)
   
   ((and use-told-subsumers-p
         (member concept2 (told-subsumers (get-negated-concept concept1))))
    (register-subsumption-ts-hit)
    nil)

   ((and use-told-subsumers-p 
         (member concept1 (told-subsumers (get-negated-concept concept2))))
    (register-subsumption-ts-hit)
    nil)
    
   (t
             
    (let* ((concept2 
            (with-disabled-concept-store
              (with-protected-concept-store 
                (make-not-concept concept2))))
          
           (and-concept  
            (with-disabled-concept-store
              (with-protected-concept-store
                (make-and-concept 
                 (list concept2 concept1))))))
           
      (when use-cached-models-p 
      
        (when (already-known-to-be-satisfiable-p and-concept)
          (return-from implies-p nil))
        
        (when (already-known-to-be-inconsistent-p and-concept)
          (return-from implies-p t))
        
        (when (and (or (not (cached-models concept2))
                       ;(not (instance-retrieval-model concept2))
                       )
                   cache-models-p)

          (unless (apply #'sat-p concept2 
                         ;:store-instance-retrieval-model-p t
                         :cache-models-p t :recompute-p t args)
            (return-from implies-p t)))
        
        (when (and (or (not (cached-models concept1))
                       ;(not (instance-retrieval-model concept1))
                       )
                   cache-models-p)

          (unless (apply #'sat-p concept1
                         ;:store-instance-retrieval-model-p t
                         :cache-models-p t :recompute-p t args)
            (return-from implies-p t)))

        (unless (cached-models concept1)
          (error "No cached models for ~A?!" concept1))
        
        ;; (unless (instance-retrieval-model concept1)
        ;;  (error "No instance retrieval model for ~A?!" concept1))
        
        (unless (cached-models concept2)
          (error "No cached models for ~A?!" concept2))
        
        ;; (unless (instance-retrieval-model concept2)
        ;;  (error "No instance retrieval model for ~A?!" concept2)))


        #| (when (models-surely-not-mergeable-p +dl+
                                           (instance-retrieval-model concept2)
                                           (instance-retrieval-model concept1))
        (return-from implies-p t) |# 

        )

      (register-true-subsumption-query)

      (not
       (apply #'sat-p and-concept
              :subsumption-test-p t 
              args))))))

(defmethod implies-p ((b concept) (a top-concept) &rest args)
  t)

(defmethod implies-p ((b top-concept) (a concept) &rest args)
  nil)

(defmethod implies-p ((b concept) (a bottom-concept) &rest args)
  nil)

(defmethod implies-p ((b bottom-concept) (a concept) &rest args)
  t)

;;;
;;;
;;;

(defmethod subsumes-p ((concept1 concept) (concept2 concept) &rest args)
  (apply #'implies-p concept2 concept1 args))

(defmethod subsumes-p (concept1 concept2 &rest args &key tbox &allow-other-keys)
  (let ((*cur-store* (if tbox
                         (concept-store tbox)
                       (concept-store *cur-tbox*))))
    (apply #'subsumes-p
           (with-protected-concept-store (parse-concept concept1))
           (with-protected-concept-store (parse-concept concept2))
           args)))
;;;
;;;
;;;

(defmethod equivalent-p ((concept1 concept) (concept2 concept) &rest args)
  (and (apply #'implies-p concept2 concept1 args)
       (apply #'implies-p concept1 concept2 args)))

(defmethod equivalent-p (concept1 concept2 &rest args &key tbox &allow-other-keys)
  (let ((*cur-store* (if tbox
                         (concept-store tbox)
                       (concept-store *cur-tbox*))))
    
    (apply #'equivalent-p
           (with-protected-concept-store (parse-concept concept1))
           (with-protected-concept-store (parse-concept concept2))
           args)))

;;;
;;; Eigene Syntax: 
;;; 

(defmacro sat? (expr &rest args)
  `(sat-p (quote ,expr) ,@args))

(defmacro subsumes? (concept1 concept2 &rest args)
  `(subsumes-p ',concept1 ',concept2 ,@args))

(defmacro equivalent? (concept1 concept2 &rest args)
  `(equivalent-p ',concept1 ',concept2 ,@args))
