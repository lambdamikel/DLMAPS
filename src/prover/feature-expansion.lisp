;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(defrule feature-expansion (dl-with-features abox)

  (multiple-value-bind (ae-concept relevant-ae-concepts node) 
      (select-attribute-exists-concepts abox *strategy* language)
    
    (cond ((not node)
           
           +insert-negative-code+)
          
          (t 

           (let* ((feature (role ae-concept))
                  (succs (apply #'append
                                (mapcar #'(lambda (f) 
                                            (get-role-successors node f))
                                        (cons feature
                                              (remove-if-not #'feature-p 
                                                             (all-superroles feature))))))
                  
                  ;;; s. Beispiel im ALCHF-RPLUS-Prover f. Begruendung!
                  ;;; eigentlich sollte das unnoetig sein -> look-for-somes in
                  ;;; in deterministic-expansion.lisp? look-for-features? 

                  (succ nil))

             ;;; (format t "SUccs: ~A" succs)
               
             (when (cdr succs)
               (error "Found more than one successor of ~A for feature ~A!" node feature))

             (if succs

                 (let ((succ (first succs)))
                   (dolist (ae-concept relevant-ae-concepts)
                     (register-as-expanded 
                      ae-concept 
                      :comment 'expand-unexpanded-attribute-exists-concept-due-to-present-successor
                      :node node
                      :depends-on (created-by succ)))


                   (perform (compute-new-feature-successor-label
                             :succ succ 
                             :node node
                             :ae-concept ae-concept
                             :relevant-ae-concepts relevant-ae-concepts)))

               (progn
                 
                 (dolist (ae-concept relevant-ae-concepts)
                   (register-as-expanded ae-concept 
                                         :comment 'expand-unexpanded-attribute-exists-concept-no-preconditions
                                         :node node))
                 
                 (unless succs
                   (announce "Expanding ~A : ~A, others: ~A - creating new node"
                             node ae-concept relevant-ae-concepts))

                   
                 (setf succ (create-anonymous-node 
                             abox 
                             :depends-on (list (list node ae-concept))))
                 
                 (let ((and-feature
                        (create-and-role (mapcar #'role relevant-ae-concepts)
                                         :feature-p t)))
                   
                   (relate node succ and-feature
                           :old-p nil 
                           :depends-on (list (list node ae-concept)))

                   (perform (compute-new-feature-successor-label 
                             :succ succ 
                             :node node
                             :ae-concept ae-concept
                             :relevant-ae-concepts relevant-ae-concepts)))))
                   
             +insert-positive-code+)))))

;;;
;;;
;;;

(defrule compute-new-feature-successor-label (dl abox 
                                                 :args (succ node ae-concept relevant-ae-concepts))

  (dolist (ae-concept2 relevant-ae-concepts)

    (let ((qualification (qualification ae-concept2))
          (deps (list ;(list node ae-concept)
                 (list node ae-concept2))))
      
      (unless (on-tableau-p succ qualification)
        (let ((added (register-as-unexpanded qualification
                                             :node succ
                                             :depends-on deps)))
          (check-for-clash succ added)))

      
      (let ((role-range (role-range (role ae-concept2))))
        (when role-range 
          (unless (on-tableau-p succ role-range)
            (let ((added
                   (register-as-unexpanded role-range
                                           :node succ
                                           :depends-on deps)))
              
              (check-for-clash succ added)))))))

  (add-meta-constraints succ))



(defrule compute-new-feature-successor-label (dl-with-combined-some-all-rule abox 
                                                                             :args (succ node ae-concept relevant-ae-concepts))

  (dolist (ae-concept2 relevant-ae-concepts)

    (let ((qualification (qualification ae-concept2))
          (deps (list ;(list node ae-concept)
                 (list node ae-concept2))))
    
      (loop-over-node-all-concepts (all-concept node)
      
        (when (implies-p (role ae-concept2) (role all-concept))
          
          (let ((qualification (qualification all-concept))
                (deps (cons (list node all-concept)
                            deps)))

            (unless (on-tableau-p succ qualification)
              (let ((added
                     (register-as-unexpanded qualification
                                             :node succ
                                             :depends-on deps)))
                (check-for-clash succ added)))
      
            (when (transitive-p all-concept)
              (unless (on-tableau-p succ all-concept)
                (let ((added
                       (register-as-unexpanded all-concept 
                                               :node succ
                                               :comment 'found-applicable-transitive-all
                                               :depends-on deps)))
              
                  (check-for-clash succ added)))))))
  
      (unless (on-tableau-p succ qualification)
        (let ((added
               (register-as-unexpanded qualification
                                       :node succ
                                       :depends-on deps)))
          
          (check-for-clash succ added)))

                     
      (let ((role-range (role-range (role ae-concept2))))
        (when role-range 
          (unless (on-tableau-p succ role-range)
            (let ((added 
                   (register-as-unexpanded role-range
                                           :node succ
                                           :depends-on deps)))
              
              (check-for-clash succ added)))))))

  (add-meta-constraints succ)
               
  (register-label-is-stable abox succ))
