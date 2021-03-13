;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(defrule some-expansion (dl-with-somes abox)

  (multiple-value-bind (some-concept node) 
      (select-some-concept abox *strategy* language)

    (cond ((not node)

           +insert-negative-code+ )
          
          (t 
           
           (let* ((role (role some-concept))
                  (new-node nil))

             (register-as-expanded some-concept 
                                   :comment 'expand-unexpanded-some-concept-no-preconditions
                                   :node node)
                 
             (when *reuse-nodes-p* 

               (let* ((succs (get-role-successors node role)))


                 (announce "Performing Non-deterministic SOME expansion of ~A : ~A" node some-concept)
                 (announce "Found candidate successors: ~A" succs)

                 (let ((new-choice-point (get-new-choice-point))
                       (collected-dependencies nil)
                       (count 0)
                       (n (length succs))
                       (sat nil)
                       (memorized-action (get-current-action)))

                   (dolist (succ succs)

                     (incf count) 
                       
                     (announce "Performing Non-deterministic SOME expansion of ~A : ~A" node some-concept)
                     (announce "Trying ~Ath (of ~A) successor ~A. " count n succ)


                     (perform (compute-new-some-successor-label :new-node succ
                                                                :new-choice-point new-choice-point
                                                                :node node
                                                                :role role
                                                                :concept some-concept))                       
                       
                     (multiple-value-bind (sub-sat deps)

                         +insert-positive-code+ 
  
                       ;;;
                       ;;; vollständiges chronologisches backtracking
                       ;;; bei  *compute-all-completions-p* erforderlich! 
                       ;;;
                         
                       (when *compute-all-completions-p*
                         (let ((*use-unsat-cache-p* nil))
                           (rollback-to abox memorized-action)))
                                      
                       (setf sat (or sat sub-sat))
                                      
                       ;;; 
                       ;;; dependencies machen keinen sinn
                       ;;; bei *compute-all-completions-p* 
                       ;;;
                         
                       (when (and (not sat) 
                                  (not *compute-all-completions-p*))
                           
                         (setf collected-dependencies
                               (append deps collected-dependencies)))
                         
                       (when (and sat (not *compute-all-completions-p*))
                         (return-from prover t))
                        
                       (when (not sat)

                         (when (not (member new-choice-point deps))
                                          
                           ;;;
                           ;;; Clash hat anderen Grund als qual!
                           ;;; Aufraeumen nicht notwendig, da der Clash-Beseitiger
                           ;;; das durchfuehrt! 
                           ;;;
                             
                           (return-from prover
                             (values nil (remove new-choice-point collected-dependencies))))
                           
                         (when (member new-choice-point deps)
                             
                           #+:use-dependency-directed-backtracking
                           (progn                                                 
                             (break "DEPENDENCY DIRECTED BACKTRACKING NOT FULLY UNDERSTOOD YET!"))
                                            
                           #-:use-dependency-directed-backtracking
                           (progn 
                               
                             ;;;
                             ;;; einfaches BACKJUMPING
                             ;;; chronologisches Abräumen
                             ;;;
                               
                             (rollback-to abox memorized-action node)))))))))


             ;;;
             ;;; deterministischer Teil der Expansion
             ;;; 

             
             (announce "~%Performing deterministic SOME expansion of ~A : ~A" node some-concept)
             
             (setf new-node
                   (create-anonymous-node 
                    abox 
                    :depends-on (list (list node some-concept))))
                 
             (relate node new-node role 
                     :old-p nil 
                     :depends-on (list (list node some-concept)))
               
             (perform (compute-new-some-successor-label :new-node new-node 
                                                        :node node
                                                        :role role
                                                        :concept some-concept))
             +insert-positive-code+ )))))

;;;
;;;
;;;

(defrule compute-new-some-successor-label (dl abox :args (new-choice-point new-node node role concept))

  (let ((Added
         (register-as-unexpanded (qualification concept)
                                 :new-choice-point new-choice-point
                                 :node new-node
                                 :depends-on (list (list node concept)))))
  
    (check-for-clash new-node added))

  (when (role-range role)
    (unless (on-tableau-p new-node (role-range role))
      (let ((added
             (register-as-unexpanded (role-range role)
                                     :new-choice-point new-choice-point
                                     :node new-node
                                     :depends-on (list (list node concept)))))
        (check-for-clash new-node added))))

  (add-meta-constraints new-node))




(defrule compute-new-some-successor-label (dl-with-combined-some-all-rule abox :args (new-choice-point new-node node role concept))

  (let ((added
         (register-as-unexpanded (qualification concept)
                                 :new-choice-point new-choice-point
                                 :node new-node
                                 :depends-on (list (list node concept)))))
  
    (check-for-clash new-node added))

  (when (role-range role)
    (unless (on-tableau-p new-node (role-range role))
      (let ((added
             (register-as-unexpanded (role-range role)
                                     :new-choice-point new-choice-point
                                     :node new-node
                                     :depends-on (list (list node concept)))))
        (check-for-clash new-node added))))


  (loop-over-node-all-concepts (all node)
    (when (implies-p role (role all))
      (unless (on-tableau-p new-node (qualification all))
        (let ((added
               (register-as-unexpanded (qualification all)
                                       :new-choice-point new-choice-point
                                       :node new-node
                                       :depends-on (list (list node concept)
                                                         (list node all)))))
        
          (check-for-clash new-node added)))))
    
  (add-meta-constraints new-node)

  (unless (has-unexpanded-at-most-concepts-p node)
    (register-label-is-stable abox new-node)))


(defrule compute-new-some-successor-label (alchf-rplus abox :args (new-choice-point new-node node role concept))

  (let ((added
         (register-as-unexpanded (qualification concept)
                                 :new-choice-point new-choice-point
                                 :node new-node
                                 :depends-on (list (list node concept)))))
  
    (check-for-clash new-node added)

    (when (role-range role)
      (unless (on-tableau-p new-node (role-range role))
        (let ((added
               (register-as-unexpanded (role-range role)
                                       :new-choice-point new-choice-point
                                       :node new-node
                                       :depends-on (list (list node concept)))))
          (check-for-clash new-node added))))

    (loop-over-node-all-concepts (all node)
      (when (implies-p role (role all))
        (unless (on-tableau-p new-node (qualification all))
          (let ((added
                 (register-as-unexpanded (qualification all)
                                         :new-choice-point new-choice-point
                                         :node new-node
                                         :depends-on (list (list node concept)
                                                           (list node all)))))
        
            (check-for-clash new-node added)))
    
        (when (transitive-p all)
          (unless (on-tableau-p new-node all)

            (announce "Propagating transitive ALL ~A to ~A" all new-node)

            (let ((added
                   (register-as-unexpanded all 
                                           :new-choice-point new-choice-point
                                           :node new-node
                                           :comment 'found-applicable-transitive-all
                                           :depends-on (list (list node concept)
                                                             (list node all)))))
                                    
              (check-for-clash new-node added)))))))

  (add-meta-constraints new-node)

  (unless (has-unexpanded-at-most-concepts-p node)
    (register-label-is-stable abox new-node)))

