;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;


(defrule simple-at-least-expansion (dl-with-number-restrictions abox)
         
  (multiple-value-bind (at-least-concept node) 
      (select-at-least-concept abox *strategy* language)
    
    (cond ((not node)

           +insert-negative-code+ )
          
          (t 
                 
           (let ((role (role at-least-concept))
                 (n (n at-least-concept)))
             
             (when *reuse-nodes-p*
               (break "To be implemented"))
                    
             (announce "EXPANDING ~A : CREATING NEW NODE" node)

             (register-as-expanded at-least-concept 
                                   :comment 'expand-at-least-concept-no-preconditions
                                   :node node)

             (loop-over-node-unexpanded-at-least-concepts (implied-at-least node)
               (when (and (implies-p (role at-least-concept) (role implied-at-least))
                          (<= (n implied-at-least) n)
                          (not (eq implied-at-least at-least-concept)))
                 
                 (register-as-expanded implied-at-least 
                                       :comment 'register-implied-at-least-concept
                                       :node node
                                       :depends-on (list (list node at-least-concept)))))
             
             (let ((new-node 
                    (create-anonymous-node abox 
                                           :depends-on (list (list node at-least-concept)))))
               
               (relate node new-node role 
                       :old-p nil 
                       :multiplicity n
                       :depends-on (list (list node at-least-concept)))
               
               (perform (compute-new-some-successor-label :new-node new-node 
                                                          :node node
                                                          :role role
                                                          :concept at-least-concept))
                 
               +insert-positive-code+))))))


(defrule simple-at-most-merging (dl-with-number-restrictions abox)
         
  (multiple-value-bind (bad-at-most-concept node edges m) 
      (select-violated-at-most-concept abox *strategy* language)

    (declare (ignorable m))
    
    (cond ((not node) 
           
           +insert-negative-code+)
          
          (t 
             
           (let* ((new-choice-point (get-new-choice-point))
                  
                  (collected-dependencies nil)
                  
                  (memorized-action (get-current-action))
                  
                  (sat nil)

                  (no (* 2 (n-over-k (length edges) 2))) ; (1 3) -> ((1 3) (3 1)) probieren! 

                  (count 0)
                  
                  (depends-on (cons (list node bad-at-most-concept)
                                    edges)))

             (loop-over-subsets-of-cardinality (partition edges 2) 

               (dolist (partition (list partition (reverse partition)))
                   
                 (incf count) 
                   
                 (let* ((last-p (= count no)))
                   
                   (announce  "Trying partition ~A" partition)
                   (when last-p 
                     (announce "This is the last partition!"))
                   
                   (multiple-value-bind (merged-p not-mergeable-deps)
                       
                       (try-to-merge node 
                                     partition 
                                     :new-choice-point new-choice-point
                                     :depends-on depends-on
                                     )

                     (if merged-p 
                         
                         (progn 

                           (announce "Found mergable partition ~A" partition)
                           
                           (multiple-value-bind (sub-sat deps)
                
                               +insert-positive-code+
                             
                             (announce "Back on level ~A!" level)

                             (when *compute-all-completions-p*
                               (let ((*use-unsat-cache-p* nil))
                                 (rollback-to abox memorized-action)))
                    
                             (setf sat (or sat sub-sat))

                             (when (and (not sat) 
                                        (not *compute-all-completions-p*))
                       
                               (push-all-to deps collected-dependencies))

                             ;;; letzte Parition? -> in jedem Fall zurückkehren! 
                           
                             (when last-p                                                       
                               (return-from prover 
                                 (if sat
                                     t
                                   (if *compute-all-completions-p* 
                                       nil
                                     (values nil 
                                             (sort-choice-points
                                              (remove new-choice-point collected-dependencies)))))))
                           
                             ;;; nicht-letzte Partition
                 
                             (when (not last-p)
                               (when (and sat (not *compute-all-completions-p*))
                                 (return-from prover t))
                   
                               (when (not sat)
                                 (when (not (member new-choice-point deps)) 
                       
                                   ;;;
                                   ;;; Clash Reason liegt nicht an der vorgenommen Partitionierung! 
                                   ;;;
                       
                                   (return-from prover
                                     (values nil
                                             (sort-choice-points
                                              (remove new-choice-point collected-dependencies)))))
                     
                                 (when (member new-choice-point deps) 
                       
                                   ;;;
                                   ;;; Clash Reason liegt an der Partition! 
                                   ;;; Aufräumen und Weitermachen, 
                                   ;;; nächste Partition aus der Schleife probieren 
                                   ;;; 
                                 
                                   (when *debug-p* 
                                     (format t "~%*** BEFORE ROLLBACK:~%")
                                     (describe-object abox t)
                                     (loop-over-abox-nodes (node abox)
                                       (describe-object node t)))
                                 
                                   #+:use-dependency-directed-backtracking
                                   (break "DEPENDENCY DIRECTED BACKTRACKING NOT FULLY UNDERSTOOD YET!")
                                 
                                   #-:use-dependency-directed-backtracking
                        
                                   (progn
                                    
                                     ;;;
                                     ;;; einfaches BACKJUMPING
                                     ;;; chronologisches Abräumen
                                     ;;;
                                   
                                     (rollback-to abox memorized-action node))
                                 
                                   (when *debug-p* 
                                     (format t "~%*** AFTER ROLLBACK:~%")
                                     (describe-object abox t)
                                     (loop-over-abox-nodes (node abox)
                                       (describe-object node t))))))))
                   
                       ;;; nodes waren nicht mergable!

                       (progn
                         
                         (when (and (not sat) 
                                    (not *compute-all-completions-p*))
                       
                           (push-all-to not-mergeable-deps collected-dependencies))

                         (when last-p                  
                       
                           (return-from prover 
                             (if sat
                                 t
                               (if *compute-all-completions-p* 
                                   nil
                             
                                 (progn 
                                   (unless collected-dependencies
                                     (break))
                                   (values nil
                                           (sort-choice-points collected-dependencies)))))))))))))

             ;;;
             ;;; diese Return wird ausgeführt, wenn cardinality = 0 war! 
             ;;; 
             
             (return-from prover

               (values nil 
                       (sort-choice-points 
                        (reduce #'append
                                (cons (get-choice-points bad-at-most-concept
                                                         :node node)
                                      (mapcar #'get-choice-points edges)))))))))))


