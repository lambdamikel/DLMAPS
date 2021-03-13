;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(defrule abox-enumeration (dl abox)
  (let* ((sat nil)    
         (collected-dependencies nil)
         (count 0)

         (underspecified-edges (edge-gen abox))

         (underspecified-edges-and-inverses
          (remove nil (append underspecified-edges 
                              (mapcar #'inverse-edge underspecified-edges ))))

         (clash-p nil)
         (successfully-repaired-p nil)
         (end-loop-p nil)
         (choice-points nil)

         (memorized-action (get-current-action)))
         
    (if (not underspecified-edges)

        +insert-body-code+
           
      (let ((abox (first-completion abox)))

        (unless abox
          (error "No completion! How do I handle this?"))
               
        (loop
                
         (incf count)
                
         (announce "Performing ABOX ENUMERATION OF ~A ON LEVEL ~A, ALTERNATIVE NO. ~A.~%DISJUNCTIVE EDGES: ~A~%CHOICE-POINTS OF DISJUNCTIVE EDGES: ~A~%" 
                   abox level count underspecified-edges
                   choice-points)

         (when *debug-p* 
           (format t  "~%~%Underspecified Edges:~%")
           (dolist (edge underspecified-edges)
             (describe-object edge t)
             (terpri))
                  
           (format t  "~%~%Edges and Inverses:~%")
           (dolist (edge underspecified-edges)
             (describe-object edge t)
             (terpri)
             (when (inverse-edge edge)
               (describe-object (inverse-edge edge) t)
               (terpri) (Terpri)))
           (terpri))

                
         (if (or end-loop-p 
                 (and sat (not *compute-all-completions-p*)))

             (progn 

               ;;; Vorherigen Enumeration Context herstellen!
                            
               (announce "*** EXITING LEVEL ~A" level)
                      
               (pop-description-stack abox)
              
               (return-from prover 
                 (if sat 
                     t
                   (values sat (set-difference collected-dependencies choice-points)))))

           (progn 

             (when (zerop (1- count))
               (setf choice-points
                     ;; der neue Choicepoint wird automatisch auch f.d. Inverse registriert!
                     (add-choice-points-for-edges abox underspecified-edges)))
             
             (announce "*** CHOICE POINTS OF DISJUNCTIVE EDGES: ~A~%ACTUAL CHOICE POINTS OF DISJ.EDGES: ~A" 
                       choice-points
                       (mapcar #'get-choice-points underspecified-edges-and-inverses))
                            
             (multiple-value-bind (sub-sat deps)
                 (if clash-p
                     (if successfully-repaired-p
                         (progn
                           
                           (announce "*** SUCCESSFULLY REPARIED CONSTRAINT SYSTEM ON LEVEL ~A!" level)
                                
                           (setf clash-p nil)
                           
                           +insert-after-successful-clash-repair-code+
                           
                           )
                       (progn

                         ;;; hier habe ich keine andere Wahl!
                         ;;; da "local" nicht reparier werden kann, 
                         ;;; müssen alle Constraints die von den 
                         ;;; Kanten abhängen zurückgenommen werden
                         
                         (announce "*** LOCAL REPAIR OF CONSTRAINT SYSTEM ON LEVEL ~A FAILED!" level)

                         (pop-description-stack abox)
                                
                         #+:use-dependency-directed-backtracking
                         (progn
                           (break "DEPENDENCY DIRECTED BACKTRACKING NOT FULLY UNDERSTOOD YET!")
                           (dolist (edge underspecified-edges-and-inverses) 
                             (rollback-all-depending-on abox edge)))

                         #-:use-dependency-directed-backtracking
                         (let ((*use-unsat-cache-p* nil))  ; wichtig!
                           ;;; simple backjumping
                           (rollback-to abox memorized-action))
                                
                         (return-from prover 
                           (if sat 
                               t
                             (values nil (set-difference collected-dependencies choice-points))))))
                   
                   +insert-body-code+ )
                
               (when *debug-p* 

                 (announce "*** RETURNED TO ABOX ENUMERATION ON LEVEL ~A -> SUB-SAT: ~A, RETURNED CHOICE POINTS: ~A~%CHOICE POINTS OF DISJUNCTIVE EDGES: ~A~%ACTUAL CHOICE POINTS OF DISJ.EDGES: ~A" level sub-sat 
                           deps 
                           choice-points (mapcar #'get-choice-points underspecified-edges-and-inverses))
                        
                 (when *debug-p* 
                   (format t  "~%~%Underspecified Edges:~%")
                   (dolist (edge underspecified-edges)
                     (describe-object edge t)
                     (terpri))
                          
                   (format t  "~%~%Edges and Inverses:~%")
                   (dolist (edge underspecified-edges)
                     (describe-object edge t)
                     (terpri)
                     (when (inverse-edge edge) 
                       (describe-object (inverse-edge edge) t)
                       (terpri) (Terpri)))
                   (terpri)))
                
               (setf sat (or sub-sat sat)
                     clash-p nil)
                                
               (if *compute-all-completions-p* 
                   (progn 

                     #+:use-dependency-directed-backtracking
                     (progn 
                       (break "DEPENDENCY DIRECTED BACKTRACKING NOT FULLY UNDERSTOOD YET!")
                       
                       (dolist (edge underspecified-edges-and-inverses)                              
                         (rollback-all-depending-on abox edge)))
                     
                     #-:use-dependency-directed-backtracking                              
                     (let ((*use-unsat-cache-p* nil)) 
                       ;;; simple backjumping
                       (rollback-to abox memorized-action))
                            
                     (let ((next-abox 
                            (when (has-next-configuration-p abox)
                              (next-completion abox))))

                              
                       (setf abox (or next-abox abox))

                       (unless next-abox
                         (announce "*** NO NEXT COMPLETION ON LEVEL ~A" level))
                              
                       (unless next-abox 
                         (setf end-loop-p t))))
                        
                 (unless sub-sat
                          
                   (setf clash-p t)

                   (when (and deps choice-points
                              (> (maximum deps) 
                                 (maximum choice-points)))
                     (error "*** Error in dependency management!"))
                    
                   (setf collected-dependencies
                         (append deps collected-dependencies))

                   ;;; kanten identifizieren, die für den clash verantwortlich sind!
                              
                   (let ((deps (intersection deps choice-points)))
                            
                     (setf deps (mapcar #'(lambda (choice-point)
                                            (remove-if-not #'(lambda (edge) 
                                                               (member choice-point
                                                                       (get-choice-points edge)))
                                                           underspecified-edges-and-inverses))
                                        (sort-choice-points deps)))
                     ;; (setf *x* abox)

                     (announce "++++ POSSIBLE RESTART EDGES: ~A" deps)
                                     
                     (let ((restart-points
                            (block here
                              (loop as dep in deps
                                    do 
                                    (loop as restart-from in dep
                                          when (has-next-configuration-p abox :restart-from restart-from)
                                          do (return-from here
                                               (list dep restart-from)))))))

                       (announce "++++ REMAINING RESTART EDGES: ~A" restart-points)
                              
                       ;;; (break "~A" restart-points)
                       (cond (restart-points
                              
                              (announce "*** REPAIRING CONSTRAINT SYSTEM ON LEVEL ~A FROM ~A!" level restart-points)

                              #+:use-dependency-directed-backtracking
                              (progn 
                                (break "DEPENDENCY DIRECTED BACKTRACKING NOT FULLY UNDERSTOOD YET!")
                                (dolist (dep (first restart-points))
                                  (rollback-all-depending-on abox dep)))

                              #-:use-dependency-directed-backtracking
                              (progn 
                                (rollback-to abox memorized-action))

                              (announce "*** Stack: ~A~%*** Description Stack: ~A" 
                                        (slot-value abox 'thematic-substrate::stack)
                                        (slot-value abox 'thematic-substrate::description-stack))

                              (let ((next-abox 
                                     (next-completion abox :restart-from (second restart-points))))
                                (setf abox (or next-abox abox))
                                (setf successfully-repaired-p 
                                      (when next-abox t))))

                             (t     
                              
                              (setf successfully-repaired-p nil)

                              (announce "*** UNABLED TO RESTART FROM ~A~%*** Stack: ~A~%*** Description Stack: ~A!" restart-points 
                                        (slot-value abox 'thematic-substrate::stack)
                                        (slot-value abox 'thematic-substrate::description-stack))))))))))))))))


;;;
;;; Der folgende Code wird benötigt, um Rollen-Disjunktionen 
;;; in der ABOX aufzulösen
;;;

(defmethod edge-setter ((edge abox-edge) (role simple-role) &key remaining &allow-other-keys)
  (declare (ignore remaining))

  (change-textual-description (description edge) role)
 
  edge)

(defmethod edge-gen ((abox abox) &key &allow-other-keys)
  (get-underspecified-edges abox :exclude-inverses-p t))

(defmethod sym-gen ((edge abox-edge) processed-edges &key &allow-other-keys)
  (declare (ignorable processed-edges))
  (let ((role (textual-description (aux-description edge))))
    (copy-list (arguments role)))) ;;; WICHTIG !! COPY-LIST !!!

(defun selector-fn (rem-edges processed-edges &key &allow-other-keys)
  (declare (ignore processed-edges))
  (first rem-edges))


(defmethod manager-fn ((sel-edge abox-edge) edges processed-edges  &key &allow-other-keys)
  (values (remove sel-edge edges)
          (cons sel-edge processed-edges)))

;;;
;;;
;;;


(defmethod role-consistent-p ((abox abox) &rest args)
  (declare (ignore args))
  t)

(defmethod enumerate-atomic-configurations ((abox abox) 
                                            &rest args
                                            &key
                                            (edge-setter #'edge-setter)

                                            (respect-inverses-p t)
                                            
                                            (sym-gen #'sym-gen)

                                            (selector-fn #'selector-fn)
                                   
                                            (edge-gen #'edge-gen)
                                            
                                            (manager-fn #'manager-fn) 
                                            
                                            (final-check-fn #'role-consistent-p)
                                   
                                            (fn #'(lambda (abox &rest args) 
                                                    (declare (ignore args)) abox))
                                   
                                            &allow-other-keys)

  (apply #'call-next-method abox
         :save-node-labels-p nil
         :save-edge-labels-p t         
         :edge-setter edge-setter 
         :respect-inverses-p respect-inverses-p 
         :edges (funcall edge-gen abox)
         :manager-fn manager-fn
         :final-check-fn final-check-fn
         :selector-fn selector-fn
         :sym-gen sym-gen
         :fn fn
         args))


;;;
;;;
;;;


(defmethod first-completion ((abox abox) &key &allow-other-keys)
  (enumerate-atomic-configurations abox :reset-p t))

(defmethod next-completion ((abox abox) &key restart-from)
  (enumerate-atomic-configurations abox :restart-from restart-from))
