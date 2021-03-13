;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;


(defrule or-expansion (dl abox)

  (multiple-value-bind (disjuncts or-concept node)
      (cond ((not *semantic-branching-p*)
             
             ;; branch syntactically
                    
             (multiple-value-bind (or-concept node)
                 (select-or-concept abox *strategy* language)
                      
               (when node 
                 (values 
                  (remove-if #'(lambda (x) 
                                 (or (on-tableau-p node x) 
                                     (on-tableau-p node (get-negated-concept x))))
                             (arguments or-concept))
                  or-concept
                  node))))
                   
            (t ;; semantic branching!

               (multiple-value-bind (disjunct or-concept node)
                   (select-open-disjunct abox *strategy* language)
          
                 (when node
                   (values 
                    (list disjunct (get-negated-concept disjunct))
                    ;;; statische Auswahl! 
                    ;;; zunächst wird das Disjunct wahr gemacht -> "or-concept" wird wahr (-> expanded)
                    ;;; dann wird das Disjunct falsch gemacht -> "or-concept" muss geht *nicht* auf expanded!!! 
                    ;;; beim Backtracking (vom 1. Disjunkt) muss also sichergestellt werden, dass "or-concept"
                    ;;; wieder auf unexpanded gelegt wird! das macht die Dependenz-Verwaltung (remove-all-...) 
                    ;;; Sonst werden nämlich Alternativen übersehen... 
                    or-concept 
                    node)))))
    
    (if (not (and node disjuncts))
        ;;; Nachfolgender Code für SOME-Expansion

        +insert-negative-code+

      (let ((sat nil)
            (new-choice-point 
             (get-new-choice-point))
            (collected-dependencies nil)
            (memorized-action (get-current-action))
            ;(put-disjunct-to-unexpanded-action nil)
            (disjuncts (copy-list disjuncts)))

        (loop while disjuncts do
                
              (let* ((disjunct (first disjuncts))
                     (last-p (not (cdr disjuncts))))
                  
                (announce "CHOICE POINT ~A. OR CONCEPT ~A : ~A, DISJUNCT ~A, ALL DISJUNCTS: ~A" 
                          new-choice-point node or-concept
                          disjunct disjuncts)
                     
                (if *semantic-branching-p*
                    (announce "Semantic selection of ~A : ~A" node disjunct)
                  (announce "Syntactic selection of ~A : ~A" node disjunct))
                
                (setf disjuncts (cdr disjuncts))                    
                       
                (let ((added
                       (register-as-unexpanded disjunct 
                                        :comment (if *semantic-branching-p*
                                                     'add-semantically-chosen-or-disjunct-to-unexpanded
                                                   'add-syntactically-chosen-or-disjunct-to-unexpanded)
                                        :node node
                                        :new-choice-point new-choice-point
                                        :depends-on (list (list node or-concept)))))
                
                ;(setf put-disjunct-to-unexpanded-action (get-current-action))

                  (check-for-clash node added))

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

                    (dolist (dep deps)
                      (push dep collected-dependencies)))

                  ;;; letztes Disjunkt? -> in jedem Fall zurückkehren! 
                    
                  (when last-p                                                       
                    (return-from prover 
                      (if sat
                          t
                        (if *compute-all-completions-p* 
                            nil
                          (values nil (remove new-choice-point collected-dependencies))))))

                  ;;; nicht-letztes Disjunkt 
                     
                  (when (not last-p)
                    (when (and sat (not *compute-all-completions-p*))
                      (return-from prover t))
                        
                    (when (not sat)
                      (when (not (member new-choice-point deps)) 
                               
                        ;;;
                        ;;; Clash Reason liegt nicht am Disjunkt! -> Backtracking
                        ;;; Achtung: hier ist *kein* Rollback notwendig! 
                        ;;; das Rollback wird immer von dem Veranlasst, der den 
                        ;;; den Choice Point aufgesetzt hat!
                        ;;; Logischerweise werden dann auch die hier noch auf dem 
                        ;;; Tableaux vorhandenen Constraints zurückgenommen, da
                        ;;; diese von dem "weiter unten liegenden Constraint "abhängen
                        ;;;

                        (return-from prover
                          (values nil (remove new-choice-point collected-dependencies))))
                         
                      (when (member new-choice-point deps) 
                               
                        ;;;
                        ;;; Clash Reason liegt am Disjunkt! 
                        ;;; Aufräumen und Weitermachen, 
                        ;;; nächstes Disjunkt aus der Schleife probieren 
                        ;;; 

                        (when *debug-p* 
                          (announce "BEFORE ROLLBACK:")
                          (describe-object abox t)
                          (loop-over-abox-nodes (node abox)
                            (describe-object node t)))

                        #+:use-dependency-directed-backtracking
                        (progn 
                                     
                          (break "DEPENDENCY DIRECTED BACKTRACKING NOT FULLY UNDERSTOOD YET!")
                                     
                          ;;(rollback-all-depending-on abox (list node disjunct))
                          ;;(undo-action abox put-disjunct-to-unexpanded-action))
                          ;;; 
                          ;;; das ist noch nicht die ganze Geschichte... 
                          ;;; vorerst auf Eis gelegt! 

                          )
                                 
                        #-:use-dependency-directed-backtracking
                        
                        (progn
                              
                          ;;;
                          ;;; einfaches BACKJUMPING
                          ;;; chronologisches Abräumen
                          ;;;

                          (rollback-to abox memorized-action node))
                               
                        (when *debug-p* 
                          (announce "AFTER ROLLBACK:")
                          (describe-object abox t)
                          (loop-over-abox-nodes (node abox)
                            (describe-object node t))))
                               
                      )))))
               
        (error "Why am I here? ~A" disjuncts)))))
