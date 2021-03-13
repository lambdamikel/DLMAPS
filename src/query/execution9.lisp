;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;;
;;;

(defmethod abox-has-changed-since-parsing-p ((query nrql-query))
  (with-slots (substrate kb-id) query
    ;; nur die Toplevel-Query hat KB-ID!
    (when kb-id
      (when (second kb-id)
        (not (= (get-abox-id substrate)
                (second kb-id)))))))

(defmethod tbox-has-changed-since-parsing-p ((query nrql-query))
  (with-slots (substrate kb-id) query
    (when (first kb-id)
      (not (= (get-tbox-id substrate)
              (first kb-id))))))

;;;
;;;
;;;

(defmethod abox-has-changed-since-parsing-p ((query query))
  nil)

(defmethod tbox-has-changed-since-parsing-p ((query query))
  nil)

;;;
;;;
;;;

(defun exclusive-queries ()
  (remove-if-not #'modifies-state-p 
                 
                 ;;; jede Query, die modifies-state-p hat, 
                 ;;; wird execute-query in with-critical-section
                 ;;; ausfuehren 
                 
                 *active-queries*))



(defun exclusive-rules ()
  (remove-if-not #'modifies-state-p 
                 
                 ;;; jede Query, die modifies-state-p hat, 
                 ;;; wird execute-query in with-critical-section
                 ;;; ausfuehren 
                 
                 *active-queries*))


(defun deadlock-queries ()
  
  ;;; wenn unter den exlusive-queries eine dabei ist, 
  ;;; die nicht pro-aktiv laeuft (also lazy-incremental), 
  ;;; dann duerfen wir keine neue Query starten! 
  ;;; 
  ;;; Denn die vorhandene Query hat Racer gelockt 
  ;;; (*process-lock*), und wartet, weil lazy, nun 
  ;;; auf den Aufruf (get-next-tuple ...). Der Lock 
  ;;; kann nicht freigegeben werden, wegen Exklusivitaet
  ;;; (die Query veraendert ja den Zustand von Racer)
  ;;; 
  ;;; Auf die Proaktiven kann man einfach warten - die 
  ;;; terminieren ja von selbst, dann wird der Lock 
  ;;; automatisch frei gegeben!  
  
  (remove-if #'proactive-tuple-computation-p (exclusive-queries)))


(defun deadlock-rules ()
  (remove-if #'proactive-tuple-computation-p (exclusive-rules)))

;;;
;;;
;;;

(defun ensure-deadlock-prevention ()
  (let ((dq (deadlock-queries)))

    (when dq

      (query-deadlock-warning dq)

      (return-from ensure-deadlock-prevention t)))

  (let ((dr (deadlock-rules)))

    (when dr

      (rule-deadlock-warning dr)
      
      (return-from ensure-deadlock-prevention t)))

  (when (or (exclusive-queries)
            (exclusive-rules))

    ;;; diese terminieren dann von selbst 
    ;;; ALLE muessen terminieren, nicht nur
    ;;; die fuer (substrate query)!!!
    
    (wait-for-queries-to-terminate)
    (wait-for-rules-to-terminate)

    (return-from ensure-deadlock-prevention nil))


  nil)

;;;
;;;
;;;

(defmethod execute-query ((query query) &rest args
                          &key
                          (how-many *how-many*)
                          (timeout (or *timeout*
                                       #+:racer-server *server-timeout*))
                          (proactive-tuple-computation-p *proactive-tuple-computation-p*)
                          (tuple-at-a-time-p *tuple-at-a-time-p*)

                          (check-abox-consistency-p *check-abox-consistency-p*)
                          (ensure-tbox-classification-p *ensure-tbox-classification-p*)
                          (initial-abox-mirroring-p *initial-abox-mirroring-p*)
                          (exclude-permutations-p *exclude-permutations-p*)
                            
                          &allow-other-keys)

  (declare (ignorable args))

  (setf (slot-value query 'special-answer) nil) ; f. reexecute!    
  
  (dolist (buq (bottom-up-evaluation-plan query))
    (prepare-to-run-query buq)
    (apply #'execute-query buq args))


  (if (tbox-has-changed-since-parsing-p query)

      (progn 
        (warn-tbox-has-changed)
        (if (is-rule-p query)
            (reexecute-rule query)
          (reexecute-query query)))
    
    (progn

      (let ((id (iterator-id query)))
                          

        (unless (bottom-up-component-query-p query)
          (if (is-rule-p query)
              (with-critical-section
               (setf *ready-rules* (delete query *ready-rules*))  
               (push query *active-rules*))
            (with-critical-section
             (setf *ready-queries* (delete query *ready-queries*))  
             (push query *active-queries*))))

        (setf (slot-value query 'process) nil
              (slot-value query 'how-many) how-many
              (slot-value query 'timeout) timeout
              (slot-value query 'tuple-at-a-time-p) tuple-at-a-time-p
              (slot-value query 'proactive-tuple-computation-p) proactive-tuple-computation-p
              (slot-value query 'check-abox-consistency-p) check-abox-consistency-p
              (slot-value query 'ensure-tbox-classification-p) ensure-tbox-classification-p
              (slot-value query 'initial-abox-mirroring-p) initial-abox-mirroring-p
              (slot-value query 'exclude-permutations-p) exclude-permutations-p
              (slot-value query 'result-bindings-hash) (mht :test #'equal))
        
        (save-dl-prover-state query)

        (setf (slot-value query 'process)
              (start-query-process
               (let ((*process-id* id))

                 #+(or (not :debug) :midelora)
                 (handler-case
                     (handler-bind ((condition (lambda (c)
                                                 (declare (ignorable c))
                                                 #+:racer-server
                                                 (print-debug-info c))))
                       
                       (progn 
                         #+:sequential-query-scheduling 
                         (with-critical-section
                          (funcall (env-setup-fn query) query))
                         #-:sequential-query-scheduling 
                         (if (modifies-state-p query)
                             (with-critical-section                        
                              (funcall (env-setup-fn query) query))
                           (funcall (env-setup-fn query) query))))

                   (error (error)            
                          
                          ;;; diese Reihenfolge ist die einzige
                          ;;; die funktioniert!!! so lassen!
                          ;;; hoch komplex - process pool etc.! 
                          
                          (setf (slot-value query 'runtime-error) error
                                (slot-value query 'process) nil)
                          
                          (abort-query query)))
                 
                 #-(or (not :debug) :midelora)
                 (progn 
                   #+:sequential-query-scheduling 
                   (with-critical-section
                    (funcall (env-setup-fn query) query))
                   #-:sequential-query-scheduling 
                   (if (modifies-state-p query)
                       (with-critical-section                        
                        (funcall (env-setup-fn query) query))
                     (funcall (env-setup-fn query) query))))))

        (if (slot-value query 'process)        
            (if (not tuple-at-a-time-p)
                (get-answer query)
              (list id :running))
          (if *multiprocess-queries*
              (progn 
                (setf (slot-value query 'special-answer)
                      :acquire-process-failed-pool-size-exceeded)
                (list id :acquire-process-failed-pool-size-exceeded))
            (get-answer query)))))))


(defmethod execute-query ((query nrql-query) 
                          &rest args
                          &key
                          (how-many *how-many*)
                          (timeout (or *timeout*
                                       #+:racer-server *server-timeout*))
                          (two-phase-processing-p *two-phase-processing-p*)
                          (deliver-phase-two-warning-tokens-p *deliver-phase-two-warning-tokens-p*)
                          (deliver-kb-has-changed-warning-tokens-p *deliver-kb-has-changed-warning-tokens-p*)
                          (proactive-tuple-computation-p *proactive-tuple-computation-p*)
                          (tuple-at-a-time-p *tuple-at-a-time-p*)
                          (add-rule-consequences-p *add-rule-consequences-p*)                          
                          (told-information-reasoning-p *told-information-reasoning-p*)
                          (continuation-based-instance-retrieval-p *continuation-based-instance-retrieval-p*)
                          (check-abox-consistency-p *check-abox-consistency-p*)
                          (ensure-tbox-classification-p *ensure-tbox-classification-p*)
                          (initial-abox-mirroring-p *initial-abox-mirroring-p*)
                          (exclude-permutations-p *exclude-permutations-p*)
                          &allow-other-keys)

  (declare (ignorable args))

  (setf (slot-value query 'special-answer) nil) ; f. reexecute!    

  (dolist (buq (bottom-up-evaluation-plan query))
    (prepare-to-run-query buq)
    (apply #'execute-query buq args))

  ;;;
  ;;; diese Slots muessen schon hier gesetzt werden,
  ;;; wegen Deadlock-Erkennung! 
  ;;; 

  (setf (slot-value query 'modifies-state-p) 
        ;;; ensure-deadlock-prevention betrachtet nur
        ;;; modifies state, daher ist es wichtig, diesen
        ;;; Slot "strenger" zu setzen als bereits durch
        ;;; das Parsing (s. auch initialize-instance :after)
        ;;; geschehen 

        (or (modifies-state-p query)
            
            (and add-rule-consequences-p 
                 (rule-con-pattern query))))
  
  (when (ensure-deadlock-prevention)
    
    (setf (slot-value query 'special-answer)
          :denied-due-to-deadlock-prevention)

    (return-from execute-query 
      (list (iterator-id query)
            :denied-due-to-deadlock-prevention)))

  (when (modifies-state-p query)
    (let ((queries 
           (remove-if #'proactive-tuple-computation-p *active-queries*))
          (rules 
           (remove-if #'proactive-tuple-computation-p *active-rules*)))

      (when (or queries rules)

        (when queries
          (query-deadlock-warning queries))

        (when rules 
          (rule-deadlock-warning rules))
        
        (setf (slot-value query 'special-answer)
              :denied-due-to-deadlock-prevention)

        (return-from execute-query 
          (list (iterator-id query)
                :denied-due-to-deadlock-prevention)))

      ;;; sonst sind nur Proaktive am Laufen

      (wait-for-queries-to-terminate)
      (wait-for-rules-to-terminate)))

  ;;;
  ;;;
  ;;;

  (if (tbox-has-changed-since-parsing-p query)
      
      (progn 
        (warn-tbox-has-changed)
        (if (is-rule-p query)
            (reexecute-rule query)
          (reexecute-query query)))

    (progn
      
      (let ((id (iterator-id query)))

        (unless (bottom-up-component-query-p query)
          (if (is-rule-p query)
              (with-critical-section
               (setf *ready-rules* (delete query *ready-rules*))
               (push query *active-rules*))

            (with-critical-section
             (setf *ready-queries* (delete query *ready-queries*))
             (push query *active-queries*))))
      
        (setf (slot-value query 'process) nil
              (slot-value query 'how-many) how-many
              (slot-value query 'timeout) timeout
              (slot-value query 'two-phase-processing-p) 

              ;;; fuer negated Queries (not (?x c)) darf
              ;;; two-phase-processing nicht verwendet werden!
              ;;; sonst kommen in Phase 1 schon zu viele Tupel
              ;;; zurueck... 

              (and two-phase-processing-p
                   (not (some #'negated-p (cons query (all-subqueries query)))))

              (slot-value query 'deliver-phase-two-warning-tokens-p) deliver-phase-two-warning-tokens-p
              (slot-value query 'deliver-kb-has-changed-warning-tokens-p) deliver-kb-has-changed-warning-tokens-p 
              
              (slot-value query 'tuple-at-a-time-p) tuple-at-a-time-p

              (slot-value query 'add-rule-consequences-p) add-rule-consequences-p
              (slot-value query 'proactive-tuple-computation-p) proactive-tuple-computation-p

              (slot-value query 'exclude-permutations-p) exclude-permutations-p
              
              (slot-value query 'told-information-reasoning-p) 
              (or (is-tbox-mirror-substrate-p (substrate query)) 
                  told-information-reasoning-p)
              
              (slot-value query 'check-abox-consistency-p) check-abox-consistency-p
              (slot-value query 'ensure-tbox-classification-p) ensure-tbox-classification-p
              (slot-value query 'initial-abox-mirroring-p) initial-abox-mirroring-p
              (slot-value query 'continuation-based-instance-retrieval-p) continuation-based-instance-retrieval-p
              
              (slot-value query 'result-bindings-hash) (mht :test #'equal))

        ;;; (format t "TWO PHASE: ~A~%" (two-phase-processing-p query))

        (with-critical-section

         (save-racer-state query))

        (setf (slot-value query 'process)
              (start-query-process
               (let ((*process-id* id))
                 (let ((*runtime-evaluation-p* t))

                   #+(or (not :debug) :midelora)
                   (handler-case
                       (handler-bind ((condition (lambda (c)
                                                   (declare (ignorable c))
                                                   #+:racer-server
                                                   (print-debug-info c))))
                         (if (modifies-state-p query)
                             (with-critical-section
                              (funcall (env-setup-fn query) query))
                           (progn 
                             #+:sequential-query-scheduling 
                             (with-critical-section
                              (funcall (env-setup-fn query) query))
                             #-:sequential-query-scheduling 
                             (funcall (env-setup-fn query) query))))
                     
                     (error (error)                          
                            
                            ;;; diese Reihenfolge ist die einzige
                            ;;; die funktioniert!!! so lassen!
                            ;;; hoch komplex - process pool etc.! 
                            
                            (setf (slot-value query 'runtime-error) error
                                  (slot-value query 'process) nil)
                            (abort-query query)))
                   
                   #-(or (not :debug) :midelora)
                   (if (modifies-state-p query)
                       (with-critical-section
                        (funcall (env-setup-fn query) query))
                     (progn 
                       #+:sequential-query-scheduling 
                       (with-critical-section
                        (funcall (env-setup-fn query) query))
                       #-:sequential-query-scheduling 
                       (funcall (env-setup-fn query) query)))))))

        (if (slot-value query 'process)
                
            (if (not tuple-at-a-time-p)
                (get-answer query)
              (list id :running))

          (if *multiprocess-queries*
              (progn 
                (setf (slot-value query 'special-answer)
                      :acquire-process-failed-pool-size-exceeded)
                (list id :acquire-process-failed-pool-size-exceeded))
            (get-answer query)))))))


  
(defmethod execute-rule ((query query) &rest args &key &allow-other-keys)
  (let ((*proactive-tuple-computation-p* nil))
    (apply #'execute-query query args)))

;;;
;;;
;;;       

(defmethod reexecute-query ((query query) &rest args)
  (multiple-value-bind (status query)
      (reprepare-query query)
    (declare (ignorable status))
    (apply #'execute-query query args)))

(defmethod reexecute-rule ((query query) &rest args)
  (multiple-value-bind (status query)      
      (reprepare-rule query)
    (declare (ignorable status))
    (apply #'execute-rule query args)))

