;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)


(defmacro with-nrql-standard-settings (&body body)

  ;;; Racer! 

  `(let ((*multiprocess-queries* nil)
         (*add-role-assertions-for-datatype-properties-p* nil)

         (*tuple-at-a-time-p* nil)
         (*proactive-tuple-computation-p* t)

         (*how-many* nil)

         (*two-phase-processing-p* nil)
         (*deliver-phase-two-warning-tokens-p* nil)
         (*deliver-kb-has-changed-warning-tokens-p* nil)
         
         (*initial-abox-mirroring-p* nil)
         (*check-abox-consistency-p* t)
         
         (*optimize-p* t)
         (*warnings-p* t) 
         (*rewrite-to-dnf-p* t)
         
         (*told-information-reasoning-p* nil) 
         (*report-inconsistent-queries-p* nil)
         (*report-tautological-queries-p* nil)
         (*rewrite-semantically-p* nil)
         (*use-repository-p* nil)
         (*put-into-repository-p* nil)
         (*classify-concepts-in-instance-assertions-p* nil)
         (*ensure-tbox-classification-p* nil)
         (*add-rule-consequences-p* nil)
         (*dont-add-abox-duplicates-p* nil)
         (saved-id *iterator-id*)
         (*type-of-substrate* 'racer-dummy-substrate))

     (prog1 
         (progn ,@body)
       (when (> (- *iterator-id* saved-id) 1)
         (nrql-error "Runtime error: bad use of with-nrql-settings! at most ONE call to racer-answer-query allowed!"))
       (unless (= *iterator-id* saved-id)
         (delete-query *last-query*)))))

;;;
;;; Hilfsfunktion f. Racer
;;;

#|
(defmacro with-nrql-timeout (&rest body)
  `(if *server-timeout*
       (with-timeout (*server-timeout*

                      (let ((substrate 
                             (find-racer-substrate abox *type-of-substrate*)))
                        
                        (when substrate
                          (substrate-needs-reset substrate))

                        (nrql-warning "Timeout encountered!")

                        :timeout))
	 ,@body)
     (progn
       ,@body)))
     
|#


(defun racer-retrieve-individual-filled-roles (from to &key (abox (current-abox)) no-inverses-p)
  (with-critical-section 
   (let ((saved-cur-substrate *cur-substrate*))
     
     (racer-prepare-substrate :abox abox :prepare-now-p t )
     
     (let ((roles nil)
           (*running-substrate* *cur-substrate*))
        
       (check-abox-consistency *running-substrate*)
         
       (dolist (role (dl-prover-all-roles *running-substrate*))
         (when (=> no-inverses-p (symbolp role))
           (evaluate-dl-prover-check-individuals-related-p from to role 
                                                       #'(lambda (&rest args) 
                                                           (declare (ignorable args))
                                                           (push role roles)))))
       
       (setf *cur-substrate* saved-cur-substrate)
       
       (remove-duplicates roles)))))


(defun racer-retrieve-related-individuals (role &key (abox (current-abox)))
  (with-critical-section 
   (let ((saved-cur-substrate *cur-substrate*))

     (racer-prepare-substrate :abox abox :prepare-now-p t)

     (let ((*running-substrate* *cur-substrate*)
           (pairs nil))

       (check-abox-consistency *running-substrate*)

       (dolist (from (dl-prover-all-individuals *running-substrate*))
         (evaluate-dl-prover-retrieve-individual-fillers from role
                                                     #'(lambda (&rest args &key to &allow-other-keys) 
                                                         (declare (ignorable args))
                                                         (push (list from to) pairs))))

       (setf *cur-substrate* saved-cur-substrate)
       
       (remove-duplicates pairs :test #'equal)))))


(defun racer-retrieve-individual-fillers (from role &key (abox (current-abox)))
  (with-critical-section 
   (let ((saved-cur-substrate *cur-substrate*)
         (fillers nil))

     (racer-prepare-substrate :abox abox :prepare-now-p t)

     (let ((*running-substrate* *cur-substrate*))
    
       (check-abox-consistency *running-substrate*)

       (evaluate-dl-prover-retrieve-individual-fillers from role 
                                                   #'(lambda (&rest args &key to &allow-other-keys) 
                                                       (declare (ignorable args))
                                                       (push to fillers))))
        
     (setf *cur-substrate* saved-cur-substrate)
     
     (remove-duplicates fillers))))

