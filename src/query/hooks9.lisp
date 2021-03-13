;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;;
;;;

(defmethod register-bindings :before ((substrate substrate) (query query) (answer-pattern list)
                                      (new-bindings list))

  (with-critical-section
   (let ((tuple (construct-result-tuple query)))
     (when tuple
       (with-slots (bindings-queue new-abox-assertions
                                   abox-assertions-to-add
                                   last-queue-item
                                   add-rule-consequences-p
                                   tuple-at-a-time-p) query
        
         (when (is-rule-p query) 
           (push tuple new-abox-assertions)
           (unless tuple-at-a-time-p
             (push tuple abox-assertions-to-add)))

         (if (not last-queue-item)
             (progn 
               (setf bindings-queue (list tuple))
               (setf last-queue-item (last bindings-queue)))
           (progn 
             (setf (cdr last-queue-item)
                   (list tuple))
             (setf last-queue-item
                   (cdr last-queue-item))))))))

  (wait-for-request-or-abort query))


(defmethod register-bindings ((substrate substrate) (query query) (answer-pattern list) (new-bindings list))
  'done)

;;;
;;;
;;;

(defmethod querying-started ((substrate substrate) (query query))
  t)

(defmethod querying-started :before ((substrate substrate) (query query))
  t)

;;;
;;;
;;;

(defmethod querying-ended ((substrate substrate) (query query)))


(defmethod querying-ended :before ((substrate substrate) (query query))
  (with-critical-section
   (unless (bottom-up-component-query-p query)
     (if (is-rule-p query) 
         (progn 
           (setf *active-rules* (delete query *active-rules*))
           (pushnew query *processed-rules*))
       (progn 
         (setf *active-queries* (delete query *active-queries*))
         (pushnew query *processed-queries*))))
   
   ;;; etwas Platz schaffen fuer den Garbage Collector!
   
   (with-slots (bottom-up-component-query-p
                result-bindings-hash
                process
                env-setup-fn) query

     (unless bottom-up-component-query-p
       (setf result-bindings-hash nil)
       (dolist (buq (bottom-up-evaluation-plan query))
         (setf (slot-value buq 'result-bindings-hash) nil)))

     (setf (slot-value (parser query) 'query-hash) nil)
     
     (setf env-setup-fn nil
           process nil))))

;;;
;;;
;;;

(defmethod querying-ended :before ((substrate substrate) (query nrql-query))
  (setf (slot-value query 'phase-two-started-p) nil))

(defmethod querying-ended ((substrate racer-dummy-substrate) (query nrql-query))
  (with-slots (added-premise-axioms substrate) query

    ;;; wichtig! das muss hier stattfinden! nicht in 
    ;;; last-tuple-delivered! kompliziert! nicht aendern!

    (with-critical-section
     (when added-premise-axioms
       (forget-statement (tbox substrate) 
                         (abox substrate)
                         added-premise-axioms)
       ;;; notwendig!
       (substrate-needs-reset substrate)))))

;;;
;;;
;;;

(defmethod last-tuple-has-been-delivered ((query query))
  t)

(defmethod last-tuple-has-been-delivered ((query nrql-query))
  (with-slots (abox-assertions-to-add added-premise-axioms substrate
                                      tuple-at-a-time-p
                                      add-rule-consequences-p) query

    
    (when add-rule-consequences-p
      (add-chosen-sets-of-rule-consequences query))
    
    (setf added-premise-axioms nil)))


(defmethod add-chosen-sets-of-rule-consequences ((query nrql-query) &rest args)
  (declare (ignore args))
  (with-slots (abox-assertions-to-add substrate) query

    (dolist (pat abox-assertions-to-add)
      (add-abox-assertions substrate pat))

    (prog1
        abox-assertions-to-add
      (setf abox-assertions-to-add nil))))


(defmethod choose-current-set-of-rule-consequences ((query nrql-query) &rest args)
  (declare (ignore args))
  (with-slots (current-bindings abox-assertions-to-add) query
    (unless (member current-bindings
                    '(:exhausted :timeout
                      :denied-due-to-deadlock-prevention
                      :warning-kb-has-changed 
                      :warning-expensive-phase-two-starts))
      (push current-bindings abox-assertions-to-add))))


;;;
;;;
;;;

(defmethod add-abox-assertions ((substrate dl-prover-substrate) (assertions list))
  (let ((added nil))
    (dolist (assertion assertions)
      (if (eq assertion :undefined)
          (list :undefined)
        (ecase (to-keyword (first assertion))
          (:instance 
           (when (=> *dont-add-abox-duplicates-p*
                     (not (member (list (second assertion) (third assertion))
                                  (dl-prover-all-concept-assertions-for-individual substrate (second assertion))
                                  :test #'equal)))

             (push assertion added)
             (apply #'dl-prover-add-concept-assertion substrate (rest assertion))))
          (:related 
           (when (=> *dont-add-abox-duplicates-p*
                     (not (member (list (list (second assertion)
                                              (third assertion))
                                        (fourth assertion))
                                  (dl-prover-all-role-assertions-for-individual-in-domain substrate 
                                                                                      (second assertion))
                                  :test #'equal)))
             (push assertion added)
             (apply #'dl-prover-add-role-assertion substrate (rest assertion))))
          (:constrained
           (when (=> *dont-add-abox-duplicates-p*
                     (not (member (third assertion)
                                  (dl-prover-retrieve-individual-attribute-fillers substrate
                                                                               (second assertion)
                                                                               (fourth assertion))
                                  :test #'same-abox-individual-p)))
             (push assertion added)
             (apply #'dl-prover-add-attribute-assertion substrate (rest assertion))))
          (:constraints
           (dolist (constraint (rest assertion))
             (when (=> *dont-add-abox-duplicates-p*
                       (not (member constraint (dl-prover-all-constraints substrate)
                                    :test #'equal)))
               (push assertion added)
               (dl-prover-add-constraint-assertion substrate constraint))))

          ;;;
          ;;; 
          ;;; 
        
          (:forget-concept-assertion
           (dl-prover-forget-concept-assertion substrate 
                                               (abox substrate) 
                                               (second assertion) (third assertion)))

          (:forget-role-assertion
           (dl-prover-forget-role-assertion substrate 
                                            (abox substrate)
                                            (second assertion) (third assertion) (fourth assertion)))

          (:forget-constrained-assertion
           (dl-prover-forget-constrained-assertion substrate 
                                                   (abox substrate)
                                                   (second assertion) (third assertion) (fourth assertion)))
          
          (:forget-constraint
           (dl-prover-forget-constraint substrate 
                                        (abox substrate)
                                        (second assertion))))))

    added))


