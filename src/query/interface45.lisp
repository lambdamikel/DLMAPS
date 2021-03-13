;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;;
;;;

(defmethod wait-for-request-or-abort ((query query))
  (with-slots (process get-next-tuple-p abort-search-p) query
    (when process
      (unless (proactive-tuple-computation-p query)
        (setf get-next-tuple-p nil)

        (process-wait (or get-next-tuple-p abort-search-p))))))


(defmethod note-phase-two-starts ((query nrql-query))
  (with-slots (phase-two-started-p
               bindings-queue last-queue-item) query
    
    (setf phase-two-started-p t)

    (let ((tuple :warning-expensive-phase-two-starts))

      ;;; zu diesem Zeitpunkt ist die Queue leer! 

      (setf bindings-queue (list tuple))
      (setf last-queue-item (last bindings-queue)))))

;;;
;;;
;;;

(defmethod cheap-query-p ((query nrql-query))
  (not (phase-two-started-p query)))

(defmethod cheap-rule-p ((query nrql-query))
  (cheap-query-p query))

(defmethod expensive-query-p ((query nrql-query))
  (phase-two-started-p query))

(defmethod expensive-rule-p ((query nrql-query))
  (expensive-query-p query))


;;;
;;;
;;;

(defmethod get-query-iterator-id ((query query))
  (if (is-rule-p query)
      (intern (format nil "RULE-~A" (incf *iterator-id*)) :keyword)
    (intern (format nil "QUERY-~A" (incf *iterator-id*)) :keyword)))

(defun find-query (id &optional (in *all-queries*))
  (if (eq id :last)
      (first (remove-if #'is-rule-p in))
    (find id in :key #'iterator-id)))

(defun find-rule (id &optional (in *all-rules*))
  (if (eq id :last)
      (first (remove-if-not #'is-rule-p in))
    (find id in :key #'iterator-id)))

;;;
;;;
;;;

(defmethod delete-query ((query query))
  (if (is-rule-p query) 
      :use-delete-rule
    (with-critical-section
     (setf *all-queries*
           (delete query *all-queries*)
           *active-queries*
           (delete query *active-queries*)
           *processed-queries*
           (delete query *processed-queries*)
           *ready-queries*
           (delete query *ready-queries*))
        
     (dolist (query (cons query 
                          (all-subqueries query)))
       (unregister-query query))

     :okay-query-deleted)))

(defmethod delete-rule ((query query))
  (if (not (is-rule-p query))
      :use-delete-query
    (with-critical-section
     (setf *all-rules*
           (delete query *all-rules*)
           *active-rules*
           (delete query *active-rules*)
           *processed-rules*
           (delete query *processed-rules*)
           *ready-rules*
           (delete query *ready-rules*))

     (dolist (query (cons query 
                          (all-subqueries query)))
       (unregister-query query))

     :okay-rule-deleted)))

;;;
;;;
;;;

(defmethod query-accurate-p ((query query))
  (and (not (abox-has-changed-since-parsing-p query))
       (not (tbox-has-changed-since-parsing-p query))))

;;;
;;;
;;;

(defmethod query-waiting-p ((query query))
  (and (not (proactive-tuple-computation-p query))
       (process query)
       (next-tuple-available-p query)))

(defmethod rule-waiting-p ((query query))
  (query-waiting-p query))

;;;
;;;
;;;

(defmethod query-active-p ((query query))
  (when (process query)
    t))

(defmethod rule-active-p ((query query))
  (when (query-active-p query)
    t))

;;;
;;;
;;;

(defmethod query-running-p ((query query))
  (and (query-active-p query)
       (not (query-waiting-p query))))

(defmethod rule-running-p ((query query))
  (query-running-p query))

;;;
;;;
;;;

(defmethod query-ready-p ((query query))
  (when (find query *ready-queries*)
    t))

(defmethod rule-ready-p ((query query))
  (when (find query *ready-rules*)
    t))

;;;
;;;
;;;

(defmethod get-next-n-remaining-tuples ((query query) &optional n)
  (let ((res nil)
        (count 0))
    (loop 
     (let ((tuple (get-next-tuple query)))
       (if (or (eq tuple :exhausted)                 
               (eq tuple :warning-expensive-phase-two-starts)
               (not tuple))
           (return-from get-next-n-remaining-tuples 
             (reverse res))
         (if (eq tuple :timeout)
             (progn 
               (push tuple res) 
               (return-from get-next-n-remaining-tuples 
                 (reverse res)))
           (progn 
             (incf count)
             (push tuple res)
             (when (and n (= count n))
               (return-from get-next-n-remaining-tuples (reverse res))))))))))

;;;
;;;
;;;

(defmethod get-all-remaining-tuples ((query query))
  (get-next-n-remaining-tuples query))

(defmethod get-all-remaining-tuples ((query nrql-query))
  (with-slots (deliver-phase-two-warning-tokens-p) query
    (let ((old deliver-phase-two-warning-tokens-p))
      (setf deliver-phase-two-warning-tokens-p nil)
      (prog1
          (get-next-n-remaining-tuples query)
        (setf deliver-phase-two-warning-tokens-p old)))))


(defmethod get-answer ((query query))
  (if (not (slot-value query 'query-satisfiable))
      :inconsistent
    (progn
      (get-all-remaining-tuples query)

      (or (special-answer query)
      
          (if (is-rule-p query) 

              (if (timeout-p query) 
                  (cons :timeout 
                        (new-abox-assertions query))

                (new-abox-assertions query))

            (if (answer-pattern query)
                (let* ((pat (answer-pattern query))
                       (res
                        (mapcar #'(lambda (binding) 
                                    (mapcar #'(lambda (var val) 
                                                (list (get-textual-head-entry var) val))
                                            pat binding))
                                (result-bindings query))))
                  (if (timeout-p query)
                      (cons :timeout res)
                    res))
              (or (eq (bindings-found-p query) t)
                  (when (timeout-p query)
                    :timeout))))))))

;;;
;;;
;;;

(defmethod abort-query ((query query))
  (setf (abort-search-p query) t) 

  (when (process query)
    (abort-process (process query)))
  
  ;; wird evtl. doppelt aufgerufen 
  (invalidate-cached-bindings query)
  
  (with-critical-section
   (setf *active-queries*
         (delete query *active-queries*))
   (setf *ready-queries*
         (delete query *ready-queries*)))
   
  :okay-query-aborted)

(defmethod abort-rule ((query query))
  (when (is-rule-p query)
    (setf (abort-search-p query) t)
    
    (when (process query)
      (abort-process (process query))
      (invalidate-cached-bindings query))

    (with-critical-section
     (setf *active-rules*
           (delete query *active-rules*))
     (setf *ready-rules*
           (delete query *ready-rules*)))

    :okay-rule-aborted))

;;;
;;; 
;;;


(defmethod describe-query-status ((query query))
  (remove nil
          (cond ((query-ready-p query)
                 (list :ready-to-run))
                ((query-running-p query)
                 (list :running))
                ((query-waiting-p query)
                 (list :waiting-for-get-next-tuple))
                ((query-processed-p query)
                 (list :processed)))))

(defmethod describe-query-status ((query nrql-query))
  (remove nil
          (cons (if (query-accurate-p query)
                    :accurate
                  :not-accurate)
                (cond ((query-ready-p query)
                       (list :ready-to-run))
                      ((query-running-p query)
                       (list :running
                             (when (two-phase-processing-p query)
                               (if (phase-two-started-p query)
                                   :phase-two
                                 :phase-one))))
                      ((query-waiting-p query)
                       (list :waiting-for-get-next-tuple
                             (when (two-phase-processing-p query)
                               (if (phase-two-started-p query)
                                   :phase-two
                                 :phase-one))))
                      ((query-processed-p query)
                       (list :processed))))))

;;;
;;;
;;;

(defmethod describe-rule-status ((query nrql-query))
  (remove nil
          (cons (if (rule-accurate-p query)
                    :accurate
                  :not-accurate)
                (cond ((rule-ready-p query)
                       (list :ready-to-run))
                      ((rule-running-p query)
                       (list :running
                             (when (two-phase-processing-p query)
                               (if (phase-two-started-p query)
                                   :phase-two
                                 :phase-one))))
                      ((rule-waiting-p query)
                       (list :waiting-for-get-next-tuple
                             (when (two-phase-processing-p query)
                               (if (phase-two-started-p query)
                                   :phase-two
                                 :phase-one))))
                      ((rule-processed-p query)
                       (list :processed))))))

;;;
;;;
;;;

(defmethod describe-query ((query query) &optional rewritten-p)
  (let* ((substrate (substrate query))
         (tbox-query-p (is-racer-tbox-mirror-substrate-p substrate)))
    
    `(,(iterator-id query)
      ,(describe-query-status query)
      ,(remove nil 
               (list (if tbox-query-p
                         'tbox-retrieve 
                       'retrieve)
           
                     (if (not rewritten-p)
                         (original-query-head query)
                       (query-head query))

                     (if (not rewritten-p)
                         (original-query-body query)
                       (query-body query))

                     (when tbox-query-p 
                       :tbox)
                     (when tbox-query-p 
                       (mirror-of-tbox substrate))
                     
                     (unless tbox-query-p
                       :abox)
                     (unless tbox-query-p 
                       (abox substrate)))))))


(defmethod describe-rule ((query query) &optional rewritten-p)
  `(,(iterator-id query)
    ,(describe-rule-status query)
    ,(list 'prepare-abox-rule
           (if (not rewritten-p)
               (original-rule-body query)
             (rule-body query))
           (if (not rewritten-p)
               (first (original-description (parser query)))
             (rule-con-pattern query))
           :abox (abox (substrate query)))))


(defmethod rule-applicable-p ((query query) &rest args)  
  (unless (rule-active-p query)
    (multiple-value-bind (status query)
        (reprepare-query query)
      (declare (ignorable status))
      (prog1
          (let ((*add-rule-consequences-p* nil)
                (*proactive-tuple-computation-p* t)
                (*tuple-at-a-time-p* nil)
                (*how-many* 1)
                (*two-phase-processing-p* nil))
	    
            (setf (slot-value query 'modifies-state-p) nil)

            (apply #'execute-query query :how-many 1 args)

            (setf (slot-value query 'modifies-state-p) t)
            ;;; WICHTIG! NICHT EXECUTE-RULE!!!
            ;;; umschaltung auf proactive wichtig hier!
            (bindings-found-p query))
        (reprepare-query query)))))

;;;
;;;
;;;       

(defmethod answer-query ((query t) (result-vois list)
                         &rest args
                         &key 
                         (execute-p t) 
                         (substrate *cur-substrate*) &allow-other-keys)
  
  (unless substrate
    (nrql-error "Parser error: No substrate given!"))

  (let ((*package* 
         #+(and :lracer (not :midelora))
         (racer-package *cur-substrate*)
         #-(and :lracer (not :midelora))
         *package*)

        (query2 nil))

    (with-timeout-cleanup
     (setf query2
           (apply #'prepare-query query result-vois substrate args))
     
     (progn 
       ;;; hier muss tatsaechlich nicht mehr gemacht
       ;;; werden, habe ich analysiert! 
       (return-from answer-query :timeout)))

    (when query2

      (if execute-p 
          (progn
            ;;; wird in without-timeout ausgefuehrt, 
            (prepare-to-run-query query2)
            
            ;;; timeout aktiv! 
            (if (is-rule-p query2)
                (apply #'execute-rule query2 args)
              (apply #'execute-query query2 args)))

        ;;; wird in without-timeout ausgefuehrt
        (prepare-to-run-query query2)))))
      


(defmethod apply-rule ((query t) (rule-con-pattern list)
                       &rest args
                       &key (execute-p t)
                       (substrate *cur-substrate*) &allow-other-keys)
  
  (unless substrate
    (nrql-error "Parser error: No substrate given!"))

  (let ((query2 nil))

    (with-timeout-cleanup
     (setf query2
           (apply #'prepare-rule query rule-con-pattern substrate args))
     
     (progn 
       (return-from apply-rule :timeout)))
    
    (when query2 

      (if execute-p
          (progn 
            (prepare-to-run-rule query2)
            (apply #'execute-rule query2 args))
      
        (prepare-to-run-rule query2)))))


