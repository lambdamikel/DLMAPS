;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;; 
;;;


(nrql-defun delete-all-queries ()
  (abort-all-queries)
  (setf *all-queries* nil
        *ready-queries* nil
        *active-queries* nil
        *processed-queries* nil)
  :okay-all-queries-deleted)

(nrql-defun delete-all-rules ()
  (abort-all-rules)
  (setf *all-rules* nil
        *ready-rules* nil
        *active-rules* nil
        *processed-rules* nil)
  :okay-all-rules-deleted)

(nrql-defun reset-all-substrates ()
  (mapc #'substrate-needs-reset *all-substrates*))

(nrql-defun reset-nrql-engine (&key full-reset-p)
  (abort-all-queries)
  (abort-all-rules)
        
  (if full-reset-p
      (progn 
        #+:midelora
        (prover::delete-all-tboxes)
        (delete-all-tboxes)
        
        (setf *iterator-id* 0)
        (delete-all-queries)
        (delete-all-rules)
        (delete-all-substrates)
        (delete-all-dboxes))
    
    (reset-all-substrates))

  (restore-standard-settings)
  
  (if full-reset-p 
      :okay-full-reset
    :okay-engine-reset))
      
(nrql-defun full-reset ()
  (reset-nrql-engine :full-reset-p t))

;;;
;;;
;;;

(nrql-defmethod delete-query ((query null))
  :not-found)

(nrql-defmethod delete-query ((query symbol))
  (delete-query (find-query query)))  

(nrql-defmethod delete-rule ((query null))
  :not-found)

(nrql-defmethod delete-rule ((query symbol))
  (delete-rule (find-rule query)))


;;;
;;;
;;;

(nrql-defun racer-answer-query (res-args query &rest args)
  (apply #'racer-prepare-query res-args query :execute-p t args))

#+:midelora
(midelora-defun midelora-answer-query (res-args query &rest args)
                (apply #'midelora-prepare-query res-args query :execute-p t args))

(nrql-defun racer-answer-query-under-premise (premise res-args query &rest args)
  (apply #'racer-prepare-query res-args query 
         :premise premise 
         :execute-p t args))

#+:midelora
(midelora-defun midelora-answer-query-under-premise (premise res-args query &rest args)
                (apply #'midelora-prepare-query res-args query 
                       :premise premise 
                       :execute-p t args))

(nrql-defun racer-prepare-query (res-args query &rest args)
  (with-racer-timeout
   (if (ensure-deadlock-prevention)
       :denied-due-to-deadlock-prevention
     (progn
       (apply #'racer-prepare-substrate args)
       (apply #'answer-query query res-args (append args (list :execute-p nil)))))))

#+:midelora
(midelora-defun midelora-prepare-query (res-args query &rest args)
                (if (ensure-deadlock-prevention)
                    :denied-due-to-deadlock-prevention
                  (progn
                    (apply #'midelora-prepare-substrate args)
                    (apply #'answer-query query res-args (append args (list :execute-p nil))))))

;;;
;;;
;;;

(nrql-defun prepare-nrql-engine (abox &rest args)
  (with-racer-timeout
   (apply #'eval-nrql-settings
          #'(lambda ()
              (apply #'racer-prepare-substrate 
                     :abox abox
                     :prepare-now-p t
                     :initial-abox-mirroring-p t
                     args))
          args)

   abox))

;;;
;;;
;;;

(nrql-defun racer-apply-rule (query res-args &rest args)
  (apply #'racer-prepare-rule query res-args :execute-p t args))


#+:midelora
(midelora-defun midelora-apply-rule (query res-args &rest args)
                (apply #'midelora-prepare-rule query res-args :execute-p t args))
    
(nrql-defun racer-prepare-rule (query res-args &rest args)                                   
  (with-racer-timeout
   (apply #'racer-prepare-substrate args)    
   (apply #'apply-rule query res-args (append args (list :execute-p nil)))))

    
#+:midelora
(midelora-defun midelora-prepare-rule (query res-args &rest args)                                   
                (apply #'midelora-prepare-substrate args)
                (apply #'apply-rule query res-args (append args (list :execute-p nil))))

;;;
;;; 
;;;

(nrql-defmacro (retrieve :nrql-function racer-answer-query))

#+:midelora
(midelora-defmacro (midelora-retrieve :nrql-function midelora-answer-query))


(nrql-defmacro (retrieve-under-premise :nrql-function racer-answer-query-under-premise))
#+:midelora
(midelora-defmacro (midelora-retrieve-under-premise :nrql-function midelora-answer-query-under-premise))


(nrql-defmacro (tbox-retrieve :nrql-function racer-answer-tbox-query))
#+:midelora
(midelora-defmacro (midelora-tbox-retrieve :nrql-function midelora-answer-tbox-query))


(nrql-defmacro (apply-abox-rule :nrql-function racer-apply-rule))
#+:midelora
(midelora-defmacro (midelora-apply-abox-rule :nrql-function midelora-apply-rule))


;;;
;;;
;;;

(nrql-defmacro (prepare-abox-query :nrql-function racer-prepare-query))
#+:midelora
(midelora-defmacro (midelora-prepare-abox-query :nrql-function midelora-prepare-query))


(nrql-defmacro (prepare-tbox-query :nrql-function racer-prepare-tbox-query))
#+:midelora
(midelora-defmacro (midelora-prep-tbox-query :nrql-function midelora-prepare-tbox-query))


(nrql-defmacro (prepare-abox-rule :nrql-function racer-prepare-rule))
#+:midelora
(midelora-defmacro (midelora-prepare-abox-rule :nrql-function midelora-prepare-rule))

;;;
;;;
;;;

(nrql-defmacro (firerule :nrql-function racer-apply-rule))

#+:midelora
(midelora-defmacro (midelora-firerule :nrql-function midelora-apply-rule))


(nrql-defmacro (preprule :nrql-function racer-prepare-rule))
#+:midelora
(midelora-defmacro (midelora-preprule :nrql-function midelora-prepare-rule))

;;;
;;;
;;;

(nrql-defun racer-answer-tbox-query (res-args query &rest args)
  (apply #'racer-prepare-tbox-query res-args query :execute-p t args))

#+:midelora
(midelora-defun midelora-answer-tbox-query (res-args query &rest args)
                (apply #'midelora-prepare-tbox-query res-args query :execute-p t args))


(nrql-defun racer-prepare-tbox-query (res-args query &rest args)
  (with-racer-timeout
   (let ((*use-repository-p* nil)
         (*rewrite-semantically-p* nil)
         (*report-inconsistent-queries-p* nil)
         (*report-tautological-queries-p* nil))

     (apply #'racer-prepare-tbox-substrate args)
     (apply #'answer-query query res-args args))))

#+:midelora
(midelora-defun midelora-prepare-tbox-query (res-args query &rest args)
                (with-racer-timeout
                 (let ((*use-repository-p* nil)
                       (*rewrite-semantically-p* nil)
                       (*report-inconsistent-queries-p* nil)
                       (*report-tautological-queries-p* nil))

                   (apply #'midelora-prepare-tbox-substrate args)
                   (apply #'answer-query query res-args args))))

;;;
;;;
;;;

(nrql-defun define-query (name head body &rest args 
                               &key keep-p (tbox (or *nrql-tbox* (current-tbox)))
                               &allow-other-keys)
  (declare (ignorable keep-p))
  (apply #'define-query1 name head body :tbox tbox args))

(nrql-defun define-and-execute-query (name head body &rest args)
  (apply #'define-query name head body :execute-p t :keep-p t args))

(nrql-defun define-and-prepare-query (name head body &rest args)
  (apply #'define-query name head body :execute-p nil :keep-p t args))

(nrql-defun undefine-query (name &key (tbox (or *nrql-tbox* (current-tbox))))
  (undefine-query1 name :tbox tbox))

;;;
;;;
;;;

(nrql-defun delete-all-definitions (&key (tbox (or *nrql-tbox* (current-tbox))))
  (let ((dbox (find-dbox tbox)))
    (when dbox 
      (delete-all-definitions1 dbox)
      :okay-all-definitions-deleted)))

(nrql-defun describe-definition (name &key (tbox (or *nrql-tbox* (current-tbox))))
  (let ((dbox (find-dbox tbox)))
    (when dbox
      (describe-definition1 dbox name))))

(nrql-defun describe-all-definitions (&key (tbox (or *nrql-tbox* (current-tbox))))
  (let ((dbox (find-dbox tbox)))
    (when dbox
      (describe-all-definitions1 dbox))))

;;;
;;;
;;;

(nrql-defmacro (defquery :nrql-function define-query))

(nrql-defmacro (def-and-prep-query :nrql-function define-and-prepare-query))

(nrql-defmacro (def-and-exec-query :nrql-function define-and-execute-query))

(nrql-defmacro (undefquery :nrql-function undefine-query))

;;;
;;; 
;;;

(nrql-defmethod get-next-tuple ((query null))
  :not-found)

(nrql-defmethod get-next-tuple ((query symbol))
  (with-racer-timeout
   (get-next-tuple (or (find-query query *active-queries*)
                       (find-query query *processed-queries*)))))

;;;
;;; 
;;;

(nrql-defmethod get-current-tuple ((query null))
  :not-found)

(nrql-defmethod get-current-tuple ((query symbol))
  (get-current-tuple (find-query query)))

;;;
;;; 
;;;

(nrql-defmethod get-next-set-of-rule-consequences ((query null))
  :not-found)

(nrql-defmethod get-next-set-of-rule-consequences ((query symbol))
  (with-racer-timeout
   (get-next-tuple (or (find-rule query *active-rules*)
                       (find-rule query *processed-rules*)))))


;;;
;;; 
;;;

(nrql-defmethod get-current-set-of-rule-consequences ((query null))
  :not-found)

(nrql-defmethod get-current-set-of-rule-consequences ((query symbol))
  (get-current-set-of-rule-consequences (find-rule query)))

;;;
;;;
;;;

(nrql-defmethod next-tuple-available-p ((query null))
  :not-found)

(nrql-defmethod next-tuple-available-p ((query symbol))
  (next-tuple-available-p (find-query query)))

;;;
;;;
;;;

(nrql-defmethod next-set-of-rule-consequences-available-p ((query null))
  :not-found)

(nrql-defmethod next-set-of-rule-consequences-available-p ((query symbol))
  (next-set-of-rule-consequences-available-p (find-rule query)))

;;;
;;;
;;;

(nrql-defmethod query-ready-p ((query null))
  :not-found)

(nrql-defmethod query-ready-p ((query symbol))
  (when (find-query query *ready-queries*)
    t))

;;;
;;;
;;;

(nrql-defmethod rule-ready-p ((query null))
  :not-found)

(nrql-defmethod rule-ready-p ((query symbol))
  (when (find-rule query *ready-rules*)
    t))

;;;
;;;
;;;

(nrql-defmethod execute-query ((query null) &rest args)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod execute-query ((query symbol) &rest args)
  (with-racer-timeout
   (apply #'execute-query (find-query query *ready-queries*) args)))

;;;
;;;
;;;

(nrql-defmethod execute-rule ((query null) &rest args)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod execute-rule ((query t) &rest args)
  (with-racer-timeout
   (apply #'execute-rule (find-rule query *ready-rules*) args)))

;;;
;;;
;;;

(nrql-defmethod reprepare-query ((query null))
  :not-found)

(nrql-defmethod reprepare-query ((query t))
  (with-racer-timeout
   (reprepare-query (find-query query *processed-queries*))))

;;;
;;;
;;;

(nrql-defmethod reprepare-rule ((query null))
  :not-found)

(nrql-defmethod reprepare-rule ((query t))
  (with-racer-timeout
   (reprepare-rule (find-rule query *processed-rules*))))

;;;
;;;
;;;

(nrql-defmethod reexecute-query ((query null) &rest args)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod reexecute-query ((query t) &rest args)
  (with-racer-timeout
   (apply #'reexecute-query (find-query query *processed-queries*) args)))

;;;
;;;
;;;

(nrql-defmethod reexecute-rule ((query null) &rest args)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod reexecute-rule ((query t) &rest args)
  (with-racer-timeout
   (apply #'reexecute-rule (find-rule query *processed-rules*) args)))

;;;
;;;
;;;

(nrql-defmethod query-prepared-p ((query t))  
  (query-ready-p query))

(nrql-defmethod rule-prepared-p ((query t))  
  (rule-ready-p query))

;;;
;;;
;;;

(nrql-defmethod rule-applicable-p ((query null) &rest args)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod rule-applicable-p ((query symbol) &rest args)
  (declare (ignorable args))
  (with-racer-timeout
   (rule-applicable-p (find-rule query))))

;;;
;;;
;;;

(nrql-defmethod choose-current-set-of-rule-consequences ((query null) &rest args)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod choose-current-set-of-rule-consequences ((query symbol) &rest args)
  (declare (ignorable args))
  (choose-current-set-of-rule-consequences (find-rule query)))

;;;
;;;
;;;

(nrql-defmethod add-chosen-sets-of-rule-consequences ((query null) &rest args)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod add-chosen-sets-of-rule-consequences ((query symbol) &rest args)
  (declare (ignorable args))
  (let ((bad-rules (remove-if #'proactive-tuple-computation-p *active-rules*)))
    (if bad-rules 
        (progn 
          (rule-deadlock-warning bad-rules)
          :denied-due-to-deadlock-prevention)
      (let ((bad-queries
             (remove-if #'(lambda (x) 
                            (or (proactive-tuple-computation-p x)
                                (not (modifies-state-p x))))
                        *active-queries*)))
        (if bad-queries
            (progn 
              (query-deadlock-warning bad-queries)
              :denied-due-to-deadlock-prevention)
          (add-chosen-sets-of-rule-consequences (find-rule query)))))))

;;;
;;;
;;;

(defun applicable-rules1 ()
  (nconc 
   (remove-if-not #'rule-applicable-p *ready-rules*)
   (remove-if-not #'rule-applicable-p *processed-rules*)))

(defun unapplicable-rules1 ()
  (nconc 
   (remove-if #'rule-applicable-p *ready-rules*)
   (remove-if #'rule-applicable-p *processed-rules*)))


(nrql-defmethod applicable-rules ()
  (with-racer-timeout
   (mapcar #'iterator-id (applicable-rules1))))

(nrql-defmethod unapplicable-rules ()
  (with-racer-timeout
   (mapcar #'iterator-id (unapplicable-rules1))))


;;;
;;;
;;;

(nrql-defmethod execute-applicable-rules ()
  (with-racer-timeout
   (mapcar #'execute-rule (applicable-rules1))))

;;;
;;;
;;;

(nrql-defmethod query-waiting-p ((query null))
  :not-found)

(nrql-defmethod query-waiting-p ((query symbol))
  (query-waiting-p (find-query query)))

;;;
;;;
;;;

(nrql-defmethod rule-waiting-p ((query null))
  :not-found)

(nrql-defmethod rule-waiting-p ((query symbol))
  (rule-waiting-p (find-rule query)))

;;;
;;; query-running-p macht keinen Sinn, wenn schon query-waiting-p da ist!!!
;;;

;;;
;;;
;;;

(nrql-defmethod query-active-p ((query null))
  :not-found)

(nrql-defmethod query-active-p ((query symbol))
  (query-active-p (find-query query)))

;;;
;;;
;;;

(nrql-defmethod rule-active-p ((query null))
  :not-found)

(nrql-defmethod rule-active-p ((query symbol))
  (rule-active-p (find-rule query)))

;;;
;;;
;;;

(nrql-defmethod cheap-query-p ((query null))
  :not-found)

(nrql-defmethod cheap-query-p ((query symbol))
  (cheap-query-p (find-query query)))

;;;
;;;
;;;

(nrql-defmethod cheap-rule-p ((query null))
  :not-found)

(nrql-defmethod cheap-rule-p ((query symbol))
  (cheap-rule-p (find-rule query)))

;;;
;;;
;;;

(nrql-defmethod active-expensive-query-p ((query null))
  :not-found)

(nrql-defmethod active-expensive-query-p ((query symbol))
  (expensive-query-p (find-query query)))

;;;
;;;
;;;

(nrql-defmethod active-expensive-rule-p ((query null))
  :not-found)

(nrql-defmethod active-expensive-rule-p ((query symbol))
  (expensive-rule-p (find-rule query)))

;;;
;;;
;;; 

(nrql-defun all-queries ()
  (mapcar #'iterator-id *all-queries*))

(nrql-defun accurate-queries ()
  (mapcar #'iterator-id (remove-if-not #'query-accurate-p *processed-queries*)))

(nrql-defun inaccurate-queries ()
  (mapcar #'iterator-id (remove-if #'query-accurate-p *processed-queries*)))

(nrql-defun cheap-queries ()
  (nconc (mapcar #'iterator-id (remove-if-not #'cheap-query-p *ready-queries*))
         (mapcar #'iterator-id (remove-if-not #'cheap-query-p *active-queries*))))

(nrql-defun expensive-queries ()
  (nconc (mapcar #'iterator-id (remove-if-not #'expensive-query-p *ready-queries*))
         (mapcar #'iterator-id (remove-if-not #'expensive-query-p *active-queries*))))

;;;
;;;
;;; 

(nrql-defun all-rules ()
  (mapcar #'iterator-id *all-rules*))

(nrql-defun accurate-rules ()
  (mapcar #'iterator-id (remove-if-not #'rule-accurate-p *processed-rules*)))

(nrql-defun inaccurate-rules ()
  (mapcar #'iterator-id (remove-if #'rule-accurate-p *processed-rules*)))

(nrql-defun cheap-rules ()
  (nconc (mapcar #'iterator-id (remove-if-not #'cheap-rule-p *ready-rules*))
         (mapcar #'iterator-id (remove-if-not #'cheap-rule-p *active-rules*))))

(nrql-defun expensive-rules ()
  (nconc (mapcar #'iterator-id (remove-if-not #'expensive-rule-p *ready-rules*))
         (mapcar #'iterator-id (remove-if-not #'expensive-rule-p *active-rules*))))

;;;
;;;
;;;

(nrql-defun ready-queries ()
  (mapcar #'iterator-id *ready-queries*))

(nrql-defun prepared-queries ()
  (ready-queries))

;;;
;;;
;;;

(nrql-defun ready-rules ()
  (mapcar #'iterator-id *ready-rules*))

(nrql-defun prepared-rules ()
  (ready-rules))

;;;
;;;
;;;

(nrql-defun active-queries ()
  (mapcar #'iterator-id *active-queries*))

(nrql-defun active-cheap-queries ()
  (mapcar #'iterator-id (remove-if-not #'cheap-query-p *active-queries*)))

(nrql-defun active-expensive-queries ()
  (mapcar #'iterator-id (remove-if #'cheap-query-p *active-queries*)))

;;;
;;;
;;;

(nrql-defun active-rules ()
  (mapcar #'iterator-id *active-rules*))

(nrql-defun active-cheap-rules ()
  (mapcar #'iterator-id (remove-if-not #'cheap-rule-p *active-rules*)))

(nrql-defun active-expensive-rules ()
  (mapcar #'iterator-id (remove-if #'cheap-rule-p *active-rules*)))

;;;
;;;
;;;


(nrql-defun running-queries ()
  (mapcar #'iterator-id (remove-if-not #'query-running-p *active-queries*)))

(nrql-defun running-cheap-queries ()
  (mapcar #'iterator-id
          (remove-if-not #'(lambda (x) 
                             (and (query-running-p x)
                                  (cheap-query-p x)))
                         *active-queries*)))

(nrql-defun running-expensive-queries ()
  (mapcar #'iterator-id
          (remove-if-not #'(lambda (x) 
                             (and (query-running-p x)
                                  (expensive-query-p x)))
                         *active-queries*)))

;;;
;;;
;;;


(nrql-defun running-rules ()
  (mapcar #'iterator-id (remove-if-not #'rule-running-p *active-rules*)))

(nrql-defun running-cheap-rules ()
  (mapcar #'iterator-id
          (remove-if-not #'(lambda (x) 
                             (and (rule-running-p x)
                                  (cheap-rule-p x)))
                         *active-rules*)))

(nrql-defun running-expensive-rules ()
  (mapcar #'iterator-id
          (remove-if-not #'(lambda (x) 
                             (and (rule-running-p x)
                                  (expensive-rule-p x)))
                         *active-rules*)))

;;;
;;;
;;;

(nrql-defun waiting-queries ()
  (mapcar #'iterator-id (remove-if-not #'query-waiting-p *active-queries*)))

(nrql-defun waiting-cheap-queries ()
  (mapcar #'iterator-id (remove-if-not #'(lambda (x) 
                                           (and (query-waiting-p x)
                                                (cheap-query-p x)))
                                       *active-queries*)))

(nrql-defun waiting-expensive-queries ()
  (mapcar #'iterator-id (remove-if-not #'(lambda (x) 
                                           (and (query-waiting-p x)
                                                (expensive-query-p x)))
                                       *active-queries*)))

;;;
;;;
;;;


(nrql-defun waiting-rules ()
  (mapcar #'iterator-id (remove-if-not #'rule-waiting-p *active-rules*)))

(nrql-defun waiting-cheap-rules ()
  (mapcar #'iterator-id (remove-if-not #'(lambda (x) 
                                           (and (rule-waiting-p x)
                                                (cheap-rule-p x)))
                                       *active-rules*)))


(nrql-defun waiting-expensive-rules ()
  (mapcar #'iterator-id (remove-if-not #'(lambda (x) 
                                           (and (rule-waiting-p x)
                                                (expensive-rule-p x)))
                                       *active-rules*)))

;;;
;;;
;;;

(nrql-defun processed-queries ()
  (mapcar #'iterator-id *processed-queries*))

(nrql-defun inactive-queries ()
  (processed-queries))

(nrql-defun terminated-queries ()
  (processed-queries))


;;;
;;;
;;;

(nrql-defun processed-rules ()
  (mapcar #'iterator-id *processed-rules*))

(nrql-defun inactive-rules ()
  (processed-rules))

(nrql-defun terminated-rules ()
  (processed-rules))

;;;
;;;
;;;

(nrql-defun abort-all-queries ()
  (let ((queries *active-queries*))
    (mapc #'abort-query queries))
  :okay-all-queries-aborted)

(nrql-defun abort-all-rules ()
  (let* ((rules *active-rules*))
    (mapc #'abort-rule rules))
  :okay-all-rules-aborted)

;;;
;;;
;;;

(nrql-defun execute-all-queries (&rest args)
  (with-racer-timeout
   (mapcar #'(lambda (query) 
               (apply #'execute-query query args))
           *ready-queries*)))

(nrql-defun reexecute-all-queries (&rest args)
  (with-racer-timeout
   (mapcar #'(lambda (query) 
               (apply #'reexecute-query query args))
           *processed-queries*)))

(nrql-defun run-all-queries (&rest args)
  (apply #'execute-all-queries args))


;;;
;;;
;;;

(nrql-defun execute-all-rules (&rest args)
  (with-racer-timeout
   (mapcar #'(lambda (x) 
               (apply #'execute-rule x args)) 
           *ready-rules*)))

(nrql-defun reexecute-all-rules (&rest args)
  (with-racer-timeout
   (mapcar #'(lambda (x)
               (apply #'reexecute-rule x args))
           *processed-rules*)))

(nrql-defun run-all-rules (&rest args)
  (apply #'execute-all-rules args))

;;;
;;;
;;;

(nrql-defmethod query-processed-p ((query null))
  :not-found)

(nrql-defmethod query-processed-p ((query symbol))
  (query-processed-p (find-query query)))

(nrql-defmethod query-processed-p ((query query))
  (when (member query *processed-queries*) 
    t))

(nrql-defmethod query-inactive-p ((query t))
  (query-processed-p query))

;;;
;;;
;;;


(nrql-defmethod rule-processed-p ((query null))
  :not-found)

(nrql-defmethod rule-processed-p ((query symbol))
  (rule-processed-p (find-rule query)))

(nrql-defmethod rule-processed-p ((query query))
  (when (member query *processed-rules*)
    t))

(nrql-defmethod rule-inactive-p ((query t))
  (rule-processed-p query))

;;;
;;;
;;;

(nrql-defmethod describe-query-status ((query null))
  :not-found)

(nrql-defmethod describe-query-status ((query symbol))
  (describe-query-status (find-query query)))

;;;
;;;
;;;

(nrql-defmethod describe-rule-status ((query null))
  :not-found)

(nrql-defmethod describe-rule-status ((query symbol))
  (describe-rule-status (find-rule query)))

;;;
;;;
;;;

(nrql-defmethod get-next-n-remaining-tuples ((query null) &optional n)
  (declare (ignorable n))
  :not-found)

(nrql-defmethod get-next-n-remaining-tuples ((query symbol) &optional n)
  (declare (ignorable n))
  (with-racer-timeout
   (get-next-n-remaining-tuples (find-query query) n)))

;;;
;;;
;;;

(nrql-defmethod get-next-n-remaining-sets-of-rule-consequences ((query null) &optional n)
  (declare (ignorable n))
  :not-found)

(nrql-defmethod get-next-n-remaining-sets-of-rule-consequences ((query symbol) &optional n)
  (declare (ignorable n))
  (with-racer-timeout
   (get-next-n-remaining-tuples (find-rule query) n)))

;;;
;;;
;;;

(nrql-defmethod get-all-remaining-tuples ((query null))
  (declare (ignorable n))
  :not-found)

(nrql-defmethod get-all-remaining-tuples ((query symbol))
  (with-racer-timeout
   (get-all-remaining-tuples (find-query query))))

;;;
;;;
;;;

(nrql-defmethod get-all-remaining-sets-of-rule-consequences ((query null))
  (declare (ignorable n))
  :not-found)

(nrql-defmethod get-all-remaining-sets-of-rule-consequences ((query symbol))
  (with-racer-timeout
   (get-all-remaining-tuples (find-rule query))))


;;;
;;;
;;;

(nrql-defmethod get-answer ((query null))
  :not-found)

(nrql-defun get-answer-size (query &optional execute-p)
  (when (and execute-p 
             (query-prepared-p query)
             (not (query-active-p query))
             (not (query-processed-p query)))
    (execute-query query))
           
  (let ((answer (get-answer query)))
    (if (eq answer :not-found)
        :not-found
      (if (consp answer)
          (length answer)
        answer))))

(nrql-defmethod get-answer ((query symbol))
  (with-racer-timeout
   (get-answer (or (find-query query *active-queries*)
                   (find-query query *processed-queries*)
                  
                   (find-rule query *active-rules*)
                   (find-rule query *processed-rules*)))))

;;;
;;;
;;;

(nrql-defmethod query-accurate-p ((query null))
  :not-found)

(nrql-defmethod query-accurate-p ((query symbol))
  (query-accurate-p (find-query query *processed-queries*)))

;;;
;;;
;;;

(nrql-defmethod rule-accurate-p ((query null))
  :not-found)

(nrql-defmethod rule-accurate-p ((query symbol))
  (rule-accurate-p (find-rule query *processed-rules*)))

(nrql-defmethod rule-accurate-p ((query query))
  (query-accurate-p query))

;;;
;;;
;;;

(nrql-defmethod query-head ((query null))
  :not-found)

(nrql-defmethod query-head ((query query))
  (tree-map #'(lambda (x) 
                (if (is-voi-p x)
                    (textual-description x)
                  x))
            (answer-pattern query)))

(nrql-defmethod query-head ((query symbol))
  (query-head (find-query query)))

;;;
;;;
;;;

(nrql-defmethod rule-head ((query null))
  :not-found)

(nrql-defmethod rule-head ((query query))
  (tree-map #'(lambda (x) 
                (if (is-voi-p x)
                    (textual-description x)
                  x))
            (answer-pattern query)))

(nrql-defmethod rule-head ((query symbol))
  (rule-head (find-rule query)))

;;;
;;;
;;;

(nrql-defmethod original-query-head ((query null))
  :not-found)

(nrql-defmethod original-query-head ((query query))
  (first (original-description (parser query))))

(nrql-defmethod original-query-head ((query symbol))
  (original-query-head (find-query query)))


;;;
;;;
;;;

(nrql-defmethod original-rule-head ((query null))
  :not-found)

(nrql-defmethod original-rule-head ((query query))
  (first (original-description (parser query))))

(nrql-defmethod original-rule-head ((query symbol))
  (original-rule-head (find-rule query)))

;;;
;;;
;;;

(nrql-defmethod query-body ((query null))
  :not-found)

(nrql-defmethod query-body ((query query))
  (unparse-query query))

(nrql-defmethod query-body ((query symbol))
  (query-body (find-query query)))


;;;
;;;
;;;

(nrql-defmethod rule-body ((query null))
  :not-found)

(nrql-defmethod rule-body ((query query))
  (unparse-query query))

(nrql-defmethod rule-body ((query symbol))
  (rule-body (find-rule query)))

;;;
;;;
;;;

(nrql-defmethod original-query-body ((query null))
  :not-found)

(nrql-defmethod original-query-body ((query query))
  (second (original-description (parser query))))

(nrql-defmethod original-query-body ((query symbol))
  (original-query-body (find-query query)))


;;;
;;;
;;;

(nrql-defmethod original-rule-body ((query null))
  :not-found)

(nrql-defmethod original-rule-body ((query query))
  (second (original-description (parser query))))

(nrql-defmethod original-rule-body ((query symbol))
  (original-rule-body (find-rule query)))


;;;
;;;
;;;

(nrql-defun get-all-answers ()
  (with-racer-timeout
   (mapcar #'(lambda (q)
               (list (iterator-id q)
                     (get-answer q)))
           (append *active-queries*
                   *processed-queries*
                   *active-rules*
                   *processed-rules*))))

;;;
;;;
;;;

(nrql-defun describe-all-queries (&optional (rewritten-p t))
  (mapcar #'(lambda (x) 
              (describe-query x rewritten-p))
          *all-queries*))

(nrql-defun describe-all-rules (&optional (rewritten-p t))
  (mapcar #'(lambda (x) 
              (describe-rule x rewritten-p))
          *all-rules*))

;;;
;;;
;;;

(nrql-defun wait-for-queries-to-terminate ()
  (with-racer-timeout
   (if (every #'proactive-tuple-computation-p *active-queries*)
       (progn 
         ;;; auf automatische Terminierung warten! 
         (process-wait (not *active-queries*))
         :okay)

     (progn
      
       (query-deadlock-warning
        (remove-if #'proactive-tuple-computation-p *active-queries*))
    
       :denied-due-to-deadlock-prevention))))

(nrql-defun wait-for-rules-to-terminate ()
  (with-racer-timeout
   (if (every #'proactive-tuple-computation-p *active-rules*)
       (progn 
         (process-wait (not *active-rules*))
         :okay)

     (progn

       (rule-deadlock-warning
        (remove-if #'proactive-tuple-computation-p *active-rules*))
    
       :denied-due-to-deadlock-prevention))))

;;;
;;;
;;;

(nrql-defmethod abort-query ((query null))
  :not-found)

(nrql-defmethod abort-query ((query symbol))
  (abort-query (find-query query *active-queries*)))

;;;
;;;
;;;

(nrql-defmethod abort-rule ((query null))
  :not-found)

(nrql-defmethod abort-rule ((query symbol))
  (abort-rule (find-rule query *active-rules*)))

;;;
;;;
;;;

#-:midelora
(nrql-defun enable-data-substrate-mirroring ()
  (setf *type-of-substrate* 'mirror-data-substrate)
  (setf *initial-abox-mirroring-p* t)
  :okay-data-substrate-mirroring-enabled)

#-:midelora
(nrql-defun disable-data-substrate-mirroring ()
  (setf *type-of-substrate* 'racer-dummy-substrate)
  :okay-data-substrate-mirroring-disabled)

;;;
;;;
;;;

#-:midelora
(nrql-defun enable-sql-data-substrate-mirroring ()
  (setf *type-of-substrate* 'mirror-sql-data-substrate)
  (setf *initial-abox-mirroring-p* t)
  :okay-sql-data-substrate-mirroring-enabled)

#-:midelora
(nrql-defun disable-sql-data-substrate-mirroring ()
  (setf *type-of-substrate* 'racer-dummy-substrate)
  :okay-sql-data-substrate-mirroring-disabled)

;;;
;;;
;;;

(nrql-defun enable-abox-mirroring ()
  (setf *initial-abox-mirroring-p* t
        *ensure-tbox-classification-p* nil
        *classify-concepts-in-instance-assertions-p* nil)
  
  :okay-abox-mirroring-enabled)

(nrql-defun enable-smart-abox-mirroring ()
  (setf *initial-abox-mirroring-p* t
        *ensure-tbox-classification-p* t
        *classify-concepts-in-instance-assertions-p* nil)
  
  :okay-smart-abox-mirroring-enabled)

(nrql-defun enable-very-smart-abox-mirroring ()
  (setf *initial-abox-mirroring-p* t
        *ensure-tbox-classification-p* t
        *classify-concepts-in-instance-assertions-p* t)

  :okay-very-smart-abox-mirroring-enabled)


(nrql-defun disable-abox-mirroring ()
  (setf *initial-abox-mirroring-p* nil)

  :okay-abox-mirroring-disabled)

;;;
;;;
;;;

(nrql-defun enable-two-phase-query-processing-mode ()
  (setf *two-phase-processing-p* t
        *initial-abox-mirroring-p* t
        *ensure-tbox-classification-p* t
        *told-information-reasoning-p* nil
        *tuple-at-a-time-p* t
        *proactive-tuple-computation-p* nil
        *check-abox-consistency-p* nil
        *deliver-kb-has-changed-warning-tokens-p* t
        *deliver-phase-two-warning-tokens-p* t)
  
  :okay-two-phase-query-processing-mode-enabled)

(nrql-defun disable-two-phase-query-processing-mode ()
  (setf *two-phase-processing-p* nil)
  
  :okay-two-phase-query-processing-mode-disabled)

;;;
;;;
;;;

(nrql-defun enable-phase-two-starts-warning-tokens ()
  (if *two-phase-processing-p*
      (progn 
        (setf *deliver-phase-two-warning-tokens-p* t)

        :okay-phase-two-warning-tokens-enabled)
    
    :ignored-not-in-two-phase-processing-mode))

(nrql-defun disable-phase-two-starts-warning-tokens ()
  (if *two-phase-processing-p*
      (progn 
        (setf *deliver-phase-two-warning-tokens-p* nil)

        :okay-phase-two-warning-tokens-disabled)
    
    :ignored-not-in-two-phase-processing-mode))

;;;
;;;
;;;

(nrql-defun enable-kb-has-changed-warning-tokens ()
  (if *tuple-at-a-time-p*
      (progn 
        (setf *deliver-kb-has-changed-warning-tokens-p* t)

        :okay-kb-has-changed-warning-tokens-enabled)

    :ignored-not-in-tuple-at-a-time-mode))

(nrql-defun disable-kb-has-changed-warning-tokens ()
  (if *tuple-at-a-time-p*
      (progn 

        (setf *deliver-kb-has-changed-warning-tokens-p* nil)

        :okay-kb-has-changed-warning-tokens-disabled)

    :ignored-not-in-tuple-at-a-time-mode))

;;;
;;;
;;;


(nrql-defun enable-nrql-warnings ()
  (setf *deliver-kb-has-changed-warning-tokens-p* t
        *deliver-phase-two-warning-tokens-p* t
        *warnings-p* t)

  :okay-warnings-enabled)


(nrql-defun disable-nrql-warnings ()
  (setf *deliver-kb-has-changed-warning-tokens-p* nil
        *deliver-phase-two-warning-tokens-p* nil
        *warnings-p* nil)

  :okay-warnings-disabled)


;;;
;;;
;;;

(nrql-defun enable-told-information-querying ()
  (setf *told-information-reasoning-p* t
        *check-abox-consistency-p* nil
        *ensure-tbox-classification-p* nil
        *classify-concepts-in-instance-assertions-p* nil
        *initial-abox-mirroring-p* t)
  
  :okay-told-information-querying-enabled)

(nrql-defun disable-told-information-querying ()
  (setf *told-information-reasoning-p* nil)

  :okay-told-information-querying-disabled)

;;;
;;;
;;;

(nrql-defun add-role-assertions-for-datatype-properties ()
  (setf *add-role-assertions-for-datatype-properties-p* t)

  :okay-adding-role-assertions-for-datatype-properties)


(nrql-defun dont-add-role-assertions-for-datatype-properties ()
  (setf *add-role-assertions-for-datatype-properties-p* nil)

  :okay-not-adding-role-assertions-for-datatype-properties)


;;;
;;;
;;;

(nrql-defun restore-standard-settings ()
  ;;(set-unique-name-assumption nil)

  #+:multiprocess-queries 
  (setf *multiprocess-queries* t)

  (setf *add-role-assertions-for-datatype-properties-p* nil
        
        *tuple-at-a-time-p* nil
        *allow-negated-roles-p* t
        *proactive-tuple-computation-p* t
        *two-phase-processing-p* nil
        *deliver-kb-has-changed-warning-tokens-p* t
        *deliver-phase-two-warning-tokens-p* nil
        *optimize-p* t        
        *optimizer-use-cardinality-heuristics-p* t
        *warnings-p* t
        *told-information-reasoning-p* nil
        *report-inconsistent-queries-p* nil
        *report-tautological-queries-p* nil
        *rewrite-to-dnf-p* t

        *rewrite-defined-concepts-p* nil

        *rewrite-semantically-p* nil
        *use-repository-p* nil
        *put-into-repository-p* nil
        *use-unique-name-assumption-p* t
        *how-many* nil
        *classify-concepts-in-instance-assertions-p* nil
        *ensure-tbox-classification-p* nil
        *check-abox-consistency-p* t
        *exclude-permutations-p* nil
        *initial-abox-mirroring-p* nil
        *add-rule-consequences-p* t
        *dont-add-abox-duplicates-p* nil
        
        #-:midelora
        *type-of-substrate* 
        #-:midelora
        'racer-dummy-substrate
        
        #+:midelora
        *type-of-substrate*
        #+:midelora
        'midelora-substrate
        
        *timeout* nil
        *syntactic-repository-p* nil)
        
  :okay-standard-settings-restored)
    
;;;
;;;
;;;
;;; 

(nrql-defun enable-eager-tuple-computation ()
  (if *tuple-at-a-time-p*
      (progn 
        (setf *proactive-tuple-computation-p* t)
        
        :okay-eager-mode-enabled)

    :ignored-not-in-tuple-at-a-time-mode))

(nrql-defun enable-lazy-tuple-computation ()
  (if *tuple-at-a-time-p*
      (progn 
        (setf *proactive-tuple-computation-p* nil)
        
        :okay-lazy-mode-enabled)

    :ignored-not-in-tuple-at-a-time-mode))

;;;
;;;
;;;

(nrql-defun check-abox-consistency-before-querying ()
  (setf *check-abox-consistency-p* t)

  :okay-checking-abox-consistency-before-querying)

(nrql-defun dont-check-abox-consistency-before-querying ()
  (setf *check-abox-consistency-p* nil)

  :okay-not-checking-abox-consistency-before-querying)


;;;
;;;
;;;

(nrql-defun set-max-no-of-tuples-bound (&optional n)
  (setf *how-many* n))

(nrql-defun get-max-no-of-tuples-bound ()
  *how-many*)

;;;
;;;
;;;

;;;; (nrql-defun get-timeout ()
;;;;   *timeout*)

;;;
;;;
;;;


(nrql-defun process-tuple-at-a-time ()
  #+:multiprocess-queries
  (progn
    (setf *tuple-at-a-time-p* t
          *deliver-kb-has-changed-warning-tokens-p* t)

    :okay-processing-tuple-at-a-time)
  #-:multiprocess-queries
  (nrql-error "Tuple at a time querying mode only possible if :multiprocess-queries is on *features*! Recompile!"))
  

(nrql-defun process-set-at-a-time ()
  (setf *tuple-at-a-time-p* nil
        *proactive-tuple-computation-p* t)
  
  :okay-processing-set-at-a-time)

;;;
;;;
;;; 

(nrql-defun add-rule-consequences-automatically ()
  (setf *add-rule-consequences-p* t)
  :okay-adding-rule-consequences-automatically)

(nrql-defun dont-add-rule-consequences-automatically ()
  (setf *add-rule-consequences-p* nil)
  :okay-not-adding-rule-consequences-automatically)


;;;
;;;
;;; 

(nrql-defun exclude-permutations ()
  (setf *exclude-permutations-p* t)

  :okay-exluding-permuatation)

(nrql-defun include-permutations ()
  (setf *exclude-permutations-p* nil)

  :okay-including-permuations)

;;;
;;;
;;; 

(nrql-defun describe-current-substrate ()
  (if *cur-substrate*
      `((:name ,(name *cur-substrate*))
        (:type ,(type-of *cur-substrate*))
        (:associated-abox ,(abox *cur-substrate*))
        (:associated-tbox ,(tbox *cur-substrate*))
        #-:dlmaps ,@(when (is-data-substrate-p *cur-substrate*)
                      `((:no-of-nodes ,(length (get-nodes *cur-substrate*)))))
        #-:dlmaps ,@(when (is-rcc-substrate-p *cur-substrate*)
                      `((:rcc-tpye ,(rcc-type *cur-substrate*))))
        )
    :not-found))


(nrql-defun describe-query-processing-mode ()
  (let ((mode 
         (list

          (list :creating-substrates-of-type
                (to-keyword *type-of-substrate*))

          (when *check-abox-consistency-p*
            :check-abox-consistency)

          (when *ensure-tbox-classification-p*
            :classify-tbox)

          (when *exclude-permutations-p*
            :exclude-permutations)

          (when *optimize-p* 
            :query-optimization-enabled)

          (when *rewrite-defined-concepts-p* 
            :query-expansion-of-defined-concepts-enabled)

          (when (and *optimize-p* 
                     *optimizer-use-cardinality-heuristics-p*)
            :optimizer-uses-cardinality-heuristics)
          
          (when *add-rule-consequences-p*
            :automatically-adding-rule-consequences)

          (when *warnings-p*
            :warnings)

          (if *told-information-reasoning-p*
              :incomplete-mode
            :complete-mode)

          (when *initial-abox-mirroring-p* 
            (if *ensure-tbox-classification-p* 
                (if *classify-concepts-in-instance-assertions-p*
                    :very-smart-abox-mirroring
                  :smart-abox-mirroring)
              :abox-mirroring))

          (let ((vector 
                 (list *ensure-tbox-classification-p*
                       *classify-concepts-in-instance-assertions-p*
                       *told-information-reasoning-p*
                       *two-phase-processing-p*)))

            (cond ((equal vector '(nil nil t nil)) :mode-0)
                  ((equal vector '(t   nil t nil)) :mode-1)
                  ((equal vector '(t   t   t nil)) :mode-2)
                  ((equal vector '(t   nil nil t)) 
                   (if *tuple-at-a-time-p*
                       :mode-4
                     :mode-6))
                  ((equal vector '(t   t   nil t)) :mode-5)
                  (t :mode-3))))))

    (remove nil
            (append mode
                    
                    (if *tuple-at-a-time-p*
                        (cons :tuple-at-a-time-mode
                              (cons
                               (if *proactive-tuple-computation-p*
                                   :eager
                                 :lazy)
                               (when *two-phase-processing-p* 
                                 `(:two-phase-query-processing-mode
                                   ,@(when *deliver-phase-two-warning-tokens-p*
                                       '(:deliver-phase-two-warning-tokens))))))
                      '(:set-at-a-time-mode))

                    (when *deliver-kb-has-changed-warning-tokens-p*
                      (list :deliver-kb-has-changed-warning-tokens))

                    (when *add-role-assertions-for-datatype-properties-p*
                      (list :adding-role-assertions-for-datatype-properties))

                    (when (or *timeout*
                              #+:racer-server *server-timeout*)
                      (list (list :timeout-after 
                                  (or *timeout*
                                      #+:racer-server *server-timeout*)
                                  :seconds)))

                    (when *how-many*
                      (list (list :max-no-of-tuples *how-many*)))
                    
                    (when *report-inconsistent-queries-p*
                      (list :reporting-inconsistent-queries))
                    
                    (when *report-tautological-queries-p*
                      (list :reporting-tautological-queries))
                    
                    (when *rewrite-semantically-p* 
                      (list :query-realization-enabled))
                     
                    (when *use-repository-p*
                      (list :maintain-query-repository))))))
          
;;;
;;;
;;;

(nrql-defun report-inconsistent-queries ()
  (setf *rewrite-to-dnf-p* t
        *warnings-p* t
        *report-inconsistent-queries-p* t)

  :okay-reporting-inconsistent-queries)

(nrql-defun dont-report-inconsistent-queries ()
  (setf *report-inconsistent-queries-p* nil)

  :okay-not-reporting-inconsistent-queries)


;;;
;;;
;;;

(nrql-defun report-tautological-queries ()
  (setf *rewrite-to-dnf-p* t
        *warnings-p* t
        *report-tautological-queries-p* t)

  :okay-reporting-tautological-queries)

(nrql-defun dont-report-tautological-queries ()
  (setf *report-tautological-queries-p* nil)

  :okay-not-reporting-tautological-queries)


;;;
;;;
;;;


(nrql-defun enable-query-repository ()
  (setf *rewrite-to-dnf-p* t
        *warnings-p* t
        *put-into-repository-p* t
        *use-repository-p* t)

  :okay-query-repository-enabled)

(nrql-defun disable-query-repository ()
  (setf *use-repository-p* nil
        *put-into-repository-p* nil)

  :okay-query-repository-disabled)


;;;
;;;
;;;


(nrql-defun enable-query-optimization ()
  (setf *optimize-p* t
        *rewrite-to-dnf-p* t)
                          
  :okay-query-optimization-enabled)

(nrql-defun disable-query-optimization ()
  (setf *optimize-p* nil)
                          
  :okay-query-optimization-disabled)

;;;
;;;
;;;


(nrql-defun optimizer-use-cardinality-heuristics ()
  (if (not *optimize-p*)

      :ignored-optimizer-is-disabled 

    (progn 
      (setf *optimize-p* t
            *optimizer-use-cardinality-heuristics-p* t
            *rewrite-to-dnf-p* t)
      
      :okay-using-cardinality-heuristics)))

(nrql-defun optimizer-dont-use-cardinality-heuristics ()
  (setf *optimizer-use-cardinality-heuristics-p* nil)
                          
  :okay-not-using-cardinality-heuristics)

;;;
;;;
;;;


(nrql-defun enable-query-realization ()
  (setf *warnings-p* t
        *rewrite-to-dnf-p* t
        *rewrite-semantically-p* t)

  :okay-query-realization-enabled)

(nrql-defun disable-query-realization ()
  (setf *rewrite-semantically-p* nil)

  :okay-query-realization-disabled)

;;;
;;;
;;;

(nrql-defmethod query-consistent-p ((query null) &key &allow-other-keys)
  :not-found)

(nrql-defmethod query-consistent-p ((query symbol) &key &allow-other-keys)
  (with-racer-timeout
   (query-consistent-p (find-query query))))

;;;
;;;
;;;

(nrql-defmethod query-inconsistent-p ((query null) &key &allow-other-keys)
  :not-found)

(nrql-defmethod query-inconsistent-p ((query symbol) &key &allow-other-keys)
  (query-inconsistent-p (find-query query)))

    
;;;
;;;
;;;

(nrql-defmethod query-tautological-p ((query null) &key &allow-other-keys)
  :not-found)

(nrql-defmethod query-tautological-p ((query symbol) &key &allow-other-keys)
  (with-racer-timeout
   (query-tautological-p (find-query query))))


;;;
;;;
;;;

(nrql-defmethod classify-query ((query null))
  :not-found)

(nrql-defmethod classify-query ((query symbol)) 
  (classify-query (find-query query)))

(nrql-defmethod classify-query ((query query))
  (with-racer-timeout
   (classify query)))

;;;
;;;
;;;


(nrql-defmethod query-entails-p ((a symbol) (b symbol) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (with-racer-timeout
   (query-entails-p (find-query a) (find-query b))))

(nrql-defmethod query-entails-p ((a symbol) (b null) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod query-entails-p ((a null) (b symbol) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod query-entails-p ((a null) (b null) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  :not-found)

;;;
;;;
;;;

(nrql-defmethod query-equivalent-p ((a symbol) (b symbol) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (with-racer-timeout
   (query-equivalent-p (find-query a) (find-query b))))

(nrql-defmethod query-equivalent-p ((a symbol) (b null) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod query-equivalent-p ((a null) (b symbol) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod query-equivalent-p ((a null) (b null) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  :not-found)

;;;
;;;
;;;
;;;

(nrql-defmethod query-parents ((query null)) 
  :not-found)

(nrql-defmethod query-parents ((query symbol)) 
  (query-parents (find-query query)))

(nrql-defmethod query-parents ((query query)) 
  (with-racer-timeout
   (classify-query query)
   (remove nil (mapcar #'iterator-id 
                       (dag-node-parents 
                        (if (in-dag query) 
                            query
                          (first (equivalents query))))))))

;;;
;;;
;;;

(nrql-defmethod query-ancestors ((query null)) 
  :not-found)

(nrql-defmethod query-ancestors ((query symbol)) 
  (query-ancestors (find-query query)))

(nrql-defmethod query-ancestors ((query query)) 
  (with-racer-timeout
   (classify-query query)
   (remove nil (mapcar #'iterator-id 
                       (dag-node-ancestors
                        (if (in-dag query) 
                            query
                          (first (equivalents query))))))))

;;;
;;;
;;;

(nrql-defmethod query-children ((query null)) 
  :not-found)

(nrql-defmethod query-children ((query symbol)) 
  (query-children (find-query query)))

(nrql-defmethod query-children ((query query))
  (with-racer-timeout
   (classify-query query)
   (remove nil (mapcar #'iterator-id
                       (dag-node-children
                        (if (in-dag query) 
                            query
                          (first (equivalents query))))))))

;;;
;;;
;;;

(nrql-defmethod query-descendants ((query null)) 
  :not-found)

(nrql-defmethod query-descendants ((query symbol)) 
  (query-descendants (find-query query)))

(nrql-defmethod query-descendants ((query query)) 
  (with-racer-timeout
   (classify-query query)
   (remove nil (mapcar #'iterator-id 
                       (dag-node-descendants 
                        (if (in-dag query) 
                            query
                          (first (equivalents query))))))))

;;;
;;;
;;;

(nrql-defmethod query-equivalents ((query null)) 
  :not-found)

(nrql-defmethod query-equivalents ((query symbol)) 
  (query-equivalents (find-query query)))

(nrql-defmethod query-equivalents ((query query)) 
  (with-racer-timeout
   (classify-query query)
   (mapcar #'iterator-id 
           (remove nil 
                   (remove-duplicates
                    (remove query
                            (cons (first (equivalents query))
                                  (equivalents 
                                   (if (in-dag query) 
                                       query
                                     (first (equivalents query)))))))))))

;;;
;;;
;;;

(nrql-defun get-abox-of-current-qbox ()
  (if (and *cur-substrate*
           (typep *cur-substrate* 
                  #-:midelora 'racer-substrate
                  #+:midelora 'midelora-substrate))
      (abox *cur-substrate*)
    :not-found))

(nrql-defun show-current-qbox (&optional definitions-p)
  (if (and *cur-substrate*
           (typep *cur-substrate*
                  #-:midelora 'racer-substrate
                  #+:midelora 'midelora-substrate))
      (show-qbox-for-abox *cur-substrate* definitions-p)
    :not-found))

(nrql-defmethod get-dag-of-current-qbox ()
  (if (and *cur-substrate*
           (typep *cur-substrate* 
                  #-:midelora 'racer-substrate
                  #+:midelora 'midelora-substrate))
      (get-dag-of-qbox-for-abox *cur-substrate*)
    :not-found))

(nrql-defmethod get-nodes-in-current-qbox ()
  (if (and *cur-substrate*
           (typep *cur-substrate* 
                  #-:midelora 'racer-substrate
                  #+:midelora 'midelora-substrate))
      (get-nodes-in-qbox-for-abox *cur-substrate*)
    :not-found))

;;;
;;;
;;;

(nrql-defmethod get-nodes-in-qbox-for-abox ((abox null) &optional type-of-substrate)
  (declare (ignorable type-of-substrate))
  :not-found)

(nrql-defmethod get-nodes-in-qbox-for-abox ((abox symbol) &optional (type-of-substrate *type-of-substrate*))
  (get-nodes-in-qbox-for-abox 
   #-:midelora (find-racer-substrate abox type-of-substrate)
   #+:midelora (find-midelora-substrate abox type-of-substrate)))
   

(nrql-defmethod get-nodes-in-qbox-for-abox ((substrate dl-prover-substrate) &optional type-of-substrate)
  (declare (ignorable type-of-substrate))
  (if (qbox substrate) 
      (mapcar #'iterator-id 
              (remove (dag-top (qbox substrate))
                      (remove (dag-bottom (qbox substrate))
                              (dag-nodes (qbox substrate)))))
    :not-found))

;;;
;;;
;;;

(nrql-defmethod show-qbox-for-abox ((abox null) &optional definitions-p type-of-substrate)
  (declare (ignorable type-of-substrate definitions-p))
  :not-found)

(nrql-defmethod show-qbox-for-abox ((abox symbol) &optional definitions-p  (type-of-substrate *type-of-substrate*))
  (show-qbox-for-abox 
   #-:midelora (find-racer-substrate abox type-of-substrate) 
   #+:midelora (find-midelora-substrate abox type-of-substrate) 
   definitions-p))


(nrql-defmethod show-qbox-for-abox ((substrate dl-prover-substrate) &optional definitions-p type-of-substrate)
  (declare (ignorable type-of-substrate))
  (if (qbox substrate) 
      (progn
        (format t "~%~%;;;~%;;; QBOX FOR ~A FOR ABOX ~A~%;;; ~%" (type-of substrate) (abox substrate))
        (show-qbox substrate definitions-p)
        :see-output-on-stdout)
    :not-found))

;;;
;;;
;;;

(nrql-defmethod get-dag-of-qbox-for-abox ((substrate null) &optional type-of-substrate)
  (declare (ignorable type-of-substrate))
  :not-found)

(nrql-defmethod get-dag-of-qbox-for-abox ((abox symbol) &optional (type-of-substrate *type-of-substrate*))
  (get-dag-of-qbox-for-abox
   #-:midelora
   (find-racer-substrate abox type-of-substrate)
   #+:midelora
   (find-midelora-substrate abox type-of-substrate)))
   

(nrql-defmethod get-dag-of-qbox-for-abox ((substrate dl-prover-substrate) &optional type-of-substrate)
  (declare (ignorable type-of-substrate))

  (let* ((qbox
          (qbox substrate)))

    (if qbox
        (let ((top (dag-top qbox))
              (bot (dag-bottom qbox)))
          (mapcar #'(lambda (x) 
                      (tree-map #'(lambda (X)
                                    (cond ((eq x top)
                                           'master-top-query)
                                          ((eq x bot)
                                           'master-bottom-query)
					  ((null x) x)
                                          (t
                                           (or (iterator-id x)
                                               (intern (format nil "SUBQUERY-~A-OF-~A"
                                                               (subquery-id x)
                                                               (iterator-id
                                                                (top-level-query x))))))))
				(list (cons x (equivalents x))
				      (dag-node-parents x)
				      (dag-node-children x))))
                  (remove-duplicates
                   (cons top
                         (append
                          (remove top 
                                  (remove bot (dag-nodes qbox)))
                          (list bot))))))
      :not-found)))

;;;
;;;
;;;

(nrql-defmethod describe-query ((query null) &optional (rewritten-p t))
  (declare (ignorable rewritten-p))
  :not-found)

(nrql-defmethod describe-query ((query symbol) &optional (rewritten-p t))
  (describe-query (find-query query) rewritten-p))

;;;
;;;
;;;

(nrql-defmethod describe-rule ((query null) &optional (rewritten-p t))
  (declare (ignorable rewritten-p))
  :not-found)

(nrql-defmethod describe-rule ((query symbol) &optional (rewritten-p t))
  (describe-rule (find-rule query) rewritten-p))

;;;
;;;
;;;

(nrql-defmacro (with-nrql-settings :nrql-function eval-nrql-settings))

;;:
;;;
;;;


(defun eval-nrql-settings (body &key (mode 3) ; f. die End-User, siehe Manual!

                                (abox-mirroring nil abox-mirroring-supplied-p)
                                 
                                ;; nil, t, :smart, :very-smart
                                
                                (query-optimization t) ; nil, t

                                (optimizer-use-cardinality-heuristics t) ; nil, t
                                
                                how-many-tuples ; nil, <n> 
                                
                                (timeout (or *timeout* 
                                             #+:racer-server *server-timeout*))

                                (warnings t)
                                
                                (add-rule-consequences-automatically t) ; nil 

                                two-phase-query-processing-mode ; nil, t
                                
                                phase-two-starts-warning-tokens ; nil, t
                                
                                (kb-has-changed-warning-tokens t) ; nil, t
                                
                                told-information-querying ; nil, t
                                
                                (tuple-computation-mode :set-at-a-time)
                                
                                exclude-permutations ; nil 
                                
                                query-repository ; nil, t

                                report-inconsistent-queries
                                
                                report-tautological-queries 
                                    
                                query-realization ; nil, t

                                (check-abox-consistency nil check-abox-consistency-supplied-p) 

                                (rewrite-to-dnf t)
                                
                                (type-of-substrate
                                 #+:midelora
                                 'midelora-substrate
                                 #-:midelora
                                 'racer-dummy-substrate
                                 )
                                
                                abox
                                
                                tbox)

  (let* ((*tuple-at-a-time-p* 
          (when 
              (or (member mode '(4 5))
                  (case tuple-computation-mode
                    (:tuple-at-a-time-lazy t)
                    (:tuple-at-a-time-eager t)
                    (:set-at-a-time nil)
                    (otherwise 
                     (nrql-error "Bad keyword: ~A" tuple-computation-mode))))
            t))

         (*add-rule-consequences-p* 
          (when add-rule-consequences-automatically
            t))

         (*two-phase-processing-p* 
          (when (or (and *tuple-at-a-time-p*
                         two-phase-query-processing-mode)
                    (member mode '(6)))
            t))

         (*proactive-tuple-computation-p* 
          (when (or (not *tuple-at-a-time-p*) ; set-at-a-time is immer proactive!
                    (eq tuple-computation-mode  :tuple-at-a-time-eager))
            t))

         (*initial-abox-mirroring-p* 
          (if abox-mirroring-supplied-p
              abox-mirroring
            ;;; alle bis auf mode 3! 
            (member mode '(0 1 2 4 5 6))))
         
         (*deliver-phase-two-warning-tokens-p*
          (when (and *two-phase-processing-p*
                     phase-two-starts-warning-tokens)
            t))

         (*deliver-kb-has-changed-warning-tokens-p*
          kb-has-changed-warning-tokens)

         (*ensure-tbox-classification-p* 
          (when (or (member mode '(1 2 4 5 6))
                    (cond ((eq abox-mirroring :smart) t)
                          ((eq abox-mirroring :very-smart) t)
                          ((eq abox-mirroring nil) nil)
                          ((eq abox-mirroring t) nil)
                          (t (nrql-error "Bad keyword: ~A" abox-mirroring))))
            t))

         (*classify-concepts-in-instance-assertions-p* 
          (when (or (eq abox-mirroring :very-smart)
                    (member mode '(2 5)))
            t))
         
         (*told-information-reasoning-p* 
          (when (or told-information-querying
                    (member mode '(0 1 2)))
            t))

         (*optimize-p* 
          (when query-optimization t))

         (*optimizer-use-cardinality-heuristics-p* 
          (when (and *optimize-p* optimizer-use-cardinality-heuristics)
            t))

         (*rewrite-to-dnf-p* 
          (when (or rewrite-to-dnf
                    query-optimization
                    report-inconsistent-queries
                    report-tautological-queries
                    query-repository
                    query-realization)
            t))

         (*report-inconsistent-queries-p* 
          (when report-inconsistent-queries t))

         (*report-tautological-queries-p* 
          (when report-tautological-queries t))
         
         (*warnings-p*
          (when (or report-inconsistent-queries 
                    report-tautological-queries
                    warnings)
            t))
          
         (*use-repository-p*
          (when query-repository t))

         (*put-into-repository-p* 
          *use-repository-p*)

         (*rewrite-semantically-p*
          (when query-realization t))

         (*exclude-permutations-p* 
          (when exclude-permutations t))

         (*check-abox-consistency-p*
          (if (not check-abox-consistency-supplied-p)
              (member mode '(3 6))
            check-abox-consistency))

         (*how-many* 
          how-many-tuples)

         #-:racer-server (*timeout* timeout) ; *timeout* hat f. nRQL in Racer keine Relevanz
         #+:racer-server (*server-timeout* timeout) ; wichtig!

         (*nrql-abox* abox)

         (*nrql-tbox* tbox)

         (*type-of-substrate* type-of-substrate))

    (funcall body)))

;;;
;;;
;;;

(nrql-defun set-nrql-mode (mode)
  (restore-standard-settings)
  (ecase mode
    (0 (enable-told-information-querying)
       (dont-check-abox-consistency-before-querying)
       :okay-mode-0)
    (1 (enable-told-information-querying)
       (dont-check-abox-consistency-before-querying)
       (enable-smart-abox-mirroring)
       :okay-mode-1)
    (2 (enable-told-information-querying)
       (dont-check-abox-consistency-before-querying)
       (enable-very-smart-abox-mirroring)
       :okay-mode-2)
    (3 :okay-mode-3)

    ;;; two-phase modes

    (4 (enable-two-phase-query-processing-mode)
       :okay-mode-4)

    (5 (enable-two-phase-query-processing-mode)
       (enable-very-smart-abox-mirroring)
       :okay-mode-5)
    
    ;;; two-phase, but set-at-a-time!
    ;;; verhindert evtl. internal-individuals-related-p
    ;;; Aufrufe in racer-retrieve-individual-fillers 

    (6 (enable-two-phase-query-processing-mode)
       (check-abox-consistency-before-querying)
       (enable-smart-abox-mirroring)
       (process-set-at-a-time)
       :okay-mode-6)))

;;;
;;;
;;;

(nrql-defun set-rewrite-defined-concepts (val)
  (setf *rewrite-defined-concepts-p* val))

;;;
;;;
;;;

#+:process-pooling
(nrql-defun set-initial-size-of-process-pool (n)
  (when (and n 
             (integerp n)
             (> n 0))
    (setf *min-pool-size* n)
    (init-process-pool))
  *min-pool-size*)

#+:process-pooling
(nrql-defun set-maximum-size-of-process-pool (n)
  (when (or (null n)
            (and n 
                 (integerp n)
                 (>= n *min-pool-size*)))
    (setf *max-pool-size* n))
  *max-pool-size*)


#+:process-pooling
(nrql-defun get-initial-size-of-process-pool ()
  *min-pool-size*)

#+:process-pooling
(nrql-defun get-maximum-size-of-process-pool ()
  *max-pool-size*)

#+:process-pooling
(nrql-defun get-process-pool-size ()
  (length *process-pool*))

;;;
;;;
;;;

(restore-standard-settings)

