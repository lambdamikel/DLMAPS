;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

(defmacro check-for-abort ()
  `(when (abort-search-p *running-query*)
     (throw 'query-execution 'abort)))


(defmacro abortable-dolist ((var list) &body body)
  `(dolist (,var ,list)
     (check-for-abort)
     ,@body))

(defmacro with-timeout1 ((timeout &body timeoutforms) &body body)
  `(if ,timeout
       (with-timeout (,timeout ,@timeoutforms)
                     ,@body)
     (progn ,@body)))


(defmacro with-vois-marked-as-bound (refs &rest body)
  (let ((ref (gensym "VOI"))
        (memo (gensym "MEMO")))
    `(let ((,memo nil))
       (unwind-protect 
           (progn
             (dolist (,ref ,refs)
               (unless (bound-to ,ref)
                 (push ,ref ,memo)
                 (setf (bound-to ,ref) t)
                 (let ((,ref (corresponding-voi ,ref)))
                   (when (and ,ref 
                              (not (bound-to ,ref)))
                     (push ,ref ,memo)
                     (setf (bound-to ,ref) t)))))
             ,@body)
         (dolist (,ref ,memo)
           (setf (bound-to ,ref) nil))))))

(defmacro with-saved-bindings (refs &rest body)
  (let ((memo (gensym)))
    `(let ((,memo nil))
       (unwind-protect 
           (progn
             (dolist (ref ,refs)
               (push (list ref (bound-to ref))
                     ,memo)
               (let ((cor-ref (corresponding-voi ref)))
                 (when cor-ref 
                   (push (list cor-ref (bound-to cor-ref))
                         ,memo))))
             ,@body)

         (dolist (entry ,memo)
           (setf (bound-to (first entry))
                 (second entry)))))))

(defmacro defquery-code ((type) &rest body)
  (let* ((tester-code 
          (rest (assoc :tester body)))
         (tester-compiler-code
          (rest (assoc :compiler tester-code)))
         (tester-runtime-code
          (rest (assoc :runtime tester-code)))
        
         (enumerator-code 
          (rest (assoc :enumerator body)))
         (enumerator-compiler-code
          (rest (assoc :compiler enumerator-code)))
         (enumerator-runtime-code
          (rest (assoc :runtime enumerator-code)))        

         (from-bound-enumerator-code 
          (rest (assoc :from-bound-enumerator body)))
         (from-bound-enumerator-compiler-code
          (rest (assoc :compiler from-bound-enumerator-code)))
         (from-bound-enumerator-runtime-code
          (rest (assoc :runtime from-bound-enumerator-code)))
        
         (to-bound-enumerator-code 
          (rest (assoc :to-bound-enumerator body)))
         (to-bound-enumerator-compiler-code
          (rest (assoc :compiler to-bound-enumerator-code)))
         (to-bound-enumerator-runtime-code
          (rest (assoc :runtime to-bound-enumerator-code))))
    
    `(progn
       ,@(when tester-compiler-code
           `((defmethod get-tester-code ((query ,type) ,@(first tester-compiler-code))
               ,@(rest tester-compiler-code))))
       ,@(when tester-runtime-code
           `((defmethod evaluate-tester ((query ,type) ,@(first tester-runtime-code))
               ,@(rest tester-runtime-code))))

       ,@(when enumerator-compiler-code
           `((defmethod get-enumerator-code ((query ,type) ,@(first enumerator-compiler-code))
               ,@(rest enumerator-compiler-code))))
       ,@(when enumerator-runtime-code
           `((defmethod evaluate-enumerator ((query ,type) ,@(first enumerator-runtime-code))
               ,@(rest enumerator-runtime-code))))
       
       ,@(when from-bound-enumerator-compiler-code
           `((defmethod get-from-bound-enumerator-code ((query ,type) ,@(first from-bound-enumerator-compiler-code))
               ,@(rest from-bound-enumerator-compiler-code))))
       ,@(when from-bound-enumerator-runtime-code
           `((defmethod evaluate-from-bound-enumerator ((query ,type) ,@(first from-bound-enumerator-runtime-code))
               ,@(rest from-bound-enumerator-runtime-code))))

       
       ,@(when to-bound-enumerator-compiler-code
           `((defmethod get-to-bound-enumerator-code ((query ,type) ,@(first to-bound-enumerator-compiler-code))
               ,@(rest to-bound-enumerator-compiler-code))))
       ,@(when to-bound-enumerator-runtime-code
           `((defmethod evaluate-to-bound-enumerator ((query ,type) ,@(first to-bound-enumerator-runtime-code))
               ,@(rest to-bound-enumerator-runtime-code)))))))


;;:
;;;
;;;

(defvar *nrql-functions* nil)

(defvar *nrql-macros* nil)

(defvar *nrql-methods* nil)

(defvar *nrql-with-macros* nil)

(defun is-with-macro-p (x)
  (search "WITH-" (symbol-name x)))

(defmacro nrql-defun (name lambda-list &body body)
  `(progn
     (pushnew (list ',name ',lambda-list) *nrql-functions* :test #'equal)
     (defun ,name ,lambda-list ,@body)))

(defmacro nrql-defmacro ((name &key nrql-function))
  (if (is-with-macro-p name)
      `(progn 
         (pushnew (list ',name ',nrql-function) *nrql-with-macros* :test #'equal)
         (defmacro ,name ((&rest args) &body body)
           (let ((fn ',nrql-function))
             `(apply (symbol-function ',fn)
                     (lambda ()
                       ,@body)
                     ',args))))
    `(progn 
       (pushnew (list ',name ',nrql-function) *nrql-macros* :test #'equal)
       (defmacro ,name (&rest args)
         (let ((fn ',nrql-function))
           `(apply (symbol-function ',fn)
                   ',args))))))
    
(defmacro nrql-defmethod (name lambda-list &body body)
  `(progn 
     (pushnew (list ',name ',lambda-list) *nrql-methods* :test #'equal)
     (defmethod ,name ,lambda-list ,@body)))

;;;
;;;
;;;

#+:midelora
(defmacro midelora-defun (name lambda-list &body body)
  `(progn
     (defun ,name ,lambda-list ,@body)))

#+:midelora
(defmacro midelora-defmacro ((name &key nrql-function))
  (if (is-with-macro-p name)
      `(progn 
         (defmacro ,name ((&rest args) &body body)
           (let ((fn ',nrql-function))
             `(apply (symbol-function ',fn)
                     (lambda ()
                       ,@body)
                     ',args))))
    `(progn 
       (defmacro ,name (&rest args)
         (let ((fn ',nrql-function))
           `(apply (symbol-function ',fn)
                   ',args))))))
    
#+:midelora
(defmacro midelora-defmethod (name lambda-list &body body)
  `(progn 
     (defmethod ,name ,lambda-list ,@body)))

;;;
;;;
;;;

#+:racer-server
(defmacro save-racer-state (query)
  `(setf (slot-value ,query 'state-of-racer-specials)
         (mapcar #'(lambda (special)
                     (if (boundp special)
                         (symbol-value special)
                       :racer-special-is-unbound))
                 racer::+racer-specials+)))



#+:lracer
(defmacro save-racer-state (query)
  (declare (ignorable query))
  t)

;;;
;;;
;;;

#+:racer-server
(defmacro with-racer-state ((query) &body body)
  `(let* ((state (state-of-racer-specials ,query))
          ,@(mapcar #'(lambda (special)
                        (list special '(pop state)))
                    racer::+racer-specials+))
     (dolist (special racer::+racer-specials+)
       (when (eq special :racer-special-is-unbound)
         (makunbound special)))
     ,@body))


#+:lracer
(defmacro with-racer-state ((query) &body body)
  (declare (ignorable query))
  `(progn
     ,@body))

;;;
;;;
;;;

#-:midelora 
(defmacro with-dl-prover-state ((query) &body body)
  `(with-racer-state (,query) ,@body))

#+:midelora 
(defmacro with-dl-prover-state ((query) &body body)
  (declare (ignore query))
  `(progn ,@body))


#-:midelora
(defmacro save-dl-prover-state (query)
  `(save-racer-state ,query))

#+:midelora
(defmacro save-dl-prover-state (query)
  (declare (ignore query))
  t)


;;;
;;; with-racer-timeout ist die "globale" Timeout-Klammer 
;;; für die nRQL-API-Funktionen
;;; 
;;; die einzelnen zeitintensiven (prepare-substrate1, answer-query, ...)
;;; setzen entsprechende "with-timeout-cleanup"-Forms 
;;; auf dann entsp. "Aufraeumarbeiten" auszuführen  
;;; 

#+:racer-server
(defmacro with-racer-timeout (&body body)
  `(if *server-timeout*
       (with-timeout (*server-timeout* :timeout)
         (let ((*timeout* nil)
               (*server-timeout* nil))
           ,@body))
     (let ((*timeout* nil)
           (*server-timeout* nil))
       ,@body)))

#-:racer-server
(defmacro with-racer-timeout (&body body)
  `(progn 
     ,@body))


;;;
;;;
;;;

#+(or :dlmaps :lracer)
(defmacro with-timeout ((&rest args) &rest body)
  (declare (ignorable args))
  `(progn
     ,@body))


#+(or :dlmaps :lracer)
(defmacro without-timeout (&rest body)
  `(progn
     ,@body))

#+:dlmaps
(defmacro without-signature-checks (&rest body)
  `(progn
     ,@body))

#+(or :dlmaps :lracer)
(defmacro with-timeout-cleanup (form &rest cleanup-forms)
  `(restart-case
       ,form
     (abort-execution ()
                      ,@cleanup-forms
                      (invoke-restart 'continue-from-timeout))))
