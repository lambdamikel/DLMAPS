;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(defmacro defvar1 (&rest body)
  `(#+:lispworks SYSTEM::WITHOUT-WARNING-ON-REDEFINITION 
                 #-:lispworks progn
                 (defvar ,@body)))

(defmacro defparameter1 (&rest body)
  `(#+:lispworks SYSTEM::WITHOUT-WARNING-ON-REDEFINITION 
                 #-:lispworks progn
                 (defparameter ,@body)))

;;;
;;;
;;;

(defmacro with-logging (&body body)
  `(with-open-file (stream "prover.log" :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)

     (setf *stream* stream)

     (let* ((*standard-output* stream)
           (*print-pretty* t)
           (*debug-io* *standard-output*)
           (*error-output* *standard-output*)
           (*trace-output* *standard-output*))
       
       (handler-case 
           ,@body
         (error (error)
                (format *standard-output* "~%~%******** ERROR ~A ************~%" error)
                (loop-over-abox-nodes (node *abox*)
                  (describe-object node *standard-output*))
                (force-output *standard-output*)
                
                'error)))))
     

(defmacro timed-defmethod (name &body body)
  (let ((var-name 
         (intern (format nil "*TIME-SPEND-IN-~A*" name)))
        (body1 (last body)))
    `(progn 

       (defvar1 ,var-name 0)

       (defmethod ,name ,@(butlast body)
         (let ((*start-time* (get-internal-run-time)))
           (unwind-protect 
               ,@body1
             (incf ,var-name (- (get-internal-run-time) *start-time*))))))))


(defmacro timed-defun (name &body body)
  (let ((var-name 
         (intern (format nil "*TIME-SPEND-IN-~A*" name)))
        (body1 (last body)))
    `(progn 

       (defvar1 ,var-name 0)

       (defun ,name ,@(butlast body)
         (let ((*start-time* (get-internal-run-time)))
           (unwind-protect 
               ,@body1
             (incf ,var-name (- (get-internal-run-time) *start-time*))))))))


(defmacro true! (body)
  `(unless ,body
     (error "~A must evaluate to TRUE!" ',body)))


(defmacro false! (body)
  `(when ,body
     (error "~A must evaluate to FALSE!" ',body)))


(defmacro push-all-to (add to-list &key (remove-duplicates-p t))
  (let ((var1 (gensym))
        (var2 (gensym)))
    `(let ((,var2 ,add))
       (dolist (,var1 ,add)
         (push ,var1 ,to-list))
       ,(when remove-duplicates-p
          `(setf ,to-list 
                 (remove-duplicates ,to-list)))
       ,var2)))

;;;
;;;
;;;
;;;

(defmacro with-tbox* ((name &rest args &key delete-if-exists-p &allow-other-keys) &rest body)
  `(let* ((*cur-tbox* 
           (when ,name
             (or (and ,(not delete-if-exists-p) (find-tbox ,name))
                 (make-tbox ,name ,@args))))

          (*cur-store* (concept-store *cur-tbox*)))

     ,@body))

(defmacro in-tbox* (name &rest args &key delete-if-exists-p &allow-other-keys)
  `(progn 
     (setf *cur-tbox*
           (when ,name
             (or (and ,(not delete-if-exists-p) (find-tbox ,name))              
                 (make-tbox ,name ,@args))))

     (setf *cur-store* 
           (concept-store *cur-tbox*))

     *cur-tbox*))

;;;
;;;
;;;

(defmacro with-tbox ((name &rest args) &rest body)
  `(with-tbox* ( (quote ,name) ,@args) 
               ,@body))

(defmacro in-tbox (name &rest args)
  `(in-tbox* (quote ,name) ,@args))

;;;
;;;
;;;

(defmacro with-abox ((name &rest args &key type delete-if-exists-p &allow-other-keys)
                     &rest body)
  `(with-substrate (,name ,@args
                          :delete-if-exists-p ,delete-if-exists-p
                          :type (or ,type
                                    (get-abox-type-for-tbox *cur-tbox*)))
     ,@body))

(defmacro in-abox (name &optional tbox &rest args &key type  delete-if-exists-p &allow-other-keys)
  (if tbox 
      `(with-tbox (,tbox)
         (in-substrate ,name ,@args
                       :delete-if-exists-p ,delete-if-exists-p
                       :type (or ,type
                                 (get-abox-type-for-tbox *cur-tbox*))))
    `(in-substrate ,name ,@args
                   :delete-if-exists-p ,delete-if-exists-p
                   :type (or ,type
                             (get-abox-type-for-tbox *cur-tbox*)))))
         


;;;
;;; die "*"-Varianten evaluieren das "name"-Argument! 
;;; s. with-substrate* etc. in substrate7.lisp
;;;


(defmacro with-abox* ((name &rest args &key type delete-if-exists-p &allow-other-keys)
                      &rest body)
  `(with-substrate* (,name ,@args
                           :delete-if-exists-p ,delete-if-exists-p
                           :type (or ,type
                                     (get-abox-type-for-tbox *cur-tbox*)))
                    ,@body))

(defmacro in-abox* (name &rest args &key type  delete-if-exists-p &allow-other-keys)
  `(in-substrate* ,name ,@args
                  :delete-if-exists-p ,delete-if-exists-p
                  :type (or ,type
                            (get-abox-type-for-tbox *cur-tbox*))))


;;;
;;;
;;;

(defmacro in-knowledge-base (tbox abox &rest args)
  `(progn 
     (in-tbox ,tbox ,@args)
     (in-abox ,abox ,@args)))


(defmacro with-knowledge-base ((tbox abox &rest args) &body body)
  `(with-tbox (,tbox ,@args)
     (with-abox (,abox ,@args)
       ,@body)))

;;;
;;;
;;;

(defmacro in-knowledge-base* (tbox abox &rest args)
  `(progn 
     (in-tbox* ,tbox ,@args)
     (in-abox* ,abox ,@args)))


(defmacro with-knowledge-base* ((tbox abox &rest args) &body body)
  `(with-tbox* (,tbox ,@args)
               (with-abox* (,abox ,@args)
                           ,@body)))

;;;
;;;
;;;

(defmacro with-kb ((tbox abox &rest args) &body body)
  `(with-knowledge-base (,tbox ,abox ,@args)
     ,@body))

(defmacro with-kb* ((tbox abox &rest args) &body body)
  `(with-knowledge-base* (,tbox ,abox ,@args)
     ,@body))

(defmacro in-kb (tbox abox &rest args)
  `(in-knowledge-base ,tbox ,abox ,@args))

(defmacro in-kb* (tbox abox &rest args)
  `(in-knowledge-base* ,tbox ,abox ,@args))

;;;
;;;
;;;

(defmacro with-timing ((var) &body body)
  `(let ((*start-time* (get-internal-run-time)))
     (unwind-protect 
         (progn ,@body)
      (incf ,var (- (get-internal-run-time) *start-time*)))))
       
