;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;;
;;;

#+:process-pooling
(progn

(defclass pooled-process ()
  ((process :accessor process :initarg :process)
   (process-function :accessor process-function :initform nil
                     :initarg :process-function)))

(defmethod initialize-instance :after ((process pooled-process) &rest initargs)
  (declare (ignorable initargs))
  (incf *pool-counter*)
  (push process *all-processes*))

(defun kill-all-processes ()
  (mapc #'(lambda (x)
            (kill-process (process x)))
        *all-processes*)
  (setf *pool-counter* 0
        *all-processes* nil
        *process-pool* nil))
  
(defun make-pooled-process ()
  (let* ((pp (make-instance 'pooled-process))
         (*multiprocess-queries* t)
         (p (start-process
              (loop
		(process-wait (process-function pp))
		(funcall (process-function pp))
		(release-process pp)))))
    
    (setf (process pp) p)
    
    pp))
   
;;;
;;;
;;;
              
(defun acquire-process ()
  (labels ((try-to-acquire ()
	     (with-critical-section
		 (when *process-pool*
		   (return-from acquire-process
		     (pop *process-pool*))))))

    (when (zerop *pool-counter*)
      (init-process-pool))

    (try-to-acquire)

    (with-critical-section  
	(when (=> *max-pool-size*
		  (< *pool-counter* *max-pool-size*))
	  (push (make-pooled-process) *process-pool*)
	  (try-to-acquire)))
    
    (if (with-critical-section  
         (some #'proactive-tuple-computation-p *active-queries*))
      (loop 
	(process-sleep +sleep-time+)
	(try-to-acquire))

      (progn 
        (process-sleep +sleep-time+)
	(try-to-acquire)))))

(defun release-process (pooled-process)
  (with-critical-section
    (setf (process-function pooled-process) nil)
    (push pooled-process *process-pool*)))

;;;
;;;
;;;

#+:lispworks 
(defmethod abort-process ((pooled-process pooled-process))
  (unless (mp::process-dead-p (process pooled-process))
    (mp:process-reset (process pooled-process))
    (release-process pooled-process)))

#+:allegro 
(defmethod abort-process ((pooled-process pooled-process))
  (multiprocessing:process-reset (process pooled-process))
  (release-process pooled-process))

(defmethod abort-process ((process t))
  (kill-process process))

;;;
;;;
;;;

(defmacro start-query-process (body)
  `(if (and *process-pooling*
            *multiprocess-queries*)
       
       (let ((process (acquire-process)))
         (when process
           (setf (process-function process)
                 #'(lambda ()
                     ,body)))
         
         process)

     (start-process ,body)))

;;;
;;;
;;;

(defun init-process-pool ()
  (kill-all-processes)
  (setf *process-pool* 
        (loop as  i from 1 to *min-pool-size* 
              collect (make-pooled-process))))


)

#-:process-pooling
(progn

(defun init-process-pool () t)

(defun kill-all-processes () t)

(defmacro start-query-process (body)
  `(start-process ,body))

(defmethod abort-process ((process t))
  (kill-process process)))



