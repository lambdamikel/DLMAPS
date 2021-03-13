;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)


(defmacro start-process (&body body)  
  #+:multiprocess-queries
  `(#-:mcl mp:process-run-function 
           #+:mcl ccl:process-run-function
           
           "Query Answering Process"
             
           ;;'(:size 1400000)  
          
           #-:mcl nil
          
           #'(lambda ()
               ,@body))

  #-:multiprocess-queries
  `(progn 
     (funcall #'(lambda ()
                  ,@body))
     
     nil ; wichtig!
     ))


#| 

#+:lispworks
(defmacro with-critical-section (&body body)
  ;; funktioniert nicht?!
  `(lispworks:without-interrupts
    ,@body))

#-:lispworks 
(defmacro with-critical-section (&body body)
  `(error "Please implement macro with-critical-section (in macros.lisp)!"))

|# 

(defvar *lock* nil)

(defmacro with-critical-section (&body body)
  #+:multiprocess-queries
  `(progn 
     (when (not (eq *lock* *process-id*))
       (loop while *lock* do
             (sleep +sleep-time+))
       (when *lock*
         (error "Someone stole the *lock*?!")))
     (setf *lock* *process-id*)
     (unwind-protect 
         ;; (lispworks:without-preemption
         ;; funktioniert nicht!!!
         ,@body
       (setf *lock* nil)))
  #-:multiprocess-queries
  `(progn 
     ,@body))

