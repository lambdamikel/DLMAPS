;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;;
;;;

#-:multiprocess-queries
(progn


  (defmacro start-process (&body body)  
    `(progn 
       (funcall #'(lambda ()
                    ,@body))
     
       nil ; wichtig!
       ))

  (defmacro with-critical-section (&body body)
    `(progn ,@body))

  (defmacro process-wait (&rest body)
    `(progn ,@body))

  )

;;;
;;;
;;;

#+:multiprocess-queries
(progn 

  #+:clisp
  (defmacro start-process (&body body)  
    `(if *multiprocess-queries*
         (to-be-implemented 'start-process)
       (progn 
         (funcall #'(lambda ()
                      ,@body)
                  nil)
         nil)))


  #+:mcl
  (defmacro start-process (&body body)  
    `(if *multiprocess-queries*
         (ccl:process-run-function
                         
          "Query Answering Process"
        
          #'(lambda ()
              ,@body))
       (progn 
         (funcall #'(lambda ()
                      ,@body))
         nil)))

  #+:allegro
  (defmacro start-process (&body body)  
    `(if *multiprocess-queries*
         (mp:process-run-function 
        
          "Query Answering Process"
        
          #'(lambda ()
              ,@body))
       
       (progn 
         (funcall #'(lambda ()
                      ,@body))
         nil)))


  #+:lispworks
  (defmacro start-process (&body body)  
    `(if *multiprocess-queries*
         (mp:process-run-function 

          "Query Answering Process"
           
          nil
          
          #'(lambda (*standard-output* 
                     *trace-output*)
              ,@body)
          *standard-output*
          *trace-output*
          )
       
       (progn 
         (funcall #'(lambda ()
                      ,@body))
         nil)))

  ;;;
  ;;;
  ;;;

  (defun kill-process (process)
    (declare (ignorable process))
    #+:lispworks
    (mp:process-kill process)
    #+:mcl 
    (ccl:process-kill process)
    #+:allegro 
    (multiprocessing:process-kill process)
    #+:clisp
    (to-be-implemented 'kill-process1))


  ;;;
  ;;;
  ;;;

  #+:allegro
  (defun make-lock ()
    (mp:make-process-lock))

  #+:lispworks
  (defun make-lock ()
    (mp:make-lock))

  #+:mcl
  (defun make-lock ()
    (ccl:make-lock))

  #+:clisp
  (defun make-lock ()
    (to-be-implemented 'make-lock))


  ;;;
  ;;;
  ;;;

  #+:allegro
  (defun process-sleep (time)
    (multiprocessing:process-wait-with-timeout "wait" time (lambda () nil)))

  #+:lispworks
  (defun process-sleep (time)
    (mp:process-wait-with-timeout "wait" time))

  #+:mcl
  (defun process-sleep (time)
    (ccl:process-wait-with-timeout "wait" time (lambda () nil)))

  #+:clisp
  (defun process-sleep (time)
    (declare (ignorable time))
    (to-be-implemented 'process-sleep))


  ;;;
  ;;;
  ;;;

  (defvar *process-lock* (make-lock))

  #+:lispworks
  (defmacro with-process-lock (&body body)
    `(mp:with-lock (*process-lock*)
       ,@body))

  #+:allegro
  (defmacro with-process-lock (&body body)
    `(mp:with-process-lock (*process-lock*)
                           ,@body))

  #+:mcl
  (defmacro with-process-lock (&body body)
    `(ccl:with-lock-grabbed (*process-lock*)
                            ,@body))


  #+:clisp
  (defmacro with-process-lock (&body body)
    (declare (ignorable body))
    (to-be-implemented 'with-process-lock))

  ;;;
  ;;;
  ;;;

  (defmacro with-critical-section (&body body)
    `(with-process-lock 
      (progn ,@body)))

  ;;;
  ;;;
  ;;;

  #+:allegro
  (defmacro process-wait (&rest body)
    `(multiprocessing:process-wait "waiting"
                                   (lambda ()
                                     ,@body)))

  #+:lispworks
  (defmacro process-wait (&rest body)
    `(mp:process-wait "waiting"
                      (lambda ()
                        ,@body)))

  #+:mcl
  (defmacro process-wait (&rest body)
    `(ccl::process-wait "waiting"
                        (lambda ()
                          ,@body)))

  #+:clisp
  (defmacro process-wait (&rest body)
    (declare (ignorable body))
    (to-be-implemented 'process-wait))

)

