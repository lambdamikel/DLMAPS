;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(defun create-package-list ()
  (with-open-file (stream "nrql:nrql-symbols.lisp"
                          :direction :output 
                          :if-does-not-exist :create
                          :if-exists :supersede)

    (format stream "(in-package cl-user)~%~%")

    (format stream ";;;~%")
    (format stream ";;;--------------------------------------------~%")
    (format stream ";;;  Automatically Generated nRQL Symbol List  ~%")
    (format stream ";;;          Version: ~A  ~%" (get-nrql-version))
    (format stream ";;;--------------------------------------------~%")
    (format stream ";;;~%~%")
    
    
    (let* ((*print-case* :downcase)
           (syms (remove-duplicates
                  (append '(WITH-NRQL-STANDARD-SETTINGS   
                             RACER-RETRIEVE-INDIVIDUAL-FILLED-ROLES
                             RACER-RETRIEVE-INDIVIDUAL-FILLERS 
                             RACER-RETRIEVE-RELATED-INDIVIDUALS
                                   
                             XML-OUTPUT
                             XML-INPUT
                             LISP-TO-XML
                             XML-TO-LISP

                             NRQL-EQUAL-ROLE)
                                 
                          (remove :racer ts::+reserved-tokens+)
                           
                          (mapcar #'first 
                                  (append ts::*nrql-functions*
                                          ts::*nrql-methods*
                                          ts::*nrql-macros*
                                          ts::*nrql-with-macros*))))))
      (format stream  
              "(defpackage nrql-symbols
                 (:export #:query")
      (terpri stream)
      (dolist (sym syms)
        ;(format stream "                          #:~A~%" sym)
        (format stream "                          ~A~%" sym))
      (format stream "))~%~%")
      (format stream "(defconstant +nrql-symbols+ '(~%")
      (dolist (sym syms)
        ;(format stream "                          #:~A~%" sym)
        (format stream "                          ~A~%" sym))
      (format stream "))~%~%"))))

(defun create-server-case ()


  (let ((*print-case* :downcase))
           
    (with-open-file (stream "nrql:nrql-server-case.lisp"
                            :direction :output 
                            :if-does-not-exist :create
                            :if-exists :supersede)

      (format stream "(in-package cl-user)~%~%")

      (format stream ";;;~%")
      (format stream ";;;--------------------------------------------~%")
      (format stream ";;;  Automatically Generated nRQL Server Case  ~%")
      (format stream ";;;          Version: ~A  ~%" (get-nrql-version))
      (format stream ";;;--------------------------------------------~%")
      (format stream ";;;~%~%")
    
      (pprint 

       `(defun process-nrql-request (expr stream n state output-string-stream)

          (case (first expr)
   
            ,@(remove nil
                  
                      (append 

                       `(((xml-output)

                          (process-racer-expr
                           (second expr)
                           nil
                           n
                           state
                           output-string-stream)

                          (let ((expr2
                        
                                 (if *last-error* 
                                   
                                     (lisp-to-xml (format nil "~a" *last-error*)
                                                  stream
                                                  :newlines-p nil
                                                  :ascii-p nil
                                                  :indentation-p nil
                                                  :top-level-attributes
                                                  (format nil "id=\"~d\" type=\"error\"" n))
                          
                                   (lisp-to-xml (list 
                                                 (format nil "~s" *last-answer*)
                                                 *last-answer*)
                                                stream
                                                :newlines-p nil
                                                :ascii-p t
                                                :indentation-p nil
                                                :top-level-attributes
                                                (format nil "id=\"~d\" type=\"answer\"" n)))))

                            (answer expr
                                    state
                                    stream
                                    n
                                    expr2
                                    output-string-stream))))

                       `(((xml-native-output)

                          (process-racer-expr
                           (second expr)
                           nil
                           n
                           state
                           output-string-stream)
                        
                          (let ((expr2
                                 (if *last-error* 
                            
                                     (lisp-to-xml (format nil "~a" *last-error*)
                                                  stream
                                                  :newlines-p nil
                                                  :ascii-p nil
                                                  :indentation-p nil
                                                  :top-level-attributes
                                                  (format nil "id=\"~d\" type=\"error\"" n))
                          
                                   (lisp-to-xml (format nil "~s" *last-answer*)
                                                stream
                                                :newlines-p nil
                                                :ascii-p t
                                                :indentation-p nil
                                                :top-level-attributes
                                                (format nil "id=\"~d\" type=\"answer\"" n)))))
                            (answer expr
                                    state
                                    stream
                                    n
                                    expr2
                                    output-string-stream))))
                     
                       (mapcar #'(lambda (x)
                                   (let ((name (first x))
                                         (corresponding-function (second x)))
                                   
                                     (unless corresponding-function
                                       (error ":nrql-function missing: ~A!" name))
                                   
                                     `((,name)
                                     
                                       (let* ((saved-timeout *server-timeout*)
                                              (*server-timeout* nil))
                                            
                                         ;;; die nRQL-Funktionen setzen ihr eigenes Timeout auf! 
                                         (answer expr
                                                 state
                                                 stream n 
                                                 (let ((*server-timeout* 
                                                        saved-timeout))
                                                   (apply (symbol-function ',corresponding-function)
                                                          (rest expr)))
                                                 output-string-stream)))))
                             
                               ts::*nrql-macros*)
           
                       (mapcar #'(lambda (x)
                                   (let ((name (first x))
                                         (corresponding-function (second x)))
                                   
                                     (unless corresponding-function
                                       (error ":nrql-function missing: ~A!" name))
                                   
                                     `((,name)

                                       (let ((*server-timeout* nil))
                                       
                                         ;; bei with-macros wird das server-timeout ignoriert! 
                                         ;; dafuer gibt es :timeout ... 
                                       
                                         (apply (symbol-function ',corresponding-function)
                                                (lambda ()
                                                  (loop for expr1 in (cddr expr) do
                                                        (process-racer-expr expr1 stream 
                                                                            n state output-string-stream)))
                                                (second expr))))))
                   
                               ts::*nrql-with-macros*)
             
                       (list 
                        (list (remove-duplicates 
                               (mapcar #'first 
                                       (append ts::*nrql-methods*
                                               ts::*nrql-functions*)))
                 
                              `(let* ((saved-timeout *server-timeout*)
                                      (*server-timeout* nil))

                                 (answer expr
                                         state
                                         stream n 
                                         (let ((*server-timeout* 
                                                saved-timeout))
                                           (apply (symbol-function (first expr))
                                                  (rest expr)))
                                         output-string-stream))))))
          
            (otherwise (error "Illegal operator in ~A" expr))))

       stream))))


(defun process-lambda (lambda)
  (when lambda
    (case (first lambda) 
      (&rest 
       (process-lambda (cddr lambda)))
      (&optional  
       `(,(second lambda) 
         ,@(process-lambda (cddr lambda))))
      (&key 
       (apply #'append 
              (mapcar 
               #'(lambda (x) 
                   `(,(intern (format nil "~A" x)
                              (find-package :keyword))
                     ,x))
               (rest lambda))))
      (otherwise 
       `(,(first lambda) 
         ,@(process-lambda (rest lambda)))))))
    

(defun cl-user (x)
  (intern (format nil "~A" x)
          (find-package :cl-user)))

(defun create-lracer-functions ()

  (let ((*print-case* :downcase))
           
    (with-open-file (stream "nrql:lracer-nrql-stubs.lisp"
                            :direction :output 
                            :if-does-not-exist :create
                            :if-exists :supersede)

      (format stream "(in-package racer)~%~%")

      (format stream ";;;~%")
      (format stream ";;;--------------------------------------~%")
      (format stream ";;;  Automatically Generated nRQL Stubs  ~%")
      (format stream ";;;          Version: ~A  ~%" (get-nrql-version))
      (format stream ";;;--------------------------------------~%")
      (format stream ";;;~%~%")
    
      (dolist (fn (append 
                   (mapcar #'(lambda (x)
                               (let* ((name (cl-user (first x)))
                                      (lambda (mapcar #'cl-user
                                                      (mapcar #'(lambda (x) 
                                                                  (if (consp x) (first x)
                                                                    x))
                                                              (second x))))
                                      (lambda2 (process-lambda 
                                                (remove '&allow-other-keys lambda)))
                                      (rest-param (second (member '&rest lambda))))
                 
                                 (declare (ignorable lambda))
                                 `(defun ,name ,lambda
                                    ,(if lambda 
                                         `(with-standard-io-syntax-1
                                           (service-request 
                                            (format nil "(~A~{ ~S~}~{ ~S~})"
                                                    ',name
                                                    (mapcar #'transform-s-expr (list ,@lambda2))
                                                    (mapcar #'transform-s-expr ,rest-param))))
                                    
                                       `(with-standard-io-syntax-1
                                         (service-request 
                                          (format nil "(~A)" ',name)))))))

                           (remove-duplicates 
                            (append ts::*nrql-functions*
                                    ts::*nrql-methods*)
                            :key #'first))

                   (mapcar #'(lambda (x)
                               (let* ((name (cl-user (first x))))
                                 `(defmacro ,name (&rest args)
                                    (with-standard-io-syntax-1
                                     `(service-request 
                                       ,(format nil "(~A~{ ~S~})"
                                                ',name
                                                (mapcar #'transform-s-expr args)))))))
                           ts::*nrql-macros*)

                   (mapcar #'(lambda (x)
                               (let* ((name (cl-user (first x))))
                                 `(defmacro ,name ((&rest args)
                                                   &body body)
                                    (with-standard-io-syntax-1
                                     `(service-request ,(format nil "(~A (~{ ~S~})~{ ~S~})"
                                                                ',name 
                                                                (transform-s-expr args)
                                                                (transform-s-expr body)))))))
                           ts::*nrql-with-macros*)))
      
        (pprint fn stream) 
        (terpri stream) 
        (terpri stream)))))


(defun create-stubs ()
  (create-package-list)
  (create-server-case)
  (create-lracer-functions))


#|
    ((XML-OUTPUT)

     (PROCESS-RACER-EXPR
      (SECOND EXPR)
      nil
      N
      STATE
      output-string-stream)

     (format stream ":answer ~D \"~A\" \"~A\"" 
             n 
             
             (transform-value 
              (with-output-to-string (stream)
                (LISP-TO-XML *last-answer* stream)))

             (convert-output-to-string "")

             #-:mcl(terpri stream)
             ;;;#+:mcl(princ #\Newline stream)
             #+:mcl(princ #\Linefeed stream)
             
             (force-output stream)))

|#

