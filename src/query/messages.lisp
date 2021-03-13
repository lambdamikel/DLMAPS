;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

(defun nrql-warning (string &rest args)
  (when *warnings-p*
    (format t "~%*** NRQL WARNING: ")
    (apply #'format t (string-upcase string) args)))

(defun nrql-error (string &rest args)
  #+:nrql-dev
  (let ((string 
         (concatenate 'string
                      "~%*** NRQL ERROR: "
                      (string-upcase string))))
    (apply #'error string args))
  #-:nrql-dev
  (apply #'error string args))

(defun nrql-runtime-error (string)
  #+:nrql-dev
  (error "~%*** NRQL RUNTIME ERROR: ~A" string)
  #-:nrql-dev
  (error string))

;;;
;;;
;;;

(defun warn-inconsistent (query) 
  (nrql-warning "~A IS INCONSISTENT" (iterator-id query)))

(defun warn-tautological (query) 
  (nrql-warning "~A IS TAUTOLOGICAL" (iterator-id query)))

(defun warn-tbox-has-changed ()
  (nrql-warning "TBox has changed - re-preparing and executing!"))

;;;
;;;
;;;

(defun query-deadlock-warning (queries)
  
  (nrql-warning "Denied due to deadlock prevention! 
    The following queries will not terminate automatically, 
    since they have been started in lazy incremental mode:
    ~A."
                (mapcar #'iterator-id queries)))

(defun rule-deadlock-warning (rules)
  
  (nrql-warning "Denied due to deadlock prevention! 
    The following rules will not terminate automatically, 
    since they have been started in lazy incremental mode:
    ~A."
                (mapcar #'iterator-id rules)))
