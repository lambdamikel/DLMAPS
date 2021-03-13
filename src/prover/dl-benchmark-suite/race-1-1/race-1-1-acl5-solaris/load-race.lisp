;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(in-package :cl-user)

(let ((excl::*redefinition-warnings* nil))
  
  (let ((*enable-package-locked-errors* nil))
    (defun excl::check-for-duplicate-definitions-in-file (fspec type when
							  &optional icsp)
      (declare (ignore fspec type when icsp)))))


(let* ((load-pathname
        (concatenate 'string
                     (directory-namestring (translate-logical-pathname *load-pathname*))))
       (example-directory
        (format nil "~Aexamples/*.*" load-pathname))
       (race-directory
        (format nil "~A/*.*" load-pathname)))
  (setf (logical-pathname-translations "race")
        `(("*.*" ,race-directory)
          ("examples;*.*" ,example-directory))))

(defun load-race ()
  (load "race:race-1-1-0.fasl" :verbose t))

(load-race)
