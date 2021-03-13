;;;-*- Mode: Lisp; Package: CL-USER -*-

(in-package "CL-USER")

(setf (logical-pathname-translations "dl-test")
      (list (list "**;*.*"
		  (concatenate 'string
                               #+:mswindows
                               (pathname-device (truename *load-pathname*))
                               #+:mswindows
                               ":"
                               (directory-namestring
                                (truename *load-pathname*))
                               #+:mcl
                               "**:*.*"
                               #+:unix
                               "**/*.*"
                               #+:mswindows
                               "**\\*.*"))
	    (list "*.*"
		  (concatenate 'string
                               #+:mswindows
                               (pathname-device (truename *load-pathname*))
                               #+:mswindows
                               ":"
                               (directory-namestring
                                (truename *load-pathname*))
                               "*.*"))))

#+:mcl(load "dl-test:timeout")
(load "dl-test:race-1-1;load-race")

;;; Initialise/set any required parameters:
(init-tbox 'dl-test)

;;; Name of the DL system:
(defparameter *system-name*
  (format nil "R-~A" *dl-prover-version*))

;;; Keywords from concept syntax which correspond to KRSS keywords
;;; top, bottom, and, or, not, some, all:
;(defparameter *concept-syntax* '(*top* *bottom* and or not some all nil nil nil))
(defparameter *concept-syntax* '(*top* *bottom* and or not some all at-least at-most exactly))


;;; KRSS style macros for Tbox and Abox definitions:

#|
;;; Macro for primitive concept definitions:
(defmacro define-primitive-concept (name &optional (definition nil))
  `(defprimconcept ,name ,definition))

;;; Macro for non-primitive concept definitions:
(defmacro define-concept (name definition)
  `(defconcept ,name ,definition))

;;; Macro for primitive role definitions:
(defmacro define-primitive-role (name)
  `(defprimrole ,name))

;;; Macro for instance assertions:
(defmacro instance (name concept))

;;; Macro for relationship assertions:
(defmacro related (name concept role))

;;; Macro to query instantiation:
(defmacro individual-instance? (name concept))
|#
 

;;; Functions for interacting with Tbox and Abox

;;; Function to initialise KB:
(defun *initialise-kb* (&optional (filename nil))
  (init-tbox 'dl-test)
  (when filename
    (setf (tbox-name (find-tbox 'dl-test))
          (intern (string-upcase (pathname-name filename))))))

;;; Function to classify KB:
(defun *classify-kb* ()
  (classify-tbox 'dl-test))

;;; Function to test the coherence of a concept in a classified KB:
(defun *test-coherent* (concept)
  (concept-satisfiable-p concept 'dl-test))

;;; Function to test the coherence of a concept expression without
;;; reference to a KB:
(defun *test-concept* (concept)
  (concept-satisfiable-p concept nil))

;;; Function to return the direct subsumers of a concept:
(defun *direct-supers* (concept)
  (mapcar #'(lambda (name-set)
              (remove +top-symbol+ name-set))
          (atomic-concept-parents concept 'dl-test)))

;;; Function to return the direct subsumees of a concept:
(defun *direct-subs* (concept)
  (mapcar #'(lambda (name-set)
              (remove +bottom-symbol+ name-set))
          (atomic-concept-children concept 'dl-test)))

;;; Function to initialise abox
(defun *initialise-abox* ()
  (init-abox 'dl-test-abox 'dl-test))

;;; Function to perform abox inferences (if required):
(defun *classify-akb* ()
  (realize-abox 'dl-test-abox))

(defun brace-reader (stream char)
  (declare (ignore char))
  (loop for ch = (read-char stream)
        until (eq ch #\}))
  (read stream))

(set-macro-character #\{ #'brace-reader)


;;; Timeout for T98 KB tests:
(defparameter *kb-timeout* 500)

;;; Timeout for T98 sat tests:
(defparameter *sat-timeout* 100)

;;; Timeout for appn KB tests:
(defparameter *appn-timeout* 10000)

;;; Timeout for T98 abox tests:
(defparameter *abox-timeout* 500)


;;;*********************************************************
;;; Load the test code:

(load "dl-test:dl-tests")


;;;*********************************************************
;;; Run the tests:

(defun run-dl-tests ()
  (abox-tests *t98-abox-files*)
  (synth-kb-tests *synth-kb-files*)
  (appn-tests *new-appn-kb-files*)
  (rand-kb-tests *random-kb-files*)
  (sat-tests *t98-con-files*)
  (kb-tests *t98-kb-files*)
  (abox-tests *t98-abox-files* :abox-data-dir *new-abox-data-dir*))
