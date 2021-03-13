;;;-*- Mode: Lisp; Package: CL-USER -*-

(in-package "CL-USER")

(setf (logical-pathname-translations "dl-test")
      (list (list "**;*.*.*"
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
                               "**\\*.*"))))

#+:MCL (load "dl-test:timeout")
(load "dl-test:FaCT-1-11;FaCT")

;;; Initialise/set any required parameters:
(reset-verbosity)

;;; Name of the DL system:
(defconstant *system-name* "FaCT")

;;; Keywords from concept syntax which correspond to KRSS keywords
;;; top, bottom, and, or, not, some, all:
(defconstant *concept-syntax* '(*top* *bottom* and or not some all nil nil nil))


;;; KRSS style macros for Tbox and Abox definitions:

;;; Macro for primitive concept definitions:
(defmacro define-primitive-concept (name &optional (definition nil))
  `(defprimconcept ,name ,definition))

;;; Macro for non-primitive concept definitions:
(defmacro define-concept (name definition)
  `(defconcept ,name ,definition))

;;; Macro for primitive role definitions:
(defmacro define-primitive-role (name &key (parents nil) (transitive nil))
  `(defprimrole ,name :parents ,(if (listp parents)
                                  parents
                                  (list parents))
     :transitive ,transitive))

;;; Macro for primitive attribute definitions:
(defmacro define-primitive-attribute (name &key (parents nil))
  `(defprimattribute ,name :parents ,parents))

;;; Macro for instance assertions:
(defmacro instance (name concept)
  (declare (ignore name concept)))

;;; Macro for relationship assertions:
(defmacro related (name concept role)
  (declare (ignore name concept role)))

;;; Macro to query instantiation:
(defmacro individual-instance? (name concept)
  (declare (ignore name concept)))


;;; Functions for interacting with Tbox and Abox

;;; Function to initialise KB:
(defun *initialise-kb* (&optional (filename nil))
  (declare (ignore filename))
  (init-tkb))

;;; Function to classify KB:
(defun *classify-kb* ()
  (classify-all-concepts))

;;; Function to test the coherence of a concept in a classified KB:
(defun *test-coherent* (concept)
  (satisfiable concept))

;;; Function to test the coherence of a concept expression without
;;; reference to a KB:
(defun *test-concept* (concept)
  (alc-concept-coherent concept))

(defun name-filter (concept)
  (let ((name (name concept)))
    (cond ((eq name '*top*) 'top)
          ((eq name '*bottom*) 'bottom)
          (t name))))

;;; Function to return the direct subsumers of a concept:
(defun *direct-supers* (concept)
  (let ((concept (cond ((eq concept 'top) '*top*)
                       ((eq concept 'bottom) '*bottom*)
                       (t concept))))
    (mapcar #'name-filter (direct-supers concept))))

;;; Function to return the direct subsumers of a concept:
(defun *direct-subs* (concept)
  (let ((concept (cond ((eq concept 'top) '*top*)
                       ((eq concept 'bottom) '*bottom*)
                       (t concept))))
    (mapcar #'name-filter (direct-subs concept))))

;;; Function to initialise abox
(defun *initialise-abox* ())

;;; Function to perform abox inferences (if required):
(defun *classify-akb* ())


;;; Timeout for T98 KB tests:
(defconstant *kb-timeout* 500)

;;; Timeout for T98 sat tests:
(defconstant *sat-timeout* 100)

;;; Timeout for appn KB tests:
(defconstant *appn-timeout* 10000)

;;; Timeout for T98 abox tests:
(defconstant *abox-timeout* 500)


;;;*********************************************************
;;; Load the generic test code:

(load "dl-test:dl-tests")


;;;*********************************************************
;;; Run the tests:

(defun run-dl-tests ()
  (kb-tests *t98-kb-files*)
  (sat-tests *t98-con-files*)
  (appn-tests *appn-kb-files*)
  (synth-kb-tests *synth-kb-files*)
  ;(rand-kb-tests *random-kb-files*)
  ;(abox-tests *t98-abox-files*)
  )
