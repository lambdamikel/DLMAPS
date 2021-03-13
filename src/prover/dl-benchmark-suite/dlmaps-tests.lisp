;;-*- Mode: Lisp; Package: CL-USER -*-

(in-package "CL-USER")


;;; setze prover::*debug-node* bei Klassfikationsfehlern auf den Knoten-Namen, 
;;; um gezielte Debug-Informationen zu erhalten *nur* fuer diesen Knoten!

(defconstant +args+ 
  (list :debug-p nil
        :cache-models-p t
        :use-cached-models-p t
        :subtableau-caching-p t
        :semantic-branching-p t
        :use-unsat-cache-p t
        :maintain-prover-state-p t
        :delete-nodes-p nil
        :reuse-nodes-p nil
        :non-determinism-p nil))

;;;
;;;
;;;

(defconstant +top-symbol+ 'prover::top)

(defconstant +bottom-symbol+ 'prover::bottom)

;;; Initialise/set any required parameters:
(prover::init-tbox 'dl-test)

;;; Name of the DL system:
(defparameter *system-name* "DLMAPS")

;;; Keywords from concept syntax which correspond to KRSS keywords
;;; top, bottom, and, or, not, some, all:
;(defparameter *concept-syntax* '(*top* *bottom* and or not some all nil nil nil))

(defparameter *concept-syntax* '(*top* *bottom* and or not some all at-least at-most exactly))

;; (defparameter *concept-syntax* '(*top* *bottom* and or not some all nil nil nil))


;;; KRSS style macros for Tbox and Abox definitions:

#| 

;;; Macro for primitive concept definitions:
(defmacro define-primitive-concept (name &optional (definition nil))
  `(define-primitive-concept ,name ,definition))

;;; Macro for non-primitive concept definitions:
(defmacro define-concept (name definition)
  `(defconcept ,name ,definition))

;;; Macro for primitive role definitions:
(defmacro define-primitive-role (name)
  `(defprimrole ,name))

;;; Macro for instance assertions:
(defmacro prover:instance (name concept))

;;; Macro for relationship assertions:
(defmacro related (name concept role))

;;; Macro to query instantiation:
(defmacro individual-instance? (name concept))

|# 

;;; Function to initialise KB:
(defun *initialise-kb* (&optional (filename nil))
  (declare (ignorable filename))
  (prover::init-tbox 'dl-test))

;;; Function to classify KB:

(defun *classify-kb* ()
  (apply #'prover::classify 'dl-test +args+))


#| 
(defun *classify-kb* ()
  nil)
|#

;;; Function to test the coherence of a concept in a classified KB:
(defun *test-coherent* (concept)
  (apply #'prover::concept-satisfiable-p concept          
         +args+))

;;; Function to test the coherence of a concept expression without
;;; reference to a KB:

(defun *test-concept* (concept)
  (prover::delete-all-tboxes)
  (let ((concept (tools:change-package-of-description concept :prover nil nil)))
    (apply #'prover::concept-satisfiable-p concept :tbox nil +args+)))

;;; Function to return the direct subsumers of a concept:
(defun *direct-supers* (concept)
  (let ((concept (tools:change-package-of-description concept :prover nil nil)))
    (apply #'prover::classify prover::*cur-tbox* +args+)
    (tools:change-package-of-description 
     (mapcar #'prover::unparse (mapcar #'first 
                                       (mapcar #'prover::sort-concepts
                                               (mapcar #'(lambda (x) 
                                                           (mapcar #'prover::parse-concept  
                                                                   (ensure-list x)))
                                                       (prover::parents concept))))))))

;;; Function to return the direct subsumees of a concept:
(defun *direct-subs* (concept)
  (let ((concept (tools:change-package-of-description concept :prover nil nil)))
    (apply #'prover::classify prover::*cur-tbox* +args+)          
    (tools:change-package-of-description
     (mapcar #'prover::unparse (mapcar #'first
                                       (mapcar #'prover::sort-concepts
                                               (mapcar #'(lambda (x) 
                                                           (mapcar #'prover::parse-concept  
                                                                   (ensure-list x)))
                                                       (prover::children concept))))))))

;;; Function to initialise abox
(defun *initialise-abox* ()
  (prover::init-abox 'dl-test-abox))

;;; Function to perform abox inferences (if required):
(defun *classify-akb* ()
  ;(realize-abox 'dl-test-abox)
  ;(apply #'prover::abox-consistent-p 'dl-test-abox +args+)
  )

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

;(load "dl-test:dl-tests")


;;;*********************************************************
;;; Run the tests:

(defparameter *max-kb* 2) ; sonst stack-overflow

(defun run-dl-tests ()
  (prover::reset-statistics)
  (prover::delete-all-tboxes)
  (racer::delete-all-tboxes)
#|
  (kb-tests *t98-kb-files*) ; OK
  (abox-tests *t98-abox-files* :abox-data-dir *new-abox-data-dir*)   ; OK 
  (synth-kb-tests *synth-kb-files*) ; OK
  (appn-tests *appn-kb-files*) ; OK
  (sat-tests *t98-con-files*) ; OK
  (kb-tests *t98-kb-files*) ; OK 
  |#
  (rand-kb-tests *random-kb-files*) ; verwenden at-least / at-most!
  
  )

