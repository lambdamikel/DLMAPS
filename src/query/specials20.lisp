;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;;
;;;

(defvar *sym-count* 0)

(defconstant +secret-prefix+ 'secret-cdsc7897qcjk)

(defun create-marker (&optional sym (new-p t))
  (if sym       
      (if new-p 
          (intern (format nil "~A-~A-~A" +secret-prefix+ sym (incf *sym-count*)) :cl-user)
        (intern (format nil "~A-~A" +secret-prefix+ sym)  :cl-user))
    (intern (format nil "~A-~A" +secret-prefix+ (incf *sym-count*))  :cl-user)))

(defvar +secret-abox+ (create-marker 'temp-abox))

;;;
;;;
;;;

(defconstant +equal-role+ 'nrql-equal-role)

;;;
;;;
;;;


(defvar *last-query* nil)

#-:dlmaps 
(defvar *cur-substrate* nil)

#+:midelora
(defvar *type-of-substrate* 'midelora-substrate)

#-:midelora
(defvar *type-of-substrate* 'racer-dummy-substrate)

;;;
;;; Prozesse
;;;

(defvar *lock* nil)

;;;
;;;
;;;

(defvar *racer-check-if-atoms-defined-p*  t)

;;;
;;; werden von "with-nrql-settings" gesetzt: 
;;; (verhindert, dass set-current-tbox/abox verwendet werden muss!)
;;; 

(defvar *nrql-abox* nil)

(defvar *nrql-tbox* nil)

;;;
;;; Waehrend der Query Execution: 
;;; 

(defvar *running-abox* nil)

(defvar *running-tbox* nil)

(defvar *running-query* nil)

(defvar *running-substrate* nil)

(defvar *previous-conjunct* nil)

(defvar *candidates* nil)

;;;
;;;
;;;

(defvar *ano-counter* 0)

(defvar *iterator-id* 0)

(defvar *process-id* nil) ; fuer with-critical-section

(defvar *multiprocess-queries* 
  #-:multiprocess-queries nil
  #+:multiprocess-queries t)  

;;;
;;; Process Pool 
;;;

(defvar *process-pooling* 
  #+:process-pooling t
  #-:process-pooling nil)

;;;
;;;
;;;

(defparameter *min-pool-size* 10)

(defparameter *max-pool-size* 50)

;;;
;;;
;;;

(defvar *pool-counter* 0)

(defvar *all-processes* nil)

(defvar *process-pool* nil)

;;;
;;;
;;;

(defvar *all-dboxes* nil)

(defvar *all-queries* nil)

(defvar *all-rules* nil)

(defvar *ready-queries* nil)

(defvar *ready-rules* nil)

(defvar *active-queries* nil) ; deren Prozess noch lebt! -> unterteilt in running und waiting! 

(defvar *active-rules* nil) 

(defvar *processed-queries* nil)

(defvar *processed-rules* nil)

;;;
;;;
;;;

(defconstant +sleep-time+ 0.01) 

;;;
;;;
;;;

(defparameter *allow-negated-roles-p* t) ; Queries der Art (?*x ?*y (NOT R)) erlaubt? 
;;; koennen erst ab Racer 1.8 bearbeitet werden, UNA muss ausgeschaltet werden!
;;; s. entsp. Code in Compiler22.lisp

;;;
;;;
;;;

(defparameter *add-role-assertions-for-datatype-properties-p* nil)

;;; 
;;; bestimmt, ob fuer concept assertions, 
;;; die durch OWL datatype properties entstehen, wie z.B.
;;;
;;; (SOME |http://www.owl-ontologies.com/unnamed.owl#age| (EQUAL RACER-INTERNAL%HAS-INTEGER-VALUE 40))
;;;
;;; Rollen-Nachfolger angelegt werden, damit ":constraint"-Query Atoms verwendet werden koennen
;;;


;;;
;;;
;;;

(defparameter *check-abox-consistency-p* t)

;;;
;;;
;;;

(defparameter *tuple-at-a-time-p* nil)

(defparameter *proactive-tuple-computation-p* t)

;;;
;;;
;;;

(defparameter *two-phase-processing-p* nil) 

(defparameter *deliver-phase-two-warning-tokens-p* nil)

(defparameter *deliver-kb-has-changed-warning-tokens-p* t)

;;;
;;;
;;;

(defparameter *initial-abox-mirroring-p* nil) ; sollen die Caches beim Erzeugen des Substrates gefuellt werden?  

(defparameter *ensure-tbox-classification-p* nil) ; nur in Kombination mit *initial-tbox-mirroring-p* 
  
(defparameter *told-information-reasoning-p* nil) ; soll auf Racer-Reasoning verzichtet werden? INCOMPLETE!

(defparameter *classify-concepts-in-instance-assertions-p* nil) ; sollen auch komplexe C in i : C TBox-klassifiziert werden? -> very smart Abox mirror

;;;
;;;
;;;

(defparameter *optimize-p* t)

(defparameter *optimizer-use-cardinality-heuristics-p* t)

(defparameter *generate-code-p* t)
 
#+:only-runtime-evaluation
(progn   
  (defparameter *compile-queries-p* nil)
  
  (defparameter *compile-inline-p* nil)

  (defparameter *runtime-evaluation-p* t))


#-:only-runtime-evaluation
(progn 
  (defparameter *compile-queries-p* t)
  
  (defparameter *compile-inline-p* nil)

  (defparameter *runtime-evaluation-p* nil))

;;;
;;; 
;;;

(defparameter *warnings-p* t)

;;;
;;;
;;;

(defparameter *rewrite-to-dnf-p* t)

(defparameter *rewrite-semantically-p* t)

(defparameter *rewrite-defined-concepts-p* nil)

;;;
;;; 
;;;

(defparameter *report-inconsistent-queries-p* nil)

(defparameter *report-tautological-queries-p* nil)

;;;
;;;
;;;

(defparameter *use-repository-p* nil)

(defparameter *put-into-repository-p* nil)

(defparameter *syntactic-repository-p* nil)

;;;
;;;
;;;

(defparameter *use-unique-name-assumption-p* t) ; NICHT redundant! damit kann man UNA-VOIS temporaer "abschalten" 
;;; hat nichts mit racer::*use-unqiue-name-assumption* zu tun!!!

(defparameter *exclude-permutations-p* nil)

;;;
;;;
;;;

(defparameter *continuation-based-instance-retrieval-p* nil)


;;;
;;;
;;;

(defparameter *timeout* nil) ; hat keine Relevanz f. in Racer eingebautes nRQL! racer::*server-timeout*! 

(defvar *how-many* nil)

;;;
;;;
;;;

(defparameter *add-rule-consequences-p* t) ; f.d. Rules

(defparameter *dont-add-abox-duplicates-p* nil) 

;;;
;;; diese werden gedumpt: 
;;;

(defconstant +nrql-specials+
  '(*all-substrates*
    *all-dboxes*
    *all-queries*
    *all-rules*
    *ready-queries*
    *ready-rules*
    ;; *active-queries* koennen nicht gedumpt werden!
    ;; *active-rules*   weil Prozesse laufen!
    *processed-queries* 
    *processed-rules* 
    *cur-substrate*
    *last-query*
    *type-of-substrate*
    *nrql-abox*
    *nrql-tbox*
    *ano-counter* 
    *iterator-id*
    *process-id*
    *sym-count*
    *lock*
    *racer-check-if-atoms-defined-p*
    *multiprocess-queries*
    *process-pooling*
    *min-pool-size*
    *max-pool-size*
    *pool-counter*
    *allow-negated-roles-p*
    *check-abox-consistency-p*
    *tuple-at-a-time-p*
    *proactive-tuple-computation-p*
    *two-phase-processing-p*
    *deliver-phase-two-warning-tokens-p*
    *initial-abox-mirroring-p*
    *ensure-tbox-classification-p*
    *told-information-reasoning-p*
    *classify-concepts-in-instance-assertions-p*
    *optimize-p*
    *generate-code-p*
    *compile-queries-p*
    *compile-inline-p*
    *runtime-evaluation-p*
    *warnings-p*
    *rewrite-to-dnf-p*
    *rewrite-semantically-p*
    *report-inconsistent-queries-p*
    *report-tautological-queries-p*
    *use-repository-p*
    *put-into-repository-p*
    *syntactic-repository-p*
    *use-unique-name-assumption*
    *exclude-permutations-p*
    *timeout*
    *how-many*
    *add-rule-consequences-p*
    *dont-add-abox-duplicates-p*
    ))