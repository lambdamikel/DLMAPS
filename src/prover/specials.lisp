;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;; Verwaltungslisten/Heaps fuehren? nur fuer ABOX1-Klasse! 
;;; STANDARDMAESSIG IST ALLES AUS!!!
;;; nur durch Strategie-Objekte werden Kontexte / Specials gesetzt!
;;; also dort aendern, s. strategy.lisp
;;; 

(defparameter *maintain-unexpanded-atomic-concepts-heap-p* nil)

(defparameter *maintain-unexpanded-atomic-concepts1-heap-p* nil)

(defparameter *maintain-unexpanded-and-concepts-heap-p* nil)

(defparameter *maintain-unexpanded-and-concepts-heap1-p* nil)

(defparameter *maintain-unexpanded-or-concepts-heap-p* nil)

(defparameter *maintain-unexpanded-or-concepts1-heap-p* nil)

(defparameter *maintain-unexpanded-some-concepts-heap-p* nil)

(defparameter *maintain-unexpanded-some-concepts1-heap-p* nil)

(defparameter *maintain-unexpanded-at-least-concepts-heap-p* nil)

(defparameter *maintain-unexpanded-at-least-concepts1-heap-p* nil)

(defparameter *maintain-unexpanded-attribute-exists-concepts-heap-p* nil)

(defparameter *maintain-unexpanded-attribute-exists-concepts1-heap-p* nil)

;;;
;;;
;;; 

(defparameter *maintain-old-nodes-p* nil) 

(defparameter *maintain-leaf-nodes-p* nil)

(defparameter *maintain-deactivated-nodes-p* nil)

(defparameter *maintain-active-nodes-p* nil)

(defparameter *maintain-cache-sat-nodes-p* nil)

(defparameter *maintain-blocked-nodes-p* nil)

;;;
;;; Syntax
;;;

(defparameter *cross-referencing-p* t)

(defparameter *syntactic-consistency-checking-p* nil)

(defparameter *syntactic-consistency-checking-of-some/all-conjuncts-p* nil)

(defparameter *create-negated-concept-p* t)

(defparameter *use-store-p* t)

(defparameter *insert-into-store-p* t)

(defvar *old-concept-p* nil)

(defvar *create-inverse-role-p* t) 

;;;
;;; f. Debugging-Zwecke: checken mit Racer
;;; beim Klassifizieren der TBOx
;;; 

(defparameter *racer-validation-p* nil)

(defparameter *logging-p* nil)

(defparameter *racer-tbox* 'racer-user::default)

;;;
;;;
;;;

(defparameter *reflexive-checks-p* t)

;;;
;;;
;;;

(defvar *start-time* 0)

(defvar *abox-init-required-p* t)

(defvar *time-for-node-creation* 0)

(defvar *time-for-edge-creation* 0)

(defvar *time-spend-in-undo* 0)

(defvar *no-of-undos* 0)

;;;
;;;
;;;

(defvar *rollback-active-p* nil)

(defvar *maintain-history-p* nil)

;;;
;;;
;;;

(defvar *how-many* nil)

(defvar *debug-p* nil)

(defvar *debug-node* nil)

(defvar *debug-concept* nil)

(defvar *announce-p* nil)

(defvar *show-prover-incarnations-p* nil)

(defvar *semantic-branching-p* t)

(defvar *maintain-prover-state-p* nil)

(defvar *check-if-still-blocked-p* t)

(defvar *reuse-nodes-p* nil)

(defvar *break-p* nil)

(defvar *visualize-p* nil)

(defvar *completion-found-hook* nil)

(defvar *compute-all-completions-p* nil)

(defvar *dont-invalidate-store-p* nil)

;;;
;;;
;;;

(defvar *blocking-enabled-p* nil)

(defvar *adjust-blocking-dependencies-p* t)

(defvar *delete-nodes-p* nil)

(defvar *combined-some-all-rule-p* nil)

(defvar *propagation-of-transitive-all-concepts-p* nil)

(defvar *dynamic-blocking-p* nil)

;;;
;;; Abox (Knoten)-Modelle
;;; 

(defparameter *store-ind-models-p* nil)

(defparameter *max-no-of-ind-models-per-node* 3)


;;;
;;; Konzept-(Tableuax)-Modelle
;;;

(defparameter *max-no-of-concept-models* 3)

(defvar *use-told-subsumers-p* t)

(defvar *compute-told-subsumers-p* t)

(defvar *use-cached-models-p* t)

(defvar *cache-models-p* t)

(defvar *subtableau-caching-p* t)

(defvar *use-unsat-cache-p* t)

;;;
;;;
;;;

(defparameter *keep-det-assertions-p* nil)

(defparameter *compute-core-model-p* nil)

(defparameter *store-instance-retrieval-model-p* nil)

;;;
;;;
;;;

(defvar *abox* nil)

(defvar *clashes* nil)

(defvar *strategy* nil)

(defvar *temp-abox-name* 0)

(defvar *completions* nil)

(defvar *meta-constraints* nil)

;;;
;;;
;;;

(defvar *abox-dispatcher* nil)

(defvar *language-dispatcher* nil)

;;;
;;;
;;;

(defvar *all-tboxes* nil)

(defvar *node* nil)

(defvar *nodes* nil)

(defvar *concept* nil)

(defvar *taxonomy* nil)


;;;
;;;
;;;

(defvar *cur-store*)

(defvar *atoms-store*)

(defvar *and-store*)

(defvar *or-store*)

(defvar *some-store*)

(defvar *all-store*)

(defvar *at-least-store*)

(defvar *at-most-store*)

(defvar *roles-store*)

#+:use-membership-tables (defvar *expanded-table*)

#+:use-membership-tables (defvar *unexpanded-table*)

;;;
;;;
;;;

(defvar *all-instance-tests* 0)

(defvar *obvious-non-instance-hits* 0)

(defvar *obvious-instance-hits* 0) 

(defvar *individual-instance-proofs* 0) 

;;;
;;;
;;;

(defvar *initial-concept-sat-cache-hits* 0)

(defvar *initial-concept-unsat-cache-hits* 0)

(defvar *initial-concept-sat-cache-queries* 0)

(defvar *initial-concept-unsat-cache-queries* 0)

(defvar *mm-queries* 0)

(defvar *mm-hits* 0)

(defvar *concept-queries* 0)

(defvar *true-concept-queries* 0)

(defvar *sat-cache-queries* 0)

(defvar *sat-cache-hits* 0)

(defvar *concept-mm-queries* 0)

(defvar *concept-mm-hits* 0)

(defvar *true-abox-consistency-tests* 0)

(defvar *true-abox-subsumption-tests* 0)


(defvar *blocked-nodes* 0)

(defvar *created-nodes* 0)




(defvar *subsumes-queries* 0)

(defvar *true-subsumes-queries* 0)

(defvar *subsumes-mm-hits* 0)

(defvar *subsumes-ts-hits* 0)

;;;
;;;
;;; 

(defvar *language* nil)

(defvar *cur-abox* nil)

(defvar *cur-tbox* nil)

;;;
;;;
;;;

(defvar *all-dls* nil)

;;;
;;;
;;;

#+:use-membership-tables
(defvar *create-membership-tables-p* t)

#-:use-membership-tables
(defvar *create-membership-tables-p* nil)

(defvar *all-aboxes* nil)

(defvar *avl-key-counter* 0)

;;;
;;;
;;;

(defmacro with-prover-standard-settings (&body body)
  `(let ((*cross-referencing-p* t)
         (*syntactic-consistency-checking-p* t)
         (*syntactic-consistency-checking-of-some/all-conjuncts-p* nil)
         (*create-negated-concept-p* t)
         (*use-store-p* t)
         (*insert-into-store-p* t)
         (*create-inverse-role-p* t)

         (*old-concept-p* nil)

         (*semantic-branching-p* t)
         (*maintain-prover-state-p* nil)
         (*reuse-nodes-p* nil)
         (*compute-all-completions-p* nil)
         (*blocking-enabled-p* nil)
         (*delete-nodes-p* nil)
         (*combined-some-all-rule-p* nil)
         (*propagation-of-transitive-all-concepts-p* nil)
         (*dynamic-blocking-p* nil)
         (*store-ind-models-p* nil)
         (*max-no-of-ind-models-per-node* 3)
         (*max-no-of-concept-models* 3)
         (*use-told-subsumers-p* t)
         (*compute-told-subsumers-p* t)
         (*use-cached-models-p* t)
         (*cache-models-p* t)
         (*subtableau-caching-p* t)
         (*use-unsat-cache-p* t)
         (*keep-det-assertions-p* nil)
         (*compute-core-model-p* nil)
         (*store-instance-retrieval-model-p* nil))

     ,@body))


(defparameter *prover-specials* 
  (sort '(*strategy*

          *MAINTAIN-DEACTIVATED-NODES-P* 
          *MAINTAIN-LEAF-NODES-P* 
          *MAINTAIN-CACHE-SAT-NODES-P*
          *MAINTAIN-OLD-NODES-P* 
          *MAINTAIN-ACTIVE-NODES-P*
          *MAINTAIN-BLOCKED-NODES-P* 

          *MAINTAIN-UNEXPANDED-SOME-CONCEPTS-HEAP-P* 
          *MAINTAIN-UNEXPANDED-SOME-CONCEPTS1-HEAP-P* 
          
          *MAINTAIN-UNEXPANDED-AT-LEAST-CONCEPTS-HEAP-P*
          *MAINTAIN-UNEXPANDED-AT-LEAST-CONCEPTS1-HEAP-P* 
          
          *MAINTAIN-UNEXPANDED-ATTRIBUTE-EXISTS-CONCEPTS-HEAP-P* 
          *MAINTAIN-UNEXPANDED-ATTRIBUTE-EXISTS-CONCEPTS1-HEAP-P* 

          *MAINTAIN-UNEXPANDED-ATOMIC-CONCEPTS-HEAP-P* 
          *MAINTAIN-UNEXPANDED-ATOMIC-CONCEPTS1-HEAP-P*
          
          *MAINTAIN-UNEXPANDED-OR-CONCEPTS-HEAP-P* 
          *MAINTAIN-UNEXPANDED-OR-CONCEPTS1-HEAP-P* 
          
          *MAINTAIN-UNEXPANDED-AND-CONCEPTS-HEAP-P*
          *MAINTAIN-UNEXPANDED-AND-CONCEPTS-HEAP1-P*
          
          *MAINTAIN-PROVER-STATE-P*
          
          *cross-referencing-p*
          *syntactic-consistency-checking-p*
          *syntactic-consistency-checking-of-some/all-conjuncts-p*
          *create-negated-concept-p*
          *use-store-p*
          *insert-into-store-p*
          *old-concept-p*
          *create-inverse-role-p* 
          *racer-validation-p*
          *logging-p*
          *racer-tbox*

          *start-time*
          *continuation-active-p*
          *time-for-node-creation*
          *time-for-edge-creation*
          *time-spend-in-undo*
          *no-of-undos*
          *how-many*
          *debug-p*
          *announce-p*
          *show-prover-incarnations-p*
          *semantic-branching-p*
          *maintain-prover-state-p*
          *reuse-nodes-p*
          *break-p*
          *visualize-p*
          *completion-found-hook*
          *compute-all-completions-p*
          *dont-invalidate-store-p*
          *blocking-enabled-p*
          *delete-nodes-p*
          *combined-some-all-rule-p*
          *propagation-of-transitive-all-concepts-p*
          *dynamic-blocking-p*
          *store-ind-models-p*
          *max-no-of-ind-models-per-node* 
          *max-no-of-concept-models*
          *use-told-subsumers-p*
          *compute-told-subsumers-p*
          *use-cached-models-p*
          *cache-models-p*
          *subtableau-caching-p*
          *use-unsat-cache-p*
          *keep-det-assertions-p*
          *compute-core-model-p*
          *store-instance-retrieval-model-p*
          *abox*
          *temp-abox-name*
          *completions*
          *meta-constraints*
          *abox-dispatcher*
          *language-dispatcher*
          *all-tboxes*
          *node*
          *nodes*
          *concept*
          *taxonomy*
          *cur-store*
          *atoms-store*
          *and-store*
          *or-store*
          *some-store*
          *all-store*
          *at-least-store*
          *at-most-store*
          *roles-store*
          #+:use-membership-tables *expanded-table*
          #+:use-membership-tables *unexpanded-table*
          *initial-concept-sat-cache-hits*
          *initial-concept-unsat-cache-hits*
          *initial-concept-sat-cache-queries*
          *initial-concept-unsat-cache-queries*
          *mm-queries*
          *mm-hits*
          *concept-queries*
          *true-concept-queries*
          *sat-cache-queries*
          *sat-cache-hits*
          *concept-mm-queries*
          *concept-mm-hits*
          *true-abox-consistency-tests*
          *true-abox-subsumption-tests*
          *blocked-nodes*
          *created-nodes*
          *subsumes-queries*
          *true-subsumes-queries*
          *subsumes-mm-hits*
          *subsumes-ts-hits*
          *language*
          *cur-abox*
          *cur-tbox*
          *all-dls*
          #+:use-membership-tables
          *create-membership-tables-p*
          #-:use-membership-tables
          *create-membership-tables-p*
          *all-aboxes*
          *avl-key-counter*)
        #'string-lessp))



