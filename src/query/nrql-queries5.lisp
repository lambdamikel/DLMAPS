;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;;
;;;

(defpersistentclass nrql-query (query)
  ((dont-add-abox-duplicates-p :reader dont-add-abox-duplicates-p :initform nil)

   (kb-id :reader kb-id :initform nil)
   (kb-changed-token-delivered-p :reader kb-changed-token-delivered-p :initform nil)
   (add-rule-consequences-p :reader add-rule-consequences-p :initform nil)   

   (deliver-phase-two-warning-tokens-p :reader deliver-phase-two-warning-tokens-p :initform nil)
   (deliver-kb-has-changed-warning-tokens-p :reader deliver-kb-has-changed-warning-tokens-p :initform t)
   
   (told-information-reasoning-p :reader told-information-reasoning-p :initform nil)
   (phase-two-started-p :reader phase-two-started-p :initform nil)

   (rule-con-pattern :reader rule-con-pattern :initform nil :initarg :rule-con-pattern) 
   (new-ind-ops :reader new-ind-ops :initform nil :initarg :new-ind-ops)
   (new-abox-assertions :reader new-abox-assertions :initform nil)

   (abox-assertions-to-add :reader abox-assertions-to-add :initform nil)

   ;;; Query Premise

   (premise :reader premise :initform nil :initarg :premise)
   (added-premise-axioms :reader added-premise-axioms :initform nil)

   (two-phase-processing-p :reader two-phase-processing-p :initform nil)))


#+:midelora
(defpersistentclass midelora-nrql-query (nrql-query))

;;;
;;;
;;;

(defmethod is-rule-p ((query nrql-query))
  (rule-con-pattern query))

(defmethod is-rule-p ((query query))
  nil)

(defmethod initialize-description :after ((query nrql-query))
  (with-slots (modifies-state-p	;;;rule-con-pattern
	       premise) query

    (when (or ;;; rule-con-pattern 
              ;;; muss nicht beurecksichtig werden, 
              ;;; nur Regeln, die wirklich was hinzufuegen
              ;;; veraendern die Abox, kann hier nicht
              ;;; entschieden werden, s. execute-query!
	   
	   premise)
      (setf modifies-state-p t))))

;;;
;;; 
;;;

(defpersistentclass nrql-atomic-query (nrql-query atomic-query) nil)

(defpersistentclass nrql-complex-query (nrql-query complex-query hybrid-query) nil)


;;;
;;;
;;;

(defpersistentclass nrql-true-query (nrql-query true-query) nil)

(defpersistentclass nrql-false-query (nrql-query false-query) nil)

;;;
;;;
;;;


(defpersistentclass nrql-top-query (top-query nrql-atomic-query) nil)

(defpersistentclass nrql-bottom-query (bottom-query nrql-atomic-query) nil)

(defpersistentclass nrql-same-as-query (same-as-query nrql-atomic-query) nil)

(defpersistentclass nrql-instance-retrieval-query (racer-instance-retrieval-query nrql-atomic-query) nil)

(defpersistentclass nrql-has-known-successor-retrieval-query (racer-has-known-successor-retrieval-query nrql-atomic-query) nil)

(defpersistentclass nrql-edge-retrieval-query (racer-edge-retrieval-query nrql-atomic-query) nil)

(defpersistentclass nrql-cd-edge-retrieval-query (racer-cd-edge-retrieval-query nrql-atomic-query) nil)  

;;;
;;;
;;;

(defpersistentclass nrql-and-query (and-query nrql-complex-query) nil) 

(defpersistentclass nrql-or-query (or-query nrql-complex-query) nil)

;;;
;;; Midelora
;;; 

#+:midelora

(progn

  (defpersistentclass midelora-nrql-atomic-query (midelora-nrql-query atomic-query) nil)

  (defpersistentclass midelora-nrql-complex-query (midelora-nrql-query complex-query) nil)

  ;;;
  ;;;
  ;;;  
  
  (defpersistentclass midelora-nrql-true-query (midelora-nrql-query true-query) nil)

  (defpersistentclass midelora-nrql-false-query (midelora-nrql-query false-query) nil)

;;;
;;;
;;;

  (defpersistentclass midelora-nrql-top-query (top-query midelora-nrql-atomic-query) nil)

  (defpersistentclass midelora-nrql-bottom-query (bottom-query midelora-nrql-atomic-query) nil)

  (defpersistentclass midelora-nrql-same-as-query (same-as-query midelora-nrql-atomic-query) nil)

  (defpersistentclass midelora-nrql-instance-retrieval-query (midelora-instance-retrieval-query midelora-nrql-atomic-query) nil)

  (defpersistentclass midelora-nrql-has-known-successor-retrieval-query (midelora-has-known-successor-retrieval-query midelora-nrql-atomic-query) nil)
  
  (defpersistentclass midelora-nrql-edge-retrieval-query (midelora-edge-retrieval-query midelora-nrql-atomic-query) nil)
  
;;;
;;;
;;;

  (defpersistentclass midelora-nrql-and-query (and-query midelora-nrql-complex-query) nil) 
  
  (defpersistentclass midelora-nrql-or-query (or-query midelora-nrql-complex-query) nil)
  
  )