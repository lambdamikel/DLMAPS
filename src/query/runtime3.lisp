;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

(defgeneric matches-p (query substrate-object ))

(defgeneric retrieve-matching-objects (substrate query &key only-thematic-objects))

;;;
;;; Reasoning: Simple Querys
;;;

(defmethod matches-p ((query substrate-simple-node-query) (description simple-node-description))
  (implies-p description query))

(defmethod matches-p ((query substrate-simple-edge-query) (description simple-edge-description))
  (implies-p description query))

;;;
;;;
;;;
;;;

(defmethod matches-p ((query substrate-racer-node-query) (description racer-node-description))
  (implies-p description query))

(defmethod matches-p ((query substrate-racer-edge-query) (description racer-edge-description))
  (implies-p description query))

;;;
;;;
;;;

(defmethod matches-p ((description description) (query query))
  (matches-p query description))

(defmethod matches-p ((query query) (description description))
  (implies-p description query))

;;;
;;;
;;;

(defmethod matches-p ((query unary-query) (object substrate-node))
  (matches-p query (description object)))

(defmethod matches-p ((query substrate-predicate-node-query) (object substrate-node))
  (funcall (predicate query) object))

;;;
;;;
;;;

(defmethod retrieve-matching-objects ((substrate substrate) (query unary-query) &key only-thematic-objects)
  (declare (ignore only-thematic-objects))
  (remove-if-not #'(lambda (x) 
                     (matches-p query x))
                 (get-nodes substrate)))

(defmethod retrieve-matching-objects ((substrate racer-substrate) (query unary-query) &key only-thematic-objects)
  (remove-if-not #'(lambda (x) 
                     (matches-p query x))
                 (if only-thematic-objects
                     (thematic-nodes substrate)
                   (get-nodes substrate))))

;;;
;;;
;;;

(defmethod matches-p ((query binary-query) (object substrate-edge))
  (matches-p query (description object)))

(defmethod matches-p ((query substrate-predicate-edge-query) (object substrate-edge))
  (funcall (predicate query) object))

;;;
;;;
;;;

(defmethod retrieve-matching-objects ((substrate substrate) (query binary-query) &key only-thematic-objects)
  (declare (ignore only-thematic-objects))
  (remove-if-not #'(lambda (x) 
                     (matches-p query x))
                 (get-edges substrate)))

(defmethod retrieve-matching-objects ((substrate racer-substrate) (query binary-query) &key only-thematic-objects)
  (remove-if-not #'(lambda (x) 
                     (matches-p query x))
                 (if only-thematic-objects
                     (thematic-edges substrate)
                   (get-edges substrate))))
