;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;; 

(defpersistentclass edge-label (rolebox-edge-description))

(defmethod reset-label ((edge-label edge-label))
  nil)

;;;
;;;
;;;

(defmethod initialize-description ((description edge-label))
  (unless (typep (textual-description description) 'role)
    (setf (textual-description description)
          (parse-role (textual-description description)))
    description))


(defmethod copy ((label edge-label) &rest args)
  (declare (ignorable args))
  (make-edge-description 'edge-label 
                         (textual-description label)
                         :type 
                         (type-of label)))

;;;
;;;
;;;

(defmethod change-textual-description ((description edge-label) (role role) &key)
  (setf (textual-description description) role))

;;;
;;;
;;;

(defmethod underspecified-p ((edge edge-label) &rest args)
  (underspecified-p (textual-description edge)))

;;;
;;;
;;;

(defmethod implies-p ((label-a edge-label) (label-b edge-label) &rest args)
  (implies-p (textual-description label-a)  (textual-description label-b)))

(defmethod implies-p ((label edge-label) (role role) &rest args)
  (implies-p (textual-description label) role))

(defmethod implies-p ((role role) (label edge-label) &rest args)
  (implies-p role (textual-description label)))

