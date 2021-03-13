;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;; s. common.lisp 
;;; 

(defparameter *debug-p* nil)

;;; (defvar *cur-substrate* nil)

(defvar *all-substrates* nil)

;;;
;;;
;;;

(defpersistentclass substrate ()
  ((name :reader name :initarg :name :initform nil)))

(defpersistentclass substrate-node () nil)

(defpersistentclass substrate-edge () nil)

;;;
;;;
;;;

(defmethod print-object ((obj substrate) stream) 
  (format stream "#<Substrate ~A of type ~A>" (name obj) (type-of obj)))

(defmethod initialize-instance :after ((substrate substrate) &rest initargs)
  (declare (ignorable initargs))
  (push substrate *all-substrates*))

;;;
;;;
;;;

(defun delete-all-substrates ()
  (setf *cur-substrate* nil
        *all-substrates* nil))

(defmethod delete-substrate ((substrate substrate) &key &allow-other-keys)
  (setf *all-substrates* 
        (delete substrate *all-substrates*)))

