;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro with-strategy ((strat) &body body)
    `(let ((*strategy* ,strat))
       (establish-context-for *strategy*
                              #'(lambda ()
                                  ,@body))))

  (defpersistentclass strategy ())

  (defpersistentclass trace-strategy (strategy))

  (defpersistentclass abox-saturation-strategy (strategy))

  ;;;
  ;;;
  ;;;

  (defmethod establish-context-for ((strategy strategy) fn)
    (funcall fn))

  (defmethod establish-context-for ((strategy abox-saturation-strategy) fn)
    (let ((*maintain-unexpanded-or-concepts1-heap-p* t)
          (*combined-some-all-rule-p* nil)
          (*maintain-active-nodes-p* t)
          )
      (funcall fn)))

  (defmethod establish-context-for ((strategy trace-strategy) fn)
    (let ((*maintain-active-nodes-p* t))

      (funcall fn)))

  ;;;
  ;;;
  ;;;

  (defconstant +strategy+ (make-instance 'strategy))
  
  (defconstant +trace-strategy+ (make-instance 'trace-strategy))

  (defconstant +abox-saturation-strategy+ (make-instance 'abox-saturation-strategy))

  )
