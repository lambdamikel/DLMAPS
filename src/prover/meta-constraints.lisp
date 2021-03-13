;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;


(defmacro add-meta-constraints (node)
  `(let ((node ,node))

     (dolist (constraint *meta-constraints*)

       (announce "Adding meta constraint ~A" constraint)
     
       (unless (on-tableau-p node constraint)
         (let ((added
                (register-as-unexpanded constraint
                                 :comment 'add-meta-constraint
                                 :node node 
                                 :new-choice-point 0)))
         
           (check-for-clash node added))))))
