(in-package :racer-user);;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: CL-USER; Base: 10 -*-

(in-package :CL-USER)


(defun test (m n)
  (full-reset)
  
  (in-rcc-box test :rcc-type :rcc8)
  
  (let ((roles (mapcar #'ts::to-keyword ts::+rcc8-roles+)))

    (dotimes (i n)
      (create-rcc-node i))

    (dotimes (i m)
      (let ((from (random n))
            (to (random n))
            (rel (loop as rcc in roles when (zerop (random 3))
                       collect rcc)))
        (rcc-related1 from to rel)))

    (princ (rcc-consistent?))))
    
#|
    (dolist (role roles) 
      (pprint (racer-answer-query '(?*x ?*y) `(?*x ?*y ,role))))
    )))
  
     
|# 