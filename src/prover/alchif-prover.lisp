;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(define-prover ((abox-sat alchif abox))
  (:main 
   (perform (deterministic-expansion)
     (:body 
      (if clashes 
          (handle-clashes)                  
        (perform (or-expansion)
          (:positive 
           (if clashes 
               (handle-clashes)           
             (restart-main)))
          (:negative 
           (perform (feature-expansion)
             (:positive
              (if clashes
                  (handle-clashes)
                (restart-main)))
             (:negative 
              (perform (some-expansion)
                (:positive
                 (if clashes
                     (handle-clashes)
                   (restart-main)))
                (:negative 
                 (success))))))))))))
