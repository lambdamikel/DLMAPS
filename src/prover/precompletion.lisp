;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

#|

(defrule compute-precompletion (dl alch) 

  (labels ((do-it ()
             (perform (deterministic-expansion)
               (:body 
                (if clashes 
                    (return-from compute-precompletion nil)
                  (perform (some-expansion)
                    (:positive
                     (if clashes
                         (return-from compute-precompletion nil)
                       (:negative 
                        +insert-body-code+)))))))))

    (let ((*combined-some-all-rule-p* nil))
      (do-it)
      t)))


(defrule compute-precompletion (dl alch) 
  
  (labels ((do-it ()
             (perform (deterministic-expansion)
               (:body 
                (if clashes 
                    (handle-clashes)                  
                  (perform (some-expansion)
                    (:positive
                     (if clashes
                         (handle-clashes)
                    (:negative 
                     +insert-body-code+)))))))))

    (let ((*combined-some-all-rule-p* nil))
      (do-it))))


|#

