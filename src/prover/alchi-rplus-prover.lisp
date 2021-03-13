;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;


(define-prover ((abox-sat alchi-rplus abox))
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
           (perform (block-nodes)
             (:body
              (perform (some-expansion)
                (:positive
                 (if clashes
                     (handle-clashes)
                   (restart-main)))
                (:negative 
                 (success))))))))))))


#|

(progn 

  (delete-all-tboxes)

  (transitive rr)

  (unless (sat? (and (some rr (some rr c)) (some s (all (inv s) (all rr (some rr d))))) :debug-p t :recompute-p t)
    (error "Prover error!"))

  (delete-all-tboxes)

  (transitive p)

  (def c (and (all (inv r) (all (inv p) (all (inv s) (Not a))))))
  
  (let ((*print-pretty* t))
    (when (sat? (and a (some s (and (some r top)
                                    (some p top)
                                    (all r c)
                                    (all p (some r top))
                                    (all p (all r c))
                                    (all p (some p top)))))
                :debug-p t :recompute-p t)
      
      (break "Prover error!"))))

|#

