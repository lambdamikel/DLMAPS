;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;


(define-prover ((abox-sat alchifn-rplus abox))
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
           (perform (simple-at-most-merging)
             (:positive
              (restart-main))
             (:negative
              (perform (block-nodes)
                (:body
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
                       (perform (simple-at-least-expansion)
                         (:positive 
                          (if clashes
                              (handle-clashes)
                            (restart-main)))
                         (:negative
                          (success))))))))))))))))))

#|

(progn 

  (delete-all-tboxes)

  (transitive r)

  (deffeature f :parents r)

  (unless (sat? (and (not c) 
                     (some (inv f) c)
                     (at-most 1 f)
                     (all (inv r) (some (inv f) (and c (at-most 1 f)))))
                
                :debug-p t)
    (error "prover error")))

(progn 

  (delete-all-tboxes)

  (transitive r)

  (deffeature f :parents r)

  (def d (and c (at-most 1 f) (some f (not c))))

  (when (sat? (and (not c) 
                   (at-most 1 f)
                   (some (inv f) d)
                   (all (inv r) (some (inv f) d)))

              :debug-p t)

    (error "prover-error"))

  (delete-all-tboxes)

  (when (sat? (and (some r (and c (some s (all (inv s) (all (inv r) (at-most 1 r))))))
                   (some r (and (not c) (some s (some r d)))))
              :debug-p t)
    (error "prover-error")))

(progn 

  (delete-all-tboxes)

  (let ((*print-pretty* t))
    (unless (sat? (and (some r (and c (some s (all (inv s) 
                                                   (all (inv r) (at-most 1 r))))))
                       (some r (and (not d) (some s (some r d)))))
                  :debug-p t)
      (error "prover-error"))))

(progn 

  (delete-all-tboxes)

  (transitive r)

  (defrole f :parents r)

  (def d (and c (at-most 1 f) (some f (not c))))

  (when (sat? (and (not c) (at-most 1 f)
                   (some (inv f) d)
                   (all (inv r) (some (inv f) d)))
              :debug-p t)
    
    (error "prover-error")))


|#



