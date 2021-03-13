;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(define-prover ((abox-sat alchfn-rplus abox))
  (:init :call-next-method)
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
              (perform (identify-stable-nodes)
                (:body
                 (perform (model-merging)
                   (:body 
                    (perform (make-models-for-nodes)
                      (:body 
                       (perform (feature-expansion)
                         (:positive
                          (if clashes
                              (handle-clashes)
                            (perform (block-nodes :blocked succ) 
                              (:body
                               (perform (model-merging :node succ)
                                 (:body 
                                  (next-round)))))))
                         (:negative
                          (perform (some-expansion)
                            (:positive
                             (if clashes
                                 (handle-clashes)
                               (perform (block-nodes :blocked new-node) 
                                 (:body
                                  (perform (model-merging)
                                    (:body
                                     (next-round)))))))
                            (:negative 
                             (perform (simple-at-least-expansion)
                               (:positive 
                                (if clashes
                                    (handle-clashes)
                                  (perform (block-nodes :blocked new-node)
                                    (:body
                                     (perform (model-merging)
                                       (:body
                                        (next-round)))))))
                               (:negative
                                (let ((*blocking-enabled-p* nil))
                                  (perform (make-models-for-nodes)
                                    (:body 
                                     (perform (make-models-for-old-nodes)
                                       (:body
                                        (success)))))))))))))))))))))))))))
      

#|
(progn 
  (with-kb (test test :delete-if-exists-p t)
    
    ;;; dieses Beispiel demonstriert, wie
    ;;; wichtig es ist, ERST die "oldest" OLD
    ;;; nodes zu expandieren; die "youngest" OLD
    ;;; zu präferieren führt wegen combined-some-all-rule
    ;;; zu falschem Ergebnis! das problem ist, dass
    ;;; bei youngest old first zunächst "c" und "b : some f c"
    ;;; gemerged werden; und dann erst "b" und "a : some f top". 
    ;;; d.h., die transitive (all r (not c)) erreicht den 
    ;;; gemergten Knoten "c" und "b : some f c" gar nicht!
    ;;; wird dagegen erst "b" und "a : some f top" gemerget, 
    ;;; so hat der merge-Knoten schon (all r (not c)) im 
    ;;; label, und das wird durchpropagiert, wenn dann 
    ;;; das label für "b : (some f c)" erzeugt wird -> clash

    (defrole f :parents (r g))

    (transitive r)

    (instance a (all r (not c)))
    (related a b g)
    (instance a (some f top))
    (instance a (at-most 1 g))
    
    (related b c g)
    (instance b (some f c))
    (instance b (at-most 1 g))

    (false! (abox-sat? test))))

|# 
