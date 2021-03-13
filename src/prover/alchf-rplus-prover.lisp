;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(define-prover ((abox-sat alchf-rplus abox))
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
                         (perform (model-merging :node new-node)
                           (:body
                            (next-round)))))))
                   (:negative 
                    (let ((*blocking-enabled-p* nil))
                      (perform (make-models-for-nodes)
                        (:body 
                         (perform (make-models-for-old-nodes)
                           (:body
                            (success)))))))))))))))))))
  

(define-prover ((abox-sat alchf-rplus abox1))
  (:init :call-next-method)
  (:main 
   (perform (focused-deterministic-expansion)
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
                      (perform (model-merging :node new-node)
                        (:body
                         (next-round)))))))
                (:negative 
                 (perform (pop-active-nodes-heap)
                   (:positive
                    (next-round))
                   (:negative 
                    (let ((*blocking-enabled-p* nil))
                      (perform (make-models-for-nodes)
                        (:body
                         (perform (make-models-for-old-nodes)
                           (:body
                            (success)))))))))))))))))))
    
#|


(progn
  (with-kb (test test :delete-if-exists-p t)

           (defrole r :transitive t)
           (deffeature f :parents (r))
  
           (unless (sat? (and (some r c) (all r (some r c))) :recompute-p t :debug-p t)
             (error "prover error")))
  

  (with-kb (test test :delete-if-exists-p t)

           (defrole r :transitive t)
           (deffeature f :parents (r))
  
           (unless (sat? (and (some f (and c (some f c)))
                              (all r (some f d)))

                         :recompute-p t :debug-p t)
             (error "prover error")))

  (with-kb (test test :delete-if-exists-p t)

           (defrole r :transitive t)
           (deffeature f :parents (r))

  
           (when (sat? (and (some f (some f c))
                            (all r (not d))
                            (all r (or (not c)
                                       (some f d))))

                       :recompute-p t :debug-p t)

             (error "prover error")))

  (with-kb (test test :delete-if-exists-p t)

           (defrole r :transitive t)
           (deffeature f :parents (r))

  
           (unless (sat? (and (some f (some f c))
                              (all r (not d))
                              (all r (or (not c)
                                         (some f d)
                                         (and (some f (some r c))
                                              (all r (some r c))))))

                         :recompute-p t :debug-p t)

             (error "prover error")))

  (with-kb (test test :delete-if-exists-p t)

           (defrole r :transitive t)
           (deffeature f :parents (r))
   
           (when (sat? (and (all r (and (not c) (not e) (not d)))
                            (some f (some f top))
                            (all r (or (some f d) 
                                       (some f c)
                                       (some f e))))
                       :recompute-p t
                       :debug-p t)
             (error "prover error")))
  
  (with-kb (test test :delete-if-exists-p t)
    
           ;;; interessanter Fall - 
           ;;; hier existiert bereits ein "g"-Nachfolger fuer "a"
           ;;; wird (a (some f top)) expandiert, dann entsteht ein
           ;;; neuer "g"-Nachfolger (weil Oberfeature von "f"). 
           ;;; da beim ALCHF-Prover aber kein Merging stattfindet, 
           ;;; muss also gleich bei der erzeugenden Feature-Expansion-Rule
           ;;; darauf geachtet werden, ob bereits Feature-Nachfolger von
           ;;; Oberfeatures von dem zu erzeugenden Feature-Nachfolger 
           ;;; existieren; in dem Fall muss dieser Knoten genommen werden, 
           ;;; und es darf kein neuer Erzeugt werden! s. Code und Kommentar in 
           ;;; feature-expansion.lisp
    
           ;;; Nachtrag: verstehe ich nicht mehr - eigentlich sollte dann dieser
           ;;; Constraint bereits durch "look-for-somes" bzw. "look-for-featueres"
           ;;; in deterministic-expansion.lisp as expandiert gekennzeichnet worden sein? 
           ;;; Pruefen!
    
           (deffeature g)

           (deffeature f :parents (r g))

           (transitive r)

           (instance a (all r (not c)))
    
           (related a b g)
           (instance a (some f top))

           (related b c g)
           (instance b (some f c))
    
           (false! (abox-sat? test :debug-p t))))
    

|#

      


