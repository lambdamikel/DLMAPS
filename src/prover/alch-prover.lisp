;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(define-prover ((abox-sat alch abox))
  (:init 

   (cond ((zerop (1- (no-of-nodes abox)))

          (loop-over-abox-nodes (node abox)
            (register-label-is-stable abox node))

          (with-strategy (+strategy+)
            (start-main)))

         (t 

          (perform (initial-abox-saturation)
            (:body
             (with-strategy (+abox-saturation-strategy+)
               (loop-over-abox-nodes (node abox)
                 (register-label-is-stable abox node))
               (perform (model-merging)
                 (:body
                  (start-main)))))))))

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
              (perform (some-expansion)
                (:positive
                 (if clashes
                     (handle-clashes)
                   (perform (model-merging :node new-node)
                     (:body 
                      (next-round)))))
                (:negative 
                 (perform (make-models-for-old-nodes)
                   (:body 
                    (success)))))))))))))

  (:success    
   (completion-found)))


(define-prover ((abox-sat alch abox1))

  ;;; Beweiser mit Tiefensuch-Strategie (Trace Technique)! 
  
  (:init  
   (cond ((zerop (1- (no-of-nodes abox)))

          (loop-over-abox-nodes (node abox)
            (register-label-is-stable abox node))

          (with-strategy (+trace-strategy+)
            (start-main)))

         (t

          (perform (initial-abox-saturation)
            (:body
             (with-strategy (+trace-strategy+)
               (loop-over-abox-nodes (node abox)
                 (register-label-is-stable abox node))
               (perform (model-merging)
                 (:body
                  (start-main)))))))))

  (:rollback 
   (let ((*maintain-active-nodes-p* t))
     (rollback)))

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
           (perform (some-expansion)
             (:positive
              (if clashes
                  (handle-clashes)
                (perform (model-merging :node new-node)
                  (:body
                   (next-round)))))
             (:negative 
              (perform (pop-active-nodes-heap)
                (:positive
                 (next-round))
                (:negative 
                 (perform (make-models-for-old-nodes)
                   (:body (success)))))))))))))

  (:success    
   (completion-found)))



#|

(progn 

  (with-abox (test :delete-if-exists-p t :language +alch+)
  
    (ins a (and a
                (or (all r (not c)) 
                    (all r (not b)))
                (all r (or (not b) c))))

    (ins b b)

    (rel a b r)
  
    (false! (abox-sat-p *cur-abox* 
                        :language +alch+
                        :debug-p t))

    (visualize *cur-abox*))


  (progn
    (delete-all-tboxes)
    (delete-all-aboxes)

    (subrole r s)

    (domain r c)
  
    (range r d)

    (with-abox (test) 
    
      (rel a b r)

      (ins a (or (not c) (all s (not d))))

      (false! (abox-sat? test :debug-p t))))


  (with-kb (test test :delete-if-exists-p t)
  
           (defrole r :domain a :range b)
  
           (rel i j r)

           (true! (individual-instance? i a))
  
           (true! (individual-instance? j b))

           (true! (individual-instance? i (some r b)))

           (true! (individual-instance? j (some (inv r) a)))

           (true! (individual-instance? i (and a (all r b))))

           (true! (individual-instance? i (and a (all r (and b (all (inv r) a)))))))



  (with-kb (test test :delete-if-exists-p t)
  
           (defrole r :domain a :range b)
  
           (def x (some r top))
  
           (def y (and a (some r b)))

           (def xx (and (some r top) 
                        (all r b)))

           (true! (equivalent? x y))

           (true! (equivalent? xx x))

           (true! (equivalent? x xx))

           (true! (equivalent? xx y)))

  
  (with-kb (test test :delete-if-exists-p t)
  
           (instance a (and d (all r c)))

           (related a b (or r s))

           (instance b (not c))
  
           (instance b (all (inv s) (not d)))
  
           (false! (abox-consistent-p *cur-abox*))))


|#



  


