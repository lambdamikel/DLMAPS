;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(define-prover ((abox-sat alchn abox))
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
                       (perform (some-expansion)
                         (:positive
                          (if clashes
                              (handle-clashes)
                            (next-round)))
                         (:negative 
                          (perform (simple-at-least-expansion)
                            (:positive 
                             (next-round))
                            (:negative
                             (perform (make-models-for-old-nodes)
                               (:body 
                                (success))))))))))))))))))))))



(define-prover ((abox-sat alchn abox1))
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
           (perform (simple-at-most-merging)
             (:positive 
              (restart-main))
             (:negative 
              (perform (identify-stable-nodes)
                (:body
                 (perform (model-merging)
                   (:body
                    (perform (some-expansion)
                      (:positive
                       (if clashes
                           (handle-clashes)
                         (next-round)))
                      (:negative 
                       (perform (simple-at-least-expansion)
                         (:positive 
                          (next-round))
                         (:negative
                          (perform (pop-active-nodes-heap)
                            (:positive
                             (next-round))
                            (:negative 
                             (perform (make-models-for-old-nodes)
                               (:body 
                                (success))))))))))))))))))))))




#|

(progn 


  (progn
    (delete-all-tboxes)
    (delete-all-aboxes)

    (with-abox (test :delete-if-exists-p t)
    
      (related a a r) 
    
      (instance a (some r p))

      (instance a (at-most 1 r))

      (instance a (all r (some r p)))
      
      (true! (abox-sat? test :debug-p t))))


  (progn
    (delete-all-tboxes)
    (delete-all-aboxes)

    (with-abox (test :delete-if-exists-p t)
    
      (related a b r) 

      (related a c r)
    
      (instance a (at-most 1 r))

      (false! (abox-sat? test :debug-p t))))


  (progn 
    (full-reset)
    
    (define-primitive-role r)

    (define-primitive-role s :parent r)
  
    (related a b r)

    (instance b b)
  
    (instance a (some s (not b)))

    (instance a (and a (at-most 1 r)))

    (false! (abox-sat-p *cur-abox* :debug-p t)))


  (progn
    (delete-all-tboxes)
    (delete-all-aboxes)

    (with-abox (test :delete-if-exists-p t)
    
      (related a f1 f)
      (related a f2 f)

      (instance a (at-most 1 f))
      
      (false! (abox-sat? test :debug-p t))))


  (progn 
    (full-reset)
  
    (define-primitive-role r)

    (define-primitive-role s :parent r)
  
    (related a b r)
  
    (instance a (some s top))

    (instance a (at-most 1 r))

    (true! (abox-sat-p *cur-abox* :debug-p t)))


  (progn 
    (full-reset)
  
    (define-primitive-role r)

    (define-primitive-role s :parent r)
  
    (related a b r)

    (instance b (not c))
  
    (instance a (some s c))

    (instance a (at-most 1 r))

    (false! (abox-sat-p *cur-abox* :debug-p t))))



|# 
