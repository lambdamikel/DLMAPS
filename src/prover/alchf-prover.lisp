;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(define-prover ((abox-sat alchf abox))
  (:init  
   (cond ((zerop (1- (no-of-nodes abox)))
          
          (loop-over-abox-nodes (node abox)
            (register-label-is-stable abox node))

          (with-strategy (+abox-saturation-strategy+)
            (start-main)))

         (t 

          (announce "Looking for initial feature clashes")
          
          (let ((features (and *cur-tbox* 
                               (get-all-features *cur-tbox*))))
            
            (loop-over-abox-nodes (node abox)
              (when (some #'(lambda (f)
                              (announce "Looking for ~A : ~A = ~A" node f) 
                              (when (cdr (get-role-successors node f))
                                (announce "Found non-resolvable feature clash ~A : ~A" node f)
                                t))
                          features)
                (return-from prover-init nil)))
       
            (perform (initial-abox-saturation)
              (:body 
               (with-strategy (+abox-saturation-strategy+)

                 ;;; (loop-over-abox-nodes (node abox)
                 ;;;  (register-label-is-stable abox node))
                 ;;; Falsch! Erst nach "Hineinmergen" von "(some f c)" 
                 ;;; kenne ich das initiale Label! 

                 (perform (model-merging)
                   (:body
                    (start-main))))))))))
  
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
                   (perform (model-merging :node succ)
                     (:body
                      (next-round)))))
                (:negative
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
                       (success))))))))))))))))




(define-prover ((abox-sat alchf abox1))
  (:init  
   
   (cond ((zerop (1- (no-of-nodes abox)))

          (loop-over-abox-nodes (node abox)
            (register-label-is-stable abox node))

          (with-strategy (+trace-strategy+)
            (start-main)))

         (t 

          (announce "Looking for initial feature clashes")
          (let ((features (and *cur-tbox* 
                               (get-all-features *cur-tbox*))))

            (loop-over-abox-nodes (node abox)
              (when (some #'(lambda (f)
                              (announce "Looking for ~A : ~A = ~A" node f) 
                              (when (cdr (get-role-successors node f))
                                (announce "Found non-resolvable feature clash ~A : ~A" node f)
                                t))
                          features)
                (return-from prover-init nil)))
         
            (perform (initial-abox-saturation)
              (:body 
               (with-strategy (+trace-strategy+)

                 ;; (loop-over-abox-nodes (node abox)
                 ;;  (register-label-is-stable abox node))
                 ;; s.o. 

                 (perform (model-merging)
                   (:body
                    (start-main))))))))))

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
           (perform (feature-expansion)
             (:positive
              (if clashes
                  (handle-clashes)
                (perform (model-merging :node succ)
                  (:body
                   (next-round)))))
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
                      (:body
                       (success))))))))))))))))
 
#|

(progn 

  (progn
    (delete-all-tboxes)
    (delete-all-aboxes)

    (with-abox (test :delete-if-exists-p t)
    
      (define-primitive-attribute f)

      (related a f1 f)
      (related a f2 f)

      (false! (abox-sat? test :debug-p t))))


  (progn
    (delete-all-tboxes)
    (delete-all-aboxes)

    (with-abox (test :delete-if-exists-p t)
    
      (define-primitive-attribute f)

      (related a b f)
      (instance b (not c))
      (instance a (some f c))

      (false! (abox-sat? test :debug-p t))))

  (progn
    (delete-all-tboxes)
    (with-tbox (test :delete-if-exists-p t)
    
      (define-primitive-attribute f)
      (define-primitive-attribute f1 :parents (f))
      (define-primitive-attribute f2 :parents (f))

      (prepare *cur-tbox* +alchf+)

      (unless (and (implies-p (parse-role 'f1) (parse-role 'f))
                   (implies-p (parse-role 'f2) (parse-role 'f)))
        (error "Bad features!"))
    
      (when (sat? (and (some f1 a) (some f2 (not a)))
                  :language +alchf+ 
                  :debug-p t
                  :cache-models-p nil
                  :use-cached-models-p nil
                  :recompute-p t
                  :visualize-p nil)
      
        (error "Bad features!"))))

  (progn
    (delete-all-tboxes)
    (with-tbox (test :delete-if-exists-p t)
    
      (define-primitive-concept a TOP)
      (define-primitive-concept b a)
    
      (define-primitive-concept c TOP)
      (define-primitive-concept d c)
    
      (define-primitive-attribute f)
      (define-primitive-attribute g)
    
      (define-concept x (AND a (SOME f (SOME g c))))
      (define-concept y (AND b (SOME f (SOME g d))))

      (unless (subsumes? x y :language +alchf+ :debug-p t)
        (error "Error in prover!"))))

  (progn
    (delete-all-tboxes)
    (with-tbox (test :delete-if-exists-p t)
    
      (define-primitive-attribute fa)
      (define-primitive-attribute fb)
      (define-primitive-attribute fc)
    
      (define-primitive-attribute f1 :parents (fa))
      (define-primitive-attribute f2 :parents (fa fb))
      (define-primitive-attribute f3 :parents (fb))
    
      (when (sat? (and (some f1 c)
                       (some f2 d)
                       (some f3 e)
                       (all fa (not (and c d e))))
                  :language +alchf+ :debug-p t)
        (error "Error in prover!"))))

  (progn

    (with-kb (test test :delete-if-exists-p t)
    
      (define-primitive-attribute f)

      (related a b f)
    ;(related a c f)

      (instance b test)
    
      (instance a (or (some f (not test))
                      (some f (not ddtest))))

      (instance a (all f ddtest))

      (princ (abox-sat? test :debug-p t))))

  (progn

    (delete-all-tboxes)

    (with-kb (test test :delete-if-exists-p t)
    
      (defrole r :domain a :range b)

      (related i j r)
    
      (unless  (individual-instance? i (and a (all r b)))
        (break "Bug!"))

      (princ "-----------------------")
      (terpri) 

      (unless (individual-instance? i (and a (all r (and b (all (inv r) a)))))
        (break "Bug!")))))

|#


