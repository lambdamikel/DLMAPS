;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;


(defun ensure-active-nodes-present ()

  (unless (is-abox1-p *abox*)
    (error "Bad ABox for trace strategy!"))

  (when (and *reflexive-checks-p* 
             (not *maintain-active-nodes-p*))
    (error "active nodes missing!")))


(defun ensure-not-blocked (node)
  (when *reflexive-checks-p*
    (unless (old-p node)
      (when (blocked-p node)
        (error "node ~A is blocked!" node))
      (when (and nil (find-blocking-node *abox* node))
        (error "node ~A is blocked (2)!" node)))))

;;;
;;;
;;;

(timed-defmethod select-det-node ((abox abox1) (strategy strategy) (language dl)) 

  (with-slots (active-nodes) abox

    (ensure-active-nodes-present)

    (let* ((node (heap-peek active-nodes)))

      (when node
        
        (announce "Checking active node for det node: ~A, ~A" 
                  node (list (not (deterministically-expanded-p node))
                             (has-unexpanded-atomic-concepts-p node)
                             (has-unexpanded-and-concepts-p node)
                             (has-unexpanded-or-concepts-p node)))
      
        (when (and node 
                   (not (deterministically-expanded-p node))
                   (or (has-unexpanded-atomic-concepts-p node)
                       (has-unexpanded-and-concepts-p node)
                       (has-unexpanded-or-concepts-p node)))

          (ensure-not-blocked node)

          node)))))

;;;
;;;
;;;


(timed-defmethod select-or-concept1  ((node abox-node) (strategy strategy) (language dl))
  (let ((concept nil)
        (best-score nil))
    
    (labels ((get-or-score (node concept)
               (declare (ignorable node concept))
               1))
                     
      
      (when *reflexive-checks-p*
        (ensure-not-blocked node)
        (unless (deterministically-expanded-p node)
          (error "SELECT-OR-CONCEPT: Strategy Error!")))
  
      (loop-over-node-unexpanded-or-concepts (or-concept node nil)
    
        (let ((score 
               (get-or-score node or-concept)))
      
          (when (or (not best-score)
                    (> score best-score))
        
            (setf best-score score
                  concept or-concept))))

      (values concept best-score))))



(timed-defmethod select-or-concept ((abox abox) (strategy strategy) (language dl))
  (let ((node nil)
        (best-score nil)
        (concept nil))
    
    (loop-over-active-nodes (or-node abox nil)
                                         
                            (when (has-unexpanded-or-concepts-p or-node nil)
                                             
                              (multiple-value-bind (or-concept score)
                                  (select-or-concept1 or-node strategy language)

                                (when (or (not concept)
                                          (> score best-score))
                                                 
                                  (setf node or-node
                                        concept or-concept
                                        best-score score)))))
                   
                   
    (when node
      (announce "~%+++ Selected OR CONCEPT ~A : ~A" node concept)
      (values concept node))))


(timed-defmethod select-or-concept ((abox abox) (strategy abox-saturation-strategy) (language dl))
  (let ((node (get-oldest-node-with-unexpanded-or-concepts abox)))

    (when node
                     
      (let ((concept
             (select-or-concept1 node strategy language)))

        (announce "~%+++ Selected OR CONCEPT ~A : ~A" node concept)
                       
        (values concept node)))))



(timed-defmethod select-or-concept ((abox abox1) (strategy strategy) (language dl))
  (call-next-method))


(timed-defmethod select-or-concept ((abox abox1) (strategy trace-strategy) (language dl))
  (with-slots (active-nodes) abox
                   
    (ensure-active-nodes-present)
  
    (let ((node (heap-peek active-nodes)))

      (when (and node (has-unexpanded-or-concepts-p node)) 

        (let ((concept 
               (select-or-concept1 node strategy language)))
      
          (announce "~%+++ Selected OR CONCEPT ~A : ~A" node concept)
                         
          (values concept node))))))

;;;
;;; Code for semantic branching
;;; 


(timed-defmethod select-open-disjunct1  ((node abox-node) (strategy strategy) (language dl))
  (let ((concept nil)
        (disjunct nil)
        (best-score nil)
        (ors nil)
        (open-diss nil))

    (labels ((get-disjunct-score (node concept)
               (declare (ignorable node concept))
               (break)))

      (when *reflexive-checks-p*
        (ensure-not-blocked node)
        (unless (deterministically-expanded-p node) 
          (error "SELECT-OPEN-DISJUNCT: Strategy Error!"))

        (unless (has-unexpanded-or-concepts-p node)
          (error "No unexpanded or concepts!")))

      (loop-over-node-unexpanded-or-concepts (or-concept node nil)
        (push or-concept ors)
        (dolist (dis (arguments or-concept))
          (unless (on-tableau-p node (get-negated-concept dis))
            (push (list dis or-concept) open-diss))))

        
      (dolist (dis-and-or open-diss)
        (let ((dis (first dis-and-or))
              (or-concept (second dis-and-or)))

          (dolist (dis (list dis (get-negated-concept dis)))
            (let ((score 0))
              (dolist (or-concept ors)
                 
                (if (is-abox-saturation-strategy-p strategy)
                    (setf score (if (is-all-concept-p dis) 2 1))
                  (when (member dis (arguments or-concept))
                    (incf score))))

              (when (or (not best-score)
                        (> score best-score))
                    
                (setf best-score score
                      concept or-concept
                      disjunct dis)))))))

    (values disjunct concept best-score)))




(timed-defmethod select-open-disjunct ((abox abox) (strategy strategy) (language dl))

  (let ((node nil)
        (best-score nil)
        (concept nil)
        (best-disjunct nil))
    
    (loop-over-active-nodes (or-node abox nil)
                                         
                            (when (has-unexpanded-or-concepts-p or-node nil)
                                      
                              (multiple-value-bind (disjunct or-concept score)
                                  (select-open-disjunct1 or-node strategy language)
                                        
                                (when (or (not concept)
                                          (> score best-score))
                                          
                                  (setf node or-node
                                        concept or-concept
                                        best-disjunct disjunct 
                                        best-score score)))))
                   
    (when node
      (announce "~%+++ Selected OPEN DISJUNCT ~A OF ~A : ~A" best-disjunct concept node)

      (values best-disjunct concept node))))


(timed-defmethod select-open-disjunct ((abox abox) (strategy abox-saturation-strategy) (language dl))
                 
  (let ((node (get-oldest-node-with-unexpanded-or-concepts abox)))

    (when node
                     
      (multiple-value-bind (best-disjunct concept)
          (select-open-disjunct1 node strategy language)
                       
        (when node
          (announce "~%+++ Selected OPEN DISJUNCT ~A OF ~A : ~A" best-disjunct concept node)
          (values best-disjunct concept node))))))


(timed-defmethod select-open-disjunct ((abox abox1) (strategy strategy) (language dl))
  (call-next-method))


(timed-defmethod select-open-disjunct ((abox abox1) (strategy trace-strategy) (language dl))
  (with-slots (active-nodes) abox
                   
    (ensure-active-nodes-present)
  
    (let ((node (heap-peek active-nodes)))

      (when (and node (has-unexpanded-or-concepts-p node)) 

        (multiple-value-bind (best-disjunct concept)
            (select-open-disjunct1 node strategy language)

          (announce "~%+++ Selected OPEN DISJUNCT ~A OF ~A : ~A" best-disjunct concept node)
                         
          (values best-disjunct concept node))))))

;;;
;;;
;;;


(timed-defmethod select-some-concept1 ((node abox-node) (strategy strategy) (language dl))
  (let ((concept nil)
        (best-score nil)
        (abox (in-graph node)))

    (labels ((get-some-score (node concept)
               (- (choice-point-counter abox)
                  (maximum (get-choice-points concept :node node)))))

      (when *reflexive-checks-p*
        (ensure-not-blocked node)
        (when (or (has-unexpanded-atomic-concepts-p node)
                  (has-unexpanded-and-concepts-p node)
                  (has-unexpanded-or-concepts-p node)
                  (has-unexpanded-attribute-exists-concepts-p node))
          (describe-object node t)
          (error "SELECT-SOME-CONCEPT: Strategy Error!"))
        (unless (has-unexpanded-some-concepts-p node)
          (error "No unexpanded some concepts!")))

     
      (loop-over-node-unexpanded-some-concepts (some-concept node nil)
    
        (let ((score 
               (get-some-score node some-concept)))
      
          (when (or (not best-score)
                    (> score best-score))
        
            (setf best-score score
                  concept some-concept))))

      (values concept best-score))))
  

(timed-defmethod select-some-concept ((abox abox) (strategy strategy) (language dl))
  (let ((node nil)
        (best-score nil)
        (concept nil))
    
    (loop-over-active-nodes (some-node abox nil)
                                           
                            (when (has-unexpanded-some-concepts-p some-node nil)
                                             

                              (multiple-value-bind (some-concept score)
                                  (select-some-concept1 some-node strategy language)

                                (when (or (not best-score)
                                          (> score best-score))
                                                 
                                  (setf node some-node
                                        concept some-concept
                                        best-score score)))))
                   
    (when node
      (announce "~%+++ Selected SOME CONCEPT ~A : ~A" node concept)
      (values concept node))))


(timed-defmethod select-some-concept ((abox abox1) (strategy strategy) (language dl))
  (call-next-method))


(timed-defmethod select-some-concept ((abox abox) (strategy abox-saturation-strategy) (language dl))
                 
  (let ((node (get-oldest-node-with-unexpanded-some-concepts abox)))

    (when node
      (let ((concept
             (select-some-concept1 node strategy language)))
                       
        (when node
          (announce "~%+++ Selected SOME CONCEPT ~A : ~A" node concept)
          (values concept node))))))


(timed-defmethod select-some-concept ((abox abox1) (strategy trace-strategy) (language dl))
  (with-slots (active-nodes) abox

    (ensure-active-nodes-present)
                   
    (let ((node (heap-peek active-nodes)))

      (when (and node (has-unexpanded-some-concepts-p node)) 

        (let ((concept
               (select-some-concept1 node strategy language)))

          (announce "~%+++ Selected SOME CONCEPT ~A : ~A" node concept)
                         
          (values concept node))))))

;;;
;;;
;;;


(timed-defmethod select-attribute-exists-concepts1 ((node abox-node) (strategy strategy) (language dl))

  (let ((concept nil)
        (ae-concepts nil)
        (relevant-ae-concepts nil)
        (best-score nil)
        (abox (in-graph node)))

    (labels ((get-ae-score (node concept)
               (- (choice-point-counter abox)
                  (maximum (get-choice-points concept :node node)))))

       
      (when *reflexive-checks-p* 
         
        (ensure-not-blocked node)
         
        (when (or (has-unexpanded-atomic-concepts-p node)
                  (has-unexpanded-and-concepts-p node)
                  (has-unexpanded-or-concepts-p node))
          (error "Strategy error - attribute-exists-rule!")))

      ;;;
      ;;;
      ;;;

      (loop-over-node-unexpanded-attribute-exists-concepts (ae-concept node nil)

        (push ae-concept ae-concepts)

        (let ((score 
               (get-ae-score node ae-concept))) 

          (when (or (not best-score)
                    (> score best-score))

            (setf best-score score
                  concept ae-concept))))

      (push concept relevant-ae-concepts)

      ;;;
      ;;;
      ;;;
              
      (let ((found t))
        (loop while found do
              (setf found nil)
              (let ((relevant-ae-concept
                     (find-if #'(lambda (x)
                                  (some #'(lambda (y) 
                                            (has-common-parent-feature (role x) (role y)))
                                        relevant-ae-concepts))
                              ae-concepts)))
                (when relevant-ae-concept
                  (setf found t)
                  (setf ae-concepts (delete relevant-ae-concept ae-concepts))
                  (push relevant-ae-concept relevant-ae-concepts)))))

      (setf relevant-ae-concepts
            (delete-duplicates relevant-ae-concepts))

      (values concept relevant-ae-concepts best-score))))


(timed-defmethod select-attribute-exists-concepts ((abox abox) (strategy strategy) (language dl))
                 
  (let ((node nil)
        (best-score nil)
        (concept nil)
        (best-concepts nil))
                   
    (loop-over-active-nodes (ae-node abox nil)
                                           
                            (when (has-unexpanded-attribute-exists-concepts-p ae-node nil)
                                             
                                             
                              (multiple-value-bind (ae-concept relevant-ae-concepts score) 
                                  (select-attribute-exists-concepts1 node strategy language)

                                (when (or (not concept)
                                          (> score best-score))
                                                 
                                  (setf node ae-node
                                        concept ae-concept
                                        best-concepts relevant-ae-concepts
                                        best-score score)))))
                   
    (when node
      (announce "~%+++ Selected group of ATTRIBUTE-EXISTS CONCEPTS ~A : ~A" node best-concepts) 
                     
      (values concept best-concepts node))))


(timed-defmethod select-attribute-exists-concepts ((abox abox1) (strategy strategy) (language dl))
  (call-next-method))




(timed-defmethod  select-attribute-exists-concepts ((abox abox) (strategy abox-saturation-strategy) (language dl))
                 
  (let ((node (get-oldest-node-with-unexpanded-attribute-exists-concepts abox)))

    (when node

      (multiple-value-bind (concept relevant-ae-concepts) 
          (select-attribute-exists-concepts1 node strategy language)
                         
        (announce "~%+++ Selected group of ATTRIBUTE-EXISTS CONCEPTS ~A : ~A" node relevant-ae-concepts) 
                         
        (values concept relevant-ae-concepts node)))))


(timed-defmethod select-attribute-exists-concepts ((abox abox1) (strategy trace-strategy) (language dl))
  (with-slots (active-nodes) abox

    (let ((node (heap-peek active-nodes)))
      
      (when (and node (has-unexpanded-attribute-exists-concepts-p node))
                       
        (multiple-value-bind (concept relevant-ae-concepts) 
            (select-attribute-exists-concepts1 node strategy language)

          (announce "~%+++ Selected group of ATTRIBUTE-EXISTS CONCEPTS ~A : ~A" node relevant-ae-concepts) 

          (values concept relevant-ae-concepts node))))))


;;;
;;;
;;;

(timed-defmethod select-at-least-concept1 ((node abox-node) (strategy strategy) (language dl))

  (let ((concept nil)
        (best-score nil)
        (abox (in-graph node)))

    (labels ((get-at-least-score (node concept)
               (- (choice-point-counter abox)
                  (maximum (get-choice-points concept :node node)))))


      (when *reflexive-checks-p* 
        
        (ensure-not-blocked node)
        
        (when (has-unexpanded-some-concepts-p node)
          (error "SELECT-AT-LEAST-CONCEPT: Strategy Error!")))

      (loop-over-node-unexpanded-at-least-concepts (at-least-concept node nil) 
          
        (let ((score 
               (get-at-least-score node at-least-concept)))
                           
          (when (or (not best-score)
                    (> score best-score))

            (setf best-score score
                  concept at-least-concept))))

      (values concept best-score))))
         

(timed-defmethod select-at-least-concept ((abox abox) (strategy strategy) (language dl))
  (let ((node nil)
        (best-score nil)
        (concept nil))
    
    (loop-over-active-nodes (at-least-node abox nil)
                                           
                            (when (has-unexpanded-at-least-concepts-p at-least-node nil)
                                             
                              (multiple-value-bind (at-least-concept score)
                                  (select-at-least-concept1 at-least-node strategy language)

                                (when (or (not concept)
                                          (> score best-score))
                                                 
                                  (setf node at-least-node
                                        concept at-least-concept
                                        best-score score)))))
                   
    (when node
      (announce "~%+++ Selected AT LEAST CONCEPT ~A : ~A" node concept)
      (values concept node))))


(timed-defmethod select-at-least-concept ((abox abox1) (strategy strategy) (language dl))
  (call-next-method))



(timed-defmethod select-at-least-concept ((abox abox) (strategy abox-saturation-strategy) (language dl))
                 
  (let ((node (get-oldest-node-with-unexpanded-at-least-concepts abox)))

    (when node
      (let ((concept
             (select-at-least-concept1 node strategy language)))

        (when node
          (announce "~%+++ Selected AT LEAST CONCEPT ~A : ~A" node concept)
          (values concept node))))))



(timed-defmethod select-at-least-concept ((abox abox1) (strategy trace-strategy) (language dl))
  (with-slots (active-nodes) abox

    (let ((node (heap-peek active-nodes)))

      (when (and node (has-unexpanded-at-least-concepts-p node))
                       
        (let ((concept
               (select-at-least-concept1 node strategy language)))

          (when node
            (announce "~%+++ Selected AT LEAST CONCEPT ~A : ~A" node concept)
            (values concept node)))))))

;;;
;;;
;;;


(timed-defmethod select-violated-at-most-concept ((abox abox) (strategy strategy) (language dl))
  (loop-over-active-nodes (node abox)
    
                          (when (has-unexpanded-at-most-concepts-p node)
        
                            ;;; kleinstes (at-most n R) pro Rolle R raussuchen 
        
                            (let ((role-n-entries nil))
          
                              (loop-over-node-unexpanded-at-most-concepts (at-most-concept node)

                                (unless (is-top-concept-p (qualification at-most-concept))
                                  (error "To be implemented!"))
             
                                (let ((role-n (assoc (role at-most-concept)
                                                     role-n-entries)))
                                  (if role-n
                                      (setf (second role-n)
                                            (if (< (n at-most-concept)
                                                   (n (second role-n)))
                                                at-most-concept
                                              (second role-n)))
                                    (push (list (role at-most-concept) at-most-concept) role-n-entries))))

                              ;;; 

                              (dolist (role-n role-n-entries)
                                (let* ((role (first role-n))
                                       (at-most-concept (second role-n))
                                       (n (n at-most-concept))
                                       (m 0)
                                       (edges nil))

                                  (loop-over-role-successors (node role) (succ edge)
                                                             ;;; (format t "~% ~A ~A ~A ~A" node role succ edge)
                                                             (if (eq node (from edge))
                                                                 (unless (zerop (multiplicity edge))
                                                                   (push edge edges)
                                                                   (incf m (multiplicity edge)))
                                                               (unless (zerop (inverse-multiplicity edge))
                                                                 (push edge edges)
                                                                 (incf m (inverse-multiplicity edge)))))
            
                                  (when (> m n)

                                    (announce "~%+++ Selected violated AT-MOST CONCEPT ~A : ~A" node at-most-concept)
                                    (announce "~%+++ Edges found: ~A" edges)

                                    (return-from select-violated-at-most-concept

                                      (values at-most-concept node edges m)))))))))


