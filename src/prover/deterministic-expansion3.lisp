;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(defmacro with-clash-handler (&rest body)
  `(let ((clashes nil))
     ,@body))


(defmacro check-for-clash (node concepts &optional handle-clash)
  `(dolist (concept (ensure-list ,concepts))
     (cond ((already-known-to-be-inconsistent-p concept)
            (register-clash ,node concept)
            (when ,handle-clash 
              (handle-clashes)))
           
           ((on-tableau-p ,node (get-negated-concept concept))
            (register-clash ,node concept)
            (when ,handle-clash 
              (handle-clashes))))))

(defmacro handle-clash (node concept culprits choice-points)
  `(progn 

     (setf ,choice-points (sort-choice-points ,choice-points))
  
     ;; (announce1 "CLASH FOR ~A AT ~A DUE TO ~A" concept node culprits)
  
     (when *debug-p*
       (announce "CLASH FOR ~A AT ~A DUE TO ~A" ,concept ,node ,culprits)
       (describe-object ,node t)                      
       (terpri)
       (announce "Returning Choice Points: ~A" ,choice-points))
  
     ;;; jedes "perform"-Macro setzt einen Prover-Block auf! 
  
     (setf (cur-clash-node *cur-abox*) ,node)
  
     (return-from prover (values nil ,choice-points))))


(defmacro register-clash (node concept)
  `(progn 
  
     (when *debug-p*
       (announce "REGISTER CLASH FOR ~A AT ~A" ,concept ,node)
       (describe-object ,node t)                      
       (terpri))
  
     (let ((culprits
            (if (already-known-to-be-inconsistent-p ,concept)
                (list ,concept)
              (get-clash-culprits ,node ,concept)))
           (choice-points nil))
    
       (dolist (culprit culprits)
         (dolist (point (get-choice-points culprit :node ,node))
           (push point choice-points)))
    
       (push (list (maximum choice-points)
                   ,node 
                   ,concept
                   culprits
                   choice-points)

             clashes))))


(defmacro handle-clashes ()
  `(when clashes 
    
     (let ((best-clash nil))
      
       (loop as clash in clashes
             as max-choice-point = (first clash) 
             do
             (when (or (not best-clash) 
                       (> max-choice-point
                          (first best-clash)))
               (setf best-clash clash)))
      
       (announce "Clashes found: ~A. Handling: ~A" clashes best-clash)
      
       (handle-clash (second best-clash)
                     (third best-clash)
                     (fourth best-clash)
                     (fifth best-clash)))))

;;;
;;;
;;;

(defrule look-for-atoms (dl abox :args (node node-changed-p))
  
  (loop-over-node-unexpanded-atomic-concepts (atomic-concept node)

    (unless clashes        
           
      (unless (expanded-p node atomic-concept)
             
        (announce "Found UNEXPANDED ATOM ~A : ~A" node atomic-concept)
        
        (if (is-bottom-concept-p atomic-concept)

            (register-clash node atomic-concept)

          (progn 
           
            (register-as-expanded atomic-concept 
                                  :comment 'expand-atom-no-preconditions
                                  :node node)
               
            (when *cur-tbox*
           
              (let* ((equivalent-names
                      (gethash (if (negated-p atomic-concept)
                                   (get-negated-concept atomic-concept)
                                 atomic-concept)
                               (equivalent-names *cur-tbox*)))
                     (equivalent-names
                      (if (negated-p atomic-concept)
                          (mapcar #'get-negated-concept equivalent-names)
                        equivalent-names)))

                (dolist (def equivalent-names)
                  (unless (on-tableau-p node def) 
                   
                    (announce "Node ~A: Adding equivalent name for ~A : ~A" 
                              node atomic-concept def)
                     
                    (setf node-changed-p t)
           
                    (let ((added
                           (register-as-unexpanded def
                                                   :comment 'add-equivalent-name
                                                   :node node 
                                                   :depends-on (list (list node atomic-concept)))))
                   
                      (check-for-clash node added)))))
                
              ;;; TBox7  

              (multiple-value-bind (def axiom)
                  (get-simple-axiom-with-left-side 
                   (if (negated-p atomic-concept)
                       (get-negated-concept atomic-concept)
                     atomic-concept))
             
                (let ((def (when def                        
                             (if (negated-p atomic-concept)
                                 (unless (primitive-p axiom)
                                   (get-negated-concept def))
                               def))))
                  
                  (when def
                    (unless (on-tableau-p node def) 
                   
                      (announce "Node ~A: Adding concept definition of ~A : ~A" 
                                node atomic-concept def)
                     
                      (setf node-changed-p t)
           
                      (let ((added
                             (register-as-unexpanded def
                                                     :comment 'add-concept-definition-of-atom
                                                     :node node 
                                                     :depends-on (list (list node atomic-concept)))))
                   
                        (check-for-clash node added))))))


              ;;; zur Maximierung der abgeleiteten Info: 
              ;;; aus A -> B  für "not B" auch "not A" folgern!
              ;;; wird durch obiges lazy Unfolding nicht abgeleitet! Zusatzregel: 

              (when (and *compute-core-model-p*
                         ;;; fuer das Instance Retrieval Model nur fuer root node?
                         (=> *store-instance-retrieval-model-p* 
                             t ; (root-p node)
                             ))

                (when (tbox-classified-p *cur-tbox*)
                  (let ((parents (ancestors atomic-concept)))
                    (dolist (parent parents)
                      (dolist (parent parent) ; equivalence classes
                        (let ((def (parse-concept parent)))
                          
                          (when def
                            (unless (on-tableau-p node def) 
                   
                              (announce "Node ~A: Adding taxonomy implied concept definition of ~A : ~A" 
                                        node atomic-concept def)
                              
                              (setf node-changed-p t)
                              
                              (let ((added
                                     (register-as-unexpanded def
                                                             :comment 'add-concept-definition-of-atom
                                                             :node node 
                                                             :depends-on (list (list node atomic-concept)))))
                                
                                (check-for-clash node added)))))))))


                (dolist (axiom (get-axioms-containing-right-side 
                                (get-negated-concept atomic-concept)))
                  
                  (let ((def (get-negated-concept (left axiom))))
                  
                    (when def
                      (unless (on-tableau-p node def) 
                   
                        (announce "Node ~A: Adding concept definition of ~A : ~A" 
                                  node atomic-concept def)
                     
                        (setf node-changed-p t)
           
                        (let ((added
                               (register-as-unexpanded def
                                                       :comment 'add-concept-definition-of-atom
                                                       :node node 
                                                       :depends-on (list (list node atomic-concept)))))
                   
                          (check-for-clash node added)))))))

              #| 

              ;;; ab TBox8

              (let ((axioms
                     (get-simple-axioms-with-left-side atomic-concept)))
                
                (dolist (axiom axioms)
                  (let ((def (right axiom)))

                    (unless (on-tableau-p node def) 
                    
                      (announce "Node ~A: Adding concept definition of ~A : ~A" 
                                node atomic-concept def)
                     
                      (setf node-changed-p t)
           
                      (let ((added
                             (register-as-unexpanded def
                                                     :comment 'add-concept-definition-of-atom
                                                     :node node 
                                                     :depends-on (list (list node atomic-concept)))))
                   
                        (check-for-clash node added))))))
|# 

              ))))))

  (incf *time-spend-in-look-for-atoms*
        (- (get-internal-run-time) *start-time*)))


(defrule look-for-ands (dl abox :args (node node-changed-p))
  
  (loop-over-node-unexpanded-and-concepts (and-concept node)
       
    (unless clashes 
      
      (unless (expanded-p node and-concept)
           
        (announce "Found UNEXPANDED AND ~A : ~A" node and-concept)
           
        (register-as-expanded and-concept 
                              :comment 'expand-and-put-to-expanded
                              :node node)
           
        (dolist (arg (arguments and-concept))
          (unless clashes               
            (unless (on-tableau-p node arg)
                 
              (setf node-changed-p t)           
           
              (let ((added
                     (register-as-unexpanded arg
                                             :comment 'expand-and-put-arguments-to-unexpanded
                                             :node node
                                             :depends-on (list (list node and-concept)))))
                 
                (check-for-clash node added))))))))
  
  (incf *time-spend-in-look-for-ands*
        (- (get-internal-run-time) *start-time*)))


(defrule look-for-ors (dl abox :args (node node-changed-p))

  (loop-over-node-unexpanded-or-concepts (or-concept node)
                     
    (unless clashes

      (unless (expanded-p node or-concept)
               
        (let* ((unknowns 0)
               (disjunct nil)
               (val         
                (dolist (arg (arguments or-concept) nil)
		  (cond ((on-tableau-p node arg)
			 (setf disjunct arg)
			 (return 'true))
			((not (on-tableau-p node (get-negated-concept arg)))
			 (setf disjunct arg)
			 (incf unknowns)))))
	       
               (val 
                (or val
                    (if (zerop unknowns)
                        'false
                      (if (= 1 unknowns)
                          (values 'deterministic disjunct)
                        'unknown)))))

          (case val

            (true 

             (announce "Found TRUE OR ~A : ~A (FOUND TRUE (SUB)ARGUMENT ON TABLEAU) : ~A, ~A" 
                       node or-concept disjunct (get-choice-points disjunct :node node))
                              
             (register-as-expanded or-concept 
                                   :comment 'expand-or-found-true-or-argument
                                   :node node
                                   :depends-on (list (list node disjunct))))
                 
            (false 
                  
             (announce "Found FALSE OR ~A : ~A (ALL NEGATED ARGUMENTS ON TABLEAU)" 
                       node or-concept)
                  
             (register-clash node or-concept))
                 
            (deterministic
            
             (announce "Found DETERMINISTIC OR ~A : ~A. Disjunct: ~A" 
                       node or-concept disjunct)
                  
             (setf node-changed-p t)
                  
             (let ((added
                    (register-as-unexpanded disjunct 
                                            :comment 'put-to-unexpanded-found-deterministic-or
                                            :node node
                                            :depends-on (cons (list node or-concept)
                                                              (mapcar #'(lambda (false-disjunct)
                                                                          (list node (get-negated-concept false-disjunct)))
                                                                      (remove disjunct (arguments or-concept)))))))
                  
               (check-for-clash node added))

             (unless clashes 
                          
               (register-as-expanded or-concept
                                     :comment 'put-to-expanded-found-deterministic-or
                                     :node node
                                     :depends-on (list (list node disjunct))))))))))

    (incf *time-spend-in-look-for-ors*
          (- (get-internal-run-time) *start-time*)))


(defrule look-for-somes (dl abox :args (node node-changed-p))
  
  (loop-over-node-unexpanded-some-concepts (some-concept node)
                      
    (unless clashes   
           
      (unless (expanded-p node some-concept)

        (let ((qual (qualification some-concept))
              (role (role some-concept)))

          (loop-over-role-successors (node role) (succ edge) 
             
                                     (when (on-tableau-p succ qual)
                            
                                       (setf node-changed-p t)
                       
                                       (announce "Found satisfied SOME ~A : ~A, satisfied by node ~A" 
                                                 node some-concept succ)
                       
                                       (register-as-expanded some-concept 
                                                             :comment 'found-satisfied-some
                                                             :node node
                                                             :depends-on (list (list succ qual) edge))
              
                                       (return-from loop)))))))
  
  (incf *time-spend-in-look-for-somes*
        (- (get-internal-run-time) *start-time*)))



(defrule look-for-at-leasts (dl abox :args (node node-changed-p))

  (loop-over-node-unexpanded-at-least-concepts (at-least-concept node)
                      
    (unless clashes   
           
      (unless (expanded-p node at-least-concept)

        (let ((succs nil)
              (m 0)
              (n (n at-least-concept))
              (qual (qualification at-least-concept))
              (role (role at-least-concept)))
          
          (loop-over-role-successors (node role) (succ edge)
                                     (when (on-tableau-p succ qual)
                                       (push succ succs)
                                       (incf m (multiplicity edge))

                                       (when (>= m n) 
                                         (return-from loop))))

          (when (>= m n)
            
            (setf node-changed-p t)
            
            (announce "Found satisfied AT-LEAST ~A : ~A, satisfied by nodes ~A" 
                      node at-least-concept succs)
                       
            (register-as-expanded at-least-concept 
                                  :comment 'found-satisfied-at-least
                                  :node node
                                  :depends-on (reduce #'append (mapcar #'created-by succs))))))))

  (incf *time-spend-in-look-for-at-leasts*
        (- (get-internal-run-time) *start-time*)))


(defrule look-for-alls (dl abox :args (node node-changed-p succs))

  (loop-over-node-UNEXPANDED-all-concepts (all-concept node)
       
    (unless clashes

      (register-as-expanded all-concept
                            :comment 'expand-all-no-preconditions
                            :node node)

           
      ;; (unless (expanded-p node all-concept)
             
      (let ((qual (qualification all-concept))
            (role (role all-concept))
            (added nil))

        (loop-over-role-successors (node role) (succ edge)
               
                                   (unless (on-tableau-p succ qual)       
                            
                                     (unless clashes
                         
                                       (announce "Found applicable ALL ~A : ~A -> ~A : ~A OVER ~A" 
                                                 node all-concept succ qual edge)
                              
                                       (setf node-changed-p t
                                             added t)
                              
                                       (let ((added
                                              (register-as-unexpanded qual 
                                                                      :comment 'found-applicable-all
                                                                      :node succ
                                                                      :depends-on 
                                                                      (list (list node all-concept) edge))))
                              
                                         (check-for-clash succ added))))
                   
                                   (unless clashes
              
                                     (when (and *propagation-of-transitive-all-concepts-p* 
                                                (transitive-p all-concept))
                
                                       (unless (on-tableau-p succ all-concept)

                                         (announce "Found applicable transitive ALL ~A : ~A -> ~A : ~A OVER ~A" 
                                                   node all-concept succ qual edge)
              
                                         (setf node-changed-p t
                                               added t)
                
                                         (let ((added
                                                (register-as-unexpanded all-concept 
                                                                        :comment 'found-applicable-transitive-all
                                                                        :node succ
                                                                        :depends-on 
                                                                        (list (list node all-concept) edge))))
                  
                                           (check-for-clash succ added)))))


                                   (when added
                                     (push succ succs))))))

            
  (incf *time-spend-in-look-for-alls*
        (- (get-internal-run-time) *start-time*)))

;;;
;;;
;;;

#|

(defrule deterministic-expansion (dl abox)
  (with-clash-handler 

   (let ((any-node-has-changed-p t))
        
     (loop while any-node-has-changed-p do 
              
           (announce "Any node has changed, new round!")
                             
           (setf any-node-has-changed-p nil)
              
           (loop-over-active-nodes (node abox)

                                   (announce "Now considering node ~A" node)
             
                                   (unless clashes

                                     (when (active-p node)

                                       (let ((node-changed-p t)
                                             (node-has-changed-p nil))
       
                                         (unless (deterministically-expanded-p node)
         
                                           (loop while node-changed-p do
                            
                                                 (setf node-changed-p nil)
                                                 
                                                 (unless clashes (perform! (look-for-atoms :node node :node-changed-p node-changed-p)))

                                                 (unless clashes (perform! (look-for-ands :node node :node-changed-p node-changed-p)))
                            
                                                 (unless clashes (perform! (look-for-ors :node node :node-changed-p node-changed-p)))
                            
                                                 (unless clashes (perform! (look-for-somes :node node :node-changed-p node-changed-p)))
                           
                                                 (unless clashes (perform! (look-for-at-leasts :node node :node-changed-p node-changed-p)))
                            
                                                 (when node-changed-p 
                                                   (announce "Node ~A has changed!" node)
                                                   (setf any-node-has-changed-p t)
                                                   (setf node-has-changed-p t)))
           
                                           (when (and (not node-has-changed-p)
                                                      (not clashes))

                                             (register-deterministically-expanded abox node)))))

                                     (when (and (not *combined-some-all-rule-p*)
                                                (not clashes)
                                                (or *dynamic-blocking-p*
                                                    (active-p node)))
                 
                                       (perform! (look-for-alls :node node :node-changed-p any-node-has-changed-p)))))))
   
   +insert-body-code+))

|# 


(defrule deterministic-expansion (dl abox)
  (with-clash-handler 

   (block main-loop

     (let ((nodes (get-active-nodes abox)))

       (loop while nodes do 

             (let ((all-nodes nil))
       
               (dolist (node nodes)

                 (unless (deterministically-expanded-p node)
                                         
                   (announce "Now considering node ~A" node)
                                         
                   (let ((node-changed-p t))
                                 
                     (loop while node-changed-p do
                            
                           (setf node-changed-p nil)
                                       
                           (perform! (look-for-atoms :node node :node-changed-p node-changed-p))

                           (when clashes (return-from main-loop))

                           (perform! (look-for-ands :node node :node-changed-p node-changed-p))
                                     
                           (when clashes (return-from main-loop))

                           (perform! (look-for-ors :node node :node-changed-p node-changed-p))
                                     
                           (when clashes (return-from main-loop))

                           (perform! (look-for-somes :node node :node-changed-p node-changed-p))
                                     
                           (when clashes (return-from main-loop))

                           (perform! (look-for-at-leasts :node node :node-changed-p node-changed-p))
                                     
                           (when clashes (return-from main-loop)))

                     (when (has-unexpanded-all-concepts-p node)
                       (push node all-nodes))

                     (register-deterministically-expanded abox node))))
               
               (setf nodes nil)
               
               (unless *combined-some-all-rule-p*
                 
                 (let ((node-changed-p nil))

                   ;;; durch Anwendung von look-for-alls wird der
                   ;;; Knoten, der die Qualifikation aus dem All erhalten hat, 
                   ;;; wieder aktiv und determinsitically-expanded wird
                   ;;; aufgehoben. Das All kann nun also expandiert gekennzeichnet 
                   ;;; werden! Die SOME-Regel betrachtet dann beim Erzeugen von 
                   ;;; NAchfolgern eben auch EXPANDIERTE (und nicht nur unexpandierte)
                   ;;; ALL-Konzepte auf dem Label

                   (declare (ignorable node-changed-p))
                   
                   (if *dynamic-blocking-p* 
                       (loop-over-abox-nodes (node abox)
                         (perform! (look-for-alls :node node
                                                  :node-changed-p node-changed-p
                                                  :succs nodes))

                         (when clashes (return-from main-loop)))

                     (dolist (node all-nodes)
                       (perform! (look-for-alls :node node
                                                :node-changed-p node-changed-p
                                                :succs nodes))
                     
                       (when clashes (return-from main-loop))))))))))
   
   +insert-body-code+))

;;;
;;;
;;;


(defrule deterministic-expansion (dl-with-combined-some-all-rule abox)
  (with-clash-handler 
   
   (loop-over-active-nodes (node abox)

                           (when (and (active-p node)
                                      (not (deterministically-expanded-p node)))

                             (let ((node-changed-p t))

                               (loop while node-changed-p do
                 
                                     (setf node-changed-p nil)
               
                                     (unless clashes (perform! (look-for-atoms :node node :node-changed-p node-changed-p)))

                                     (unless clashes (perform! (look-for-ands :node node :node-changed-p node-changed-p)))
               
                                     (unless clashes (perform! (look-for-ors :node node :node-changed-p node-changed-p)))
               
                                     (unless clashes (perform! (look-for-somes :node node :node-changed-p node-changed-p)))

                                     (unless clashes (perform! (look-for-at-leasts :node node :node-changed-p node-changed-p))))
           
                               (unless clashes
                                 (register-deterministically-expanded abox node)))))
   
   +insert-body-code+))


(defrule focused-deterministic-expansion (dl-with-combined-some-all-rule abox1)
  (with-clash-handler 

   (let ((rule-was-applicable-p t))

     (let ((node (select-det-node abox *strategy* language)))

       (when node

         (loop while rule-was-applicable-p do

               (setf rule-was-applicable-p nil)
             
               (unless clashes
                 (perform! (look-for-atoms :node node :node-changed-p rule-was-applicable-p)))

               (unless clashes
                 (perform! (look-for-ands :node node :node-changed-p rule-was-applicable-p)))
               
               (unless clashes
                 (perform! (look-for-ors :node node :node-changed-p rule-was-applicable-p)))

               (unless clashes
                 (perform! (look-for-somes :node node :node-changed-p rule-was-applicable-p)))

               (unless clashes 
                 (perform! (look-for-at-leasts :node node :node-changed-p rule-was-applicable-p))))
         
         (unless clashes
           (register-deterministically-expanded abox node))))
               
     +insert-body-code+)))
       
       

     
    
