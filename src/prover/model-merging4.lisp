;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(defrule make-models-for-nodes (dl-with-model-merging abox)

  (when *cache-models-p*

    ;;;
    ;;; Blätter identifizieren  
    ;;;
    
    (let ((leaf-nodes nil) ; Tableau-Blaetter (keine alten) 
          (satisfiable-nodes nil)) ; Knoten, die sicher erfuellbar sind 

      (labels ((do-it (node) 

                 (when (not (or (old-p node)
                                (root-p node)))

                   ;;; vollstaendig expandierte Blätter identifizieren
          
                   (when (and (active-p node)
                              (stable-p node) ; fuer ALCHN erforderlich! 
                              (complete-p node))
            
                     ;; (describe-object node t)

                     (deactivate-node abox node))

                   ;;; inaktive Sammeln 
                   ;;; ein Knoten ist inaktiv, wenn 
                   ;;; 1. soeben deaktiviert, oder
                   ;;; 2. blockiert, oder 
                   ;;; 3. als erfuellbar bekannt (dann cache-satisfiable-p = T) 
          

                   (when (and (not (active-p node))
                              (stable-p node)
                              (=> *blocking-enabled-p*
                                  (not (blocked-p node))))
                       
                       (push node leaf-nodes)))))


        (announce "All leaf nodes   : ~A" (get-leaf-nodes abox))
        (announce "All blocked nodes: ~A" (get-blocked-nodes abox))
        

        (loop-over-leaf-nodes (node abox)
          ;;; haben niemals aktive Nachfolger!
          (do-it node))
        (loop-over-blocked-nodes (node abox)
          ;;; haben niemals aktive Nachfolger!
          (do-it node)))

      (announce "Identified leaf nodes: ~A" leaf-nodes)

      ;;;
      ;;; Knoten identifizieren, die nun *sicher* als erfuellbar bekannt sind. 
      ;;; Von den Blaettern beginnen! 
      ;;; 

      (labels ((mark-sat-nodes (node)

                 (when (=> *blocking-enabled-p*
                              
                           ;;; kann ausgeschaltet werden
                           ;;; wenn das Tableaux am Ende
                           ;;; als Erfuellbar bekannt ist, 
                           ;;; koennen auch von geblockten Knoten
                           ;;; Modelle erzeugt werden!
                           ;;; s. cache-and-delete-Anwendung
                           ;;; vor (success) im ALCH-Prover 
                              
                           (not (blocked-p node)))

                   ;;; geblockte  Knoten oder Vorgaenger von 
                   ;;; geblockten Knoten koennen dann niemals
                   ;;; als really-satisfiable-p markiert werden
                 
                   (announce "Node ~A = ~A ~A" node 
                             (outgoing node)
                             (mapcar #'(lambda (out)
                                         (really-satisfiable-p (to out)))
                                     ;; natuerlich auch wahr fuer Blaetter!
                                     (outgoing node)))

                   (when (every #'(lambda (out)
                                    (really-satisfiable-p (to out)))
                                ;; natuerlich auch wahr fuer Blaetter!
                                (outgoing node))
                   
                     (when (and (stable-p node)
                                (or (cache-satisfiable-p node)
                                    (blocked-p node)
                                    (complete-p node)))

                       (unless (really-satisfiable-p node)
                         (when (active-p node)
                           (deactivate-node abox node))

                         (setf (really-satisfiable-p node) t)
                         (push node satisfiable-nodes))
                         
                       (dolist (in (incoming node))
                         (mark-sat-nodes (from in))))))))

        (dolist (leaf leaf-nodes)
          (mark-sat-nodes leaf))
        (loop-over-cache-sat-nodes (leaf abox)
          (mark-sat-nodes leaf)))
        
      (announce "Identified satisfiable nodes: ~A" satisfiable-nodes)
    
      ;;;
      ;;; 
      ;;;

      (when *cache-models-p*
      
        (dolist (node satisfiable-nodes) ; hier sind niemals alte Knoten enthalten 

          (announce "Cache building, now considering node ~A" node) 

          ;;; hierdurch werden evtl. weitere Caches gefuellt!!!
          ;;; (durch Konzept-Xreferencing): 
                   
          (when (initial-concept node)
            (register-already-known-to-be-satisfiable abox node))
            
          (when (and (not (cache-satisfiable-p node))
                     (not (blocked-p node)))

            (announce "~A is not cache satisfiable and not blocked -> concept model can be created" node)
            
            ;;; nur wenn die Nachfolger expandiert wurden, kann
            ;;; ein Modell erzeugt werden! 

            (when (every #'(lambda (outgoing)
                             (not (active-p (to outgoing))))
                         (outgoing node))

              (announce "Successors of ~A are inactive" node)

              ;;; die ausgehenden eines nicht-alten Knotens sind 
              ;;; ebenfalls nicht alt 

              ;;; Konzept-Modelle 
                         
              (let ((model 
                     (make-model-from-node node)))

                (when (initial-concept node)

                  (announce "~A has initial concept ~A" node (initial-concept node))
                  
                  (if (< (length (cached-models
                                  (initial-concept node)))
                         *max-no-of-concept-models*)

                      (progn 
                    
                        (announce "Making concept model for initial concept ~A of node ~A" 
                                  (initial-concept node)
                                  node)
                        
                        ;(register-concept-model (get-node-concept abox node) model)
                        (register-concept-model (initial-concept node) model))

                    (announce "Already ~A models for initial concept ~A of node ~A" 
                              *max-no-of-concept-models*
                              (initial-concept node)
                              node))))))))))

  +insert-body-code+)



(defrule delete-nodes (dl abox)

  (when *delete-nodes-p* 
    (let ((change t))
      (loop while change do
            (setf change nil)
            (loop-over-leaf-nodes (node abox)
              (unless (active-p node)
                (delete-node abox node)
                (setf change t))))))

  +insert-body-code+)



(defrule make-models-for-old-nodes (dl-with-model-merging abox)

  ;;; darf nur vor (success) gerufen werden! 
  
  (when (or *store-ind-models-p* *cache-models-p*)

    (loop-over-nodes (node abox)

      (when (and (not (already-known-to-be-inconsistent-p node))
                 (or (old-p node)
                     (root-p node)))

        ;;; geblockte sind niemals alt (oder root)! 

        (announce "Cache building (old nodes), now considering node ~A" node)
            
        (when (initial-concept node)
          (register-already-known-to-be-satisfiable abox node))

        (unless (cache-satisfiable-p node)

          (when (and *store-ind-models-p*
                     (< (length (ind-models node))
                        *max-no-of-ind-models-per-node*))
                
            (announce "Making ind model for node ~A" node)
            
            (push (make-model-from-node node)
                  (slot-value node 'ind-models)))
          
          ;;; Achtung! Ind und Concept Models fuer alte Knoten sind nicht
          ;;; das selbe! Denn in einer ABox mit mehr als einem Knoten haben
          ;;; die Knoten kein initial-concept! (da ja das Label erst durch 
          ;;; die erforderliche Precompletion stabil wird, die ist jedoch 
          ;;; nichtdeterministisch) 
          
          (when (initial-concept node)
            ;;; hierdurch werden Caches gefuellt!
            (register-already-known-to-be-satisfiable abox node))
          
          (when (and *cache-models-p*  
                     (initial-concept node)
                     (< (length (cached-models
                                 (initial-concept node)))
                        *max-no-of-concept-models*))
            
            (announce "Making concept model for initial concept ~A of node ~A" 
                      (initial-concept node)
                      node)

            (let ((model (make-model-from-node node)))
            
              (register-concept-model (initial-concept node) model)
            ;(register-concept-model (get-node-concept abox node) model)
              ))))))

  +insert-body-code+)


;;;
;;;
;;;


(defrule pop-active-nodes-heap (dl abox1) 

  ;;; nur fuer Tiefensuche-Strategie mit ABOX1 !!!
  
  (with-slots (active-nodes) abox

    (let ((node (heap-peek active-nodes)))

      ;;; keine Regel war anwendbar auf den aktuell aktiven Knoten ->
      ;;; Knoten ist complete, deaktivieren, naechster Knoten vom Heap
      ;;; wird aktueller Knoten
      ;;; (Tiefensuche) 

      (if node 

          (progn

            (unless (active-p node) 
              (error "Found a deactivated node on active-nodes heap: ~A!" node))

            (when (or (not (complete-p node))
                      (blocked-p node)
                      (cache-satisfiable-p node))
                
              (describe-object node t)
              (error "deactivate nodes: ~A!" node))

            (deactivate-node abox node)

            ;;; hier tritt nun Backtracking fuer den BLOCKIERENDEN Knoten 
            ;;; auf, also ist der fertig expandiert -> nun steht auch der
            ;;; Erfuellbarkeitsstatus aller evtl. blockierten Nachfolger positiv fest, 
            ;;; vorher nicht!
              
            (when *blocking-enabled-p* 
              (dolist (blocked (slot-value node 'blocking-for))

                (setf *bb* (list node blocked ))
                
                (unless (deleted-p blocked)
                  (loop-between-blocked-blocking (var blocked node)
                                                 (setf (really-satisfiable-p var) t)))))

            ;;;
            ;;; weiter 
            ;;;

            (when (every #'(lambda (out)
                             ;;; also: entweder Complete oder Cache-Sat Hit! 
                             (or (really-satisfiable-p (to out))
                                 ;; (blocked-p (to out)) falsch
                                 (already-known-to-be-satisfiable-p (to out)))) 
                         (outgoing node))

              (setf (really-satisfiable-p node) t)                  
      
              (when *cache-models-p*
                    
                (announce "Cache building, now considering node ~A" node) 
                    
                (when (initial-concept node)
                  (register-already-known-to-be-satisfiable abox node))
            
                (let ((model 
                       (make-model-from-node node)))
                      
                  (when (and (initial-concept node)
                             (< (length (cached-models
                                         (initial-concept node)))
                                *max-no-of-concept-models*))
                        
                    (announce "Making concept model for initial concept ~A of node ~A" 
                              (initial-concept node)
                              node)
                      
                      ;(register-concept-model (get-node-concept abox node) model)
                    (register-concept-model (initial-concept node) model)))))

            +insert-positive-code+)

        (progn 
          +insert-negative-code+)))))

;;;
;;;
;;;


(defrule model-merging (dl-with-model-merging abox :args (node))

  (when *use-cached-models-p* 
             
    (labels ((do-it (node)
  
               (when (and (active-p node) 
                          (stable-p node)
                          (not (complete-p node)) 
                          ;; kann fuer ALCHN vorkommen! 
                          (not (checked-p node)))

                 ;;;
                 ;;; Unsat Cache
                 ;;; 

                 (announce "Checking Unsat Cache for stable node ~A" node) 
      
                 (when (and *use-unsat-cache-p*
                            (initial-concept node)
                            (register-initial-concept-unsat-cache-query)
                            (already-known-to-be-inconsistent-p node))           

                   (setf (cur-clash-node abox) nil)
                   (register-initial-concept-unsat-cache-hit)

                   ;;; 
                   ;;; Backtracking ausloesen!
                   ;;; 
                   
                   (let ((cps
                          #| Aua! das war wohl nix... 
                             (sort 
                           (copy-list
                            (if (is-and-concept-p (initial-concept node))
                                (get-choice-points (arguments (initial-concept node))
                                                   :node node)
                              (get-choice-points (initial-concept node)
                                                 :node node)))
                           #'>) |# 

                          (sort 
                           (copy-list
                            (get-choice-points
                             (multiple-value-call #'list
                               (get-unexpanded-concept-lists node))
                             :node node))
                           #'>)))

                     
                     (when *debug-p* 
                       (describe-object node t)
                       (announce "Returning choice points: ~A" cps)
                       (announce "Choice points: ~A : ~A" 
                                 (multiple-value-call #'list (get-unexpanded-concept-lists node))
                                 (get-choice-points (multiple-value-call #'list
                                                      (get-unexpanded-concept-lists node))
                                                    :node node)))
                     (return-from prover
                       (values nil cps))))
                             

                 ;;;
                 ;;; Sat Cache und Model Merging
                 ;;;
        
                 (announce "Checking Sat Cache for stable node ~A" node) 

                 (register-sat-cache-query)

                 (cond ((and (initial-concept node)
                             
                             (=> *store-ind-models-p*
                                 ;; die muessen expandiert werden, sonst hab ich kein Modell!
                                 (not (root-or-old-p node)))
                                 
                             (progn 
                               (register-initial-concept-sat-cache-query)
                               (or (cached-models (initial-concept node))
                                   (already-known-to-be-satisfiable-p node))))

                        (register-initial-concept-sat-cache-hit)
                        (register-sat-cache-hit)

                        (announce "Found cached model for initial concept ~A of stable node ~A" 
                                  (initial-concept node)
                                  node)

                        (register-already-known-to-be-satisfiable abox node)
                        (register-cache-satisfiable abox node)
             
                        (deactivate-node abox node))

                
                       ;;
                       ;; das initial-concept wird beim Aufruf von "register-label-is-stable"
                       ;; gesetzt; dort wird ein AND-Konzept geparsed, und wenn bereits eines
                       ;; im Concept-Store mit cached-model ist, bin ich fein raus!
                       ;;
                       ;; ansonsten: versuche, ob mergeable
                       ;; wenn mergeable, könnte man auch die gemergten Modelle (to be implemented!)
                       ;; als Modell fuer initial-concept nehmen!
                       ;;
                       ;; wenn das alles fehlschlaegt, wird der Knoten expandiert, und am Ende, 
                       ;; wenn er compelte-p ist, wird sein Modell gecached fuer initial-concept!
                       ;; s. mode-caching-rule
                       ;;

                       ( (let ((concepts nil))

                           (when (=> *store-ind-models-p*
                                     ;; die muessen expandiert werden, sonst hab ich kein Modell!
                                     (not (root-or-old-p node)))

                             (loop-over-node-unexpanded-concepts (concept node)
                               (push concept concepts))
                             
                             (register-mm-query) 
                             
                             (models-of-concepts-mergeable-p language concepts)))


                         (announce "Unexpanded concepts ~A of stable node ~A are mergeable" 
                                   (get-unexpanded-concept-lists node)
                                   node)
              
                         (register-mm-success)
                         (register-sat-cache-hit)
              
                         (register-already-known-to-be-satisfiable abox node)
                         (register-cache-satisfiable abox node)
                  
                         (deactivate-node abox node)
                  
                         (announce "Found MM cache hit of stable node ~A" node))))

               ;;;
               ;;;
               ;;;
    
               (setf (checked-p node) t)))

      (if node
          (do-it node)
        (loop-over-active-nodes (node abox)
          (do-it node)))
  
      +insert-body-code+)))




(defrule identify-stable-nodes (alchn abox)
  
  (let ((changed t))

    (loop while changed do

          (setf changed nil)

          (loop-over-active-nodes (node abox)
    
            (when (and (not (stable-p node))
                       (every #'(lambda (in)
                                  (let ((pre (from in)))
                                    (and (not (has-unexpanded-generating-concepts-p pre))
                                         (stable-p pre))))
                              (incoming node)))
      
              (announce "Identified node ~A as stable!" node)
              (setf changed t)
              (register-label-is-stable abox node)))))

    +insert-body-code+)

