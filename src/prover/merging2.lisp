;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;; Merging etc. 
;;;


(defun try-to-merge (node 
                     partition 
                     &key 
                     depends-on
                     new-choice-point)

  (let ((failed-choice-points (get-choice-points depends-on)))

    (labels ((mergable-p (nodes)
                      
               (cond ((every #'old-p nodes)
                    
                      ;;; da alle Knoten "old" sind, gibt es keine Alternativen ->
                      ;;; das at-most muss verhindert werden! 

                      (values nil failed-choice-points))

                     ((some #'(lambda (a)
                                (some #'(lambda (b)
                                          (and (not (eq a b))
                                               (intersection (created-by a)
                                                             (created-by b))))
                                      nodes))
                            nodes)

                      (values nil failed-choice-points))

                     (t
                    
                      (let ((clash-p nil))
                          
                        (mapl #'(lambda (a) 
                                
                                  (let ((a (first a))
                                        (b (rest a)))
                                        
                                    (dolist (b b) 
                                      (loop-over-node-concepts (ca a)
                                        (when (on-tableau-p b (get-negated-concept ca))
                                          ;; (break)
                                          ;;; ordinärer Clash, müsste nicht hier abgehandelt werden,
                                          ;;; aber warum nicht... 
                                          (setf clash-p t)
                                          (push-all-to (get-choice-points ca :node a) 
                                                       failed-choice-points)
                                          (push-all-to (get-choice-points (get-negated-concept ca) :node b) 
                                                       failed-choice-points))))))
                              nodes)
                            
                        (if clash-p 
                            (values nil failed-choice-points)
                          t))))))

      (let* ((edge-to-a (first partition))
             (edge-to-b (second partition))

             (a (if (eq node (from edge-to-a))
                    (to edge-to-a)
                  (from edge-to-a)))

             (b (if (eq node (from edge-to-b))
                    (to edge-to-b)
                  (from edge-to-b)))

             (nodes (list a b))

             (abox (in-graph a))

             (type (if (eq node (from edge-to-a))
                       (if (eq node (from edge-to-b))
                           'out-out
                         'out-in)
                     (if (eq node (from edge-to-b))
                         'in-out
                       'in-in))))

        (labels ((create-merge-node ()

                   (register-action change-state nil edge-to-a)
                   (register-action change-state nil edge-to-b)
             
                   (ecase type
                     (out-out
                      (decf (slot-value edge-to-a 'multiplicity))
                      (decf (slot-value edge-to-b 'multiplicity)))

                     (in-out
                      (decf (slot-value edge-to-a 'inverse-multiplicity))
                      (decf (slot-value edge-to-b 'multiplicity)))

                     (out-in
                      (decf (slot-value edge-to-a 'multiplicity))
                      (decf (slot-value edge-to-b 'inverse-multiplicity))))
                 
                   
                   (let ((new-node 
                          (create-anonymous-node abox
                                                 
                                                 ;;; wichtig! sonst werden fuer einen 
                                                 ;;; at-least-Constraint generierte Successors
                                                 ;;; miteinander gemerged! s. (intersection ... ) in
                                                 ;;; try-to-merge 
                                                 
                                                 :created-by (append (created-by a)
                                                                     (created-by b))
                              
                                                 :new-choice-point new-choice-point
                                                 :depends-on depends-on)))

                     
                     
                     (dolist (node nodes)

                       (register-action change-state nil node)

                       (when (and (=> (eq node a)
                                      (ecase type
                                        (out-out
                                         (zerop (slot-value edge-to-a 'multiplicity)))
                                        (in-out
                                         (zerop (slot-value edge-to-a 'inverse-multiplicity)))
                                        (out-in
                                         (zerop (slot-value edge-to-a 'multiplicity)))))
                                  (=> (eq node b)
                                      (ecase type
                                        (out-out
                                         (zerop (slot-value edge-to-b 'multiplicity)))
                                        (in-out
                                         (zerop (slot-value edge-to-b 'multiplicity)))
                                        (out-in
                                         (zerop (slot-value edge-to-b 'inverse-multiplicity))))))
                                  
                         (setf (slot-value node 'deleted-p) t)
                         ;(delete-node abox node)
                         )
                       
                       (loop-over-node-concepts (concept node)
                         (unless (on-tableau-p new-node concept)
                           (register-as-unexpanded concept
                                                   :node new-node
                                                   :new-choice-point new-choice-point
                                                   :depends-on (cons (list node concept)
                                                                     depends-on))))
                     
                       (loop-over-node-expanded-concepts (concept node)
                         (unless (expanded-p new-node concept)
                           (register-as-expanded concept 
                                                 :node new-node))))
                     
                     new-node)))

          (multiple-value-bind (mergable-p deps)
            
              (mergable-p nodes)

            (if (or (not mergable-p)

                    (ecase type
                     (out-out
                      (or (zerop (slot-value edge-to-a 'multiplicity))
                          (zerop (slot-value edge-to-b 'multiplicity))))

                     (in-out
                      (or 
                       (zerop (slot-value edge-to-a 'inverse-multiplicity))
                       (zerop (slot-value edge-to-b 'multiplicity))))

                     (out-in
                      (or 
                       (zerop (slot-value edge-to-a 'multiplicity))
                       (zerop (slot-value edge-to-b 'inverse-multiplicity))))))

                (values nil deps)

              (let ((new-node (create-merge-node))
                    (role nil))

                (ecase type
                
                  (out-out 
            
                   (setf role
                         (create-and-role (mapcar #'role partition))))

                  (in-out 

                   (setf role 
                         (create-and-role (list (slot-value (role edge-to-a) 'inverse-role)
                                                (role edge-to-b)))))

                  (out-in

                   (setf role 
                         (create-and-role (list (role edge-to-a)
                                                (slot-value (role edge-to-b) 'inverse-role)))))

                  (in-in 
                   (setf role 
                         (create-and-role (list (slot-value (role edge-to-a) 'inverse-role)
                                                (slot-value (role edge-to-b) 'inverse-role))))))
                                              

                (relate node new-node role 
                        :old-p nil
                        :new-choice-point new-choice-point
                        :depends-on depends-on)
           
                ;;;
                ;;; Tableau umbauen: 
                ;;;

                (dolist (node nodes)
                  (loop-over-role-successors (node nil) (succ edge)
                    
                    (when (and (not (member edge partition)))
                      
                      ;;; multiplicity muss nicht betrachtet werden!
                      ;;; die neuen Kanten sollen exakte Kopien sein!
                               
                      (relate new-node succ (role edge) 
                              :old-p nil 
                              :new-choice-point new-choice-point
                              :depends-on (cons edge depends-on)
                              :multiplicity (multiplicity edge)
                              :inverse-multiplicity (inverse-multiplicity edge))))
                              
                  
                  (loop-over-role-predecessors (node nil) (succ edge)
                    
                    (when (and (not (member edge partition)))

                      (relate succ new-node (role edge) 
                              :old-p nil
                              :new-choice-point new-choice-point
                              :depends-on (cons edge depends-on)
                              :multiplicity (multiplicity edge)
                              :inverse-multiplicity (inverse-multiplicity edge)))))

                ;; (princ "****")
                ;; (visualize-abox (in-graph node)) (break)
                
                new-node))))))))
                


