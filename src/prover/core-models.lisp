;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;


(timed-defmethod delete-non-det-assertions ((abox abox))
  (with-abox* (abox)
    (let ((*reflexive-checks-p* nil)) ; wegen Error checking in delete-node etc.

      (labels ((det-p (cp)
                 (and (not (cdr cp))
                      (or (not (first cp))
                          (zerop (first cp)))))
           
               (deterministic-p (node-or-edge)
                 (every #'(lambda (constraint)
                            (if (consp constraint)
                                (let* ((node (first constraint))
                                       (concept (second constraint))
                                       (cp (get-choice-points concept :node node)))
                                  (det-p cp))
                              (det-p (get-choice-points constraint))))
                        (created-by node-or-edge)))

               (relevant-p (node)  
                 (=> *store-instance-retrieval-model-p* 
                     (or (= (id node) 1)
                         ;;; nachfolger vom root node? 
                         (some #'(lambda (in)
                                   (= (id (from in)) 1))
                               (incoming node))))))
      
        (loop-over-abox-nodes (node abox)
          (if (or (and (not (old-p node))
                       (not (deterministic-p node)))
                  (not (relevant-p node)))

              (delete-node abox node)


            (progn 
              (dolist (slot '(choice-points
                              really-satisfiable-p 
                              cache-satisfiable-p
                              ;deterministically-expanded-p
                              realized-p))
    
                (setf (slot-value node slot) nil))
              (reset-label (description node)))))

        


        (loop-over-abox-edges (edge abox)
                              (reset-label (description edge))

                              (when (and (not (old-p edge))
                                         (not (deterministic-p edge)))

                                (delete-edge abox edge)))

        (let ((history (get-history abox)))
          (dolist (action history)
            (when (action-det-action-p action)
              (let* ((post (slot-value action 'post)))
                (dolist (post post)
                  (let ((concept (second post))
                        (node (first post)))

                    (when (relevant-p node)
                    
                      (pushnew concept (slot-value node 'told-concepts))

                      (if (or (is-all-concept-p concept)
                              (is-at-most-concept-p concept))
                    
                          (add-to-unexpanded node concept)

                        (add-to-expanded node concept)))))))))

        (with-slots (action-timestamp-counter current-action choice-point-counter) abox
          (setf action-timestamp-counter 0
                current-action nil
                choice-point-counter 0))

        abox))))
                             



(timed-defmethod compute-core-model ((abox abox))
  (with-abox (abox)
    (loop-over-abox-nodes (node abox)
      (when (or (and (old-p node)
                     ;;; fuers instance retrieval model brauch ich nur den root node!
                     (not *store-instance-retrieval-model-p* ))
                (= (id node) 1))
        (setf (slot-value node 'core-model) 
              (make-model-from-node node))))

    abox))


            
