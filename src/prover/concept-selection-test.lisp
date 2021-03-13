;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(defmethod count-no-of-open-disjuncts ((node abox-node) (or-concept or-concept))
  (loop as arg in (arguments or-concept)
        when (and (not (on-tableau-p node arg))
                  (not (on-tableau-p node (get-negated-concept arg))))
        sum 1))

;;;
;;; Code for syntactic branching
;;;

(defun get-or-score (node or-concept)
  (declare (ignorable node or-concept))
  1)
    
(defmethod select-or-concept ((abox abox) (language dl))
  (let ((or nil)
        (node nil)
        (score nil))
    
    (loop-over-abox-nodes (or-node abox nil)

      (when (and (active-p or-node)
                 (has-unexpanded-or-concepts-p or-node nil))

        (when *reflexive-checks-p*
          (unless (deterministically-expanded-p or-node)
            (error "SELECT-OR-CONCEPT: Strategy Error!")))
      
        (loop-over-node-unexpanded-or-concepts (or-concept or-node nil)
        
          (let ((new-score 
                 (get-or-score or-node or-concept)))
            (when (or (not or)
                      (> new-score score))
              (setf score new-score)
              (setf or or-concept node or-node)))))
      
      (when or 
        
        (announce "~%+++ Selected OR CONCEPT ~A : ~A" node or)
        
        (return-from select-or-concept
          (values or node))))))

;;;
;;; Code for semantic branching
;;; 

(defmethod select-open-disjunct ((abox abox) (language dl))
  (let ((best-score nil)
        (best nil)
        (concept nil)
        (best-node nil)
        (score nil)
        (ors-found-p nil))

    (loop-over-abox-nodes (node abox)
      (when (and (active-p node)
                 (has-unexpanded-or-concepts-p node nil))

        (when *reflexive-checks-p*
          (unless (deterministically-expanded-p node)
            (error "SELECT-OPEN-DISJUNCT: Strategy Error!")))

        (let ((score (get-node-score node)))
          (when (or (not best-score)
                    (> score best-score))

            (setf best-score score)
            (setf best-node node)))))

    (when best-node


      (let ((open-disjuncts nil)
            (ors nil)
            (node best-node))
          
        (loop-over-node-unexpanded-or-concepts (or-concept node nil)

          (push or-concept ors)
          (setf ors-found-p t)

          (dolist (disjunct (arguments or-concept))
            
            (when (and ;(not (on-tableau-p node disjunct)) ;; kann nicht auftreten! 
                   (not (on-tableau-p node (get-negated-concept disjunct))))
              
              ;;(return-from select-open-disjunct 
              ;;  (values disjunct or-concept node))

              (push (list disjunct or-concept) open-disjuncts))))


        (dolist (disjunct-and-or open-disjuncts)
          (let ((disjunct (first disjunct-and-or))
                (or-concept2 (second disjunct-and-or)))

            (dolist (disjunct (list disjunct (get-negated-concept disjunct)))
              (let ((score 0))
                  
                #|
                  (dolist (or-concept ors)
                    (when (member disjunct (arguments or-concept))
                      (incf score))) |# 

                (setf score
                      (get-node-score node))

                (unless (zerop score)
                  ;;; ein solches Disjunkt bewirkt nichts, 
                  ;;; ist in keiner Disjunktion ein Argument! 
                  (when (or (not best)
                            (> score best-score))
                    
                    (setf best-node node 
                          concept or-concept2
                          best disjunct
                          best-score score)))))))

        (when (and ors-found-p (not best))
          (error "Could not select disjunct!"))

        (when best

          (announce "~%+++ Selected OPEN DISJUNCT ~A OF ~A : ~A" best concept best-node)
     
          (return-from select-open-disjunct
            (values best concept best-node)))))))

;;;
;;;
;;;
;;;



;;;
;;;
;;;
    

;;;
;;;
;;;

(defun get-at-least-score (node at-least-concept)
  (- 1000000000
     (tools:maximum (get-choice-points at-least-concept :node node))))
    
(defmethod select-at-least-concept ((abox abox) (language dl))
  (let ((node 
         (or 
          (get-oldest-node-satisfying1 abox ; wichtig! sonst falsch! 
                                         #'(lambda (x) 
                                             (and (active-p x)
                                                  (old-p x)
                                                  (has-unexpanded-at-least-concepts-p x nil))))
          (get-youngest-node-satisfying1 abox ; Tiefensuche!
                                         #'(lambda (x) 
                                             (and (active-p x)
                                                  (has-unexpanded-at-least-concepts-p x nil)))))))
    (when node
      (let ((at-least nil)
            (score nil))
        (loop-over-node-unexpanded-at-least-concepts (at-least-concept node nil)
          (let ((new-score 
                 (get-at-least-score node at-least-concept)))
            (when (or (not at-least)
                      (> new-score score))
              (setf score new-score)
              (setf at-least at-least-concept))))

        (when at-least

          (announce "~%+++ Selected AT-LEAST CONCEPT ~A : ~A" node at-least)
              
          (return-from select-at-least-concept
            (values at-least node)))))))

;;;
;;;
;;;

(defmethod select-violated-at-most-concept ((abox abox) (language dl))
  (loop-over-abox-nodes (node abox)
    
    (let ((role-n-entries nil))
      
      (when (and (active-p node)
                 (has-unexpanded-at-most-concepts-p node))
        
        (when *reflexive-checks-p*
          (unless (deterministically-expanded-p node)
            (error "Strategy Error!")))

        ;;; kleinstes (at-most n R) pro Rolle R raussuchen 
        
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

;;;
;;;
;;;

(defmethod select-attribute-exists-concepts ((abox abox) (language dl))
  (let ((node 
         (or (get-oldest-node-satisfying1 abox ; wichig! sonst falsch! 
                                          ;;; s. Gegenbeispiel in alchfnrplus-prover.lisp!
                                          #'(lambda (x) 
                                              (and (active-p x)
                                                   (old-p x)
                                                   (has-unexpanded-attribute-exists-concepts-p x))))
             (get-youngest-node-satisfying1 abox
                                          #'(lambda (x) 
                                              (and (active-p x)
                                                   (has-unexpanded-attribute-exists-concepts-p x))))))
        (ae-concept nil)
        (ae-concepts nil)
        (relevant-ae-concepts nil)
        (score nil))

    (when node
           
      (loop-over-node-unexpanded-attribute-exists-concepts (concept node nil)
        (let ((new-score 
               (get-some-score node ae-concept)))
          (when (or (not ae-concept)
                    (> new-score score))
            (setf score new-score)
            (setf ae-concept concept))
          (push concept ae-concepts)))

      (push ae-concept relevant-ae-concepts)

      (when ae-concept

        (announce "~%+++ Selected ATTRIBUTE-EXISTS CONCEPT ~A : ~A" node ae-concept)
             
        (let* ((found t))

          (loop while found do
                (setf found nil)
                (let ((x
                       (find-if #'(lambda (x)
                                    (some #'(lambda (y) 
                                              (has-common-parent-feature (role x) (role y)))
                                          relevant-ae-concepts))
                                ae-concepts)))
                  (when x
                    (setf found t)
                    (setf ae-concepts (delete x ae-concepts))
                    (push x relevant-ae-concepts)))))

        (setf relevant-ae-concepts
              (delete-duplicates relevant-ae-concepts))

        (announce "~%+++ Selected group of ATTRIBUTE-EXISTS CONCEPTS ~A : ~A" node relevant-ae-concepts) 

        (values ae-concept relevant-ae-concepts node)))))
