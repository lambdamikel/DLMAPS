;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(defstruct (node-model (:type list :constructor :auto))
  all-expanded
  atoms
  successors
  attributes
  alls 
  at-mosts)

(defstruct (role-profile (:type list :constructor :auto))
  role 
  counter
  models)

(defmethod make-model-from-node ((node abox-node)
                                 &key 
                                 (all-expanded nil all-expanded-p)
                                 (atoms nil atoms-p)
                                 (alls nil alls-p)
                                 (attributes nil attributes-p)
                                 (at-mosts nil at-mosts-p))
                                 
  ;(describe-object node t)

  (when (or (blocked-p node)
            (cache-satisfiable-p node))

    (error "make model from node: ~A is blocked or cache satisfiable!" node))
  

  (make-node-model

   :all-expanded
   (if all-expanded-p
       all-expanded 

     (let ((concepts nil))

       (loop-over-node-expanded-concepts (concept node)
         (push concept concepts))

       concepts))

   :atoms 
   (if atoms-p 
       atoms
     (let ((concepts nil))
       (loop-over-node-expanded-atomic-concepts (concept node)
         (push concept concepts))
       concepts))

   :alls
   (if alls-p
       alls
     (let ((concepts nil))
       (loop-over-node-all-concepts (concept node)
         (push concept concepts))
       concepts))

   :attributes 
   (if attributes-p
       attributes
     (let ((concepts nil))
       (loop-over-node-unexpanded-attribute-exists-concepts (concept node)
         (push concept concepts))
       concepts))

   :at-mosts 
   (if at-mosts-p
       at-mosts

     (let* ((at-mosts nil))
     
       (loop-over-node-unexpanded-at-most-concepts (concept node)
         (push concept at-mosts))
     
       (loop as role in (remove-duplicates (mapcar #'role at-mosts))
             collect
             (let ((min
                    (loop as at-most in at-mosts when (eq (role at-most) role)
                          minimize (n at-most))))
               (make-role-profile :role role 
                                  :counter min
                                  :models nil)))))

   :successors 
   (let ((role-profiles nil))
     (loop-over-role-successors (node nil) (succ edge)
                                (let* ((role (role edge))
                                       (rp (assoc role role-profiles))
                                       (others (remove-if-not #'(lambda (rp2)
                                                                  (implies-p role (role-profile-role rp2)))
                                                              role-profiles))
                                       (m (multiplicity edge)))

                                  (unless (zerop m)
         
                                    (unless rp
                                      (push (make-role-profile :role role 
                                                               :counter m
                                                               :models nil)
                                            role-profiles))
           
                                    (mapc #'(lambda (rp2)
                                              (incf (role-profile-counter rp2) m))
                                          others))))
     
   
     (loop-over-role-predecessors (node nil) (succ edge)
                                  (let* ((role (get-inverse-role (role edge)))
              
                                         (rp (assoc role role-profiles))
                                         (others (remove-if-not #'(lambda (rp2)
                                                                    (implies-p role (role-profile-role rp2)))
                                                                role-profiles))

                                         (m (inverse-multiplicity edge)))
     
                                    (unless (zerop m)
    
                                      (unless rp
                                        (push (make-role-profile :role role
                                                                 :counter m
                                                                 :models nil)
                                              role-profiles))
           
                                      (mapc #'(lambda (rp2)
                                                (incf (role-profile-counter rp2) m))
                                            others))))

     role-profiles)))

;;;
;;;

(defmethod models-mergeable-p ((language alch) a b) 
  (and (not (some #'(lambda (atom)
                      (let ((neg-atom (negated-concept atom)))
                        (find neg-atom (node-model-atoms b))))
                  (node-model-atoms a)))

       ;;;
       ;;; Some/Feature/At-least - All Interaction
       ;;;

       (not (some #'(lambda (all)
                      (find-if #'(lambda (rp) 
                                   (implies-p (role-profile-role rp) (role all)))
                               (node-model-successors b)))
                  (node-model-alls a)))

       (not (some #'(lambda (all)
                      (find-if #'(lambda (rp) 
                                   (implies-p (role-profile-role rp) (role all)))
                               (node-model-successors a)))
                  (node-model-alls b)))))


(defmethod models-mergeable-p ((language alchf) a b)
  (and (models-mergeable-p +alch+ a b)
       (not (some #'(lambda (rp-a)
                      (let ((r-a (role-profile-role rp-a)))
                        (find-if #'(lambda (rp-b) 
                                     (let ((r-b (role-profile-role rp-b)))
                                       (has-common-parent-feature r-a r-b)))
                                 (node-model-successors b))))
                  (node-model-successors a)))))

(defmethod models-mergeable-p ((language alchn) a b)
  (and (models-mergeable-p +alch+ a b)
       (not (possible-at-least-at-most-confict-p a b))))


(defun possible-at-least-at-most-confict-p (a b)
  (let* ((at-most-roles
          (delete-duplicates 
           (nconc (mapcar #'role-profile-role (node-model-at-mosts a))
                  (mapcar #'role-profile-role (node-model-at-mosts b)))))

         (at-least-roles
          (delete-duplicates 
           (nconc (mapcar #'role-profile-role (node-model-successors a))
                  (mapcar #'role-profile-role (node-model-successors b)))))
              
         (at-most-profiles
          (mapcar #'(lambda (role) 
                      (make-role-profile :role role
                                         :counter 
                                         (let ((a (find role (node-model-at-mosts a) :key #'role-profile-role))
                                               (b (find role (node-model-at-mosts b) :key #'role-profile-role)))
                                           (if a
                                               (if b
                                                   (min (role-profile-counter a)
                                                        (role-profile-counter b))
                                                 (role-profile-counter a))
                                             (if b 
                                                 (role-profile-counter b)
                                               (error "!"))))))                                        
                  at-most-roles))

         (at-least-profiles
          (mapcar #'(lambda (role) 
                      (make-role-profile :role role
                                         :counter 
                                         (let ((a (find role (node-model-successors a) :key #'role-profile-role))
                                               (b (find role (node-model-successors b) :key #'role-profile-role)))
                                           (if a
                                               (if b
                                                   (+ (role-profile-counter a)
                                                      (role-profile-counter b))
                                                 (role-profile-counter a))
                                             (if b 
                                                 (role-profile-counter b)
                                               (error "!"))))))
                  at-least-roles)))
              
    (find-if #'(lambda (at-most) 
                 (let* ((at-least (find (role-profile-role at-most)
                                        at-least-profiles 
                                        :key #'role-profile-role)))
                   
                   (and at-least
                        (< (role-profile-counter at-most)
                           (role-profile-counter at-least)))))

             at-most-profiles)))




(defun at-least-at-most-confict-p (a b) 
  (let* ((at-most-roles
          (delete-duplicates 
           (nconc (mapcar #'role-profile-role (node-model-at-mosts a))
                  (mapcar #'role-profile-role (node-model-at-mosts b)))))

         (at-least-roles
          (delete-duplicates 
           (nconc (mapcar #'role-profile-role (node-model-successors a))
                  (mapcar #'role-profile-role (node-model-successors b)))))
              
         (at-most-profiles
          (mapcar #'(lambda (role) 
                      (make-role-profile :role role
                                         :counter 
                                         (let ((a (find role (node-model-at-mosts a) :key #'role-profile-role))
                                               (b (find role (node-model-at-mosts b) :key #'role-profile-role)))
                                           (if a
                                               (if b
                                                   (min (role-profile-counter a)
                                                        (role-profile-counter b))
                                                 (role-profile-counter a))
                                             (if b 
                                                 (role-profile-counter b)
                                               (error "!"))))))                                        
                  at-most-roles))

         (at-least-profiles
          (mapcar #'(lambda (role) 
                      (make-role-profile :role role
                                         :counter 
                                         (let ((a (find role (node-model-successors a) :key #'role-profile-role))
                                               (b (find role (node-model-successors b) :key #'role-profile-role)))
                                           (if a
                                               (if b
                                                   ;;; MIN statt + -> wichtiger unterschied zu possible-at-least-at-most-conflict-p !
                                                   (min (role-profile-counter a)
                                                        (role-profile-counter b))
                                                 (role-profile-counter a))
                                             (if b 
                                                 (role-profile-counter b)
                                               (error "!"))))))
                  at-least-roles)))
              
    (find-if #'(lambda (at-most) 
                 (let* ((at-least (find (role-profile-role at-most)
                                        at-least-profiles 
                                        :key #'role-profile-role)))
                   
                   (and at-least
                        (< (role-profile-counter at-most)
                           (role-profile-counter at-least)))))

             at-most-profiles)))

;;;
;;;
;;;

(defmethod models-surely-not-mergeable-p ((language dl) a b) 
  (labels ((check-if-member (concepts) 
		      
             (some #'(lambda (member-concept)
                       (let ((neg-concept (negated-concept member-concept)))
				  
                         (or (find neg-concept
                                   (node-model-all-expanded b))
				      
                             (find neg-concept
                                   (node-model-alls b)))))

                   concepts)))

    (when (or (check-if-member (node-model-all-expanded a))
              (check-if-member (node-model-alls a))
              (at-least-at-most-confict-p a b))

      ;(princ "@")
      t)))
      
;;;
;;; Subtableaux Caching und MM 
;;; 

(defun register-initial-concept-sat-cache-query ()
  (incf *initial-concept-sat-cache-queries*))

(defun register-initial-concept-unsat-cache-query ()
  (incf *initial-concept-unsat-cache-queries*))


(defun register-initial-concept-sat-cache-hit ()
  (incf *initial-concept-sat-cache-hits*))

(defun register-initial-concept-unsat-cache-hit ()
  (incf *initial-concept-unsat-cache-hits*))

(defun register-mm-query ()
  (incf *mm-queries*))

(defun register-mm-success ()
  (incf *mm-hits*))


(defun register-sat-cache-query ()
  (incf *sat-cache-queries*))

(defun register-sat-cache-hit ()
  (incf *sat-cache-hits*))


;;;
;;; aus Interface.lisp
;;; 

(defun register-concept-mm-query ()
  (incf *concept-mm-queries*))

(defun register-concept-mm-hit ()
  (incf *concept-mm-hits*))

(defun register-concept-query ()
  (incf *concept-queries*))

(defun register-true-concept-query ()
  (incf *true-concept-queries*))

(defun register-subsumption-query ()
  (incf *subsumes-queries*))

(defun register-true-subsumption-query ()
  (incf *true-subsumes-queries*))

(defun register-subsumption-mm-hit ()
  (incf *subsumes-mm-hits*))

(defun register-subsumption-ts-hit ()
  (incf *subsumes-ts-hits*))

(defun register-abox-consistency-test ()
  (incf *true-abox-consistency-tests*))

(defun register-abox-subsumption-test ()
  (incf *true-abox-subsumption-tests*))

;;;
;;;
;;;

(defmethod models-of-concepts-mergeable-p ((language dl) list-of-concepts)
  (let ((concepts nil))
    
    (dolist (concept list-of-concepts)
      (etypecase concept
        (top-concept)
        (bottom-concept
         (return-from models-of-concepts-mergeable-p nil))
        ((or atomic-concept
             some-concept 
             all-concept 
             at-least-concept
             at-most-concept
             attribute-exists-concept)
         (if (cached-models concept)
             (push concept concepts)
           (return-from models-of-concepts-mergeable-p nil)))
        (and-concept
         (if (cached-models concept)
             (push concept concepts)
           (if (every #'cached-models (arguments concept))
               (dolist (arg (arguments concept))
                 (push arg concepts))
             (return-from models-of-concepts-mergeable-p nil))))
        (or-concept 
         (if (cached-models concept)
             (push concept concepts)
           (let ((arg (find-if #'cached-models 
                               (arguments concept))))
             (if arg
                 (push arg concepts)
               (return-from models-of-concepts-mergeable-p nil)))))))
        
    (loop as ca on concepts do
          (loop as cb in (rest ca) do
                (let* ((ca (first ca))
                       (res
                            
                        (some #'(lambda (ma) 
                                  (some #'(lambda (mb)
                                            (models-mergeable-p language ma mb))
                                        (cached-models cb)))
                              (cached-models ca))))
                      
                  (unless res 
                    (return-from models-of-concepts-mergeable-p nil)))))

    t))


