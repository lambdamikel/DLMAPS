;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;; hier sind auch einige Funktionen nach Racer benannt!  
;;;

(defmacro implies (left right)
  `(progn
     ;(impl (not ,right) (not ,left))
     (impl ,left ,right)))

(defmacro equivalent (left right)	
  `(equi ,left ,right))

(defmacro disjoint (&rest concepts)
  (unless (cdr concepts)
    (error "DISJOINT needs at least 2 concepts!"))
  `(progn 
     ,@(mapcar #'(lambda (concept)
                   `(impl ,concept
                          (and ,@(mapcar #'(lambda (x) 
                                             `(not ,x))
                                         (remove concept concepts)))))
               concepts)))

;;;
;;;
;;;

(defmacro define-primitive-concept (left &optional (right 'top))
  `(def* ,left ,right))

(defmacro defprimconcept (left &optional (right 'top))
  `(define-primitive-concept ,left ,right))

(defmacro define-concept (left &optional right)
  (if right 
      `(def ,left ,right)
    `(def* ,left)))

(defmacro defconcept (left &optional right)
  `(define-concept ,left ,right))

;;;
;;;
;;;

(defmacro primitive-concept? (concept)
  `(primitive-concept-p ',concept))

(defmacro defined-concept? (concept)
  `(defined-concept-p ',concept))

;;;
;;;
;;;

(defmacro define-primitive-role (role &rest args)
  `(defrole ,role ,@args))

(defmacro define-primitive-attribute (role &rest args)
  `(deffeature ,role ,@args))

;;;
;;;
;;;

(defmacro instance (name def &optional abox &rest args)
  `(with-abox* ((or ',abox *cur-abox*))
     (ins ,name ,def ,@args)))

(defmacro db-instance (name def &optional abox &rest args)
  `(with-abox* ((or ',abox *cur-abox*))
     (ins ,name ,def 
          :update-db-p t
          :type 'db-abox-node ,@args)))

(defmacro forget (name def &optional abox &rest args)
  `(with-abox* ((or ',abox *cur-abox*))
     (for ,name ,def ,@args)))

(defmacro db-forget (name def &optional abox &rest args)
  `(with-abox* ((or ',abox *cur-abox*))
     (for ,name ,def 
          :update-db-p t ,@args)))

(defmacro related (a b role &optional abox &rest args)
  `(with-abox* ((or ',abox *cur-abox*))
     (rel ,a ,b ,role ,@args)))

(defmacro db-related (a b role &optional abox &rest args)
  `(with-abox* ((or ',abox *cur-abox*))
     (rel ,a ,b ,role 
          :type 'db-abox-edge
          :node-type 'db-abox-node
          :update-db-p t
          ,@args)))

(defmacro unrelated (a b role &optional abox &rest args)
  `(with-abox* ((or ',abox *cur-abox*))
     (unrel ,a ,b ,role ,@args)))


(defmacro db-unrelated (a b role &optional abox &rest args)
  `(with-abox* ((or ',abox *cur-abox*))
     (unrel ,a ,b ,role 
            :look-into-db-p nil
            :update-db-p t
            ,@args)))

;;;
;;;
;;;

(defmacro db-del  (name &rest args)
  `(delete-node *cur-abox* ',name 
                :look-into-db-p t
                :update-db-p t
                ,@args))

;;;
;;;
;;;

(defun add-concept-assertion (abox ind concept)
  (with-abox* (abox :delete-if-exists-p nil) 
              (node-instance ind concept)))

(defun forget-concept-assertion (abox ind concept)
  (with-abox* (abox :delete-if-exists-p nil)
              (node-forget ind concept)))

(defun add-role-assertion (abox from to role)
  (with-abox* (abox :delete-if-exists-p nil)
              (relate from to role)))

(defun forget-role-assertion (abox from to role)
  (with-abox* (abox :delete-if-exists-p nil)
              (unrelate from to role)))

;;;
;;;
;;;

(defun init-abox (&optional (abox (name *cur-abox*)))
  (in-abox* abox :delete-if-exists-p t))

(defun init-tbox (&optional (tbox (name *cur-tbox*)))
  (in-tbox* tbox :delete-if-exists-p t))

;;;
;;;
;;;

(defun set-current-abox (&optional (abox (name *cur-abox*)))
  (in-abox* abox :delete-if-exists-p nil))

(defun set-current-tbox (&optional (tbox (name *cur-tbox*)))
  (in-tbox* tbox :delete-if-exists-p nil))

;;;
;;; 
;;;

(defun tbox-coherent-p (&optional (tbox *cur-tbox*) &rest args)
  (apply #'tbox-sat-p tbox args))

(defun classify-tbox (&optional (tbox *cur-tbox*) &rest args)
  (apply #'classify tbox args))

;;;
;;;
;;;

(defun concept-satisfiable-p (concept &rest args)
  (apply #'sat-p concept args))
 
(defun concept-subsumes-p (concept1 concept2 &rest args)
  (apply #'subsumes-p concept1 concept2 args))

(defmethod concept-disjoint-p (concept1 concept2 &rest args  &key tbox &allow-other-keys)
  (let ((*cur-store* (if tbox
                         (concept-store (find-tbox tbox :error-p t))
                       (concept-store *cur-tbox*))))
    (apply #'concept-disjoint-p
           (parse-concept concept1)
           (parse-concept concept2) args)))

(defmethod concept-disjoint-p ((concept1 concept) (concept2 concept) &rest args)
  (not (apply #'sat-p 
              (make-and-concept (list concept1 concept2)) args)))

(defun concept-equivalent-p (concept1 concept2 &rest args)
  (apply #'equivalent-p concept1 concept2 args))


;;;
;;;
;;;


(defmacro concept-satisfiable? (expr &rest args)
  `(concept-satisfiable-p (quote ,expr) ,@args))

(defmacro concept-subsumes? (concept1 concept2 &rest args)
  `(concept-subsumes-p ',concept1 ',concept2 ,@args))

(defmacro concept-equivalent? (concept1 concept2 &rest args)
  `(concept-equivalent-p ',concept1 ',concept2 ,@args))

(defmacro concept-disjoint? (concept1 concept2 &rest args)
  `(concept-disjoint-p ',concept1 ',concept2 ,@args))

;;;
;;;
;;;

(defmacro abox-consistent? (&optional abox &rest args)
  `(abox-consistent-p (or ',abox *cur-abox*) ,@args))

(defmacro tbox-coherent? (&optional tbox &rest args)
  `(tbox-coherent-p (or ',tbox *cur-tbox*) ,@args))

;;;
;;;
;;;

(defmacro tbox-classified? (&optional tbox)
  `(tbox-classified-p (find-tbox (or ',tbox *cur-tbox*) :error-p t)))

(defmacro abox-realized? (&optional abox)
  `(abox-realized-p (find-abox (or ',abox *cur-abox*) :error-p t)))

;;;
;;;
;;;

(defun all-atomic-concepts (&optional (tbox *cur-tbox*))
  (let ((tax (taxonomy (find-tbox tbox :error-p t))))
    (mapcar #'node-representation (dag-nodes tax))))

;;;
;;;
;;;

(defun get-concept-parents (concept &optional (tbox *cur-tbox*))
  (with-tbox* (tbox) 
    (parents (parse-concept concept))))

(defun atomic-concept-parents (&rest args)
  (apply #'get-concept-parents args))

(defun get-concept-children (concept  &optional (tbox *cur-tbox*))
  (with-tbox* (tbox) 
    (children (parse-concept concept))))

(defun atomic-concept-children (&rest args)
  (apply #'get-concept-children args))

(defun get-concept-equivalents (concept  &optional (tbox *cur-tbox*))
  (with-tbox* (tbox) 
    (equivalents (parse-concept concept))))

(defun atomic-concept-synonyms (&rest args)
  (apply #'get-concept-equivalents args))

(defun get-concept-ancestors (concept  &optional (tbox *cur-tbox*))
  (with-tbox* (tbox) 
    (ancestors (parse-concept concept))))

(defun atomic-concept-ancestors (&rest args)
  (apply #'get-concept-ancestors args))

(defun get-concept-descendants (concept  &optional (tbox *cur-tbox*))
  (with-tbox* (tbox) 
    (descendants (parse-concept concept))))

(defun atomic-concept-descendants (&rest args)
  (apply #'get-concept-descendants args))

;;;
;;;
;;;

(defmacro concept-parents (concept &optional tbox)
  `(get-concept-parents ',concept (or ',tbox *cur-tbox*)))

(defmacro concept-children (concept &optional tbox)
  `(get-concept-children ',concept (or ',tbox *cur-tbox*)))

(defmacro concept-equivalents (concept &optional tbox)
  `(get-concept-equivalents ',concept (or ',tbox *cur-tbox*)))

(defmacro concept-ancestors (concept &optional tbox)
  `(get-concept-ancestors ',concept (or ',tbox *cur-tbox*)))

(defmacro concept-descendants (concept &optional tbox)
  `(get-concept-descendants ',concept (or ',tbox *cur-tbox*)))

;;;
;;;
;;;

(defun get-roles (&optional (tbox *cur-tbox*))
  (unparse (get-all-roles (find-tbox tbox :error-p t))))

(defun all-roles (&rest args)
  (apply #'get-roles args))

(defun get-transitive-roles (&optional (tbox *cur-tbox*))
  (unparse (get-all-transitive-roles (find-tbox tbox :error-p t))))

(defun all-transitive-roles (&rest args)
  (apply #'get-transitive-roles args))

(defun get-features (&optional (tbox *cur-tbox*))
  (unparse (get-all-features (find-tbox tbox :error-p t))))

(defun all-features (&rest args)
  (apply #'get-features args))

;;;
;;;
;;;

(defmacro roles (&optional tbox)
  `(get-roles (or ',tbox *cur-tbox*)))

(defmacro transitive-roles (&optional tbox)
  `(get-transitive-roles (or ',tbox *cur-tbox*)))

(defmacro features (&optional tbox)
  `(get-features (or ',tbox *cur-tbox*)))

;;;
;;;
;;;

(defun get-role-parents (role &optional (tbox *cur-tbox*))
  (with-concept-store ((concept-store (find-tbox tbox :error-p t)))
    (unparse (superroles (parse-role role)))))

(defun atomic-role-parents (&rest args)
  (apply #'get-role-parents args))

(defun get-role-children (role &optional (tbox *cur-tbox*))
  (with-concept-store ((concept-store (find-tbox tbox :error-p t)))
    (unparse (subroles (parse-role role)))))

(defun atomic-role-children (&rest args)
  (apply #'get-role-children args))

(defun get-role-ancestors (role &optional (tbox *cur-tbox*))
  (with-concept-store ((concept-store (find-tbox tbox :error-p t)))
    (unparse (all-superroles (parse-role role)))))

(defun atomic-role-ancestors (&rest args)
  (apply #'get-role-ancestors args))

(defun get-role-descendants (role &optional (tbox *cur-tbox*))
  (with-concept-store ((concept-store (find-tbox tbox :error-p t)))
    (unparse (all-subroles (parse-role role)))))

(defun atomic-role-descendants (&rest args)
  (apply #'get-role-descendants args))

;;;
;;;
;;;

(defun atomic-role-inverse (role &optional (tbox *cur-tbox*))
  (with-concept-store ((concept-store (find-tbox tbox :error-p t)))
    (unparse (get-inverse-role (parse-role role)))))

;;;
;;;
;;;

(defmacro role-parents (role &optional tbox)
  `(get-role-parents ',role (or ',tbox *cur-tbox*)))

(defmacro role-children (role &optional tbox)
  `(get-role-children ',role (or ',tbox *cur-tbox*)))

(defmacro role-ancestors (role &optional tbox)
  `(get-role-ancestors ',role (or ',tbox *cur-tbox*)))

(defmacro role-descendants (role &optional tbox)
  `(get-role-descendants ',role (or ',tbox *cur-tbox*)))


;;;
;;;
;;;

(defun role-subsumes-p (r1 r2 &optional (tbox *cur-tbox*))
  (with-concept-store ((concept-store 
                        (find-tbox tbox :error-p t)))
    (when (member (parse-role r2) (all-subroles (parse-role r1)))
      t)))

(defmacro role-subsumes? (r1 r2 &optional tbox)
  `(role-subsumes-p ',r1 ',r2 (or ',tbox *cur-tbox*)))

;;;
;;;
;;;

(defun get-concept-assertions (&optional (abox *cur-abox*))
  (let ((assertions nil)
        (abox (find-abox abox :error-p t)))
    (loop-over-abox-nodes (node abox)
      (when (old-p node)
        (dolist (concept (get-concept-assertions-for-individual node abox))
          (push concept assertions))))
    assertions))

(defmacro concept-assertions (&optional abox)
  `(get-concept-assertions (or ',abox *cur-abox*)))

(defun all-concept-assertions (&rest args)
  (apply #'get-concept-assertions args))

;;;
;;;
;;;

(defun get-concept-assertions-for-individual (ind &optional (abox *cur-abox*))
  (let ((name (unparse ind)))
    (mapcar #'(lambda (x) 
                (list name x))
            (unparse
             (told-concepts (find-node
                             (find-abox abox :error-p t)
                             ind
                             :error-p t))))))

(defmacro concept-assertions-for-individual (ind &optional abox)
  `(get-concept-assertions-for-individual ',ind (or ',abox *cur-abox*)))

(defun all-concept-assertions-for-individual (&rest args)
  (apply #'get-concept-assertions-for-individual args))

;;;
;;;
;;;

(defun individual-p (ind &optional (abox *cur-abox*))
  (unparse 
   (find-node (find-abox abox :error-p t)
             ind
             :error-p nil)))

(defmacro individual? (ind &optional abox)
  `(individual-p ',ind (or ',abox *cur-abox*)))

;;;
;;; 
;;; 

(defun abox-consistent-p (&optional (abox *cur-abox*) &rest args)
  (apply #'abox-sat-p abox args))

;;;
;;; 
;;;

(defun individual-instance-p (ind concept &optional (abox *cur-abox*) &rest args)
  (apply #'instance-p abox ind concept args))

(defmacro individual-instance? (ind concept &optional abox &rest args)
  `(individual-instance-p ',ind ',concept (or ',abox *cur-abox*) ,@args))

;;;
;;;
;;;

(defun get-concept-instances (concept &optional (abox *cur-abox*) &rest args)
  (unparse (apply #'retrieve-concept-instances abox concept args)))

(defmacro concept-instances (concept &optional abox &rest args)
  `(get-concept-instances ',concept (or ',abox *cur-abox*) ,@args))

;;;
;;;
;;;

(defun get-role-assertions (&optional (abox *cur-abox*))
  (let ((assertions nil)
        (abox (find-abox abox :error-p t)))
    (loop-over-abox-edges (edge abox)
      (when (old-p edge)
        (push (unparse
               (list (list (from edge) 
                           (to edge))
                     (role edge)))

              assertions)))
    assertions))

(defmacro role-assertions  (&optional abox)
  `(get-role-assertions (or ',abox *cur-abox*)))

(defun all-role-assertions (&rest args)
  (apply #'get-role-assertions args))

;;;
;;;
;;;

(defun get-all-role-assertions-for-individual-in-domain (ind &optional 
                                                             (abox *cur-abox*)
                                                             role)
  (let* ((abox (find-abox abox :error-p t))
         (node (find-node abox ind :error-p t))
         (assertions nil))
    (with-abox* (abox) 

                (let ((role (and role (parse-role role))))
                  
                  (dolist (out (outgoing node))
                    (when (and (old-p out)
                               (old-p (to out))
                               (=> role 
                                   (eq (role out)
                                       role)))
                      (push (unparse
                             (list (list node 
                                         (to out))
                                   (role out)))
                            assertions)))))

    assertions))

(defun all-role-assertions-for-individual-in-domain (&rest args)
  (apply #'get-all-role-assertions-for-individual-in-domain args))


(defun get-all-role-assertions-for-individual-in-range (ind &optional
                                                             (abox *cur-abox*)
                                                              role)
  (let* ((abox (find-abox abox :error-p t))
         (node (find-node abox ind :error-p t))
         (assertions nil))
    (with-abox* (abox) 

                (let ((role (and role (parse-role role))))
                  
                  (dolist (out (incoming node))
                    (when (and (old-p out)
                               (old-p (from out))
                               (=> role 
                                   (eq (role out)
                                       role)))
                      (push (unparse
                             (list (list (from out)
                                         node)
                                   (role out)))
                            assertions)))))

    assertions))


(defun all-role-assertions-for-individual-in-range (&rest args)
  (apply #'get-all-role-assertions-for-individual-in-range args))


;;;
;;;
;;;


(defun get-individual-fillers (ind role &optional (abox *cur-abox*) &rest args)
  (unparse (apply #'retrieve-role-fillers abox ind role args)))
         
(defmacro individual-fillers (ind role &optional abox &rest args)
  `(get-individual-fillers ',ind ',role (or ',abox *cur-abox*) ,@args))


;;;
;;;
;;;


(defun individuals-related-p (a b role &optional (abox *cur-abox*) &rest args)
  (apply #'related-p abox a b role args))

(defmacro individuals-related? (a b role &optional abox &rest args)
  `(individuals-related-p ',a ',b ',role (or ',abox *cur-abox*) ,@args))

;;;
;;;
;;;

(defun get-abox-language (&optional (abox *cur-abox*))
  (name (get-language (find-abox abox :error-p t))))


;;;
;;;
;;;

(defun all-individuals (&optional (abox *cur-abox*))
  (let ((abox (find-abox abox :error-p t)))
    (unparse
     (remove-if-not #'old-p (get-nodes abox)))))

;;;
;;;
;;;

(defun get-tbox-version (tbox)
  (tbox-version tbox))

(defun get-abox-version (abox)
  (abox-version abox))


(defun full-reset ()
  (reset-statistics)
  (delete-all-aboxes)
  (delete-all-tboxes))


;;;
;;; Dummies: 
;;;


(defmacro DEFINE-DATATYPE-PROPERTY (&rest args)
  (declare (ignorable args))
  t)


