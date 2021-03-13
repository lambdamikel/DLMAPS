;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;;
;;;

(defgeneric compute-minimal-edge-label (rolebox-edge &key &allow-other-keys))

(defgeneric label-minimal-p (jepd-substrate &key &allow-other-keys))

(defgeneric compute-minimal-label (jepd-substrate &key &allow-other-keys))

(defgeneric add-missing-edges (jepd-substrate &key nodes &allow-other-keys))

;;;
;;;
;;;

(defpersistentclass jepd-node-description (rolebox-node-description))

(defpersistentclass jepd-edge-description (rolebox-edge-description))

(defpersistentclass jepd-substrate (rolebox-substrate))

(defpersistentclass jepd-substrate-node (rolebox-substrate-node))

(defpersistentclass jepd-substrate-edge (rolebox-substrate-edge))


(defun jepd-substrate-edge-p (edge)
  (typep edge 'jepd-substrate-edge))

(defun jepd-substrate-node-p (edge)
  (typep edge 'jepd-substrate-node))

;;;
;;;
;;;

(defmethod get-standard-node-description-class ((substrate jepd-substrate))
  'jepd-node-description)

(defmethod get-standard-edge-description-class ((substrate jepd-substrate))
  'jepd-edge-description)

(defmethod get-standard-edge-class ((substrate jepd-substrate))
  'jepd-substrate-edge)

(defmethod get-standard-node-class ((substrate jepd-substrate))
  'jepd-substrate-node)

;;;
;;;
;;;
;;;
;;;
;;;

(defmethod initialize-instance :after ((substrate jepd-substrate) &rest initargs &key (rbox *cur-rbox*))
  (unless (typep (find-rbox rbox) 'jepd-rolebox)
    (error "Give me a JEPD-rolebox!")))

;;;
;;;
;;;

(defmethod create-node ((substrate jepd-substrate) (name symbol) (description jepd-node-description) 
                        &rest args)
  (apply #'make-node substrate :name name :description description args))

(defmethod make-node ((substrate jepd-substrate) &rest args)
  (apply #'call-next-method substrate args))

(defmethod create-edge ((substrate jepd-substrate) (from jepd-substrate-node) (to jepd-substrate-node)
                        (description jepd-edge-description) 
                        &rest args)
  (apply #'make-edge substrate from to 
         :description description 
         args))

(defmethod make-edge ((substrate jepd-substrate) (from jepd-substrate-node) (to jepd-substrate-node)
                      &rest args
                      &key
                      (error-p t)
                      description
                      (create-inverse-p t) 
                      copy-p 
                      &allow-other-keys)
  (let ((edges (get-edges-between substrate from to :error-p nil)))
    
    (when (and error-p 
               edges)
      (error "~A is a JEPD substrate! Edge between ~A and ~A already exists!" substrate from to))

    (let ((edge (apply #'call-next-method substrate from to 
                       :description description
                       :create-inverse-p nil
                       args)))
        
      (when (and (not copy-p)
                 create-inverse-p)
        (cond ((eq from to)
               ;;; nur beim JEPD-Substrate kann ich sicher
               ;;; sein, dass die Inverse die Kante selbst ist!
               ;;; ansonsten können nämlich die Beschreibungen
               ;;; an den Kanten differieren, bzw. werden
               ;;; gebraucht, um eine Inkonsistenz festzustellen
               ;;; z.B. (1,1) : PP -> (1,1) : PPI 
               (with-slots (inverse-edge) edge
                 (setf inverse-edge edge)))
              (t
               (with-slots (rbox) substrate
                 (when rbox 
                   (let* ((inv-description 
                           (make-inverse-description rbox description)))
                     (when inv-description
                       (let* ((iedge (apply #'make-edge substrate to from  
                                            :description inv-description
                                            :create-inverse-p nil
                                            args)))
                         (with-slots (inverse-edge) iedge
                           (setf inverse-edge edge))
                         (with-slots (inverse-edge) edge
                           (setf inverse-edge iedge))))))))))
      edge)))

;;;
;;;
;;;

(defmethod delete-edge ((substrate jepd-substrate) (edge jepd-substrate-edge)
                        &rest args
                        &key (delete-inverse-p t) (error-p t) &allow-other-keys)   
  (apply #'call-next-method substrate edge 
         :delete-inverse-p (and delete-inverse-p 
                                (not (reflexive-edge-p edge)))
         :error-p error-p 
         args))

;;;
;;;
;;;

(defmethod get-implied-label ((edge jepd-substrate-edge) &key (edges (get-edges (in-graph edge))) &allow-other-keys)
  (with-slots (rbox) (in-graph edge)
    (with-marked-objects (edges)
      (let ((comps nil)
            (support (get-support edge)))
        (dolist (supp support)
          (let ((r (first supp))
                (s (second supp)))
            (when (and (marked-p r)
                       (marked-p s))
              (push (lookup rbox r s) comps))))
        ;;; make-and-description liefert nil, wenn equivalent zu *bottom* 
        (let* ((descriptions (cons (description edge) 
                                   (mapcar #'(lambda (comp)
                                               (change-textual-description (copy (description edge)) comp))
                                           comps)))
               (label 
                (make-and-description (first descriptions)
                                      :descriptions (rest descriptions))))
                                            
          (if (inconsistent-p label)
              (values nil support)
            label))))))

(defmethod consistent-p ((substrate jepd-substrate) &key
                         (check-node-labels-p t)
                         (check-edge-labels-p t)
                         (edges (get-edges substrate)) strict-p &allow-other-keys)
  (and 
   (=> check-node-labels-p (every #'consistent-p (get-nodes substrate)))
   (=> check-edge-labels-p (every #'consistent-p (get-edges substrate)) )
   (=> strict-p 
       ;;; zw. jedem Paar Knoten genau eine Kante? 
       (every #'(lambda (i)
                  (every #'(lambda (j)
                             (let ((e (get-edges-between substrate i j)))
                               (and e
                                    (not (cdr e)))))
                         (get-nodes substrate)))
              (get-nodes substrate)))
   (every #'get-implied-label edges)))



(defmethod compute-minimal-label ((substrate jepd-substrate) &key (edges (get-edges substrate)) &allow-other-keys)
  (let ((change t))
    (loop while change do 
          (setf change nil)
          (dolist (edge edges)
            (multiple-value-bind (new-label support)
                (get-implied-label edge :edges edges)
              (if (not new-label)
                  (return-from compute-minimal-label 
                    (values nil support))
                (setf change (not (implies-p (description edge) new-label))
                      (description edge) new-label)))))
    substrate))

(defmethod add-missing-edges ((substrate jepd-substrate)
                              &key
                              (edge-constructor
                               #'(lambda (from to)
                                   (create-edge substrate 
                                                from to
                                                (make-edge-description 
                                                 (get-standard-edge-description-class substrate)
                                                 (full-disjunctive-role (substrate-rbox substrate)))
                                                :create-inverse-p t)))
			      &allow-other-keys)
  (dolist (from-to-pair (get-missing-edges substrate :loops-p nil :inverses-p nil))
    (apply edge-constructor from-to-pair))
  substrate)

(defmethod apply-rolebox-axioms ((substrate jepd-substrate) &rest args &key 
                                 &allow-other-keys)
  (apply #'add-missing-edges substrate args)
  (compute-minimal-label substrate))
  

;;;
;;;
;;;

(defmethod eq-edge-p ((edge jepd-substrate-edge))
  (equal '(eq) (textual-description (description edge))))

(defmethod eq-collapse ((substrate jepd-substrate))
  (let ((eq-edges         
         (remove-if #'reflexive-edge-p 
                    (remove-if-not #'eq-edge-p
                                   (get-edges substrate)))))
    (loop while eq-edges do
          (let ((edge (first eq-edges)))
            (setf eq-edges (delete edge eq-edges))
            (setf eq-edges (delete (inverse-edge edge) eq-edges))
            
            (join-nodes (from edge) (to edge))))
    substrate))


(defmethod join-nodes ((a jepd-substrate-node) (b jepd-substrate-node))
  (unless (eq (in-graph a) (in-graph b))
    (error "Nodes are not in same graph!"))
  (let ((description (make-and-description (description a) 
                                           :descriptions (list (description b))))
        (graph (in-graph a)))
    (delete-node graph b)
    (change-description a description)))


;;;
;;; Prädikate
;;;


(defmethod label-minimal-p ((substrate jepd-substrate) &key &allow-other-keys)
  (every #'(lambda (edge)
             (let ((impl (get-implied-label edge)))
               (and impl
                    (equivalent-p impl (description edge)))))
         (get-edges substrate)))

#|

(with-rbox (test-box :type jepd-rolebox :roles (r eq)
                     :delete-if-exists-p t
                     :inverse-roles ((r r))
                     :delete-if-exists-p t
                     ;:reflexive-roles (eq)
                     :axioms   
                     ((r r r)
                      (r eq r)  
                      (eq eq eq)
                      (eq r r)))
  
  (with-substrate (hello :type 'jepd-substrate :delete-if-exists-p t)

    (setf *x* *cur-substrate*)
    (node a (ncon a-descr))
    (node b (ncon b-descr))
    (node c (ncon c-descr))

    (edge a b (econ r))
    (edge b c (econ r))
    ;(edge a c (econ r))
    
    
    (princ (label-minimal-p *cur-substrate*))
    (terpri)

    ;(add-missing-edges *cur-substrate*)    
    ;; (princ (label-minimal-p *cur-substrate*))

    
    ;(compute-minimal-label *cur-substrate*)
    
    ;(princ (label-minimal-p *cur-substrate*))

    (apply-rolebox-axioms *cur-substrate*)

    (visualize *cur-substrate*)))



(with-rbox (rcc5-rolebox)
  
  (with-substrate (test :type 'jepd-substrate :delete-if-exists-p t)

    (setf *x* *cur-substrate*)
    
    (node a (ncon a))
    (node b (ncon b))
    (node c (ncon c))
    
    (edge a b (econ pp))
    (edge b c (econ pp))    

    (apply-rolebox-axioms *cur-substrate*)
    (add-missing-edges *x*)
    (compute-minimal-label *x*)

    (visualize *x*)))
    
    


(with-rbox (rcc5-rolebox)  
  (with-substrate (hello :type 'jepd-substrate :delete-if-exists-p t)
    (setf *x* *cur-substrate*)
    (node n0 (ncon 0))
    (node n1 (ncon 1))
    (node n2 (ncon 2))
    (node n3 (ncon 3))

    (edge n0 n1 (econ ppi))
    (edge n0 n2 (econ ppi))
    (edge n1 n3 (econ ppi))
    (edge n1 n2 (econ ppi))
    (edge n2 n3 (econ pp))
    

    (apply-rolebox-axioms *cur-substrate*)
    (break "~A" (consistent-p *cur-substrate*))

    (visualize *cur-substrate*)))




(with-rbox (rcc5-rolebox)

  
  (with-substrate (hello :type 'jepd-substrate :delete-if-exists-p t)
  
    (node a (ncon a-descr))
    (node b (ncon b-descr))
    (node c (ncon c-descr))
    (setf *x* *cur-substrate*)
    (edge a b (econ pp))
    (edge b c (econ pp))

    (add-missing-edges *cur-substrate*)
    
    (compute-minimal-label *cur-substrate*)

    (visualize *cur-substrate*)))



(with-rbox (rcc5-rolebox)
  (with-substrate (hello :type 'jepd-substrate :delete-if-exists-p t)
  
    (node a (ncon a-descr))
    (node b (ncon b-descr))
    (node c (ncon c-descr))

    (setf *x* *cur-substrate*)
    
    (edge a b (econ pp))
    
    (edge a c (econ (pp dr)))

    (edge b c (econ pp))

    (compute-minimal-label *x*)

    (visualize *x*)))




(with-rbox (rcc8-rolebox)
  (with-substrate (hello :type 'jepd-substrate :delete-if-exists-p t)
  
    (node u (ncon c-descr))
    
    (node y (ncon b-descr))
    (node z (ncon c-descr))

    (setf *x* *cur-substrate*)
    
    
    (edge y z (econ ec))
    (edge y u (econ po))

    (edge z u (econ (tpp ntpp)))
    
    
    (add-missing-edges *x*)
    ;(scramble *x*)
    (compute-minimal-label *x*)
    (princ (consistent-p *x* :strict-p t))
    
    (visualize *x*)))





(with-rbox (rcc8-rolebox)
  (with-substrate (hello :type 'jepd-substrate :delete-if-exists-p t)
  
    (node a (ncon a))
    (node b (ncon b))
    (node c (ncon c))
    (node d (ncon d))
    (node e (ncon e))
    (node f (ncon f))
    (node g (ncon g))

    (setf *x* *cur-substrate*)
    
    (edge a b (econ tpp))
    (edge a c (econ tpp))

    (edge c d (econ tpp))
    (edge b d (econ tpp))

    (edge a d (econ ntpp))
    (edge d f (econ ntpp))
    

    (edge d e (econ tpp))
    (edge d g (econ tpp))
    (edge e f (econ tpp))

    (edge g f (econ tpp))

    (edge c b (econ po))
    (edge g e (econ po))

    

    (edge b e (econ ntpp))
    (edge c g (econ ntpp))
    (edge a g (econ ntpp))
    (edge a e (econ ntpp))
    

    (add-missing-edges *x*)
    ;(scramble *x*)
    (compute-minimal-label *x*)
    
    (let ((completions 
           (get-all-atomic-configurations *x*
                                          :final-check-fn #'consistent-p
                                          :how-many 1000)))
    
      (visualize completions))))





(with-rbox (rcc8-rolebox)
  (with-substrate (hello :type 'jepd-substrate :delete-if-exists-p t)
  
    (node a (ncon a))
    (node b (ncon b))
    (node c (ncon c))
   
    (edge a b (econ eq))
    (edge a c (econ po))
    
    (setf *x* *cur-substrate*)
    (add-missing-edges *x*)
    (compute-minimal-label *x*)
    (visualize (list *x*
                     (eq-collapse (copy *x*))))))




(with-rbox (rcc5-rolebox)
  
  (with-substrate (test :type 'jepd-substrate :delete-if-exists-p t)

    (setf *x* *cur-substrate*)
    
    (node a (ncon a))
    (node b (ncon b))
    (node c (ncon c))
    (node d (ncon d))
    
    (edge a b (econ pp))
    (edge b c (econ pp))    

    (edge b d (econ dr))    

    ;(apply-rolebox-axioms *cur-substrate*)
    ;(add-missing-edges *x*)
    ;(compute-minimal-label *x*)

    (visualize (list *x*
                     (let ((x (copy *x*)))
                       (add-missing-edges x)
                       x)
                     (let ((x (copy *x*)))
                       (apply-rolebox-axioms x)
                       
                       x)))))


|#

