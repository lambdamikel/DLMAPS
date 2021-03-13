;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;;
;;;

(defgeneric get-support (edge &key &allow-other-keys))

(defgeneric apply-rolebox-axioms (rolebox-substrate &key &allow-other-keys))

;;;
;;; Das Rolebox-Substrate bietet die Möglichkeit, ein Substrate 
;;; mit einer Rolebox zu assoziieren. Da in der Rolebox Rollenaxiome, 
;;; inverse und reflexive Rollen deklariert werden, verwaltet das 
;;; Substrate automatisch die Inversen und Reflexiven.
;;; 

(defpersistentclass rolebox-edge-description (simple-edge-description))


(defmethod implies-p ((descr1 rolebox-edge-description) (descr2 rolebox-edge-description) &rest args)
  (subsetp (ensure-list (textual-description descr1))
           (ensure-list (textual-description descr2))))

;;;
;;;
;;;

(defmethod eq-description-p ((description rolebox-edge-description))
  (equal '(eq) (textual-description description)))

;;;
;;;
;;;

(defmethod copy ((description rolebox-edge-description) &rest args)
  (make-edge-description (constructor-sym description) (textual-description description)))

;;;
;;;
;;;

(defpersistentclass rolebox-node-description (simple-node-description) )

(defmethod copy ((description rolebox-node-description) &rest args)
  (make-edge-description (constructor-sym description) (textual-description description)))

;;;
;;;
;;;

(defpersistentclass rolebox-substrate (substrate) 
  ((rbox :initarg :rbox :reader substrate-rbox :initform nil)))

(defmethod initialize-instance :after ((substrate rolebox-substrate) &rest initargs &key (rbox *cur-rbox*))
  (setf (slot-value substrate 'rbox) 
        (find-rbox rbox)))

;;;
;;;
;;;

(defpersistentclass rolebox-substrate-node (substrate-node))

(defun rolebox-substrate-node-p (node)
  (typep node 'rolebox-substrate-node))

;;;
;;;

(defpersistentclass rolebox-substrate-edge (substrate-edge)
  ((inverse-edge :reader inverse-edge :initform nil)
   (triangles :reader triangles :initform nil)
   (referenced-by :reader referenced-by :initform nil)))

(defun rolebox-substrate-edge-p (edge)
  (typep edge 'rolebox-substrate-edge))

;;;
;;;
;;:


(defmethod establish-context-for ((substrate rolebox-substrate) continuation)
  (let ((*cur-rbox* 
         (substrate-rbox substrate)))
    (if (next-method-p)
        (call-next-method)
      (funcall continuation))))

(defmethod set-context-for progn ((substrate rolebox-substrate))
  (setf *cur-rbox* 
        (substrate-rbox substrate)))

;;;
;;;
;;;

(defmethod get-standard-node-description-class ((substrate rolebox-substrate))
  'rolebox-node-description)

(defmethod get-standard-edge-description-class ((substrate rolebox-substrate))
  'rolebox-edge-description)

(defmethod get-standard-edge-class ((substrate rolebox-substrate))
  'rolebox-substrate-edge)

(defmethod get-standard-node-class ((substrate rolebox-substrate))
  'rolebox-substrate-node)

;;;
;;;
;;:

(defmethod create-node ((substrate rolebox-substrate) (name symbol) (description node-description)
                        &rest args)
  (apply #'make-node substrate 
         :name name
         :description description
         args))

(defmethod make-node ((substrate rolebox-substrate) 
                      &rest initargs 
                      &key copy-p
                      (create-reflexive-edges-p t)
                      (edge-constructor #'(lambda (node ref-role)
                                            (create-edge substrate
                                                         node node
                                                         (make-edge-description 
                                                          (get-standard-edge-description-class substrate)
                                                          ref-role)
                                                         :create-inverse-p nil
                                                         :error-p nil)))
                      &allow-other-keys)
  
  (let ((node (apply #'call-next-method substrate initargs)))
    (when node 
      (unless copy-p
        (with-slots (rbox) substrate
          (when (and rbox create-reflexive-edges-p)
            (dolist (ref-role (reflexive-roles rbox))
              (funcall edge-constructor node ref-role))))))
    node))

;;;
;;;
;;;

(defmethod create-edge ((substrate rolebox-substrate) 
                        (from substrate-node) (to substrate-node)
                        (description rolebox-edge-description)
                        &rest args)
  (apply #'make-edge substrate from to 
         :description description 
         args))

(defmethod make-edge ((substrate rolebox-substrate) (from rolebox-substrate-node) (to rolebox-substrate-node) 
                      &rest args
                      &key
                      description
                      (create-inverse-p t) 
                      copy-p 
                      &allow-other-keys)
  (let ((edge (apply #'call-next-method substrate from to args)))
    (when (and (not copy-p)
               edge
               create-inverse-p)
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
                  (setf inverse-edge iedge))))))))
    edge))

;;;
;;;
;;;

(defmethod compute-triangle-index ((object rolebox-substrate-edge))
  (with-slots (triangles in-graph from to) object
    (loop-over-nodes (between in-graph)
      (let ((from-between
             (get-edges-between in-graph from between))
            (between-to
             (get-edges-between in-graph between to)))
        (when (and from-between between-to)
          (loop as fb in from-between do
                (loop as bt in between-to do
                      (push (list fb bt) triangles)
                      (push object (slot-value fb 'referenced-by))
                      (push object (slot-value bt 'referenced-by))))))))
  object)

(defmethod compute-triangle-index ((substrate rolebox-substrate))
  (loop-over-edges (edge substrate)
    (compute-triangle-index edge))
  substrate)

(defmethod get-support ((edge rolebox-substrate-edge) &key &allow-other-keys)
  (or (triangles edge)
      (progn 
        (compute-triangle-index edge)
        (triangles edge))))

;;;
;;;
;;;

(defmethod delete-edge ((substrate rolebox-substrate) (edge rolebox-substrate-edge)
                        &key (delete-inverse-p t) (error-p t)  &allow-other-keys)  
  (call-next-method)
  
  (dolist (referencing-edge (referenced-by edge))
    (with-slots (triangles) referencing-edge
      (setf triangles nil)))
    
  (with-slots (from to inverse-edge) edge
    (when (and inverse-edge delete-inverse-p)
      (let ((inv-edge inverse-edge))
        (setf inverse-edge nil)
        (with-slots (inverse-edge) inv-edge ; verhindert Endlos-Schleife! 
          (setf inverse-edge nil))
        (delete-edge substrate inv-edge :error-p error-p)))))

;;;
;;;
;;;

(defmethod copy ((substrate rolebox-substrate) &rest args
                 &key
                 (rbox (substrate-rbox substrate))
                 &allow-other-keys) 
  (let ((copy (apply #'call-next-method substrate :rbox rbox args)))
    (when copy 
      (dolist (old-edge (get-edges substrate))
        (when (inverse-edge old-edge)
          (let ((new-edge (find-edge copy (id old-edge)))
                (new-inv-edge (find-edge copy (id (inverse-edge old-edge)))))
            (with-slots (inverse-edge) new-edge
              (setf inverse-edge new-inv-edge))))))
    copy))

;;;
;;;
;;;

(defmethod make-inverse-description ((rbox rolebox) (description rolebox-edge-description))
  (let ((inv-role (inv-role rbox (textual-description description))))
    (when inv-role 
      (make-edge-description (constructor-sym description) inv-role))))
                         
(defmethod lookup ((box rolebox) (r rolebox-edge-description) (s rolebox-edge-description))
  (lookup box (textual-description r) (textual-description s)))

(defmethod lookup ((box rolebox) (r rolebox-substrate-edge) (s rolebox-substrate-edge))
  (lookup box (description r) (description s)))
  
;;;
;;;
;;;

(defmethod apply-rolebox-axioms ((substrate rolebox-substrate) &key 
                                 (edges (get-edges substrate))
                                 (nodes (get-nodes substrate))                                 
                                 (edge-constructor 
                                  #'(lambda (from to comp &key support)
                                      (declare (ignore support))
                                      (create-edge substrate 
                                                   from to 
                                                   (make-edge-description
                                                    (get-standard-edge-description-class substrate) 
                                                    comp))))
                                 &allow-other-keys)
  (let ((iter t)
        (dummy-description
         (make-edge-description (get-standard-edge-description-class substrate) 'dummy)))
    (with-marked-objects (nodes)
      (loop while iter do
            (setf iter nil)          
            (dolist (r edges)
              (let* ((from (from r))
                     (over (to r))
                     (s (outgoing over)))
                (when (and (marked-p from)
                           (marked-p over))
                  (dolist (s s)
                    (when (rolebox-substrate-edge-p s)
                      (let ((to (to s)))
                        (when (marked-p to)

                          (let* ((comp
                                  (lookup (substrate-rbox substrate) r s))
                                 (comp
                                  (if (consp comp)
                                      (if (cdr comp)
                                          comp
                                        (first comp))
                                    comp)))
                                       
                            (unless (eq (to r) (from s)) 
                              (error "Bad triangle found!"))
                        
                            (when comp
                              (change-textual-description dummy-description comp)
                          
                              (let ((edge (=>-edge-present-p substrate from to dummy-description)))
                            
                                (unless edge 
                          
                                  (funcall edge-constructor from to comp :support (list r s))
                                  (setf iter t))))))))))))))
    substrate))

;;;
;;;
;;;                    

(defmethod consistent-p ((substrate rolebox-substrate) &key 
                         (nodes (get-nodes substrate))
                         (edges (get-edges substrate))
                         (check-node-labels-p t)
                         (check-edge-labels-p t)
                         
                         aux-description-p
                         check-loops-p 
                         check-inverses-p)
  (let ((dummy-description 
         (make-edge-description (get-standard-edge-description-class substrate) 'dummy)))
    
    ;;; T, iff reflexive, inverse, and role composition frame conditions are EXPLICITLY satisfied! 

    (and (=> check-node-labels-p (every #'consistent-p (get-nodes substrate)))
         (=> check-edge-labels-p (every #'consistent-p (get-edges substrate)) )

         (with-marked-objects (nodes)    
           (with-marked-objects (edges nil)    
             (and 
              (=> check-loops-p 
                  (every #'(lambda (ref-role)
                             (change-textual-description dummy-description ref-role)
                             (every #'(lambda (node) 
                                        (=>-edge-present-p substrate node node dummy-description))
                                    nodes))
                         (reflexive-roles (substrate-rbox substrate))))

              (=> check-inverses-p 
                  (every #'(lambda (edge)
                             (let ((inv-descr (inv-role (substrate-rbox substrate) 
                                                        (textual-description (description edge)))))
                               (=> inv-descr
                                   (progn 
                                     (change-textual-description dummy-description inv-descr)
                                     (=>-edge-present-p substrate (to edge) (from edge) dummy-description)))))
                         edges))

              (dolist (r edges t)            
                (let* ((from (from r))
                       (over (to r))
                       (s (outgoing over)))
                  (dolist (s s)
                    (when (and (rolebox-substrate-edge-p s)
                               (marked-p s))
                      (let ((to (to s)))
                        (when (marked-p to)
                          (let ((comp (if aux-description-p 
                                          (lookup (substrate-rbox substrate)
                                                  (aux-description r)
                                                  (aux-description s))
                                        (lookup (substrate-rbox substrate)
                                                (description r)
                                                (description s)))))
                            (unless (eq (to r) (from s)) 
                              (error "Bad triangle found!"))
                            (when comp
                              (change-textual-description dummy-description comp)
                              (unless (=>-edge-present-p substrate from to dummy-description)
                                (return-from consistent-p nil))))))))))))))))

(defmethod aux-description-consistent-p ((substrate rolebox-substrate) processed-edges)
  (declare (ignore processed-edges))
  (consistent-p substrate :aux-description-p t))

;;;
;;;
;;;

(defmethod enumerate-atomic-configurations ((substrate rolebox-substrate) 
                                            &rest args
                                            &key
                                            (edge-setter
                                             #'(lambda (edge sym &key respect-inverses-p &allow-other-keys)
                                                 (change-textual-description (description edge) sym)
                                                 (when (and (inverse-edge edge) respect-inverses-p)
                                                   (change-textual-description 
                                                    (description (inverse-edge edge))
                                                    (inv-role (substrate-rbox substrate) sym)))))
                                            
                                            (respect-inverses-p t)

                                            (edges 
                                             (if *non-determinism-p*
                                                 (reorder (get-underspecified-edges
                                                           substrate 
                                                           :exclude-inverses-p respect-inverses-p))
                                               (get-underspecified-edges 
                                                substrate
                                                :exclude-inverses-p respect-inverses-p)))

                                            (manager-fn #'(lambda (sel-edge edges processed-edges
                                                                            &key respect-inverses-p &allow-other-keys)
                                                            (values (let ((edges (remove sel-edge edges)))
                                                                      (if (and respect-inverses-p 
                                                                               (inverse-edge sel-edge))
                                                                          (remove (inverse-edge sel-edge) edges)
                                                                        edges))
                                                                    (if (and respect-inverses-p
                                                                             (inverse-edge sel-edge))
                                                                        (cons sel-edge
                                                                              (cons (inverse-edge sel-edge)
                                                                                    processed-edges))
                                                                      (cons sel-edge
                                                                            processed-edges)))))

                                            (final-check-fn #'(lambda (substrate &rest args)
                                                                (apply #'consistent-p substrate args)))

                                            &allow-other-keys)

  (apply #'call-next-method substrate 
         :edge-setter edge-setter 
         :respect-inverses-p respect-inverses-p 
         :edges edges
         :manager-fn manager-fn
         :final-check-fn final-check-fn
         args))

;;;
;;;
;;;

#|

(with-rbox (test-box :delete-if-exists-p t
                     :roles (r s t)
                     :reflexive-roles (t) 
                     :inverse-roles ((r s) (s r))
                     :axioms                     
                     ((r r s)
                      (r s s)))
  
  (with-substrate (hello :type 'rolebox-substrate :delete-if-exists-p t)
    
    (node a (ncon a))
    (node b (ncon a))
    (node c (ncon c))
    (node d (ncon d))

    (edge a b (econ r))
    (edge b c (econ r))
    (edge c d (econ r))

    
    (visualize (list *cur-substrate*
                     (apply-rolebox-axioms (copy *cur-substrate*))))))




(with-rbox (test-box :delete-if-exists-p t
                     :roles (r s)
                     ;:reflexive-roles (r) 
                     :inverse-roles ((r s) (s r)))
  
  (with-substrate (hello :type 'rolebox-substrate :delete-if-exists-p t)
    
    (node a (ncon a-descr))
    (node b (ncon b-descr))

    (edge a b (econ (r s)))
    
    (visualize (cons (copy *cur-substrate*)
                     (get-all-atomic-configurations (apply-rolebox-axioms *cur-substrate*))))))



(with-rbox (test-box  :delete-if-exists-p t
                      :roles (r s t)
                      :inverse-roles ((s r))
                      :reflexive-roles (t)
                      :axioms 
                      ((r r (s r t))))

  
  (with-substrate (hello :type 'rolebox-substrate :delete-if-exists-p t)
  
    (node a (ncon a-descr))
    (node b (ncon b-descr))
    (node c (ncon c-descr))

    (edge a b (econ r))
    (edge b c (econ r))
    
    (apply-rolebox-axioms *cur-substrate*)
    (terpri)
    (princ (consistent-p *cur-substrate*))

    
    (visualize 
     (let ((copy (copy *cur-substrate*)))
       (cons copy                         
             (get-all-atomic-configurations (apply-rolebox-axioms *cur-substrate*)))))))



(with-rbox (rcc5-rolebox)
  
  (with-substrate (hello :type 'rolebox-substrate :delete-if-exists-p t)
    (setf *x* *cur-substrate*)
    (node a (ncon a-descr))
    (node b (ncon b-descr))
    (node c (ncon c-descr))
    (node d (ncon d-descr))

    (edge a b (econ ppi))
    (edge b c (econ dr))
    (edge c d (econ po))

    (princ (consistent-p *cur-substrate*))
    (apply-rolebox-axioms *cur-substrate*)
    (princ (consistent-p *cur-substrate*))
    
    (let ((all
           (cons (copy *cur-substrate*)
                 (get-all-atomic-configurations *cur-substrate*))))
      (princ (mapcar #'consistent-p all))
      (visualize (remove-if-not #'consistent-p all)))))



(with-rbox (test-box  :delete-if-exists-p t
                      :roles (r s t)
                      :inverse-roles ((s r))
                      :reflexive-roles (t)
                      :axioms 
                      ((r r (s r t))))

  
  (with-substrate (hello :type 'rolebox-substrate :delete-if-exists-p t)
  
    (node a (ncon a-descr))
    (node b (ncon b-descr))
    (node c (ncon c-descr))
    (node d (ncon c-descr))

    (setf ab (edge a b (econ r)))
    (setf bc (edge b c (econ r)))
    (princ (consistent-p *cur-substrate*)) (terpri)
    (princ (consistent-p *cur-substrate* :edges (list ab)))

    
    (apply-rolebox-axioms *cur-substrate*)

    (dolist (edge (get-edges-between *cur-substrate* 'd 'd))
      (delete-edge *cur-substrate* edge))

    (terpri) (princ "Consistent: ")
    (princ (consistent-p *cur-substrate* :check-loops-p t))

    (visualize *cur-substrate*)))



(with-rbox (rcc5-rolebox)
  
  (with-substrate (hello :type 'rolebox-substrate :delete-if-exists-p t)
  
    (setf a (node a (ncon a-descr)))
    (setf b (node b (ncon b-descr)))
    (setf c (node c (ncon c-descr)))
    (node d (ncon c-descr))

    (setf ab (edge a b (econ po)))
    (setf bc (edge b c (econ po)))
    (edge c d (econ po))
        
    (apply-rolebox-axioms *cur-substrate* :edges (list ab bc) :nodes (list a b c))

    (visualize *cur-substrate*)))



(with-rbox (rcc5-rolebox)
  
  (with-substrate (hello :type 'rolebox-substrate :delete-if-exists-p t)
  
    (setf a (node a (ncon a-descr)))
    (setf b (node b (ncon b-descr)))
    (setf c (node c (ncon c-descr)))

    (edge a b (econ pp))
    (edge b c (econ pp))
    
    (edge a c (econ dr))

    (visualize *cur-substrate*)
        
    (princ (consistent-p *cur-substrate*))))
    

(with-substrate (hello :type 'rolebox-substrate :delete-if-exists-p t
                       :rbox 'rcc5-rolebox)
  
  (setf a (node a (ncon a-descr)))
  (setf b (node b (ncon b-descr)))
  (setf c (node c (ncon c-descr)))

  (edge a b (econ pp))
  (edge b c (econ pp))
   
  (edge a c (econ pp))

  (visualize *cur-substrate*)
        
  (princ (consistent-p *cur-substrate*)))

|#

