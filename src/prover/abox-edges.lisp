;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;; 

(defpersistentclass abox-edge (substrate-edge abox-item)
  ((multiplicity :reader multiplicity :initarg :multiplicity :initform 1)
   (inverse-multiplicity :reader inverse-multiplicity :initarg :inverse-multiplicity :initform 1)))


(defmethod copy-edge ((abox abox) (edge abox-edge) &rest args)
  (declare (ignorable args))
  (let ((copy (call-next-method))
        (state (get-state-vector edge)))

    (set-state-vector copy state)

    (dolist (slot '(old-p
                    active-p
                    deleted-p
                    
                    choice-points
                    ;;; mit denen kann man zwar nicht weiterrechnen (da Referenzen auf
                    ;;; alte Objekte enthalten!), aber gut fuer Inspektionszwecke!
                    precondition-for-actions
                    postcondition-for-actions
                    
                    created-by))
                    
      (setf (slot-value copy slot) (copy-tree (slot-value edge slot))))

    copy))

;;;
;;;
;;;

(defmethod inverse-edge ((edge abox-edge))
  nil)

(defmethod role ((edge abox-edge))
  (textual-description (description edge)))

;;;
;;; Unter der Annahme, dass in einer einfach ABox niemals 
;;; inverse Kanten explizit verzeichnet werden! 
;;; Dafür ist die "rolebox-abox" zuständig
;;; 

(defmethod implies-p ((edge abox-edge) (role role) &rest args)
  (implies-p (description edge) role))

;;;
;;;
;;;

(defmacro loop-over-role-successors ((node role) (succ edge-var) &body body)
  (let ((var (gensym)))
    (if (not role) 
        `(let ((,var nil))

           ;;; Keine Rolle ->
           ;;; enummeriert dann einfach die *ausgehenden* Kanten!
           ;;; incomming werden nicht betrachtet!
           ;;; unproblemeatisch; wird sowieso nur fuer den Aufbau
           ;;; von Modellen im Model Caching verwendet, und das
           ;;; funktioniert sowieso nicht fuer inverse Rollen

           (loop-over-cluster-nodes (equi ,node)
                                    (unless (deleted-p equi)
                                      (dolist (,edge-var (outgoing equi))
                 
                                        (when (and (not (deleted-p ,edge-var))

                                                   ;;; diese Funktionen werden (role = NIL!)
                                                   ;; NUR zu "low level" Inspektion des Tableaus
                                                   ;; verwendet! daher: Kanten muessen selbst
                                                   ;; interpretiert werden ! 
                                                   ;;;(not (zerop (multiplicity ,edge-var))))
                            
                                                   (let ((,succ (representative 
                                                                 (to ,edge-var))))
                                                     (when (and (not (deleted-p ,succ)))
                                                       (push ,succ ,var)
                                                       ,@body))))))))
      
      `(block loop

         (with-new-marking-context              
         
          (let ((role ,role)
                (node ,node)
                (,var nil))

            (declare (ignorable node role ,var))

            (unwind-protect
       
                (typecase role
                  (simple-role
              
                   (loop-over-cluster-nodes (equi ,node)
                                            (unless (deleted-p equi)
                                              (dolist (,edge-var (outgoing equi))
                                                (when (and (not (deleted-p ,edge-var))
                                                           (not (zerop (multiplicity ,edge-var))))
                                                  (when (implies-p ,edge-var
                                                                   role)
                                                    (let ((,succ (representative 
                                                                  (to ,edge-var))))
                                                      (when (and (and (not (marked-p ,succ))
                                                                      (not (deleted-p ,succ))))
                                                        (mark ,succ)
                                                        (push ,succ ,var)
                                                        ,@body)))))))
                    
                   (let ((inv-role (get-inverse-role role)))
                     (loop-over-cluster-nodes (equi ,node)
                                              (unless (deleted-p equi)
                                                (dolist (,edge-var (incoming equi))
                                                  (when (and (not (deleted-p ,edge-var))
                                                             (not (zerop (inverse-multiplicity ,edge-var))))
                                                    (when (implies-p ,edge-var
                                                                     inv-role)
                                                      (let ((,succ (representative 
                                                                    (from ,edge-var))))
                                                        (when (and (and (not (marked-p ,succ))
                                                                        (not (deleted-p ,succ))))
                                                          (mark ,succ)
                                                          (push ,succ ,var)
                                                          ,@body))))))))

                   (loop-over-cluster-nodes (equi ,node)
                                            (dolist (,succ (compute-virtual-successors (in-graph ,node)
                                                                                       equi role))
                                              (when (and (and (not (marked-p ,succ))
                                                              (not (deleted-p ,succ))))
                                                (mark ,succ)
                                                (push ,succ ,var)
                                                (let ((,edge-var 
                                                       (materialize-virtual-edge (in-graph ,node) 
                                                                                 ,node ,succ ,role)))
                                                  (declare (ignorable ,edge-var))
                                                  ,@body)))))
                    
                  ((or and-role or-role)
                   (break "support for complex roles: to be implemented!")))

              (unmark ,var))))))))
    
(defmacro loop-over-role-predecessors ((node role) (pre edge-var) &body body)
  (if (not role) 
      (let ((var (gensym)))
        `(let ((,var nil))

           ;;; Keine Rolle ->
           ;;; enummeriert dann einfach die *eingehenden* Kanten!
           ;;; s. auch "loop-over-role-successors", role = NIL-Kommentar! 
           
           (loop-over-cluster-nodes (equi ,node)
                                    (unless (deleted-p equi)
                                      (dolist (,edge-var (incoming equi))
                                        (when (and (not (deleted-p ,edge-var))
                                                   ;; s. Kommentar loop-over-role-successors
                                                   ;; (not (zerop (inverse-multiplicity ,edge-var)))
                                                   )
                                          (let ((,pre (representative 
                                                       (from ,edge-var))))
                                            (unless (deleted-p ,pre)
                                              (push ,pre ,var)
                                              ,@body))))))))
    
    `(loop-over-role-successors (,node (get-inverse-role ,role)) (,pre ,edge-var)
                                ,@body)))

;;;
;;;
;;;
;;;

(defmacro get-role-successors (node role)
  `(let ((res nil))
     (loop-over-role-successors (,node ,role) (succ edge-var)
                                (push succ res))
     res))

(defmacro get-role-predecessors (node role)
  `(let ((res nil))
     (loop-over-role-predecessors (,node ,role) (pre edge-var)
                                  (push pre res))
     res))

