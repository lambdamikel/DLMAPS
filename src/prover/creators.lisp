;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;
;;;

(defmethod create-abox-node ((abox abox) name-for-node 
                             &rest args)
  (apply #'create-node
         abox name-for-node
         (make-node-description (get-standard-node-description-class abox) nil) 
         args))
                             

(defmethod create-node ((abox abox) (name-for-node symbol) (description node-label)
                        &rest args
                        &key 
                        depends-on created-by
                        (old-p t) 
                        &allow-other-keys)


  (let ((*start-time* (get-internal-run-time)))
                             
    (let* ((created-by (or created-by depends-on))
           (node (apply #'call-next-method
                        abox name-for-node description
                        :delete-if-exists-p nil
                        :old-p old-p
                        :created-by created-by
                        :edge-constructor #'(lambda (node ref-role)
                                              (create-edge abox
                                                           node node
                                                           (make-edge-description 
                                                            (get-standard-edge-description-class abox)
                                                            ref-role)
                                                           :created-by created-by
                                                           :create-inverse-p nil
                                                           :error-p nil))

                        args)))

      (setf (slot-value (description node) 'of-node) node
            (slot-value node 'cluster-nodes) (list node))
    
      (when created-by
        (register-action create created-by node))

      (incf *time-for-node-creation* 
            (- (get-internal-run-time)
               *start-time*))

      node)))

(defmethod register-node ((node abox-node))
  (let ((abox (in-graph node)))
    
    (with-slots (all-nodes old-nodes node-table) abox

      (when (is-abox1-p abox)

        (if (not *use-avl-trees-for-abox1-p*)
            (push node all-nodes)
          (insert-into-avl-tree node all-nodes :key #'id))

        (when (and *maintain-old-nodes-p* (old-p node))
          (push node old-nodes)))
      
      (activate-node abox node)
    
      (when (or (old-p node)
                (root-p node)
                (not (is-abox1-p abox)))
          
        (unless node-table
          (setf node-table
                (make-weak-hash-table :size 10 :rehash-size 100)))

        (call-next-method)))))


;;;
;;;
;;;

(defmethod create-edge ((abox abox) 
                        (from abox-node) (to abox-node)
                        (description edge-label)
                        &rest args
                        &key 
                        (old-p t) 
                        depends-on created-by
                        (new-choice-point 0)
			&allow-other-keys)

  (let ((*start-time* (get-internal-run-time)))
  
  
    (let ((created-by (or created-by depends-on))
          (edge (apply #'call-next-method abox from to
                       description 
                       :created-by created-by
                       :old-p old-p 
                       args)))

      (unless *dont-invalidate-store-p*
        (note-abox-has-changed abox)
        (reset-sat-status from)
        (reset-sat-status to)
      ;(reset-sat-status (concept-store (tbox abox)))
        )

      ;;;
      ;;; Adresse verwalten
      ;;;

      (incf (slot-value from 'succ-counter))

      (setf (slot-value to 'address)
            (cons (slot-value from 'succ-counter)
                  (slot-value from 'address)))

      (setf (slot-value to 'rev-address)
            (reverse (slot-value to 'address)))
      
      ;;;
      ;;;  Blaetter 
      ;;;
      
      (apply #'register-as-unexpanded edge 
             :new-choice-point new-choice-point 
             args)

      (when (inverse-edge edge)
        (apply #'register-as-unexpanded (inverse-edge edge) 
               :new-choice-point new-choice-point 
               args))

      (when created-by
        (let ((created-by 
               (if (eq created-by t)
                   nil
                 created-by)))

          (register-action create 
                           created-by
                           edge)

          (when (and (inverse-edge edge)
                     (not (eq (inverse-edge edge) edge)))

            (register-action create 
                             created-by
                             (inverse-edge edge)))))

      (incf *time-for-edge-creation* 
            (- (get-internal-run-time)
               *start-time*))

      edge)))





(defmethod register-edge ((edge abox-edge))
  (let ((abox (in-graph edge))
        (from (from edge))
        (to (to edge)))

    (with-slots (all-edges edge-table) abox


      (when (is-abox1-p abox)

        (unless (cdr (outgoing from))
          (setf (slot-value abox 'leaf-nodes)
                (delete from (slot-value abox 'leaf-nodes))))

        (if (not *use-avl-trees-for-abox1-p*)
            (push edge all-edges)
          (insert-into-avl-tree edge all-edges :key #'id)))

      (if (or (old-p edge)
              (not (is-abox1-p abox)))
          
          (progn 
            (unless edge-table
              (setf edge-table 
                    (make-weak-hash-table :size 10 :rehash-size 100 :test #'equal)))
            (call-next-method))
        (progn 
          (push to (slot-value from 'successors))
          (push from (slot-value to 'predecessors))
      
          (push edge (slot-value from 'outgoing))
          (push edge (slot-value to 'incoming)))))))

;;;
;;;
;;;

(defmethod relate ((from abox-node) (to abox-node) (description edge-label) &rest args)
  (apply #'create-edge *cur-abox* from to description args))


(defmethod relate ((from abox-node) (to abox-node) role &rest args 
                   &key description-type
                   &allow-other-keys)
  (let* ((descr (make-edge-description
                 (or description-type
                     (get-standard-edge-description-class *cur-abox*)) role))
         (role (textual-description descr)))

    (declare (ignorable role))

    (apply #'create-edge *cur-abox* from to descr args)))
      

(defmethod relate ((from symbol) (to symbol) role 
                   &rest args &key (create-nodes-p t) 
                   (node-type (get-standard-node-class *cur-abox*)) &allow-other-keys)
  (apply #'relate 
         (or (apply #'find-node *cur-abox* from :error-p nil args)
             (when create-nodes-p
               (apply #'create-abox-node *cur-abox* from :type node-type args)))
         (or (apply #'find-node *cur-abox* to :error-p nil args)
             (when create-nodes-p 
               (apply #'create-abox-node *cur-abox* to :type node-type args)))
         role 
         args))

;;;
;;;
;;;

(defmethod unrelate ((from abox-node) (to abox-node) role &rest args)

  (unless (eq (in-graph from)
              (in-graph to))
    (error "Individuals must be in same ABox!"))

  (let* ((abox (in-graph from))
         (role (parse-role role))
         (edges
          (remove-if-not #'(lambda (edge)
                             (eq (role edge) role))
                         (apply #'get-edges-between abox from to args))))

    (dolist (edge edges)
      (apply #'delete-edge abox edge
             :update-db-p t 
             args))))

(defmethod unrelate ((from symbol) (to symbol) role &rest args)
  (apply #'unrelate 
         (apply #'find-node *cur-abox* from :error-p t args)
         (apply #'find-node *cur-abox* to :error-p t args)
         (parse-role role)
         args))

;;;
;;;
;;;

(defun create-anonymous-node (abox &rest args)
  (incf *created-nodes*)

  (apply #'create-abox-node abox nil :old-p nil args))


