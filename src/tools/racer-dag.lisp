;; -*- Mode: LISP; Syntax: Common-Lisp; Package: RACER-DAG -*-

(in-package :RACER-DAG)

;;;
;;;
;;;

(defclass racer-taxonomy (dag) 
  ((dag::tprinter :initform 'equivalent-nodes)))

(defclass racer-taxonomy-node (dag-node) 
  ((equivalent-nodes :accessor equivalent-nodes :initarg :equivalent-nodes)))

(defun get-racer-taxonomy (tbox &optional (classify-tbox-p t))
  (let ((*print-readably* nil))
    (when classify-tbox-p (classify-tbox tbox))
    (set-current-tbox tbox)
    (let ((dag (make-dag :name (format nil "Taxonomy of RACER TBox ~A" tbox) 
                         :printer #'(lambda (x stream)
                                      (format stream "~A" (dag-node-name x)))
                         :type 'racer-taxonomy))
          (nodes (make-hash-table :size 1000))
          (nodes-list nil))
    
      (dolist (concept-cluster 
               (remove-duplicates
                (mapcar #'first (taxonomy tbox))
                :test #'equal))

        (let* ((concept-cluster (tools::ensure-list concept-cluster))
               (node-name (first concept-cluster))
               (node (make-dag-node :name 
                                    node-name
                                    :equivalent-nodes 
                                    concept-cluster
                                    :type 'racer-taxonomy-node)))
          (push node nodes-list)
          
          (dolist (concept concept-cluster)
            (setf (gethash concept nodes) 
                  node))))

      (dolist (triple (taxonomy tbox))
        
        (let* ((node-name (first (tools::ensure-list (first triple))))
               (parents (second triple))
               (children (third triple))
              
               (node (gethash node-name nodes))

               (parents (remove-duplicates 
                         (mapcar #'(lambda (x) 
                                     (gethash (first (tools::ensure-list x)) nodes))
                                 parents)))
              
               (children (remove-duplicates 
                          (mapcar #'(lambda (x) 
                                      (gethash (first (tools::ensure-list x)) nodes))
                                  children))))

          (setf (dag-node-children node) children)
          (setf (dag-node-parents node) parents)
          
          (insert-dag-node dag node)))

      dag)))

#+:clim
(defun show-racer-taxonomy (&optional (tbox (racer:current-tbox)))
  (visualize-dag (get-racer-taxonomy tbox)))

