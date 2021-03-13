;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;;
;;;

(defpersistentclass spatial-substrate (rolebox-substrate)
  ((consistency-relevant-edges :reader consistency-relevant-edges :initform nil)))

(defmethod create-substrate ((class (eql 'spatial-substrate)) 
                             &key name tbox abox rbox (real-class 'spatial-substrate))
  (create-substrate 'rolebox-substrate :name name :abox abox :tbox tbox :rbox rbox :real-class 'spatial-substrate))

;;;
;;;
;;; 

(defmethod simplify ((substrate spatial-substrate))
  substrate)

(defmethod add-eq-loops ((substrate spatial-substrate))
  (dolist (node (network-nodes substrate))
    (create-edge substrate node node 'eq)))

(defmethod complete ((substrate spatial-substrate))
  (add-eq-loops substrate)
  ;;;(add-inverse-edges substrate)
  (add-missing-edges substrate))


(defmethod enumerate-atomic-configurations ((substrate spatial-substrate) 
                                            &key 
                                            how-many 
                                            (edges (consistency-relevant-edges substrate))
                                            
                                            (sym-gen #'(lambda (edge processed-edges)
                                                         (description edge)))

                                            (selector-fn #'(lambda (rem-edges processed-edges)
                                                             (first rem-edges)))


                                            (manager-fn #'(lambda (sel-edge edges processed-edges)
                                                            (values (remove sel-edge
                                                                            (remove (inverse-edge sel-edge) 
                                                                                    edges))
                                                                    (cons sel-edge (cons (inverse-edge sel-edge)
                                                                                         processed-edges)))))


                                            (fn #'materialize) 
                                            (final-check-fn #'(lambda (x) (consistent-p x :force-p t)))
                                            (construction-check-fn #'(lambda (x) t)))
  
  
  (apply #'call-next-method
         substrate
         :edges edges
         
         :how-many how-many
         
         :sym-gen sym-gen

         :selector-fn selector-fn
         :manager-fn manager-fn

         :fn fn
         
         :final-check-fn final-check-fn
         :construction-check-fn construction-check-fn
         nil))


;;;
;;; 
;;;

(defpersistentclass spatial-object (rolebox-substrate-node)
  nil)

(defmethod create-node ((substrate spatial-substrate) name description &rest args &key (real-class 'spatial-object))
  (apply #'call-next-method substrate name description :real-class real-class args))

(defpersistentclass spatial-relation (rolebox-substrate-edge)
  ((consistency-relevant-a-triangles :reader consistency-relevant-a-triangles :initform nil)))

(defmethod create-edge ((substrate spatial-substrate) from to description &rest args &key (error-p t) (real-class 'spatial-relation))
  (if (get-edges-between substrate from to)
      (when error-p (error "Edge (~A,~A) already exists in substrate ~A!"
                           from to substrate))
    (apply #'call-next-method substrate from to description :real-class real-class :error-p error-p args)))

;;;
;;; Spatial-Relation-Methoden 
;;;

(defmethod initialize-instance :after ((object spatial-relation) &rest initargs)
  (with-slots (consistency-relevant-edges) (in-substrate object) 
    (when (consistency-relevant-p object)
      (push object consistency-relevant-edges))))


(defmethod consistency-relevant-p ((edge spatial-relation)) 
  (not (eq (from edge) (to edge))))


(defmethod delete-edge :after ((substrate spatial-substrate) (edge spatial-relation) &key &allow-other-keys) 
  (with-slots (inverse-edge) edge
    (with-slots (consistency-relevant-edges) substrate
      (setf consistency-relevant-edges
            (delete edge consistency-relevant-edges))
      (dolist (referencing-edge (referenced-by edge))
        (when (typep referencing-edge 'spatial-relation) 
          (with-slots (consistency-relevant-a-triangles) referencing-edge
            (setf consistency-relevant-a-triangles
                  (delete-if #'(lambda (tri) (member edge tri)) 
                             consistency-relevant-a-triangles))))))))


(defmethod get-support ((edge spatial-relation))
  (with-slots (consistency-relevant-a-triangles in-substrate) edge
    (with-slots (consistency-relevant-edges) in-substrate
      (or consistency-relevant-a-triangles
          (setf consistency-relevant-a-triangles 
                (remove-if-not #'(lambda (tri)
                                   (and (consistency-relevant-p (first tri))
                                        (consistency-relevant-p (first tri))))
                               (a-triangles edge)))))))

(defun compute-label-from-given-support (edge support)
  (let* ((rbox (substrate-rbox (in-substrate edge)))
         (role (mapcar #'(lambda (supp)
                           (let* ((r (current-label (first supp)))
                                  (s (current-label (second supp)))
                                  (res (lookup rbox r s)))
                             res))
                       (loop as tri in (consistency-relevant-a-triangles edge)
                             when (and (member (first tri)  
                                               support)
                                       (member (second tri)))
                             collect tri))))
    (if role
        (reduce #'intersection role)
      (full-disjunctive-role rbox))))
