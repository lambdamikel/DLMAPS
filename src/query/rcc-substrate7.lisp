;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :thematic-substrate)

;;;
;;;
;;;

(defvar *rcc-type* :rcc5)

(defpersistentclass rcc-substrate (data-substrate)
  ((rbox :reader rbox)
   (rcc-type :reader rcc-type)

   (edge-consistent-p :initform :dont-known)
   (saved-edge-consistent-p)

   (minimal-label-computed-p :initform nil)))

(defmethod initialize-instance :after ((substrate rcc-substrate) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (rbox rcc-type) substrate
    (setf rcc-type *rcc-type*)
    (setf rbox
          (case *rcc-type*
            (:rcc1 +rcc1-rolebox+)
            (:rcc2 +rcc2-rolebox+)
            (:rcc3 +rcc3-rolebox+)
            (:rcc5 +rcc5-rolebox+)
            (:rcc8 +rcc8-rolebox+)))))

;;;
;;;
;;;

(defpersistentclass nrql-rcc-query-parser (nrql-data-query-parser) nil)

(defmethod get-parser-class-for-substrate ((substrate rcc-substrate))
  'nrql-rcc-query-parser)

;;;
;;;
;;;

(defpersistentclass rcc-substrate-query (nrql-query) nil)

(defpersistentclass rcc-substrate-node-query (data-substrate-node-query rcc-substrate-query) nil)

(defpersistentclass rcc-substrate-edge-query (data-substrate-edge-query rcc-substrate-query) nil)

;;;
;;;
;;;

(defmethod initialize-description :after ((query rcc-substrate-edge-query))
  (with-slots (textual-description) query
    (setf textual-description 
          (mapcar #'to-keyword 
                  (ensure-list textual-description)))))

;;;
;;;
;;;

(defmethod make-dispatcher ((parser nrql-rcc-query-parser) sym)
  (case sym    
    
    (substrate-simple-and-node-query 
     (make-instance 'rcc-substrate-node-query :dont-initialize-p t))
    
    (substrate-simple-or-edge-query
     (make-instance 'rcc-substrate-edge-query :dont-initialize-p t))
    
    (otherwise 

     (call-next-method))))

;;;
;;;
;;;

(defmethod valid-node-or-description-p ((parser nrql-rcc-query-parser) expr)
  (declare (ignorable expr))
  nil)

(defmethod valid-edge-and-description-p ((parser nrql-rcc-query-parser) expr)
  (declare (ignorable expr))
  nil)

(defmethod valid-edge-or-description-p ((parser nrql-rcc-query-parser) expr)
  (subsetp (ensure-list expr) (roles (rbox (substrate parser)))))
           

;;;
;;;
;;;

(defmethod loop-over-all-successors1 ((substrate rcc-substrate) from fn &optional descr)
  (let ((succs nil))
    (loop-over-successors (substrate from descr)
                          (to) 
                          (progn 
                            (push to succs)
                            (funcall fn to)))
  
    (loop-over-nodes (substrate to)
      (unless (member to succs)
        (when (related-p substrate from to descr)
          (funcall fn to))))))


(defmethod loop-over-all-predecessors1 ((substrate rcc-substrate) to fn &optional descr)
  (let ((preds nil))
    (loop-over-predecessors (substrate to descr)
                            (from) 
                            (progn 
                              (push from preds)
                              (funcall fn from)))
  
    (loop-over-nodes (substrate from)
      (unless (member from preds)
        (when (related-p substrate from to descr)
          (funcall fn from))))))

;;;
;;; 
;;;

(defun make-intersection-description (descr)
  (reduce #'(lambda (x y)
              (intersection x y :test #'equalp))
          descr))
                                 

(defmethod node-instance :before ((substrate rcc-substrate) name &key &allow-other-keys)
  (declare (ignorable name))
  (with-slots (minimal-label-computed-p  data-nodes edge-consistent-p) substrate
    (when edge-consistent-p ; also :unkown oder T!
      (setf minimal-label-computed-p nil
            edge-consistent-p :unknown))))


(defmethod nodes-related ((substrate rcc-substrate) from to description &key &allow-other-keys)
  (with-slots (data-edges data-nodes minimal-label-computed-p edge-consistent-p) substrate
    (let ((description (ensure-list description)))

      (when (or (not (every #'keywordp description))
                (not (subsetp description
                              (roles (rbox substrate)))))
        (nrql-error "~S is not a valid ~A relationship!" description (rcc-type substrate)))
        
      (labels ((relate (from to description)
                 
                 (when edge-consistent-p ; also :unkown oder T!
                   (setf edge-consistent-p :unknown))

                 (setf minimal-label-computed-p nil)
                 
                 (unless (node-p substrate from)
                   (node-instance substrate from))
                 
                 (unless (node-p substrate to)
                   (node-instance substrate to))
                 
                 (push to (node-info-successors (gethash from data-nodes)))
                 (push from (node-info-predecessors (gethash to data-nodes)))
                 
                 (let* ((key (list from to))
                        (info (gethash key data-edges))
                        (new-descr 
                         (make-intersection-description 
                          (if info 
                              (list (edge-info-type info) description)
                            (if (eq from to) 
                                (list (reflexive-roles (rbox substrate))
                                      description)
                              (list description))))))

                   (unless new-descr
                     (setf edge-consistent-p nil))
                       
                   (if info
                       (setf (edge-info-type info) new-descr)
                     
                     (setf (gethash key data-edges)
                           (list new-descr new-descr)))
                   
                   new-descr)))

        (relate from to description)))))

;;;
;;;
;;;

(defmethod delete-node :after ((substrate rcc-substrate) name &rest args)
  (declare (ignorable name args))
  (with-slots (edge-consistent-p minimal-label-computed-p) substrate
    (setf minimal-label-computed-p nil 
          edge-consistent-p :unknown)))

(defmethod delete-edge :after ((substrate rcc-substrate) from to &rest args)
  (declare (ignorable from to args))
  (with-slots (edge-consistent-p minimal-label-computed-p) substrate
    (setf minimal-label-computed-p nil
          edge-consistent-p :unknown)))

;;;
;;;
;;;

(defmethod get-edge-between ((substrate rcc-substrate) from to) 
  (with-slots (data-edges) substrate
    (multiple-value-bind (fromto found1p)
        (gethash (list from to) data-edges)
      (if found1p 
          (values fromto t)
        (if (eq from to)
            (values (copy-tree (list (reflexive-roles (rbox substrate)) nil)) nil)
          (multiple-value-bind (tofrom found2p)
              (gethash (list to from) data-edges)
            (if found2p
                (values (list (inv-role (rbox substrate) (first tofrom))
                              (inv-role (rbox substrate) (second tofrom)))
                        t)
              (values (copy-tree (list (roles (rbox substrate)) nil)) nil))))))))

;;;
;;;
;;;


(defmethod compute-minimal-label ((substrate rcc-substrate))
  (let* ((iter t)
         (rbox (rbox substrate))
         (consistent-p t)
         (nodes (get-nodes substrate)))

    (with-slots (edge-consistent-p minimal-label-computed-p) substrate

      (unless minimal-label-computed-p

        (block loop

          (loop while iter do
                (setf iter nil)

                (mapl #'(lambda (nodes1) 

                          (let ((from (first nodes1)))

                            (mapl #'(lambda (nodes2) 

                                      (let* ((to (first nodes2)))

                                        (multiple-value-bind (from-to foundp)
                                            (get-edge-between substrate from to)

                                          (declare (ignorable foundp))
                                              
                                          (dolist (over nodes)
                                            
                                            (multiple-value-bind (from-over foundp)
                                                (get-edge-between substrate from over)

                                              (declare (ignorable foundp))
                                                
                                              (multiple-value-bind (over-to foundp)
                                                  (get-edge-between substrate over to)
                                                  
                                                (declare (ignorable foundp))

                                                  ; (format t "~A ~A ~A ~%" from over to)

                                                (let ((comp 
                                                       (lookup rbox
                                                               (edge-info-type from-over)
                                                               (edge-info-type over-to))))
                                                                    
                                                  (unless (implies-p
                                                           (list (edge-info-type from-to))
                                                           (list comp))

                                                    (setf iter t)
                                                                      
                                                    (let ((res 
                                                           (intersection 
                                                            (edge-info-type from-to)
                                                            comp)))

                                                      (nodes-related substrate from to res)

                                                      (unless res 
                                                        (setf consistent-p nil)
                                                        (return-from loop)))))))))))

                                  nodes1)))

                      nodes)))

        (setf edge-consistent-p consistent-p)

        (when consistent-p
          (setf minimal-label-computed-p t))

        substrate))))

;;;
;;;
;;;

(defmethod related-p ((substrate rcc-substrate) from to &optional descr)
  (compute-minimal-label substrate)

  (multiple-value-bind (edge foundp) 
      (get-edge-between substrate from to)

    (declare (ignorable foundp))

    (let ((type (edge-info-type edge)))

      (or 
         
       (implies-p (list type) descr)
       
       #|

         (let* ((roles (roles (rbox substrate)))
                (descr (if (is-query-p descr)
                           (textual-description descr)
                         (ensure-list descr)))
                (diff (set-difference roles descr)))

           (save-state substrate)
             
           (nodes-related substrate from to diff)

           (prog1 
               (not (consistent-p substrate))

             (restore-state substrate))) |# ))))


;;;
;;;
;;;

(defmethod consistent-p ((substrate rcc-substrate))
  (compute-minimal-label substrate)
  (slot-value substrate 'edge-consistent-p))

;;;
;;; API 
;;;

(defmethod save-state :after ((substrate rcc-substrate))
  (setf (slot-value substrate 'saved-edge-consistent-p)
        (slot-value substrate 'edge-consistent-p)))

(defmethod restore-state :after ((substrate rcc-substrate))
  (setf (slot-value substrate 'edge-consistent-p)
        (slot-value substrate 'saved-edge-consistent-p)))

;;;
;;;
;;;

(defmethod reset-substrate :after ((substrate rcc-substrate) &key &allow-other-keys)
  (setf (slot-value substrate 'edge-consistent-p) :unknown))

(nrql-defun set-rcc-box (name &optional (rcc-type :rcc8))
  (setf *type-of-substrate* 'rcc-substrate)
  (let ((*rcc-type* rcc-type))
    (set-data-box name))
  name)


(nrql-defmacro (in-rcc-box :nrql-function set-rcc-box))

;;;
;;;
;;;

(nrql-defun create-rcc-node (&rest args)
  (apply #'create-data-node args))

(nrql-defun create-rcc-edge (&rest args)
  (apply #'create-data-edge args))

(nrql-defun rcc-consistent-p (&optional abox type-of-substrate)
  (with-data-box (abox type-of-substrate)
                 (consistent-p *cur-substrate*)))

(nrql-defmacro (rcc-consistent? :nrql-function rcc-consistent-p))

;;;
;;;
;;;

(nrql-defun rcc-instance1 (&rest args)
  (apply #'data-node1 args))

(nrql-defmacro (rcc-instance :nrql-function rcc-instance1))

(nrql-defun rcc-related1 (&rest args)
  (apply #'data-edge1 args))

(nrql-defmacro (rcc-related :nrql-function rcc-related1))

(nrql-defun rcc-node1 (&rest args)
  (apply #'data-node1 args))

(nrql-defmacro (rcc-node :nrql-function rcc-node1))

(nrql-defun rcc-edge1 (&rest args)
  (apply #'data-edge1 args))

(nrql-defmacro (rcc-edge :nrql-function rcc-edge1))

;;;
;;;
;;;

(nrql-defun rcc-node-label1 (&rest args)
  (apply #'node-label1 args))

(nrql-defmacro (rcc-node-label :nrql-function rcc-node-label1))

(nrql-defun rcc-edge-label1 (&rest args)
  (apply #'edge-label1 args))

(nrql-defmacro (rcc-edge-label :nrql-function rcc-edge-label1))

;;;
;;;
;;;

(nrql-defun del-rcc-node1 (&rest args)
  (apply #'del-data-node1 args))

(nrql-defun del-rcc-edge1 (&rest args)
  (apply #'del-data-edge1 args))

(nrql-defmacro (del-rcc-node :nrql-function del-rcc-node1))

(nrql-defmacro (del-rcc-edge :nrql-function del-rcc-edge1))




