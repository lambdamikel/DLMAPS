;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;; CLSQL initilization
;;;

(setf clsql:*default-database-type* :mysql)

(defconstant +db-user+ "root")

(defconstant +db-passwd+ "root")

(clsql-sys::initialize-database-type)

;; (locally-enable-sql-reader-syntax)

(defparameter +string-marker+ 'string-marker-123)

(defparameter +symbol-marker+ 'symbol-marker-123)

;;;
;;;
;;;

(defpersistentclass db-abox (abox)
  ((db-name :initform nil)
   (db-spec :initform nil)
   (db-type :initform nil)

   
   ;;; verwenden, um wiederholte Datenbankabfragen zu vermeiden! 
   ;;; noch nicht genutzt

   (db-nodes-complete-p :accessor db-nodes-complete-p :initform nil)
   (db-edges-complete-p :accessor db-edges-complete-p :initform nil)
   (successors-complete-for :accessor successors-complete-for
                            :initform (make-hash-table :test #'equal))))
   

(defpersistentclass db-abox-node (abox-node)
  ((from-db-p :initform nil :accessor from-db-p :initarg :from-db-p)))

(defpersistentclass db-abox-edge (abox-edge)
  ((from-db-p :initform nil :accessor from-db-p :initarg :from-db-p)))


;;;
;;; Aux Functions
;;;

(defun decode-value (val)
  ;; (setf *x* val)
  (labels ((do-it (val)
             (if (stringp val)
                 val
               (if (consp val)
                   (if (eq (first val) +symbol-marker+)
                       (if (second val)
                           (intern (format nil "~A" (third val))
                                   (find-package (second val)))
                         (gensym (format nil "~A" (third val))))
                     (mapcar #'do-it val))
                 val))))
    (let ((*package* (find-package :prover)))
      (do-it (read-from-string val)))))

(defun encode-value (val)
  ;; (setf *y* val)
  (if (stringp val)
      (format nil "~S" val)
    (if (listp val)
        (format nil "~A" 
                (mapcar #'encode-value val))
      (if (symbolp val)
          (format nil "(~A ~A |~A|)"
                  +symbol-marker+
                  (if (symbol-package val)
                      (package-name (symbol-package val))
                    nil)
                  (symbol-name val))
        (format nil "~A" val)))))

;;;
;;;
;;;

(defun convert-db-tuples (tuples)
  (if (consp (car tuples))
      (mapcar #'(lambda (tuple) (mapcar #'(lambda (x) 
                                            (decode-value x))
                                        tuple))
              tuples)
    (mapcar #'decode-value tuples)))

(defun combine-labels (a b)
  (encode-value
   (remove-duplicates
    (append (ensure-list a) 
            (ensure-list b))
    :test #'set-equal)))

;;;
;;;
;;;

(defmacro with-db-connection ((db db-abox) &body body)
  `(when (is-db-abox-p ,db-abox)
     (with-slots (db-spec db-type) ,db-abox
       (clsql:with-database (,db db-spec
                                 :database-type db-type
                                 :pool t
                                 :make-default t)
         (clsql:set-autocommit t :database ,db)
         ,@body))))
  
(defun all-dbs ()
  (clsql:list-databases (list "localhost" "" +db-user+ +db-passwd+) :database-type :mysql))

(defun destroy-all-dbs ()
  (dolist (db (clsql:list-databases
               (list "localhost" "" +db-user+ +db-passwd+) :database-type :mysql))
    (when (search "MIDELORA-DB-ABOX" db)
      (destroy-db db))))

(defun destroy-db (db)
  (clsql:destroy-database (list "localhost" db +db-user+ +db-passwd+) :database-type :mysql))

(defun all-db-nodes ()
  (with-db-connection (db *cur-abox*)
    (convert-db-tuples 
     (clsql::query "SELECT * FROM NODES"
                   :database db))))

(defun all-db-edges ()
  (with-db-connection (db *cur-abox*)
    (convert-db-tuples 
     (clsql::query "SELECT * FROM EDGES"
                   :database db))))

(defun delete-all-nodes ()
  (mapc #'(lambda (x) 
            ;; db-knoten werden nur geloescht, wenn :update-db-p t 
            (delete-node *cur-abox* x))
        (get-nodes *cur-abox*)))


(defun delete-all-edges ()
  (mapc #'(lambda (x) 
            ;; db-knoten werden nur geloescht, wenn :update-db-p t 
            (delete-edge *cur-abox* x))
        (get-edges *cur-abox*)))
;;;
;;;
;;;
;;;

(defmethod find-node ((abox db-abox) name &key error-p satisfying 
                      (look-into-db-p t) &allow-other-keys)
  (declare (ignorable satisfying))
  
  (let ((node (call-next-method)))

    (or node
        
        (when look-into-db-p

          (with-abox* (abox) 
            (let* ((name-label
                    (convert-db-tuples
                     (first 
                      (with-db-connection (db abox)
                        (clsql::query 
                         (format nil "SELECT name, label FROM NODES WHERE NAME = '~A'" 
                                 (encode-value name))
                         :database db)))))

                   (name (first name-label))
                   (label (second name-label)))
                
              (when name
                (let ((node (create-abox-node abox name 
                                              :type 'db-abox-node
                                              :look-into-db-p nil
                                              :update-db-p nil)))
                  (dolist (concept label)
                    (node-instance node concept :update-db-p nil))

                  node)))))
        
        (when error-p 
          (error "Node ~A neither found in materialized ABox nor in DB!" name)))))


(defmethod get-edges-between ((abox db-abox) (from db-abox-node) (to db-abox-node) &rest args &key error-p 
                              look-into-db-p &allow-other-keys)
  (declare (ignorable args))
  
  (let ((edge (call-next-method)))

    (or edge

        (let ((from1 (encode-value (name from)))
              (to1 (encode-value (name to))))

          (when look-into-db-p
            
            (with-db-connection (db abox)
              (with-abox* (abox) 
                (let ((label 
                       (convert-db-tuples
                        (first 
                         (clsql::query 
                          (format nil "SELECT label FROM EDGES WHERE FROMNODE = '~A' AND TONODE = '~A'" 
                                  from1
                                  to1)
                          :database db))))
                      (edges nil))
            
                  (dolist (label label)
                    (push (relate from to label
                                  :type 'db-abox-edge
                                  :from-db-p t)
                          edges))

                  edges)))))
        
        (when error-p 
          (error "Edge ~A ~A neither found in materialized ABox nor in DB!" from to)))))
    
;;;
;;;
;;;

(defmethod initialize-instance :after ((abox db-abox) &rest args)
  (declare (ignorable args))
  (initialize-db abox))


(defmethod initialize-db ((abox db-abox))
  (with-slots (db-spec db-type db-name name) abox

    (setf db-type :mysql)

    (setf db-spec 
          (list "localhost" (format nil "MIDELORA-DB-ABOX-~A" 
                                    name
                                    )
                +db-user+
                +db-passwd+))

    (setf db-name (clsql:database-name-from-spec db-spec db-type))

    (cond ((member (second db-spec) 
                   (all-dbs)
                   :test #'string-equal)
           
           (format t "~%Database ~A already exists. Using existing DB.~%" name))          

          (t 
           
           (clsql:create-database db-spec :database-type db-type)

           (with-db-connection (db abox)
             
             (clsql:execute-command "CREATE TABLE NODES(NAME VARCHAR(255), LABEL BLOB,  PRIMARY KEY(NAME))"
                                    :database db)
             (clsql:execute-command "CREATE TABLE EDGES(FROMNODE VARCHAR(255), TONODE VARCHAR(255), LABEL BLOB)"
                                    :database db))))))
           

(defmethod delete-substrate :after ((substrate db-abox) &key &allow-other-keys)
  (destroy-db (second (slot-value substrate 'db-spec))))


;;;
;;; Node construction
;;; 

(defmethod create-node ((abox db-abox) (name-for-node symbol) (description node-label)
                        &rest args
                        &key 
                        (update-db-p nil)
                        (old-p t) type 
                        &allow-other-keys)

  (if (and (eq type 'db-abox-node)
           update-db-p)

      (progn 
        (unless old-p 
          (error "DB ABox individuals must have OLD-P = T!"))
        (let* ((node 
                (apply #'call-next-method
                       abox name-for-node description
                       :type 'db-abox-node
                       :look-into-db-p nil
                       args))

               (av (list (list 'name (encode-value name-for-node))
                         (list 'label (encode-value nil)))))

          (unless node
            (error "Can't create node ~A!" name-for-node))

          (setf (db-nodes-complete-p abox) nil)

          (with-db-connection (db abox)
          
            (clsql:insert-records :into 'nodes
                                  :av-pairs av
                                  :database db))

          node))

    (call-next-method)))



(defmethod delete-node :after ((abox db-abox) (node db-abox-node) &key update-db-p &allow-other-keys)  

  (when update-db-p
    (with-db-connection (db abox)
      (let ((name (encode-value (name node))))
        (clsql:delete-records :from 'nodes
                              :database db
                              :where (clsql:sql-operation '= (clsql:sql-expression :attribute 'name) name))))))


(defmethod node-instance :after ((ind db-abox-node) (concept concept) &rest args &key (update-db-p t) &allow-other-keys)
  (let ((abox (in-graph ind)))
    
    (when update-db-p 
      (with-db-connection (db abox)
        (let* ((label (mapcar #'unparse (told-concepts ind)))
               (name (encode-value (name ind)))
               (av (list (list 'label (encode-value label)))))

          (with-db-connection (db abox)
            (clsql:update-records 'nodes
                                  :av-pairs av
                                  :where (clsql:sql-operation '= (clsql:sql-expression :attribute 'name) name)
                                  :database db)))))))




(defmethod node-forget :after ((ind db-abox-node) (concept concept) &rest args &key update-db-p &allow-other-keys)
  (declare (ignorable args))

  (when update-db-p 
    (let ((abox (in-graph ind)))
 
      (with-db-connection (db abox)
        (let* ((label (mapcar #'unparse (told-concepts ind)))
               (name (encode-value (name ind)))
               (av (list (list 'label (encode-value label)))))

          (with-db-connection (db abox)
            (clsql:update-records 'nodes
                                  :av-pairs av
                                  :where (clsql:sql-operation '= (clsql:sql-expression :attribute 'name) name)
                                  :database db)))))))

;;;
;;;
;;;

(defmethod abox-nodes-iterator :after ((abox db-abox) fn &optional (consider-merged-class-p t))
  (declare (ignorable consider-merged-class-p))

  (unless (db-nodes-complete-p abox)

    (dolist (tuple 
             (convert-db-tuples 
              (with-db-connection (db abox)
                (clsql::query "SELECT * FROM NODES"
                              :database db)))) 
    
      ;;; hier werden nur die NAMEN der virtuellen Knoten ermittelt!
      ;;; -> evtl. materialisieren 

      (let ((node (first tuple)))

        (unless (find-node abox node :look-into-db-p nil)
          (let ((node (find-node abox node :look-into-db-p t)))
            (funcall fn node)))))

    (setf (db-nodes-complete-p abox) t)))


(timed-defmethod abox-edges-iterator :after ((abox db-abox) fn &key (satisfying #'yes))

  (unless (db-edges-complete-p abox)

    (dolist (tuple 
             (convert-db-tuples 
              (with-db-connection (db abox)
                (clsql::query "SELECT * FROM EDGES"
                              :database db))))

      (let* ((from (first tuple))
             (to (second tuple))
             (label (third tuple))

             (edge (relate from to label
                           :type 'db-abox-edge
                           :from-db-p t)))

        (when (funcall satisfying edge)

          (funcall fn edge))))

    (setf (db-edges-complete-p abox) t)))


;;;
;;; Edge construction
;;; 


(defmethod create-edge ((abox db-abox) 
                        (from db-abox-node) (to db-abox-node)
                        (description edge-label)
                        &rest args
                        &key 
                        (update-db-p nil)
                        (old-p t) 
                        type
                        &allow-other-keys)

  (if (and (eq type 'db-abox-edge) 
           update-db-p)

      (progn 
        (unless old-p 
          (error "DB ABox edges must have OLD-P = T!"))

        (let* ((edge 
                (apply #'call-next-method
                       abox from to description
                       :type 'db-abox-edge
                       args))

               (av (list (list 'fromnode (encode-value (name from)))
                         (list 'tonode (encode-value (name to)))
                         (list 'label (encode-value (unparse (textual-description description)))))))

          (unless edge
            (error "Can't create edge between ~A and ~A!" from to))

          (setf (db-edges-complete-p abox) nil)

          (with-db-connection (db abox)
          
            (clsql:insert-records :into 'edges
                                  :av-pairs av
                                  :database db))
          edge))

    (call-next-method)))



(defmethod delete-edge :after ((abox db-abox) (edge db-abox-edge) &key update-db-p &allow-other-keys)

  (when update-db-p
    (with-db-connection (db abox)

      (let ((from (encode-value (name (from edge))))
            (to   (encode-value (name (to edge))))
            (label (encode-value (unparse (textual-description (description edge))))))

        (princ label)

        (when update-db-p 
          
          (with-db-connection (db abox)
            (clsql-sys:delete-records :from 'edges
                                      :database db
                                      :where 
                                      (clsql:sql-operation 
                                       'and 
                                       (clsql:sql-operation '= (clsql:sql-expression :attribute 'fromnode) from)
                                       (clsql:sql-operation '= (clsql:sql-expression :attribute 'tonode) to)
                                       (clsql:sql-operation '= (clsql:sql-expression :attribute 'label) label)))))))))
                                       

;;;
;;;
;;;

(defmethod compute-virtual-successors ((abox db-abox) (node db-abox-node) &optional role)
  (unless (gethash (list node role) 
                   (successors-complete-for abox))

    (with-db-connection (db abox)

      (let ((succs
             (convert-db-tuples
              (clsql::query 
               (format nil "SELECT TONODE, LABEL FROM EDGES WHERE FROMNODE = '~A'" 
                       (encode-value (name node)))
               :database db)))
            
            (pres
             (convert-db-tuples
              (clsql::query 
               (format nil "SELECT FROMNODE, LABEL FROM EDGES WHERE TONODE = '~A'" 
                       (encode-value (name node)))
               :database db)))

            (inv-role (when role (get-inverse-role role))))

        (setf (gethash (list node role) 
                       (successors-complete-for abox)) t)
        
        (nconc (delete nil
                       (mapcar #'(lambda (x) 
                                   (when (=> role (implies-p (parse-role (second x)) role))
                                     (find-node abox (first x))))
                               succs))
               (delete nil 
                       (mapcar #'(lambda (x) 
                                   (when (=> inv-role (implies-p (parse-role (second x)) inv-role))
                                     (find-node abox (first x))))
                               pres)))))))

(defmethod materialize-virtual-edge ((abox db-abox) (node db-abox-node) (succ db-abox-node) role)
  (relate node succ role 
          :type 'db-abox-edge
          :created-by t 
          :comment 'virtual-computed-midelora-rcc-edge
          :old-p t
          :materialized-virtual-p t))

(defmethod obvious-non-instance ((abox db-abox) node concept)
  (cond ((is-bottom-concept-p concept)

         t)

        ((is-top-concept-p concept)

         nil)

        ((member concept (told-concepts node))
         
         nil)
	
        (t

         (let* ((negated (negated-concept concept)))

           (or (member negated (told-concepts node))
               
               (obvious-instance abox node negated)
               

               ;;; erfordert das Retrieval-Konzeptmodell keine Nachfolger, dann 
               ;;; ist der Test wieder anwenbar! 
               
               (some #'(lambda (ma) 
                         (some #'(lambda (mb)
                                   (and (not (or (node-model-successors mb)
                                                 (node-model-attributes mb)
                                                 (node-model-alls mb)
                                                 (node-model-at-mosts mb)))
                                        (models-mergeable-p +alch+ ma mb)))
                               (cached-models negated)))
                     (ind-models node)))))))

#|

(progn 

  (delete-all-aboxes)

  (in-abox test nil :type 'db-abox)

  (princ (all-db-nodes))
  (princ (all-db-edges))

  (db-instance a a)

  (db-related a b r)

  (db-instance b b)

  (del a) 
  (del b)

  (princ (concept-instances (or (and a (some r b))
                                (and b (some (inv r) a)))))
  
  )

|# 

