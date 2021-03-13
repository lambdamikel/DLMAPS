;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

(defconstant +table-size-data-edges+ 1000)

(defconstant +table-size-data-nodes+ 100)

(defvar *level-id* 0)

(defconstant +reserved-data-tokens+ '(:satisfies :predicate))

(defconstant +recognized-predicates+
  '((string= (x y) (string string))
    (string-equal (x y) (string string))
    (string-not-equal (x y) (string string))
    (string< (x y) (string string))
    (string<= (x y) (string string))
    (string-lessp (x y) (string string))
    (string-greaterp (x y) (string string))
    (string> (x y) (string string))
    (string>= (x y) (string string))
    (string-not-greaterp (x y) (string string))
    (string-not-lessp (x y) (string string))
    (string/= (x y) (string string))
                                       
    (search (y x) (string string))
                                       
    (= (x y) (number number))
    (/= (x y) (number number))
    (> (x y) (number number))
    (< (x y) (number number))
    (>= (x y) (number number))
    (<= (x y) (number number))
                                       
    (find (x y) (t sequence))
                                       
    (stringp (x) (t))
    (zerop (x) (t))
    (integerp (x) (t))
    (numberp (x) (t))
    (consp (x) (t))
    (symbolp (x) (t))
    (keywordp (x) (t))
    (rationalp (x) (t))
    (floatp (x) (t))
    (minusp (x) (number))

    ))

;;;
;;;
;;;

(defmacro node-info-type (node-info)
  `(first ,node-info))

(defmacro node-info-successors (node-info)
  `(second ,node-info))

(defmacro node-info-predecessors (node-info)
  `(third ,node-info))

;;;
;;;
;;;

(defmacro edge-from (edge) 
  `(first ,edge))

(defmacro edge-to (edge) 
  `(second ,edge))

;;;
;;;
;;;

(defmacro edge-info-type (edge-info)
  `(first ,edge-info))

(defmacro edge-info-saved-type (edge-info)
  `(second ,edge-info))


;;;
;;;
;;;

(defpersistentclass nrql-data-query-parser (nrql-abox-query-parser) nil) 

;;;
;;;
;;;

(defpersistentclass data-substrate (racer-dummy-substrate)
  ((data-nodes :accessor data-nodes :initform (mht :test #'eql :size +table-size-data-nodes+))
   (data-edges :accessor data-edges :initform (mht :test #'equal :size +table-size-data-edges+))
   
   (marked-list :accessor marked-list :initform nil)

   (told-info :accessor told-info :initform nil)))

;;;
;;;
;;;

(defpersistentclass data-substrate-query (nrql-query) nil)

(defpersistentclass data-substrate-node-query (data-substrate-query unary-query) nil)

(defpersistentclass data-substrate-edge-query (data-substrate-query binary-query) nil)

(defpersistentclass data-substrate-has-known-successor-retrieval-query (data-substrate-query unary-query) nil)

(defpersistentclass data-substrate-predicate-edge-query (data-substrate-query binary-query) nil)

;;;
;;;
;;;

(defun is-predicate-p (literal &optional arity)
  (and (consp literal)
       (member (first literal) '(:predicate))
       (let* ((predicate (second literal))
              (predicate 
               (if (consp predicate)
                   (first predicate)
                 predicate)))
         (find-if #'(lambda (x) 
                      (let ((pname (first x))
                            (parity (length (second x))))
                        (and (equal pname predicate)
                             (=> arity
                                 (= parity
                                    arity)))))
                  +recognized-predicates+))))

           
(defun get-arity (predicate)
  (second (find predicate +recognized-predicates+
                :key #'first)))

(defun get-domains (predicate)
  (third (find predicate +recognized-predicates+
               :key #'first)))

(defun convert-predicate (predicate edge-predicate-p)
  (let* ((predicate (ensure-list (second predicate)))
         (fn (first predicate))
         (literal (second predicate))
         (arity (get-arity fn))
         (domains (get-domains fn))
         (x-guard (first domains))
         (y-guard (second domains)))

    (cond 
     ((equal arity '(x))
      #'(lambda (x) 
          (when (typep x x-guard)
            (funcall (symbol-function fn) x))))

     ((equal arity '(x y))
      (if edge-predicate-p
          #'(lambda (x y) 
              (when (and (typep x x-guard)
                         (typep y y-guard))
                (funcall (symbol-function fn) x y)))
        #'(lambda (x) 
            (when (and (typep x x-guard)
                       (typep literal y-guard))
              (funcall (symbol-function fn) x literal)))))
     
     ((equal arity '(y x))
      (if edge-predicate-p
          #'(lambda (x y) 
              (when (and (typep x x-guard)
                         (typep y y-guard))
                (funcall (symbol-function fn) y x)))
        #'(lambda (x) 
            (when (and (typep x x-guard)
                       (typep literal y-guard))
              (funcall (symbol-function fn) literal x)))))

     (t (parser-error 
         (format nil "Unrecognized data predicate ~A" predicate))))))
            
(defmethod initialize-description :after ((query data-substrate-node-query))
  (initialize-data-substrate-query query))

(defmethod initialize-description :after ((query data-substrate-edge-query))
  (initialize-data-substrate-query query))

(defmethod initialize-description :after ((query data-substrate-has-known-successor-retrieval-query))
  (pop (textual-description query))

  (initialize-data-substrate-query query))

(defmethod initialize-description :after ((query data-substrate-predicate-edge-query))
  (pop (textual-description query))
  ;;; (:satisfies ...) entfernen!

  (initialize-data-substrate-query query))

(defmethod initialize-data-substrate-query ((query data-substrate-query))
  (with-slots (textual-description) query
    (let* ((descr (ensure-list textual-description))
           (descr 
            (if (is-predicate-p descr)
                (list descr)
              descr))
           (new-description nil))

      (dolist (conjunct descr)
        (cond ((is-predicate-p conjunct)
               (push (convert-predicate conjunct 
                                        (is-data-substrate-predicate-edge-query-p query))
                     new-description))
              
              ((consp conjunct)
               (push 
                (mapcar #'(lambda (disjunct)
                            (if (is-predicate-p disjunct)
                                (convert-predicate disjunct
                                                   (is-data-substrate-predicate-edge-query-p query))
                              disjunct))
                        conjunct)
                new-description))

              (t 
               (push conjunct new-description))))

      (setf textual-description new-description))))

;;;
;;;
;;;

(defmethod marked-p ((data-substrate-node t))
  (member data-substrate-node (marked-list *running-substrate*) :test #'equal))

(defmethod mark ((data-substrate-node t) &optional val)
  (declare (ignorable val))
  (push data-substrate-node (marked-list *running-substrate*)))

(defmethod unmark ((data-substrate-node t))
  (setf (marked-list *running-substrate*)
        (delete data-substrate-node (marked-list *running-substrate*) :test #'equal)))

;;;
;;;
;;;

(defmethod get-associated-substrate-node ((substrate data-substrate) (abox-ind symbol))
  (when (node-p substrate abox-ind)
    abox-ind))
      
(defmethod get-associated-abox-individual ((substrate data-substrate) (data-node symbol))
  (when (dl-prover-individual-exists-p  *running-substrate* 
                                        data-node)
    data-node))

(defmethod get-associated-abox-individual ((substrate data-substrate) (data-node t))
  nil)

;;;
;;;
;;;

(defmethod get-parser-class-for-substrate ((substrate data-substrate))
  'nrql-data-query-parser)

;;;
;;;
;;;

(defmethod make-dispatcher ((parser nrql-data-query-parser) sym)
  (case sym    

    ((substrate-predicate-node-query
      substrate-simple-and-node-query)
     
     (make-instance 'data-substrate-node-query :dont-initialize-p t))
    
    (substrate-simple-or-edge-query
     (make-instance 'data-substrate-edge-query :dont-initialize-p t))

    (substrate-predicate-edge-query 
     (make-instance 'data-substrate-predicate-edge-query :dont-initialize-p t))

    (substrate-has-known-successor-retrieval-query
     (make-instance 'data-substrate-has-known-successor-retrieval-query :dont-initialize-p t))
    
    (otherwise 

     (call-next-method))))

;;;
;;;
;;;

(defmethod valid-data-literal-p ((parser nrql-data-query-parser) literal &optional arity only-predicates-p)
  (if only-predicates-p 
      (or (is-predicate-p literal arity)
          (and (consp literal) 
               (every #'(lambda (x) (valid-data-literal-p parser x arity only-predicates-p)) literal)))
    (typecase literal
      (symbol (not (member literal +reserved-data-tokens+)))
      (string t)
      (number t)
      (null t)
      (list (or (is-predicate-p literal arity)
                (every #'(lambda (x) (valid-data-literal-p parser x arity)) literal)))
      (otherwise nil))))


;;;
;;;
;;;

(defmethod valid-node-or-description-p ((parser nrql-data-query-parser) expr)
  (declare (ignorable expr))
  nil)

(defmethod valid-node-and-description-p ((parser nrql-data-query-parser) expr)
  (let ((expr (ensure-list expr)))
    (or (valid-data-literal-p parser expr)
        (every #'(lambda (x) (valid-data-literal-p parser x)) expr))))

(defmethod valid-edge-and-description-p ((parser nrql-data-query-parser) expr)
  (declare (ignorable expr))
  nil)

(defmethod valid-edge-or-description-p ((parser nrql-data-query-parser) expr)
  (let* ((expr (ensure-list expr)))
    (or (valid-data-literal-p parser expr)
        (every #'(lambda (x) (valid-data-literal-p parser x)) expr))))

(defmethod valid-predicate-description-p ((parser nrql-data-query-parser) expr)
  (and (consp expr)
       (member (to-keyword (first expr)) '(:satisfies))
       (valid-data-literal-p parser
                             (rest expr) 2 t)))

(defmethod valid-has-known-successor-substrate-description-p ((parser nrql-data-query-parser) expr)
  (and (consp expr)
       (eq (to-keyword (first expr)) :has-known-successor)
       (valid-edge-or-description-p parser (second expr))))

;;;
;;; Node construction
;;; 
    
(defvar +no-descr-marker+ 'ndcrmrk123xyz)

;;;
;;;
;;;

(defmethod node-instance :after ((substrate data-substrate) name 
                                 &key (description nil des-supplied-p) told-info-p)
  (with-slots (told-info) substrate
    (when told-info-p
      (push (if des-supplied-p 
                `(node-instance ,name :description ,description)
              `(node-instance ,name))
            told-info))))

;;;
;;;
;;;

(defmacro loop-over-nodes ((substrate node &optional info) &body body)
  (let ((var (or info (gensym))))
    `(loop-over-nodes1 ,substrate 
                       #'(lambda (,node ,var)
			   (declare (ignorable ,node ,var))
                           ,@body))))

;;;
;;; ---------------------------------------------------
;;; Adapter-Methoden die ueberschrieben werden muessen!
;;; ---------------------------------------------------
;;; 

(defmethod node-instance ((substrate data-substrate) name 
                          &key (description nil des-supplied-p) &allow-other-keys)

  (with-slots (data-nodes) substrate
    (let* ((info (gethash name data-nodes))
           (description (mapcar #'ensure-list (ensure-list description)))
           (description (remove-duplicates description :test #'set-equal)))

      (if info
          (when des-supplied-p
            (if (eq (node-info-type info) +no-descr-marker+)
                (setf (first info) description)
              (setf (first info)
                    (remove-duplicates 
                     (nconc (first info)
                            description)
                     :test #'set-equal))
              ;;; (nconc (first info) description)
              ))
        
        (setf (gethash name data-nodes)
              (if des-supplied-p 
                ; descr, succs, preds, hash
                  (list description nil nil)
                (list +no-descr-marker+ nil nil)))))))

(defmethod loop-over-nodes1 ((substrate data-substrate) fn)
  (with-slots (data-nodes) substrate
    (maphash fn data-nodes)))

(defmethod get-node-info ((substrate data-substrate) node)
  (with-slots (data-nodes) substrate
    (multiple-value-bind (info foundp)
        (gethash node data-nodes)
      (if foundp 
          info
        +no-descr-marker+))))

(defmethod node-p ((substrate data-substrate) node)
  (with-slots (data-nodes) substrate
    (multiple-value-bind (res foundp) 
        (gethash node data-nodes)
      (declare (ignorable res))
      foundp)))

;;;
;;; ---------------------------------------------------
;;;

(defmethod all-nodes ((substrate data-substrate))
  (let ((nodes nil))
    (loop-over-nodes (substrate node info)
      (push (list node info) nodes))
    nodes))

(defmethod get-nodes ((substrate data-substrate) &key &allow-other-keys)
  (let ((nodes nil))
    (loop-over-nodes (substrate node)
      (push node nodes))
    nodes))

;;;
;;;
;;;

(defmethod nodes-related :after ((substrate data-substrate) from to description &key told-info-p)
  (with-slots (told-info) substrate
    (when told-info-p 
      (push 
       `(nodes-related ,from ,to ,description)
       told-info))))

(defmethod get-edge-between ((substrate data-substrate) from to) 
  (get-edge-info substrate from to))


;;;
;;; ---------------------------------------------------
;;; Adapter-Methoden die ueberschrieben werden muessen!
;;; ---------------------------------------------------
;;; 

(defmethod nodes-related ((substrate data-substrate) from to description &key told-info-p)
  (with-slots (data-edges data-nodes told-info) substrate
    (unless (node-p substrate from)
      (node-instance substrate from :told-info-p told-info-p))
    
    (unless (node-p substrate to)
      (node-instance substrate to :told-info-p told-info-p))

    (push to (node-info-successors (gethash from data-nodes)))
    (push from (node-info-predecessors (gethash to data-nodes)))
    
    (let* ((key (list from to))
           (descr (gethash key data-edges))
           (description (mapcar #'ensure-list (ensure-list description)))
           (description (remove-duplicates description :test #'set-equal)))

      (if descr
          (setf (first descr) 
                (remove-duplicates
                 (nconc (first descr) description)
                 :test #'set-equal))
      
        (setf (gethash key data-edges)
              (list description nil)))))

  (values))


(defmethod get-node-successors ((substrate data-substrate) from)
  (with-slots (data-nodes) substrate
    (node-info-successors (gethash from data-nodes))))

(defmethod get-node-predecessors ((substrate data-substrate) to)
  (with-slots (data-nodes) substrate
    (node-info-predecessors (gethash to data-nodes))))

(defmethod loop-over-edges1 ((substrate data-substrate) fn)
  (with-slots (data-edges) substrate
    (maphash fn data-edges)))

(defmethod get-edge-info ((substrate data-substrate) from to)
  (with-slots (data-edges) substrate
    (multiple-value-bind (info foundp)
        (gethash (list from to) data-edges)
      (if foundp
          info 
        +no-descr-marker+))))

(defmethod edge-p ((substrate data-substrate) from to) 
  (with-slots (data-edges) substrate
    (multiple-value-bind (info foundp)
        (gethash (list from to) data-edges)
      (declare (ignorable info))
      foundp)))

;;;
;;; ---------------------------------------------------
;;;

(defmacro loop-over-edges ((substrate edge &optional info) &body body)
  (let ((var (or info (gensym))))
    `(loop-over-edges1 ,substrate
                       #'(lambda (,edge ,var)
                           (declare (ignorable ,edge ,var))
                           ,@body))))


(defmacro loop-over-successors ((substrate from &optional descr) (node-var &optional edge-info) &rest body)
  (let ((edge-info-var (or edge-info (gensym))))
    `(dolist (,node-var (get-node-successors substrate ,from))
       (let ((,edge-info-var (get-edge-between ,substrate ,from ,node-var)))
         (unless (eq ,edge-info-var +no-descr-marker+)
           (when (implies-p (edge-info-type ,edge-info-var) ,descr)
             ,@body))))))

(defmacro loop-over-predecessors ((substrate to &optional descr) (node-var &optional edge-info) &rest body)
  (let ((edge-info-var (or edge-info (gensym))))
    `(dolist (,node-var (get-node-predecessors substrate ,to))
       (let ((,edge-info-var (get-edge-between ,substrate ,node-var ,to)))
         (unless (eq ,edge-info-var +no-descr-marker+)
           (when (implies-p (edge-info-type ,edge-info-var) ,descr)
             ,@body))))))

;;;
;;;
;;;


(defmethod all-edges ((substrate data-substrate))
  (let ((edges nil))
    (loop-over-edges (substrate edge info)
      (push (list (edge-from edge) (edge-to edge) (edge-info-type info)) edges))
    edges))

;;;
;;;
;;;

(defmethod node-description ((substrate data-substrate) name)
  (let ((node (get-node-info substrate name)))
    (if (not (eq node +no-descr-marker+))
        (node-info-type node)
      :not-found)))

(defmethod edge-description ((substrate data-substrate) from to)
  (let ((edge (get-edge-info substrate from to)))
    (if (not (eq edge +no-descr-marker+))
        (edge-info-type edge)
      :not-found)))

;;;
;;; 
;;;

(defmethod delete-node :around ((substrate data-substrate) name &key told-info-p)
  (with-slots (told-info) substrate
    (if (not (node-p substrate name))
        :not-found
    
      (progn
        (dolist (to (get-node-successors substrate name))
          (delete-edge substrate name to))
        (dolist (from (get-node-predecessors substrate name))
          (delete-edge substrate from name))

        (call-next-method)
            
        (when told-info-p
          (push `(delete-node ,name) told-info))
      
        :okay-deleted))))


(defmethod delete-edge :around ((substrate data-substrate) from to &key told-info-p)
  (with-slots (told-info) substrate
    (if (not (edge-p substrate from to))
        :not-found

      (progn
        (call-next-method)
        (when told-info-p 
          (push `(delete-edge ,from ,to) told-info))
        :okay-deleted))))

;;;
;;; ---------------------------------------------------
;;; Methoden, die evtl. ueberschrieben werden muessen!
;;; ---------------------------------------------------
;;; 

(defmethod delete-node ((substrate data-substrate) name &key &allow-other-keys)
  (with-slots (data-nodes) substrate
    (remhash name data-nodes)))


(defmethod delete-edge ((substrate data-substrate) from to &key &allow-other-keys)
  (with-slots (data-edges) substrate
    (remhash (list from to) data-edges)))

;;;
;;; ---------------------------------------------------
;;; 


;;;
;;; Implication for conjuntive normal form, only positive atoms! 
;;;

(defmethod implies-p (descr-a descr-b)
  ;;; a         -> ( (a) )      =  (and (or a))         =  a 
  ;;; (a b)     -> ( (a) (b) )  =  (and (or a) (or b))  =  (and a b)
  ;;; ( (a b) ) -> ( (a b) )    =  (and (or a b))       =  (or a b)  

  (or (not descr-a)

      (unless (eq descr-a +no-descr-marker+)
        
        (let ((descr-a (mapcar #'ensure-list 
                               (ensure-list descr-a)))
              (descr-b (mapcar #'ensure-list 
                               (ensure-list descr-b))))
          (every #'(lambda (b) 
                     (some #'(lambda (a)
                               (subsetp a b :test #'(lambda (ia ib)
                                                      (if (functionp ib)
                                                          (funcall ib ia)
                                                        (equalp ia ib)))))
                           descr-a))
                 descr-b)))))

(defmethod implies-p (descr-a (descr-b data-substrate-node-query))
  (implies-p descr-a (textual-description descr-b)))

(defmethod implies-p (descr-a (descr-b data-substrate-edge-query))
  (implies-p descr-a (textual-description descr-b)))

(defmethod implies-p (descr-a (descr-b data-substrate-has-known-successor-retrieval-query))
  (implies-p descr-a (textual-description descr-b)))

;;;
;;;
;;;                    

(defmethod save-state ((substrate substrate))
  (loop-over-edges (substrate r r-info)
    (declare (ignorable r))
    (setf (edge-info-saved-type r-info) 
          (copy-tree (edge-info-type r-info))))
  (values))

(defmethod restore-state ((substrate substrate))
  (loop-over-edges (substrate r r-info)
    (declare (ignorable r))
    (setf (edge-info-type r-info) 
          (edge-info-saved-type r-info))
    (setf (edge-info-saved-type r-info) 
          nil))
  (values))


;;;
;;;
;;;

(defmethod describe-object ((substrate data-substrate) stream)
  (terpri)
  (loop-over-nodes (substrate node info)
    (format stream "Node   ~S~%       ~S~%" node info))
  (terpri) (terpri)
  (loop-over-edges (substrate edge info)
    (format stream "Edge   ~S~%       ~S~%" edge info))
  (values))
  

;;;
;;; Querying
;;;                    

(defmethod related-p ((substrate data-substrate) from to &optional descr)
  (let ((info (get-edge-between substrate from to)))
    (unless (eq info +no-descr-marker+)
      (implies-p (edge-info-type info)
                 descr))))
  
(defmethod predicate-related-p ((substrate data-substrate) from to &optional descr)
  (let ((a-conjuncts (node-info-type (get-node-info substrate from)))
        (b-conjuncts (node-info-type (get-node-info substrate to))))

    (unless (or (eq a-conjuncts +no-descr-marker+)
                (eq b-conjuncts +no-descr-marker+))

      (some #'(lambda (a-conjunct)
                ;;; eigentlich every!
                (some #'(lambda (b-conjunct)
                          ;;; eigentlich every! von der Logik her!
                          (some #'(lambda (a-disjunct)
                                    (some #'(lambda (b-disjunct)
                                              (every #'(lambda (descr)
                                                         (some #'(lambda (descr)
                                                                   (funcall descr
                                                                            a-disjunct
                                                                            b-disjunct))
                                                               (ensure-list descr)))
                                                     (textual-description descr)))
                                          b-conjunct))
                                a-conjunct))
                      b-conjuncts))
            a-conjuncts))))

  
;;;
;;;
;;;                    

(defmethod reset-substrate :after ((substrate data-substrate) &key &allow-other-keys)
  )

(defmethod reset-caches :after ((substrate data-substrate))
  nil)

;;;
;;;
;;;

(defmethod consistent-p ((substrate data-substrate))
  t)

;;;
;;;
;;;

(defmacro loop-over-all-successors ((substrate from &optional descr) (var) &rest body)
  `(loop-over-all-successors1 ,substrate ,from 
                              #'(lambda (,var)
                                  ,@body)
                              ,descr))


(defmacro loop-over-all-predecessors ((substrate from &optional descr) (var) &rest body)
  `(loop-over-all-predecessors1 ,substrate ,from 
                                #'(lambda (,var)
                                    ,@body)
                                ,descr))

;;;
;;; ---------------------------------------------------
;;; Adapter-Methoden die ueberschrieben werden muessen!
;;; ---------------------------------------------------
;;; 


(defmethod loop-over-all-successors1 ((substrate data-substrate) from fn &optional descr)
  (loop-over-successors (substrate from descr)
                        (to) 
                        (funcall fn to)))


(defmethod loop-over-all-predecessors1 ((substrate data-substrate) from fn &optional descr)
  (loop-over-predecessors (substrate from descr)
                          (to) 
                          (funcall fn to)))

;;;
;;; ---------------------------------------------------
;;;

;;;
;;; Functional API 
;;;

(defmacro with-data-box ((abox type-of-substrate)
                         &rest body)
  `(let* ((abox (or ,abox 
                    *nrql-abox* 
                    (current-abox)))
          (type-of-substrate (or ,type-of-substrate
                                 *type-of-substrate*))
          (*cur-substrate* 
           (find-racer-substrate abox type-of-substrate abox)))
    
     (if (not *cur-substrate*)
         (nrql-error "Can't find ~A of ABox ~A" type-of-substrate abox)
       (progn 
         ,@body))))

;;;
;;;
;;;

(nrql-defun create-data-node (name &key 
                                   abox 
                                   type-of-substrate
                                   (racer-descr nil racer-descr-supplied-p)
                                   (descr nil descr-supplied-p)
                                   (told-info-p t))
  (with-data-box (abox type-of-substrate)
                 (if descr-supplied-p
                     (node-instance *cur-substrate* 
                                    name
                                    :description descr
                                    :told-info-p told-info-p)
          
                   (node-instance *cur-substrate* name
                                  :told-info-p told-info-p))
        
                 (when racer-descr-supplied-p
                   (dl-prover-add-concept-assertion *cur-substrate* name racer-descr))
                 name))


(nrql-defun create-data-edge (from to descr &key 
                                   abox
                                   type-of-substrate
                                   (racer-descr nil racer-descr-supplied-p)
                                   (told-info-p t))

  (with-data-box (abox type-of-substrate)
                 (nodes-related *cur-substrate* from to descr
                                :told-info-p told-info-p)
    
                 (when racer-descr-supplied-p
                   (dl-prover-add-role-assertion *cur-substrate* from to racer-descr))
    
                 (list from to)))


(nrql-defun delete-data-node (name &key 
                                   abox 
                                   type-of-substrate
                                   (told-info-p t))
  (with-data-box (abox type-of-substrate)
                 (delete-node *cur-substrate* name
                              :told-info-p told-info-p)))

(nrql-defun delete-data-edge (from to &key 
                                   abox 
                                   type-of-substrate
                                   (told-info-p t))
  (with-data-box (abox type-of-substrate)
                 (delete-edge *cur-substrate* from to
                              :told-info-p told-info-p)))


(nrql-defun get-data-node-label (name &key 
                                      abox 
                                      type-of-substrate)
  (with-data-box (abox type-of-substrate)
                 (node-description *cur-substrate* name)))


(nrql-defun get-data-edge-label (from to &key 
                                      abox 
                                      type-of-substrate)
  (with-data-box (abox type-of-substrate)
                 (edge-description *cur-substrate* from to)))


;;;
;;; Macro API 
;;;

(nrql-defun set-data-box (name)
  (when (eq *type-of-substrate* 'racer-dummy-substrate)
    ;;; nur setzen, wenn nicht ein spezielleres Substrate
    ;;; erzeugt werden soll! 
    (setf *type-of-substrate* 'data-substrate))
  (racer-prepare-substrate :abox name :create-abox-if-not-found-p t))

(nrql-defmacro (in-data-box :nrql-function set-data-box))

;;;
;;;
;;;

(defun data-node1 (name &optional (descr nil descr-supplied-p) (racer-descr nil racer-descr-supplied-p) abox type-of-substrate)
  (if descr-supplied-p
      (if racer-descr-supplied-p
          (create-data-node name :descr descr :racer-descr racer-descr 
                            :told-info-p t 
                            :abox abox
                            :type-of-substrate type-of-substrate)
        (create-data-node name :descr descr
                          :told-info-p t
                          :abox abox 
                          :type-of-substrate type-of-substrate))
    (create-data-node name
                      :told-info-p t
                      :abox abox 
                      :type-of-substrate type-of-substrate)))

(defun data-edge1 (from to data-relation &optional (racer-descr nil racer-descr-supplied-p) abox type-of-substrate)
  (if racer-descr-supplied-p
      (create-data-edge from to data-relation :racer-descr racer-descr
                        :told-info-p t 
                        :abox abox
                        :type-of-substrate type-of-substrate)
    (create-data-edge from to data-relation 
                      :told-info-p t
                      :abox abox
                      :type-of-substrate type-of-substrate)))

(defun del-data-node1 (name &optional abox type-of-substrate)
  (delete-data-node name 
                    :told-info-p t
                    :abox abox 
                    :type-of-substrate type-of-substrate))

(defun del-data-edge1 (from to &optional abox type-of-substrate)
  (delete-data-edge from to
                    :told-info-p t
                    :abox abox 
                    :type-of-substrate type-of-substrate))
  
(defun node-label1 (name &optional abox type-of-substrate)
  (get-data-node-label name 
                       :abox abox
                       :type-of-substrate type-of-substrate))

(defun edge-label1 (from to &optional abox type-of-substrate)
  (get-data-edge-label from to
                       :abox abox
                       :type-of-substrate type-of-substrate))

;;;
;;;
;;;

(nrql-defmacro (data-node :nrql-function data-node1))

(nrql-defmacro (data-edge :nrql-function data-edge1))

(nrql-defmacro (del-data-node :nrql-function del-data-node1))

(nrql-defmacro (del-data-edge :nrql-function del-data-edge1))
  
(nrql-defmacro (node-label :nrql-function node-label1))

(nrql-defmacro (edge-label :nrql-function edge-label1))

;;;
;;;
;;;

(defquery-code (data-substrate-node-query)
  (:tester 
   (:runtime
    (continuation &rest args &key var &allow-other-keys)
    (with-slots (negated-p voi substrate) query
      (let ((var (or var (bound-to voi))))
        (if negated-p
            (unless (implies-p (node-info-type
                                (get-node-info substrate var))
                               query)
              (apply continuation :var var args))
          (when (implies-p (node-info-type 
                            (get-node-info substrate var))
                           query)
            (apply continuation :var var args)))))))
  (:enumerator
   (:runtime 
    (continuation &rest args &key &allow-other-keys)
    (with-slots (negated-p substrate substrate) query    
      (if negated-p
          (loop-over-nodes (substrate var info)
            (unless (implies-p (node-info-type info) query)
              (apply continuation :var var args)))
        (loop-over-nodes (substrate var info)
          (when (implies-p (node-info-type info) query)
            (apply continuation :var var args))))))))


(defquery-code (data-substrate-edge-query)
  (:tester
   (:runtime
    (continuation &rest args &key from to &allow-other-keys)
    (with-slots (negated-p substrate voi-from voi-to) query
      (let ((from (or from (bound-to voi-from)))
            (to (or to (bound-to voi-to))))
        (if negated-p
            (unless (related-p substrate from to query)
              (apply continuation :from from :to to args))
          (when (related-p substrate from to query)
            (apply continuation :from from :to to args)))))))
  
  (:from-bound-enumerator 
   (:runtime 
    (continuation &rest args &key &allow-other-keys)
    (with-slots (negated-p substrate voi-from) query    
      (if negated-p
          (loop-over-nodes (substrate var)
            (unless (related-p (bound-to voi-from) var query)
              (apply continuation :to var args)))

        (loop-over-all-successors (substrate (bound-to voi-from) query)
                                  (var)
                                  (apply continuation :to var args))))))

  (:to-bound-enumerator
   (:runtime 
    (continuation &rest args &key &allow-other-keys)
    (with-slots (negated-p substrate voi-to) query    
      (if negated-p
          (loop-over-nodes (substrate var)
            (unless (related-p var (bound-to voi-to) query)
              (apply continuation :from var args)))

        (loop-over-all-predecessors (substrate (bound-to voi-to) query)
                                    (var)
                                    (apply continuation :from var args))))))


  (:enumerator
   (:runtime
    (continuation &rest args &key &allow-other-keys)
    (with-slots (negated-p substrate) query
      (if negated-p
          (if (eq (voi-from query)
                  (voi-to query))
              (loop-over-nodes (substrate from)
                (unless (related-p substrate from from query)
                  (apply continuation :from from :to from args)))
            (loop-over-nodes (substrate from)
              (loop-over-nodes (substrate to)
                (unless (related-p substrate from to query)
                  (apply continuation :from from :to to args)))))
        (if (eq (voi-from query)
                (voi-to query))        
            (loop-over-nodes (substrate from)
              (when (related-p substrate from from query)
                (apply continuation :from from :to from args)))
          (loop-over-nodes (substrate from)
            (loop-over-nodes (substrate to)
              (when (related-p substrate from to query)
                (apply continuation :from from :to to args))))))))))



(defquery-code (data-substrate-has-known-successor-retrieval-query)
  (:tester 
   (:runtime
    (continuation &rest args &key var &allow-other-keys)
    (with-slots (negated-p substrate voi) query    
      (let* ((var (or var (bound-to voi)))
             (succ 
              (catch 'found
                (loop-over-all-successors (substrate var query)
                                          (succ)
                                          (throw 'found succ)))))
        (when (or (and (not negated-p) succ)
                  (and negated-p (not succ)))
          (apply continuation :var var args))))))
  
  (:enumerator
   (:runtime 
    (continuation &rest args &key &allow-other-keys)
    (with-slots (negated-p substrate substrate) query    
      (loop-over-nodes (substrate var)
        (let ((succ 
               (catch 'found
                 (loop-over-all-successors (substrate var query)
                                           (succ)
                                           (throw 'found succ)))))
          (when (or (and (not negated-p) succ)
                    (and negated-p (not succ)))
            (apply continuation :var var args))))))))


(defquery-code (data-substrate-predicate-edge-query)
  (:tester
   (:runtime
    (continuation &rest args &key from to &allow-other-keys)
    (with-slots (negated-p substrate voi-from voi-to) query
      (let ((from (or from (bound-to voi-from)))
            (to (or to (bound-to voi-to))))
        (if negated-p
            (unless (predicate-related-p substrate from to query)
              (apply continuation :from from :to to args))
          (when (predicate-related-p substrate from to query)
            (apply continuation :from from :to to args)))))))
  
  (:from-bound-enumerator 
   (:runtime 
    (continuation &rest args &key &allow-other-keys)
    (with-slots (negated-p substrate voi-from) query    
      (if negated-p
          (loop-over-nodes (substrate var)
            (unless (predicate-related-p substrate (bound-to voi-from) var query)
              (apply continuation :to var args)))

        (loop-over-nodes (substrate var)
          (when (predicate-related-p substrate (bound-to voi-from) var query)
            (apply continuation :to var args)))))))

  (:to-bound-enumerator
   (:runtime 
    (continuation &rest args &key &allow-other-keys)
    (with-slots (negated-p substrate voi-to) query    
      (if negated-p
          (loop-over-nodes (substrate var)
            (unless (predicate-related-p substrate var (bound-to voi-to) query)
              (apply continuation :from var args)))

        (loop-over-nodes (substrate var)
          (when (predicate-related-p substrate var (bound-to voi-to) query)
            (apply continuation :from var args)))))))

  (:enumerator
   (:runtime
    (continuation &rest args &key &allow-other-keys)
    (with-slots (negated-p substrate) query
      (if negated-p
          (if (eq (voi-from query)
                  (voi-to query))
              (loop-over-nodes (substrate from)
                (unless (predicate-related-p substrate from from query)
                  (apply continuation :from from :to from args)))
            (loop-over-nodes (substrate from)
              (loop-over-nodes (substrate to)
                (unless (predicate-related-p substrate from to query)
                  (apply continuation :from from :to to args)))))
        (if (eq (voi-from query)
                (voi-to query))        
            (loop-over-nodes (substrate from)
              (when (predicate-related-p substrate from from query)
                (apply continuation :from from :to from args)))
          (loop-over-nodes (substrate from)
            (loop-over-nodes (substrate to)
              (when (predicate-related-p substrate from to query)
                (apply continuation :from from :to to args))))))))))


;;;
;;; Optimizer
;;;

(defmethod get-score ((query data-substrate-node-query))
  (if (bound-to (voi query))
      (values 100 :tester)
    (if (negated-p query) 
        (values 70 :enumerator)
      (values 80 :enumerator))))

(defmethod get-score ((query data-substrate-edge-query))
  (if (negated-p query)       
      (if (bound-to (voi-from query))
          (if (bound-to (voi-to query))
              (values 95 :tester)
            (values 60 :from-is-bound-enumerator))
        (if (bound-to (voi-to query))
            (values 60 :to-is-bound-enumerator)
          (values 20 :enumerator)))

    (if (bound-to (voi-from query))
        (if (bound-to (voi-to query))
            (values 96 :tester)
          (values 60 :from-is-bound-enumerator))
      (if (bound-to (voi-to query))
          (values 96 :to-is-bound-enumerator)
        (values 60 :enumerator)))))


(defmethod get-score ((query data-substrate-predicate-edge-query))
  (if (negated-p query)       
      (if (bound-to (voi-from query))
          (if (bound-to (voi-to query))
              (values 95 :tester)
            (values 20 :from-is-bound-enumerator))
        (if (bound-to (voi-to query))
            (values 20 :to-is-bound-enumerator)
          (values 10 :enumerator)))

    (if (bound-to (voi-from query))
        (if (bound-to (voi-to query))
            (values 94 :tester)
          (values 90 :from-is-bound-enumerator))
      (if (bound-to (voi-to query))
          (values 90 :to-is-bound-enumerator)
        (values 40 :enumerator)))))

;;;
;;; Reasoning 
;;;

(defmethod data-conjuncts-consistent-p ((from-query and-query) (conjuncts list) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  t)


(nrql-defun description-implies-p (a b)
  (implies-p a b))

(nrql-defmacro (description-implies? :nrql-function implies-p))


