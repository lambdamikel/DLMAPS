;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(defun sort-choice-points (cps)
  (sort (copy-list cps) #'>))

;;;
;;;
;;;

(defmacro avl-tree-insert (entry avl-tree)
  ;;; entry = (concept info) 
  `(insert-into-avl-tree ,entry ,avl-tree
                         :key #'first
                         :match-fn #'(lambda (x y)
                                       (= (id x) (id y)))
                         :compare-fn #'(lambda (x y)
                                         (< (id x) (id y)))))


(defmacro avl-tree-find (concept avl-tree)
  `(find-in-avl-tree ,concept ,avl-tree
                     :apply-key-to-item-argument-p nil
                     :key #'first
                     :match-fn #'(lambda (x y)
                                   (= (id x) (id y)))
                     :compare-fn #'(lambda (x y)
                                     (< (id x) (id y)))))

(defmacro avl-tree-delete (concept avl-tree)
  `(delete-from-avl-tree ,concept ,avl-tree
                         :apply-key-to-item-argument-p nil
                         :key #'first
                         :match-fn #'(lambda (x y)
                                       (= (id x) (id y)))
                         :compare-fn #'(lambda (x y)
                                         (< (id x) (id y)))))


;;;
;;; Modell Checking: 
;;;

#|

(defmethod satisfied-p ((concept top-concept) (node abox-node))
  t)

(defmethod satisfied-p ((concept bottom-concept) (node abox-node))
  nil)

(defmethod satisfied-p ((concept atomic-concept) (node abox-node))
  (eq (truth-value concept node) 'true))

(defmethod satisfied-p ((concept and-concept) (node abox-node))
  (eq (truth-value concept node) 'true))

(defmethod satisfied-p ((concept or-concept) (node abox-node))
  (eq (truth-value concept node) 'true))

(defmethod satisfied-p ((concept some-concept) (node abox-node))
  (some #'(lambda (succ)
            (satisfied-p (qualification concept) succ))
        (get-role-successors node (role concept))))

(defmethod satisfied-p ((concept all-concept) (node abox-node))
  (every #'(lambda (succ)
             (satisfied-p (qualification concept) succ))
         (get-role-successors node (role concept))))

  
(defmethod model-p ((abox abox) (concept concept))
  (some #'(lambda (node) 
            (satisfied-p concept node))
        (get-nodes abox)))

(defmethod globally-satisfied-p ((abox abox) (concept concept))
  (every #'(lambda (node) 
             (satisfied-p concept node))
         (get-nodes abox)))                    


|#


;;;
;;;
;;;

(defmethod get-expanded-concept-lists (node)
  (let ((atoms nil)
        (somes nil)
        (alls nil)
        (ands nil)
        (ors nil)
        (features nil)
        (at-leasts nil)
        (at-mosts nil))
    
    (loop-over-node-slot (concept node expanded-atomic-concepts)
                         (push concept atoms))
    
    (loop-over-node-slot (concept node expanded-and-concepts)
                         (push concept ands))
    
    (loop-over-node-slot (concept node expanded-or-concepts)
                         (push concept ors))

    (loop-over-node-slot (concept node expanded-some-concepts)
                         (push concept somes))
    
    (loop-over-node-slot (concept node expanded-all-concepts)
                         (push concept alls))

    (loop-over-node-slot (concept node expanded-attribute-exists-concepts)
                         (push concept features))

    (loop-over-node-slot (concept node expanded-at-least-concepts)
                         (push concept at-leasts))

    (loop-over-node-slot (concept node expanded-at-most-concepts)
                         (push concept at-mosts))

    (values atoms ands ors somes alls features at-leasts at-mosts)))


(defmethod get-unexpanded-concept-lists (node)
  (let ((atoms nil)
        (somes nil)
        (alls nil)
        (ands nil)
        (ors nil)
        (features nil)
        (at-leasts nil)
        (at-mosts nil))
    
    (loop-over-node-slot (concept node unexpanded-atomic-concepts)
                         (push concept atoms))
    
    (loop-over-node-slot (concept node unexpanded-and-concepts)
                         (push concept ands))
    
    (loop-over-node-slot (concept node unexpanded-or-concepts)
                         (push concept ors))

    (loop-over-node-slot (concept node unexpanded-some-concepts)
                         (push concept somes))
    
    (loop-over-node-slot (concept node unexpanded-all-concepts)
                         (push concept alls))

    (loop-over-node-slot (concept node unexpanded-attribute-exists-concepts)
                         (push concept features))

    (loop-over-node-slot (concept node unexpanded-at-least-concepts)
                         (push concept at-leasts))

    (loop-over-node-slot (concept node unexpanded-at-most-concepts)
                         (push concept at-mosts))


    (values atoms ands ors somes alls features at-leasts at-mosts)))

;;;
;;;
;;;

(defmethod describe-object ((node abox-node) stream) 
  (labels ((concepts (concepts)
             (mapcar #'(lambda (c) 
                         (list c (get-choice-points c :node node)))
                     concepts)))

    (multiple-value-bind (u-atoms u-ands u-ors u-somes u-alls u-features u-at-leasts u-at-mosts)
        (get-unexpanded-concept-lists node)
      (multiple-value-bind (e-atoms e-ands e-ors e-somes e-alls e-features e-at-leasts e-at-mosts)
          (get-expanded-concept-lists node)
        (let ((u-atoms (sort u-atoms
                             #'string-lessp
                             :key #'(lambda (x) (symbol-name (name x)))))
              (e-atoms (sort e-atoms
                             #'string-lessp
                             :key #'(lambda (x) (symbol-name (name x))))))            
          (format stream 
                  "~%~%#<ABOX Node
   ID               ~W
   NAME             ~W
   REPRESENTATIVE   ~W
   CLUSTER NODES    ~W
   OLD              ~W
   INITIAL CONCEPT  ~W
   CREATOR          ~W
   BLOCKED BY       ~W 
   BLOCKING FOR     ~W 
   ACTIVE           ~W 
   STABLE           ~W
   DETERM.EXPANDED  ~W
   REALLY SAT       ~W
   CACHE SAT        ~W
   DELETED          ~W
   TOLD             ~W
   -------------------------------
   UNEXPANDED POSITIVE ATOMS    ~W 
   UNEXPANDED NEGATIVE ATOMS    ~W
   UNEXPANDED          ANDS     ~W
   UNEXPANDED          ORS      ~W
   UNEXPANDED          SOMES    ~W
   UNEXPANDED          ALLS     ~W
   UNEXPANDED          FEATURES ~W
   UNEXPANDED          AT-LEAST ~W
   UNEXPANDED          AT-MOST  ~W
   -------------------------------
   EXPANDED POSITIVE ATOMS      ~W
   EXPANDED NEGATIVE ATOMS      ~W
   EXPANDED          ANDS       ~W
   EXPANDED          ORS        ~W
   EXPANDED          SOMES      ~W
   EXPANDED          ALLS       ~W
   EXPANDED          FEATURES   ~W
   EXPANDED          AT-LEAST   ~W
   EXPANDED          AT-MOST    ~W~%>"

                  (slot-value node 'id)
                  (name node)
                  
                  (representative-p node)

                  (mapcar #'id (cluster-nodes node))

                  (old-p node)
                  (initial-concept node)
                  
                  (created-by node)

                  (slot-value node 'blocked-by)
                  (slot-value node 'blocking-for)
                  
                  (slot-value node 'active-p)
                  (slot-value node 'stable-p)
                  (slot-value node 'deterministically-expanded-p)
                  (slot-value node 'really-satisfiable-p)
                  (slot-value node 'cache-satisfiable-p)
                  (slot-value node 'deleted-p)
                  
                  (slot-value node 'told-concepts)
                  
                  (concepts (remove-if #'negated-p u-atoms))
                  (concepts (remove-if-not #'negated-p u-atoms))

                  (concepts u-ands)
                  (concepts u-ors)
                  (concepts u-somes)
                  (concepts u-alls)
                  (concepts u-features)
                  (concepts u-at-leasts)
                  (concepts u-at-mosts)

                  (concepts (remove-if #'negated-p e-atoms))
                  (concepts (remove-if-not #'negated-p e-atoms))

                  (concepts e-ands)
                  (concepts e-ors)
                  (concepts e-somes)
                  (concepts e-alls)
                  (concepts e-features)
                  (concepts e-at-leasts)
                  (concepts e-at-mosts)))))))


(defmethod describe-object ((abox abox) stream)

  (format stream 
          "#<~A ~A, 
 Nodes    : ~A, 
 Old      : ~A, 
 Active   : ~A, 
 Inactive : ~A, 
 Blocked  : ~A, 
 Leafs    : ~A, 
 Cache Sat: ~A,
 Edges: ~A>~%"  

          (type-of abox)
          (name abox)
            
          (mapcar #'id (get-nodes abox))
          (mapcar #'id (get-old-nodes abox))
          (mapcar #'id (get-active-nodes abox))
          (mapcar #'id (get-deactivated-nodes abox))
          (mapcar #'id (get-blocked-nodes abox))
          (mapcar #'id (get-leaf-nodes abox))
          (mapcar #'id (get-cache-sat-nodes abox))
            
          (let ((*print-pretty* t))
            (mapcar #'(lambda (x) 
                        (format nil "(~A,~A):~A[~A] x ~A"
                                (id (from x)) (id (to x))
                                (multiplicity x)                                
                                (inverse-multiplicity x)                                
                                (textual-description (description x))))
                    (get-edges abox)))))


(defmethod describe-object :after ((abox abox1) stream)
  (with-slots (active-nodes 
               cache-sat-nodes
               deactivated-nodes 
               old-nodes
               leaf-nodes
               blocked-nodes

               nodes-not-and-expanded
               nodes-not-atom-expanded
               nodes-not-det-expanded 
               nodes-not-or-expanded
               nodes-not-or-expanded1
               nodes-not-some-expanded
               nodes-not-some-expanded1
               nodes-not-feature-expanded
               nodes-not-feature-expanded1
               nodes-not-at-least-expanded
               nodes-not-at-least-expanded1) abox
    
    (format stream 
          "#<~A ~A, (M=Maintained) Index Structures:  
 Old      : M=~A ~A, 
 Active   : M=~A ~A, 
 Inactive : M=~A ~A, 
 Blocked  : M=~A ~A, 
 Leafs    : M=~A ~A, 
 Cache Sat: M=~A ~A,

 nodes-not-or-expanded        : M=~A, ~A, 
 nodes-not-or-expanded1       : M=~A, ~A, 
 nodes-not-some-expanded      : M=~A, ~A, 
 nodes-not-some-expanded1     : M=~A, ~A, 
 nodes-not-feature-expanded   : M=~A, ~A, 
 nodes-not-feature-expanded1  : M=~A, ~A, 
 nodes-not-at-least-expanded  : M=~A, ~A, 
 nodes-not-at-least-expanded1 : M=~A, ~A>~%"  
          
          (type-of abox)
          (name abox)
         
          *maintain-old-nodes-p* (mapcar #'id old-nodes)
          *maintain-active-nodes-p*
          (if active-nodes 
              (mapcar #'id (heap-items active-nodes))
            :no-heap)
          *maintain-deactivated-nodes-p* (mapcar #'id deactivated-nodes)
          *maintain-blocked-nodes-p* (mapcar #'id blocked-nodes)
          *maintain-leaf-nodes-p* (mapcar #'id leaf-nodes)
          *maintain-cache-sat-nodes-p* (mapcar #'id cache-sat-nodes)

          *maintain-unexpanded-or-concepts-heap-p* 
          (mapcar #'id (heap-items nodes-not-or-expanded))
                  
          *maintain-unexpanded-or-concepts1-heap-p*
          (mapcar #'id (heap-items nodes-not-or-expanded1))

          *maintain-unexpanded-some-concepts-heap-p*
          (mapcar #'id (heap-items nodes-not-some-expanded))

          *maintain-unexpanded-some-concepts1-heap-p*
          (mapcar #'id (heap-items nodes-not-some-expanded1))


          *maintain-unexpanded-attribute-exists-concepts-heap-p*
          (mapcar #'id (heap-items nodes-not-feature-expanded))

          *maintain-unexpanded-attribute-exists-concepts1-heap-p*
          (mapcar #'id (heap-items nodes-not-feature-expanded1))

          *maintain-unexpanded-at-least-concepts-heap-p*
          (mapcar #'id (heap-items nodes-not-at-least-expanded))

          *maintain-unexpanded-at-least-concepts1-heap-p*
          (mapcar #'id (heap-items nodes-not-at-least-expanded1)))))


(defmethod describe-object ((edge abox-edge) stream)
  (let ((*print-pretty* t))
    (format stream 
            "#<~A/~A = (~A/~A,~A/~A): ~A x ~A  [~A] / ~A [~A], ~A x ~A>"
            (name edge)
            (id edge)

            (name (from edge))
            (id (from edge))
          
            (name (to edge))
            (id (to edge))

            (multiplicity edge)
          
            (role edge)
            (slot-value edge 'choice-points)
          
            (if (aux-description edge)
                (textual-description (aux-description edge))                        
              'none)
          
            (if (aux-description edge)
                (slot-value (aux-description edge) 'choice-points)
              '())

            (inverse-multiplicity edge)
            `(inv ,(role edge)))))
          

;;;
;;;
;;;

(defmethod kind ((node abox-node))
  (mapcar #'(lambda (x) 
              (sort x #'< :key #'id))
          (cons (slot-value node 'told-concepts)
                (multiple-value-call #'list (get-expanded-concept-lists node))))) 

(defmethod kind ((edge abox-edge))
  (role edge))

;;;
;;;
;;;

(defmethod info ((item abox-node) &key &allow-other-keys)
  (format nil "ID:~A NAME:~A" (id item) (name item)))

(defmethod info ((item abox-edge) &key for-graph-visualizer-p)
  (let ((*print-pretty* t))
    (if for-graph-visualizer-p 
        (format nil "<~A,~A>: ~A-~A / ~A-(INV ~A)" 
              
                (id (from item))
                (id (to item))
                (multiplicity item)
                (role item)
                (inverse-multiplicity item)
                (role item))

      (format nil "~A/~A <~A,~A>: ~A-~A / ~A-(INV ~A)," 
              (name item) 
              (id item) 
            
              (id (from item)) (id (to item))
              (multiplicity item)
              (role item)
            
              (inverse-multiplicity item)
              (role item)))))


(defmethod info ((abox abox) &key  &allow-other-keys)
  (describe abox nil))
            
;;;
;;;
;;;

(defmethod print-object ((abox abox) stream)
  (format stream "#<~A ~A/~A/~A>"
          (type-of abox)
          (name abox)
          (id abox)
          (satisfiable abox)))

(defmethod print-object ((item abox-node) stream)
  (format stream "#<~A ~A>" (type-of item) (info item)))

(defmethod print-object ((item abox-edge) stream)
  (format stream "#<~A ~A ~A>" (type-of item) (info item) (get-choice-points item)))

;;;
;;;
;;;

(defun latex-translate-concept (concept)
  (if (symbolp concept)
      (format nil "\\mi{~(~A~)}" 
	      (stringsubst-char-with-string 
	       (symbol-name concept)
	       #\-
	       "\\_"))
    (let ((op (first concept)))
      (case op
	(not 
	 (format nil "\\neg ~A"
		 (latex-translate-concept (second concept))))
	((and or) 
	 (let ((args 		
		(splice-in 
		 (if (eq op 'or) 
		     "\\sqcup"
		   "\\sqcap")
		 (mapcar #'latex-translate-concept (rest concept)))))
	   (format nil "~A" args)))
	(=> 
	 (format nil "~A \\Rightarrow ~A" 
		 (latex-translate-concept (second concept))
		 (latex-translate-concept (third concept))))
	(<= 
	 (format nil "~A \\Leftarrow ~A" 
		 (latex-translate-concept (second concept))
		 (latex-translate-concept (third concept))))	
	(<=>
	 (format nil "~A \\Leftrightarrow ~A" 
		 (latex-translate-concept (second concept))
		 (latex-translate-concept (third concept))))	
	(some 
	 (format nil "( \\dex{\\rn{~A}}{~A} )"
		 (second concept)
		 (latex-translate-concept (third concept))))
	(all 
	 (format nil "( \\dax{\\rn{~A}}{~A} )"
		 (second concept)
		 (latex-translate-concept (third concept))))))))


#|

(defmethod latex-export ((obj tbox) fn)
  (with-open-file (stream fn :direction :output :if-exists :supersede)
    (format stream "\\begin{equalities}~%")
    (dolist (def (reverse (definitions obj)))
      (when (org-concept def)
	(if (primitive-p def)
	    (format stream "\\tsubs{~A}{~A}~%"
		    (translate-concept (name def))
		    (translate-concept (org-concept def)))
	  (format stream "\\tequi{~A}{~A}~%"
		  (translate-concept (name def))
		  (translate-concept (org-concept def))))))
    (format stream "\\end{equalities}~%")))
|#


;;;
;;;
;;;

(defun visualize-taxonomy (&optional (tbox *cur-tbox*))
  (visualize-dag (taxonomy (find-tbox tbox :error-p t))
		 :view
		 #+:clim :graphically
		 #-:clim :textually))

#+:clim
(defun visualize-abox (&optional (abox *cur-abox*))
  (visualize (find-abox abox :error-p t))) 




(defun compute-all-subsets (set &optional (akku '(nil)))
  (let ((newakku akku))
    (mapl #'(lambda (lset)
	      (let ((item (first lset)))		    
		(unless (some #'(lambda (set) (member item set)) newakku)
		  (dolist (expand akku)
		    (let ((new (cons item expand)))
		      (push new newakku)))
		  (setf newakku (compute-all-subsets (rest lset) newakku)))))
	  set)
    newakku))


(defun compute-all-subsets-of-cardinality (set m)
  (let ((all nil))
    (labels ((do-it (set n) 
               (cond ((zerop n)
                      (list nil))
                     (t                     
                      (let ((local nil))
                        (mapl #'(lambda (res) 
                                  (let* ((item (first res))
                                         (res (do-it (rest res) (1- n))))
                            
                                    (dolist (res res)
                                      (push (cons item res) local)
                                      (when (= m n)
                                        (push (cons item res) all)))))
                              set)
                        local)))))

      (do-it set m)

      all)))


(defmacro loop-over-subsets-of-cardinality ((subset set card) &body body)
  (let ((n (gensym))
        (akku (gensym))
        (remset (gensym))
        (i (gensym)))
    
    `(labels ((do-it (,remset ,n ,akku)
                (if (zerop ,n)
                    (let ((,subset ,akku))
                      ,@body)
                  (loop as ,remset on ,remset
                        as ,i = (first ,remset) do
                        (do-it (rest ,remset)
                               (1- ,n) 
                               (cons ,i ,akku))))))

       (do-it ,set ,card nil))))

;;;
;;;
;;;

(defun topo-sort (nodes smaller-p)
  (labels ((do-it (root-nodes)
             (let ((next-layer
                    (delete-duplicates
                     (remove-if-not #'(lambda (y)
                                        (some #'(lambda (x) 
                                                  (funcall smaller-p x y))
                                              root-nodes))
                                    
                                    nodes))))

               (nconc root-nodes
                       (when next-layer
                         (do-it next-layer))))))
                       
    (let ((root-nodes 
           (remove-if #'(lambda (x)
                          (some #'(lambda (y)
                                    (funcall smaller-p y x))
                                nodes))
                      nodes)))

      (delete-duplicates (do-it root-nodes)))))
         

(defun find-cluster (nodes smaller-p)
  (setf nodes (remove-duplicates nodes))
  (labels ((do-it (start-node node cycle rem-nodes) 
             
             (if (member node cycle)
                 cycle

               (let ((next-layer
                      (remove-if-not #'(lambda (y)
                                         (funcall smaller-p node y))
                                     rem-nodes)))
                 
                 (let ((res nil))
                   (dolist (next next-layer)
                     (dolist (found
                              (do-it start-node next (cons node cycle)
                                     (remove next rem-nodes)))
                       (push found res)))
                   res)))))
    
    (let ((clusters nil))
      
      (loop while nodes do
            (let* ((node (first nodes))
                   (cluster 
                    (do-it node node nil nodes)))
              
              (setf nodes (set-difference nodes (cons node cluster)))
              (when cluster (push cluster clusters))))

      clusters)))

;;;
;;;
;;;

(defun concepts-left ()
  (- (length *nodes*)
     (position *node* *nodes*)))

(defun monitor ()
  (loop (sleep 1)
        (princ *concept*) 
        (princ " ") 
        (princ *node*)
        (princ " ") 
        (princ (concepts-left))
        (terpri)))


(defun statistics-monitor ()
  (reset-statistics)
  (loop (sleep 3)
        (show-statistics)))

