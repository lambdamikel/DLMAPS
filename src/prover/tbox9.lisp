;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;; TBoxen
;;;

(defun mht ()
  (make-hash-table :size 10000 :rehash-size 10000))

(defpersistentclass tbox ()
  ((name :accessor name :initform nil :initarg :name)

   (tbox-version :accessor tbox-version :initform 0)
   (prepared-p :reader prepared-p :initform nil)
   (needs-blocking-p :reader needs-blocking-p :initform nil)
   
   (rbox :accessor rbox :initform nil :initarg :rbox)

   (used-by-aboxes :accessor used-by-aboxes :initform nil)

   (orig-axioms-with-left-side  :accessor orig-axioms-with-left-side  :initform (mht))
   (orig-axioms-with-right-side :accessor orig-axioms-with-right-side :initform (mht))
      
   (new-axioms-with-left-side  :accessor new-axioms-with-left-side  :initform (mht))
   (new-axioms-with-right-side :accessor new-axioms-with-right-side :initform (mht))

   (equivalent-names :accessor equivalent-names :initform (mht))
   (primitive-names  :accessor primitive-names :initform nil)
   (defined-names  :accessor defined-names :initform nil)
   (cyclic-names  :accessor cyclic-names :initform nil)

   (language :initform nil)

   (in-definition-order-p :accessor in-definition-order-p :initform nil) 
   
   (told-subsumers-computed-p :accessor told-subsumers-computed-p :initform nil)

   (satisfiable :reader satisfiable :initform :not-tested) 

   (meta-constraints :reader get-meta-constraints :initform nil)

   (taxonomy :initform nil)

   (axiom-counter :accessor axiom-counter :initform 0) 

   (concept-store :reader concept-store :initarg :concept-store)

   (classification-order :accessor classification-order :initform nil)))


;;;
;;;
;;;

(defmethod get-all-atoms ((tbox tbox))
  (all-atoms (concept-store tbox)))

(defmethod get-all-positive-atoms ((tbox tbox))
  (positive-atoms (concept-store tbox)))

(defmethod get-all-negative-atoms ((tbox tbox))
  (negative-atoms (concept-store tbox)))

;;;
;;;
;;;

(defmethod get-abox-type-for-tbox ((tbox tbox))
  (let ((rbox (rbox tbox)))
    (etypecase rbox
      ; (null 'abox)
      (null (get-standard-abox-class))
      (jepd-rolebox 'jepd-abox)
      (rolebox 'rolebox-abox))))

(defmethod get-abox-type-for-tbox ((tbox null))
  (get-standard-abox-class))

;;;
;;;
;;;

(defmethod tbox-classified-p ((tbox tbox))
  (when (slot-value tbox 'taxonomy)
    t))

;;;
;;;
;;;

(defpersistentclass taxonomy-node (dag-node)
  ((concept :accessor concept :initarg :concept :initform nil)
   (definition :accessor definition :initarg :definition :initform nil)
   (equivalents :accessor equivalents :initform nil)))


(defmethod print-object ((node taxonomy-node) stream)
  (format stream "#<~A ~A>"
          (type-of node)
          (concept node)))

;;;
;;;
;;;


(defpersistentclass taxonomy (dag)
  ((tbox :reader tbox :initform nil :initarg :tbox)))

(defpersistentclass used-by-dag (dag))


(defmethod print-object ((taxonomy taxonomy) stream)
  (format stream "#<~A ~A ~A>"
          (type-of taxonomy)
          (dag-name taxonomy)
          (tbox taxonomy)))

;;;
;;;
;;;


(defpersistentclass axiom ()
  ((id :reader id :initarg :id)

   (left :accessor left :initarg :left)
   (right :accessor right :initarg :right)
   
   (or-concept :accessor or-concept :initarg :or-concept)

   (primitive-p :accessor primitive-p :initform nil :initarg :primitive-p)
   (name-equivalence-axiom-p :accessor name-equivalence-axiom-p :initform nil :initarg
                             :name-equivalence-axiom-p)

   (role-domain-axiom-p :accessor role-domain-axiom-p :initform nil :initarg :role-domain-axiom-p)
   (role-range-axiom-p :accessor role-range-axiom-p :initform nil :initarg :role-range-axiom-p)

   (marked-p :accessor marked-p :initform nil :initarg :marked-p)

   (gci-p :accessor gci-p :initform nil :initarg :gci-p)))


(defmethod print-object ((axiom axiom) stream)
  (if *print-pretty*
      (if (primitive-p axiom)
          (format stream "(~A ~A ~A)~%" 
                  (if (gci-p axiom) 'implies 'def*)
                  (left axiom) (right axiom))
        (format stream "(~A ~A ~A)~%"
                (if (gci-p axiom) 'equivalent 'def)
                (left axiom) (right axiom)))
    (if (primitive-p axiom)
        (format stream "#<~A: ~A => ~A>~%" 
                (if (gci-p axiom) 'GCI 'DEF*)
                (left axiom) (right axiom))
      (format stream "#<~A: ~A = ~A>~%" 
              (if (gci-p axiom) 'GCI 'DEF)
              (left axiom) (right axiom)))))

(defmethod unparse ((axiom axiom))
  (let ((*print-pretty* t))
    (read-from-string 
     (format nil "|~A|" (print-object axiom nil)))))

;;;
;;;
;;;    

(defmethod print-object ((tbox tbox) stream)
  (if (prepared-p tbox)
      (progn 
        (format stream "#<TBOX ~A, PREPARED, LANGUAGE ~A, " (name tbox) (get-language tbox))
        (when (slot-value tbox 'taxonomy)
          (format stream " CLASSIFIED,"))
        (unless (eq (slot-value tbox 'satisfiable) :not-tested)
          (format stream " SATISFIABLE: ~A," (slot-value tbox 'satisfiable)))
        (format stream " ~A PCDs, ~A CDs, ~A GCIs, ~A MCs>"
                (length (get-primitive-concept-definitions tbox))
                (length (get-defined-concept-definitions tbox))
                (length (get-gcis tbox))
                (length (get-meta-constraints tbox))))

    (progn 
      (format stream "#<TBOX ~A, ~A AXIOMS>" 
              (name tbox) 
              (length (get-original-axioms tbox))))))

;;;
;;; 
;;;

(defun get-axioms-from (table)
  (let ((res nil))
    (maphash #'(lambda (key val)
                 (declare (ignore key))
                 ;(princ key) (princ val)
                 (dolist (val val)
                   (push val res)))
             table)

    (sort (remove-duplicates res)
          #'< :key #'id)))

;;;
;;;
;;;

(defmethod get-original-axioms ((tbox tbox))
  (get-axioms-from (orig-axioms-with-left-side tbox)))

(defmethod get-new-axioms ((tbox tbox))
  (get-axioms-from (new-axioms-with-left-side tbox)))


(defmethod get-axioms-with-left-side ((left concept) &key orig-p)  
  (gethash left 
           (if orig-p
               (orig-axioms-with-left-side (tbox left))
             (new-axioms-with-left-side (tbox left)))))

(defmethod get-axioms-with-right-side ((right concept) &key orig-p)  
  (gethash right 
           (if orig-p 
               (orig-axioms-with-right-side (tbox right)) 
             (new-axioms-with-right-side (tbox right)))))

;;;
;;;
;;;

(defmethod get-axioms-containing-right-side ((right concept) &key orig-p)  
  (let* ((tbox (tbox right))
         (axioms 
          (if orig-p (orig-axioms-with-left-side tbox) (new-axioms-with-left-side tbox)))
         (found nil))

    (maphash  #'(lambda (key axioms) 
                  (declare (ignorable key))
                  (dolist (axiom axioms)
                    (when (or (eq (right axiom) right)
                              (and (is-and-concept-p (right axiom))
                                   (member right (arguments (right axiom)))))
                      (push axiom found))))
              axioms)
    found))

;;;
;;;
;;;


(defmethod get-axioms ((tbox tbox) &key orig-p)
  (get-axioms-from
   (if orig-p (orig-axioms-with-left-side tbox) (new-axioms-with-left-side tbox))))

(defmethod get-simple-axioms ((tbox tbox) &key orig-p)
  (remove-if #'gci-p 
             (get-axioms-from
              (if orig-p (orig-axioms-with-left-side tbox) (new-axioms-with-left-side tbox)))))

(defmethod get-gcis ((tbox tbox) &key orig-p)
  (remove-if-not #'gci-p 
                 (get-axioms-from
                  (if orig-p (orig-axioms-with-left-side tbox) (new-axioms-with-left-side tbox)))))

;;;
;;;
;;;

(defun sort-concepts (list)
  (sort list #'< :key #'id))

;;;
;;;
;;;

(defmethod get-primitive-concept-definitions ((tbox tbox) &key orig-p)
  (remove-if-not #'primitive-p (get-simple-axioms tbox :orig-p orig-p)))

(defmethod get-defined-concept-definitions ((tbox tbox) &key orig-p)
  (remove-if #'primitive-p (get-simple-axioms tbox :orig-p orig-p)))

;;;
;;;
;;;

(defmethod get-simple-axiom-with-left-side ((name top-concept) &key &allow-other-keys)
  nil)

(defmethod get-simple-axiom-with-left-side ((name bottom-concept) &key &allow-other-keys)
  nil)

(defmethod get-simple-axiom-with-left-side ((name atomic-concept) &key orig-p)
  (let ((defs (remove-if #'gci-p (get-axioms-with-left-side name :orig-p orig-p))))
    (when (and defs (not (cdr defs)))
      (values (right (first defs))
              (first defs)))))

;;;
;;;
;;;

(defmethod get-simple-axioms-with-left-side ((name top-concept) &key &allow-other-keys)
  nil)

(defmethod get-simple-axioms-with-left-side ((name bottom-concept) &key &allow-other-keys)
  nil)

(defmethod get-simple-axioms-with-left-side ((name atomic-concept) &key orig-p)
  (remove-if #'gci-p (get-axioms-with-left-side name :orig-p orig-p)))

;;;
;;;
;;;

(defmethod get-concept-definition ((name atomic-concept) &key orig-p)
  (multiple-value-bind (right def) 
      (get-simple-axiom-with-left-side name :orig-p orig-p)
    (when (and def (not (primitive-p def)))
      (values right def))))

(defmethod get-primitive-concept-definition ((name atomic-concept) &key orig-p)
  (multiple-value-bind (right def) 
      (get-simple-axiom-with-left-side name :orig-p orig-p)
    (when (and def (primitive-p def))
      (values right def))))

;;;
;;;
;;;

(defun ts-sort (atoms)
  (topo-sort atoms
             #'(lambda (x y)  
                 (member x (told-subsumers y)))))


(defun def-order-sort (atoms)
  (topo-sort atoms
             #'(lambda (x y)  
                 (member x (references-atoms y)))))

;;;
;;;
;;;
                 

(defmethod compute-told-subsumers ((tbox tbox) &key orig-p)
  (declare (ignorable orig-p))
  (let ((axioms 
         (append (get-axioms tbox :orig-p orig-p )
                 ;(get-axioms tbox :orig-p t)
                 ))

        (atoms (get-all-atoms tbox)))


    (when *debug-p*
      (terpri)
      (format t "All positive atoms:               ~A~%" atoms))

    
    (labels ((compute-direct-told-subsumers ()
               
               (dolist (axiom axioms)
                 (dolist (axiom (if (primitive-p axiom)
                                    (list 
                                     (list (left axiom) (right axiom)))
                                  (list 
                                   (list (left axiom) (right axiom))
                                   (list (right axiom) (left axiom))))) 

                   (dolist (axiom (list axiom
                                        (list (get-negated-concept (second axiom))
                                              (get-negated-concept (first axiom)))))

                     (let* ((left (first axiom))
                            (right (second axiom))
                            
                            (left-atoms 
                             (cond ((is-atomic-concept-p left) (list left))
                                   ((is-or-concept-p left) 
                                    (remove-if-not #'is-atomic-concept-p (arguments left)))))
                            
                            (right-atoms 
                             (cond ((is-atomic-concept-p right) (list right))
                                   ((is-and-concept-p right)
                                    (remove-if-not #'is-atomic-concept-p (arguments right))))))

                       (dolist (left left-atoms)
                         (dolist (right right-atoms)
                           (dolist (left (cons left (get-equivalent-names left)))
                             (dolist (right (cons right (get-equivalent-names right)))

                               ;;; es macht nur Sinn, told subsumers fuer positive Atome
                               ;;; zu berechnen, denn nur diese werden ja in der 
                               ;;; Taxonomie gehalten
                               
                               ;(unless (negated-p left)
                               (pushnew right (told-subsumers left))
                               
                               (when (negated-p right)
                                 (pushnew (get-negated-concept left)
                                          (told-subsumers (get-negated-concept right)))))))))))))
             

             (compute-all-told-subsumers (name)
               (if (marked-p name)
                   (told-subsumers name)
                 (progn
                   (dolist (atom (told-subsumers name))
                     (dolist (ts (compute-all-told-subsumers atom))
                       (pushnew ts (told-subsumers name))))
                   (setf (marked-p name) t)
                   (told-subsumers name)))))
             
      (unmark-all-atoms tbox)

      ;;;
      ;;; direkte fuer alle berechnen
      ;;; 

      (compute-direct-told-subsumers)

      (when *debug-p*
        (dolist (name atoms)
          (format t "Direct told subsumers of ~A: ~A~%" name
                  (told-subsumers name))))

      ;;;
      ;;; transitive Huelle fuer nicht zu Zyklen fuehrende berechnen 
      ;;; 

      (labels ((leads-to-cycle-p (name path)
                 (or (member name path)
                     (some #'(lambda (x)
                               (leads-to-cycle-p x (cons name path)))
                           (told-subsumers name)))))

        (let ((leads-to-cycle 
               (remove-if-not #'(lambda (x) 
                                  (leads-to-cycle-p x nil))
                              atoms)))

          (when *debug-p*
            (format t "Atoms that lead to a cyclic atom: ~A~%" leads-to-cycle))

          (dolist (name (reverse 
                         (ts-sort (set-difference atoms
                                                  leads-to-cycle))))

            (compute-all-told-subsumers name)
            
            (when *debug-p*
              (format t "All told subsumers of ~A: ~A~%" name
                      (told-subsumers name))))

          ;;;
          ;;; Cluster bestimmen 
          ;;;
      
          (let ((clusters
                 (find-cluster (remove-if-not #'cyclic-p atoms) 
                               
                               ;; das ist eine Obermenge! 
                               ;; hier fuehren ja auch C -> (some R D), D -> etc. zu Zyklen, 
                               ;; aber D ist kein Told Subsumer von C! 
                             
                               #'(lambda (x y) 
                                   (member y (told-subsumers x))))))
                    
            (dolist (cluster clusters)
              (let ((all nil))
                (dolist (name cluster)
                  (setf (ts-cyclic-p name) t)
                  (dolist (told (told-subsumers name))
                    (pushnew told all)
                    (dolist (told (told-subsumers told))
                      (pushnew told all))))
              
                (dolist (name cluster)
                  (setf (told-subsumers name) all))
            
                (when *debug-p*
                  (format t "All told subsumers of cluster ~A: ~A~%" cluster all)))))

          ;;;
          ;;; Told Subsumers fuer zu Cluster fuehrende setzen 
          ;;;

          (dolist (name leads-to-cycle)
            (when (positive-p name)
              (unless (ts-cyclic-p name) ; nicht selbst im Cluster, aber fuehr zum Cluster
                (labels ((find-cluster-node (node)
                           (princ node)
                           (if (ts-cyclic-p node)
                               node
                             (find-if #'find-cluster-node
                                      (told-subsumers node)))))
              
                  (let ((cluster-node (find-cluster-node name)))

                    (unless cluster-node
                      (break "Found no cluster node for ~A" name))

                    (setf (told-subsumers name)
                          (told-subsumers cluster-node))
                
                    (when *debug-p*
                      (format t "All told subsumers of ~A (leading to cluster ~A): ~A~%" 
                              name cluster-node (told-subsumers name)))))))))

        ;;;
        ;;;
        ;;; 
        
        (unmark-all-atoms tbox)

        (setf (slot-value tbox 'told-subsumers-computed-p) t)))))

;;;
;;;
;;;

(defmethod initialize-instance :after ((tbox tbox) &rest initargs &key dont-initialize-p)
  (unless dont-initialize-p
    (pushnew tbox *all-tboxes*)))

(defun make-tbox (name &rest args
                       &key (rbox *cur-rbox*) (delete-if-exists-p t) &allow-other-keys)
  (when delete-if-exists-p t
    (delete-tbox name :error-p nil :all-p t))

  (let* ((store 
          (make-concept-store name))
         (tbox     
          (apply #'make-instance 'tbox
                 :name name 
                 :rbox rbox 
                 :concept-store store
                 :allow-other-keys t 
                 args)))

    (setf (slot-value store 'tbox) tbox)

    (let ((*cur-tbox* tbox))
      (with-concept-store (store)
                          (make-top-concept)
                          (make-bottom-concept)))

    tbox))

(defun find-tbox (name &key error-p)
  (or 
   (if (typep name 'tbox)
       name
     (find name *all-tboxes* :key #'name 
           :test #'(lambda (x y) 
                     (or (equal x y)
                         (and (Stringp x) 
                              (stringp y)
                              (string-equal x y))))))
   (when error-p
     (error "Can't find TBox named ~A!" name))))

(defun delete-tbox (name &key all-p (error-p t))
  (let ((found nil))
    (loop
     (let ((tbox (find-tbox name :error-p error-p)))
       (when tbox
         (setf found t))
       (setf error-p nil)
       (unless tbox 
         (when found
           (in-tbox default-tbox)
           (in-abox default-abox))
         (return))
       (setf *all-tboxes* 
             (delete tbox *all-tboxes*))
       
       (dolist (abox (used-by-aboxes tbox))
         (delete-abox abox))
       
       (unless all-p
         (in-tbox default-tbox)
         (in-abox default-abox)
         (return))))))

(defun delete-all-tboxes ()
  (setf *all-tboxes* nil)
  (in-tbox default-tbox)
  (delete-all-aboxes)
  *cur-tbox*)

(defun all-tboxes ()
  *all-tboxes*)

(defun current-tbox ()
  *cur-tbox*)

;;;
;;;
;;;

(defmethod make-axiom ((left concept) (right concept) &key (tbox *cur-tbox*) 
                       primitive-p gci-p (orig-p t) marked-p
                       role-domain-axiom-p
                       role-range-axiom-p)
  
  (unless tbox
    (error "No TBox!"))

  (if (eq left right)
      (format t "~%*** WARNING: Bad axiom ~A -> ~A: equivalent to TOP! Axiom ignored.~%" left right)
  
    (let* ((left (if (not primitive-p)
                     (if (is-atomic-concept-p left)
                         left
                       (if (is-atomic-concept-p right)
                           right
                         left))
                   left))
         
           (right (first (remove left (list left right)))))

      (note-tbox-modified tbox)

      (let ((found (find-if #'(lambda (x) 
                                (eq (right x) left))
                            (get-axioms-with-left-side right :orig-p orig-p))))
      
        (when found
          (setf (primitive-p found) nil))

        (let ((axiom (or found 
                         (make-instance 'axiom
                                        :id (incf (axiom-counter tbox))
                                        :gci-p gci-p
                                        :primitive-p primitive-p 
                                        :marked-p marked-p 
                                        :left left :right right
                                        :role-domain-axiom-p role-domain-axiom-p 
                                        :role-range-axiom-p role-range-axiom-p))))

          (when orig-p     
            (if (gethash left (orig-axioms-with-left-side tbox))
                (pushnew axiom (gethash left (orig-axioms-with-left-side tbox)))
              (setf (gethash left (orig-axioms-with-left-side tbox))
                    (list axiom)))
        
            (if (gethash right (orig-axioms-with-right-side tbox))
                (pushnew axiom (gethash right (orig-axioms-with-right-side tbox)))
              (setf (gethash right (orig-axioms-with-right-side tbox))
                    (list axiom))))

          (unless orig-p 
            (if (gethash left (new-axioms-with-left-side tbox))
                (pushnew axiom (gethash left (new-axioms-with-left-side tbox)))
              (setf (gethash left (new-axioms-with-left-side tbox))
                    (list axiom)))
        
            (if (gethash right (new-axioms-with-right-side tbox))
                (pushnew axiom (gethash right (new-axioms-with-right-side tbox)))
              (setf (gethash right (new-axioms-with-right-side tbox))
                    (list axiom))))
      
          axiom)))))
	

(defmethod delete-axiom ((tbox tbox) (axiom axiom) &key (orig-p t))
  
  (note-tbox-modified tbox)

  (when orig-p 
    (setf (gethash (left axiom) (orig-axioms-with-left-side tbox))
          (delete axiom (gethash (left axiom) (orig-axioms-with-left-side tbox))))
    (setf (gethash (right axiom) (orig-axioms-with-right-side tbox))
          (delete axiom (gethash (right axiom) (orig-axioms-with-right-side tbox)))))

  (unless orig-p 
    (setf (gethash (left axiom) (new-axioms-with-left-side tbox))
          (delete axiom (gethash (left axiom) (new-axioms-with-left-side tbox))))
    (setf (gethash (right axiom) (new-axioms-with-right-side tbox))
          (delete axiom (gethash (right axiom) (new-axioms-with-right-side tbox))))))
  
;;;
;;;
;;;

(defmethod unmark-all-atoms ((tbox tbox))
  (dolist (atom (get-all-atoms tbox))
    (setf (marked-p atom) nil)))

(defmethod compute-referenced-atoms ((concept atomic-concept) &key orig-p)
  (labels ((do-it (name)
             (dolist (name (cons name (get-equivalent-names name)))
               (let ((axioms (get-axioms-with-left-side name :orig-p orig-p)))
                 (dolist (axiom axioms)
                   (dolist (name (get-all-atoms (right axiom)))
                     (when (and (positive-p name)
                                (not (marked-p name)))
                       (push name (references-atoms concept))
                       (setf (marked-p name) t)
                       (do-it name))))))))

    (unmark-all-atoms (tbox concept))
    (do-it concept)
    (setf (references-atoms concept)
          (remove-duplicates (references-atoms concept)))))


(defmethod compute-cross-references ((tbox tbox) &key orig-p)
  (dolist (name (get-all-atoms tbox))
    (compute-referenced-atoms name :orig-p orig-p)
    (when (member name (references-atoms name))
      (setf (cyclic-p name) t)
      (setf (slot-value tbox 'needs-blocking-p) t)))
  (unmark-all-atoms tbox))

;;;
;;;
;;;

(defmethod partition ((tbox tbox) &key (orig-p t) (allow-cyclic-primitive-definitions-p t))

  (let* ((axioms 
          (if orig-p 
              (get-original-axioms tbox)
            (get-new-axioms tbox)))
         
         (left-sides
          (sort-concepts
           (remove-duplicates (mapcar #'left axioms))))
         
         (new-axioms nil)

         (name-equivalence-axioms nil))


    ;;;
    ;;; C -> D, C -> E => C -> D /\ E
    ;;; 

    (dolist (left-side left-sides)
      (let* ((axioms (get-axioms-with-left-side left-side :orig-p orig-p))
             (prim-axioms (remove-if-not #'primitive-p axioms))
             (def-axioms (remove-if #'primitive-p axioms)))

        (when prim-axioms
          (let* ((axiom (first prim-axioms))
                 (rem-axioms (rest prim-axioms))
                 (right-sides 
                  (mapcar #'right prim-axioms)))
                   
            (setf (right axiom)
                  (make-and-concept right-sides))

            (push axiom new-axioms)

            (dolist (axiom rem-axioms)
              (delete-axiom tbox axiom :orig-p orig-p))))

        (dolist (axiom def-axioms)
          ;;; (def C D)
          (if (and (is-atomic-concept-p (left axiom))
                   (is-atomic-concept-p (right axiom))
                   (not (is-top-concept-p (left axiom)))
                   (not (is-top-concept-p (right axiom)))
                   (not (is-bottom-concept-p (left axiom)))
                   (not (is-bottom-concept-p (right axiom))))
              ;;; (def CN DN) ?
              (push axiom name-equivalence-axioms)
            (push axiom new-axioms)))))

    ;;;
    ;;; ab hier habe ich hoechstens ein Axiom der Form C -> D
    ;;; aber es koennen noch Axiome der Form C = E zusaetzlich
    ;;; vorhanden sein! 
    ;;; Außerdem sind equivalente Namesdefinition (DEF CN DN)
    ;;; in name-equivalence-axioms!
    ;;;

    (dolist (axiom new-axioms)
      (setf (marked-p axiom) nil))

    (let* ((new-axioms (reverse new-axioms)) ; wichtig!
           
           (simple-axioms 
            (remove-if #'(lambda (axiom) 
                           (or (not (is-atomic-concept-p (left axiom)))
                               (is-top-concept-p (left axiom))
                               (is-bottom-concept-p (right axiom))
                               (negative-p (left axiom))
                               (and (not (primitive-p axiom))
                                    ;; (def  C TOP) ist GCI, aber 
                                    ;; (def* C TOP) ist sogar redundant!
                                    (is-top-concept-p (right axiom)))))
                       new-axioms))

           (simple-axioms ; ordnung 
            (append (remove-if-not #'primitive-p simple-axioms)
                    (remove-if #'primitive-p simple-axioms)))
                        
           (sure-gci-axioms 
            (set-difference new-axioms simple-axioms))

           (partitions nil))
      
      (dolist (axiom sure-gci-axioms)
        (setf (gci-p axiom) t))
      
      (dolist (axiom simple-axioms)
        (setf (gci-p axiom) nil))

      (unmark-all-atoms tbox)
      
      (labels ((cyclic-p (cur axiom &key (orig-p t) (consider-only-non-primitive-axioms-p t))

                 (cond ((eq (right cur) (left axiom))

                        t)

                       (t
               
                        (some #'(lambda (atom)
                                  (let ((axioms (get-axioms-with-left-side atom :orig-p orig-p)))
                                    (when axioms
                                      (cond ((member axiom axioms)
                                          
                                             t)

                                            (t
                                             (let ((axioms (remove-if #'(lambda (axiom)
                                                                          (or (not (marked-p axiom))
                                                                              (gci-p axiom)
                                                                              (and consider-only-non-primitive-axioms-p
                                                                                   (primitive-p axiom))))
                                                                      axioms)))

                                               (when (cdr axioms)
                                                 (error "Error in TBox partioning procedure!"))

                                               (when axioms 
                                                 (cyclic-p (first axioms) axiom))))))))
                              
                              (get-all-atoms (right cur))))))

               (causes-cycle-p (axiom &rest args)
                 (apply #'cyclic-p axiom axiom args))

               (do-it (u-axioms rem-axioms)

                 ;;;
                 ;;; da rem-axioms sortiert ist
                 ;;; (erst die primitiven, dann 
                 ;;; die nicht-primitiven!) 
                 ;;; wird dolist immer zunaechst 
                 ;;; primitive finden
                 ;;; 

                 (let ((good-axiom 

                        (loop while rem-axioms do
                              (let ((x (first rem-axioms)))
                                
                                (if (primitive-p x)                              
                                    ;;; primitive-p                            
                                    (if (or (marked-p x)
                                            (gci-p x)
                                            (and (marked-p (left x))
                                                 (not allow-cyclic-primitive-definitions-p))
                                            (and (not allow-cyclic-primitive-definitions-p)
                                                 (causes-cycle-p x
                                                                 :orig-p orig-p
                                                                 :consider-only-non-primitive-axioms-p nil)))
                                        (setf rem-axioms (delete x rem-axioms))
                                      (return x))
                                  ;;; not primitive-p
                                  (if (or (marked-p x)
                                          (gci-p x)
                                          (marked-p (left x))
                                          (and (is-atomic-concept-p (right x))
                                               (marked-p (right x)))
                                          (causes-cycle-p x
                                                          :orig-p orig-p
                                                          :consider-only-non-primitive-axioms-p t))
                                      (setf rem-axioms (delete x rem-axioms))
                                    (return x))))

                              finally (return nil))))

                   (if (not good-axiom)
                       (push (reverse u-axioms) partitions)
                     (let ((name (left good-axiom)))
                       ;(princ name) (terpri)
                       (setf (marked-p name) t)
                       (setf (marked-p good-axiom) t)
                       (do-it (cons good-axiom u-axioms)
                              (remove good-axiom rem-axioms)))))))
        
        (do-it nil simple-axioms)

        (setf partitions (sort partitions #'> :key #'length))
        
        (let* ((u-part (first partitions))

               (g-part (append (set-difference simple-axioms u-part)
                               ;;; die nicht in u-part uebernommen
                               ;;; simple-axioms muessen zusaetzlich
                               ;;; als GCIs behandelt werden...
                               sure-gci-axioms)))
          
          (loop as axiom in g-part do
                (setf (gci-p axiom) t))

          (loop as axiom in u-part do
                (setf (gci-p axiom) nil))
          
          (loop as axiom in name-equivalence-axioms do
                (setf (name-equivalence-axiom-p axiom) t))

          #|
          (setf x u-part)
          (setf y g-part)
          (setf z name-equivalence-axioms)
            |#

          (when *debug-p* 
          
            (terpri)
            (princ "U-Part     : ") (princ (length u-part)) (terpri)
            (princ "G-Part     : ") (princ (length g-part)) (terpri)
            (princ "E-Part     : ") (princ (length name-equivalence-axioms)) (terpri)
            ;(break)
            )

          ;;;
          ;;; primitive Atome markieren 
          ;;; 

          (dolist (axiom u-part)
            (when (primitive-p axiom)
              (setf (primitive-p (left axiom)) t)))
          
          (let ((change t))
            (loop while change do
                  (setf change nil)
                  (dolist (axiom name-equivalence-axioms)
                    (cond ((and (primitive-p (left axiom))
                                (not (primitive-p (right axiom))))
                           (setf (primitive-p (right axiom)) t
                                 change t))
                          ((and (primitive-p (right axiom))
                                (not (primitive-p (left axiom))))
                           (setf (primitive-p (left axiom)) t
                                 change t))))))
          
          ;;;
          ;;;
          ;;; 

          (values u-part g-part))))))

;;;
;;;
;;;

(defmethod positive-p ((atom atomic-concept))
  (not (negated-p atom)))

(defmethod positive-p ((concept and/or-concept))
  (every #'positive-p (arguments concept)))

(defmethod positive-p ((concept some/all-concept))
  (positive-p (qualification concept)))

;;;
;;;
;;;

(defmethod negative-p ((concept concept))
  (negated-p concept))

(defmethod negative-p ((concept and/or-concept))
  (every #'negative-p (arguments concept)))

(defmethod negative-p ((concept some/all-concept))
  (negative-p (qualification concept)))
      
;;;
;;;
;;;

(defmethod get-equivalent-names ((name atomic-concept))
  (gethash name (equivalent-names (tbox name))))

(defmethod absorb-gcis ((tbox tbox))
  (labels ((try-to-absorb (set) 
             (let ((loop t))
               
               (loop while loop do 

                     ;(setf set (sort set #'< :key #'id))
                     
                     (when *debug-p* 
                       (format t "~%*** SET: ~A~%" set))

                     (setf loop nil)                                             
              
                     ;;;
                     ;;; DL-Handbook s. 327 
                     ;;;
                     ;;; (ii) 
                     ;;;
                     
                     (let* ((found nil)
                            
                            (w (find-if #'(lambda (w)
                                            (and (is-atomic-concept-p w)
                                                 (positive-p w)
                                                 (multiple-value-bind (right def)
                                                     (get-simple-axiom-with-left-side w)
                                                   (declare (ignorable right))
                                                   (when (and def (primitive-p def))
                                                     (setf found def)
                                                     t))))
                                        set)))
                       
                       (when w                          
                         (let* ((right (right found))
                                (c 
                                 (make-and-concept
                                  (list right
                                        (make-not-concept
                                         (make-and-concept (remove w set)))))))

                           (setf (slot-value found 'right) c)
                           
                           (setf (gethash right (new-axioms-with-right-side tbox))
                                 (delete found (gethash right (new-axioms-with-right-side tbox))))
                           
                           (if (gethash c (new-axioms-with-right-side tbox))
                               (push found (gethash c (new-axioms-with-right-side tbox)))
                             (setf (gethash c (new-axioms-with-right-side tbox))
                                   (list found))))

                         (when *debug-p* 
                           (format t "~%*** ABSORBED ~A (~A) INTO ~A!" w set found))
                         
                         (return-from try-to-absorb nil)))
                     
                     ;;;
                     ;;; (iii)
                     ;;;
                     
                     (let* ((found nil)
                            (w (find-if #'(lambda (w)
                                            (and (is-atomic-concept-p w)
                                                 (positive-p w)
                                                 (multiple-value-bind (right def)
                                                     (get-simple-axiom-with-left-side w)
                                                   (declare (ignore right))            
                                                   (when (and def (not (primitive-p def)))
                                                     (setf found def)
                                                     t))))
                                        set)))

                       (when w 
                         (when *debug-p* (format t "*** (III)~%"))
                         (setf loop t)
                         (setf set (remove w set))
                         (push (right found) set)))

                     ;;;
                     ;;; (iv)
                     ;;;
                     
                     (unless loop
                  
                       (let* ((found nil) 
                              (w (find-if #'(lambda (w)                                     
                                              (and (is-atomic-concept-p w)
                                                   (negative-p w)
                                                   (multiple-value-bind (right def)
                                                       (get-simple-axiom-with-left-side 
                                                        (get-negated-concept w))
                                                     (when (and def
                                                                (not (primitive-p def))
                                                                (is-or-concept-p right))
                                                       (setf found def)
                                                       t))))
                                          set)))
                         
                         (when w 
                           (when *debug-p* (format t "*** (IV)~%"))
                           (setf loop t)
                           (setf set (remove w set))
                           (push (get-negated-concept (right found)) set))))

                     ;;;
                     ;;; (v)
                     ;;; 

                     (unless loop
                       (let ((inner-loop t))
                         (loop while inner-loop do ; AND's expandieren
                               (setf inner-loop nil)
                               
                               (let ((and (find-if #'is-and-concept-p set)))
                                 (when and 
                                   (when *debug-p* (format t "*** (V)~%"))
                           
                                   (setf inner-loop t)
                                   (setf loop t)
                                   (setf set (remove and set))
                                   (setf set (append set (arguments and))))))))

                     ;;;
                     ;;; (vi)
                     ;;; 
                     
                     (unless loop
                       (let ((or (find-if #'is-or-concept-p set))
                             (disjuncts-to-keep nil))
                         (when or
                           ;(setf loop t)
                           (when *debug-p* (format t "*** (VI)~%"))
                           (dolist (arg (arguments or))
                             (let ((res (try-to-absorb (cons arg (remove or set)))))
                               ;;; wurde ein Disjunkt absorbiert? 
                               ;;; -> OR ersetzen gegen übriggebliebene Diskunkte, 
                               ;;; die nicht absorbiert werden konnten
                               (when res
                                 (push arg disjuncts-to-keep))))
                           (setf set 
                                 (if (not disjuncts-to-keep)
                                     (remove or set)
                                   (cons (make-or-concept disjuncts-to-keep)
                                         (remove or set))))))))
               
               (when *debug-p* (format t "*** RETURNING ~A~%!" set))
             
               set)))
  
    (with-slots (meta-constraints) tbox
      
      (let* ((axioms 
              (get-original-axioms tbox))
             
             (simple-axioms ; GCI-P: s. "partition-tbox"
              (remove-if #'gci-p axioms))

             (name-equivalence-axioms
              (remove-if-not #'name-equivalence-axiom-p simple-axioms))
             
             (simple-axioms
              (remove-if #'name-equivalence-axiom-p simple-axioms))
                          
             (gcis
              (remove-if-not #'gci-p axioms))

             (simple-gcis
              (remove-if-not #'(lambda (x) 
                                 (is-atomic-concept-p (left x)))
                             gcis))
             
             (gcis (sort 
                    (append simple-gcis
                            (set-difference gcis simple-gcis))
                    #'<
                    :key #'(lambda (x) 
                             (id (left x)))))
             
             (new-gcis nil)

             (rem-names 
              (remove-if #'(lambda (x) 
                             (or (is-top-concept-p x)
                                 (is-bottom-concept-p x)
                                 (get-simple-axioms-with-left-side x :orig-p t)))
                         (get-all-atoms tbox))))
        
        (dolist (name rem-names)
          ;;; (def* AN TOP) f. undefiniertes AN 
          (make-axiom name 
                      (make-top-concept) 
                      :orig-p nil
                      :tbox tbox
                      :gci-p nil
                      :primitive-p t))
        
        (dolist (axiom simple-axioms)
          (make-axiom (left axiom) (right axiom) 
                      :tbox tbox
                      :gci-p nil
                      :marked-p t
                      :primitive-p (primitive-p axiom)
                      :orig-p nil))

        ;;;
        ;;; GCI-Absorption
        ;;;
        
        (dolist (gci gcis)
          ;; (format t "~% GCI: ~A~%" gci)
          (let ((sets (if (primitive-p gci)
                          (list  (list (get-negated-concept (right gci))
                                       (left gci)))
                        (list (list (get-negated-concept (right gci))
                                    (left gci))                              
                              (list (get-negated-concept (left gci))
                                    (right gci))))))
            (dolist (set sets)
              (let ((res
                     (try-to-absorb set)))
                ;(unless res
                ;  (format t "~% ABSORBED ~A!" set))
                (when res
                  (when *debug-p* 
                    (format t "~%*** Warning: Unable to absorb: ~A -> ~A~%" gci res)
                    ;(break)
                    )
                  (push res new-gcis))))))

        ;;;
        ;;; Axiome der Art (def CN DN) CN, DN = Konzeptnamen (Atome)
        ;;;
        
        ;;; (princ name-equivalence-axioms) (break)
          
        (dolist (axiom name-equivalence-axioms)
          (let* ((left (left axiom))
                 (right (right axiom))
                 (names (cons left 
                              (cons right
                                    (append (get-equivalent-names left)
                                            (get-equivalent-names right))))))
            
            (dolist (name names)
              (let ((names (remove name names)))
                (when names
                  (setf (gethash name (equivalent-names (tbox name))) names)
                  (setf (gethash (get-negated-concept name)
                                 (equivalent-names (tbox name)))
                        (mapcar #'get-negated-concept 
                                (get-equivalent-names name))))))))

        ;;;
        ;;; Meta-Constraints erzeugen / GCIs
        ;;; 

        (when new-gcis

          (setf (slot-value tbox 'needs-blocking-p) t)

          (let ((meta-args
                 (arguments
                  (make-and-concept
                   ;; wichtig wegen Normalisierung!!! (and (and ...)) !
                   (mapcar #'(lambda (new-gci)
                               (get-negated-concept 
                                (make-and-concept new-gci)))
                           new-gcis))))

                (rem-meta-args nil))

            (dolist (meta-arg meta-args)

              (cond ((is-all-concept-p meta-arg)

                     ;;; top -> (all r c)
                     
                     (when *debug-p*
                       (format t "Absorbing meta constraint conjunct ~A into role range of ~A (~A)~%" 
                               meta-arg (role meta-arg) (qualification meta-arg)))
                     
                     (register-range (role meta-arg) (qualification meta-arg)))
                    
                    ((and (is-or-concept-p meta-arg)
                          (find-if #'(lambda (x)
                                       (and (is-all-concept-p x)
                                            (is-bottom-concept-p (qualification x))))
                                   (arguments meta-arg)))

                     ;;; (or (all r bottom) (and c d)) = (some r top) -> (and c d) 

                     (let* ((all (find-if #'(lambda (x)
                                             (and (is-all-concept-p x)
                                                  (is-bottom-concept-p (qualification x))))
                                         (arguments meta-arg)))

                            (rem (make-or-concept (remove all (arguments meta-arg)))))

                       (when *debug-p*
                         (format t "Absorbing meta constraint conjuncts ~A into role domain of ~A (~A)~%" 
                                 meta-arg (role all) rem))
                       
                       (register-domain (role all) rem)))

                    (t (push meta-arg rem-meta-args))))

            (when rem-meta-args
                     
              (setf (slot-value tbox 'meta-constraints)
                    ;;; immer nur einer! 
                  (list (make-and-concept rem-meta-args)))
          
              (make-axiom (parse-concept 'top)
                          (first (slot-value tbox 'meta-constraints))
                          :tbox tbox
                          :gci-p t
                          :primitive-p t
                          :orig-p nil))))

        ;;;
        ;;; Axiome (defprimtconcept A TOP) loeschen
        ;;;
        
        (dolist (axiom (get-axioms-with-right-side (make-top-concept) :orig-p nil))
          (when (and (primitive-p axiom)
                     ;;; notwendig, weil in das Axiom hineinabsorbiert
                     ;;; worden sein kann!
                     (is-top-concept-p (right axiom)))
            (delete-axiom tbox axiom :orig-p nil))))))

      
  tbox)

;;;
;;;
;;;             


(defmethod compute-classification-order ((tbox tbox))

  ;;;
  ;;; Classification Order / In Definition Order? 
  ;;;

  (let* ((atoms (get-all-positive-atoms tbox))

         (non-cyclic-atoms
          (remove-if #'cyclic-p atoms))
         
         (cyclic-atoms
          (remove-if-not #'cyclic-p atoms))

         (ts-cyclic-cyclic-atoms
          (remove-if-not #'ts-cyclic-p cyclic-atoms))

         (rem-cyclic-atoms 
          (remove-if #'ts-cyclic-p cyclic-atoms))

         (top (parse-concept 'top))
         (bottom (parse-concept 'bottom)))

    
    (unmark-all-atoms tbox)
    (dolist (atom atoms)
      (setf (marked-p atom) t))

    ;; (setf *x* (list non-cyclic-atoms cyclic-atoms ts-cyclic-cyclic-atoms))
    ;; (breaK)

    
    (setf (classification-order tbox)

          (cons top
                (cons bottom
                      (delete top
                              (delete bottom
                                      (append (def-order-sort non-cyclic-atoms)
                                              (ts-sort rem-cyclic-atoms)
                                              ts-cyclic-cyclic-atoms))))))

    (unless cyclic-atoms
      (setf (in-definition-order-p tbox) t))

    ;;;
    ;;;
    ;;;

    tbox))

;;;
;;;
;;;


(defmethod prepare ((tbox tbox) (language dl))

  ;;; an dieser Stelle ist die Sprache der TBox
  ;;; noch nicht bekannt!!!

  (unless (prepared-p tbox)

    (setf (slot-value tbox 'language) nil
          (slot-value tbox 'taxonomy) nil
          (slot-value tbox 'satisfiable) :not-tested)
    
    (reset-sat-status (concept-store tbox))
    
    (dolist (abox (used-by-aboxes tbox))
      (reset-sat-status abox))
  
    (when *debug-p* 
      (terpri)
      (princ "Preparing TBox...")
      (terpri))
      
    (clrhash (new-axioms-with-left-side tbox))
    (clrhash (new-axioms-with-right-side tbox))

    (when *debug-p*
      (princ "  Partitioning TBox -> "))
      
    (partition tbox)

    (when *debug-p*
      (format t "Simple Axioms: ~A. GCIs: ~A.~%" 
              (length (get-simple-axioms tbox :orig-p t))
              (length (get-gcis tbox :orig-p t)))

      (princ "  Performing GCI-Absorption -> "))

    (absorb-gcis tbox)

    (when *debug-p*
      (format t "Simple Axioms: ~A. GCIs: ~A.~%" 
              (length (get-simple-axioms tbox))
              (length (get-gcis tbox)))
    
      (princ "  Computing cross references -> "))
      
    (compute-cross-references tbox)

    (when *debug-p* 
      (princ "  Preparing Roles -> "))

    (prepare-roles tbox)

    (when *debug-p*
      (format t "Roles: ~A. Transitive Roles: ~A. Features: ~A~%"
              (length (get-all-roles tbox))
              (length (get-all-transitive-roles tbox))
              (length (get-all-features tbox))))
        
      
    (dolist (name (get-all-atoms tbox))
        
      (when (cyclic-p name)
        (push name (cyclic-names tbox)))

      (let ((axioms (get-axioms-with-left-side name)))
        (cond ((or (not axioms)
                   (and (not (cdr axioms))
                        (primitive-p (first axioms))))
                 
               (setf (primitive-p name) t)
               (push name (primitive-names tbox)))
                
              (t 
               (push name (defined-names tbox))))))

    (when *debug-p*
      (princ "DONE!") (terpri))


    
    (when *debug-p* 
      (princ "  Cyclical Concepts -> ") 
      (princ (cyclic-names tbox))
      (terpri)

      (princ "  Blocking Needed? -> ")
      (princ (needs-blocking-p tbox))
      (terpri))

    (when *compute-told-subsumers-p*

      (when *debug-p*
        (princ "  Computing told subsumers -> "))
        
      (unless (told-subsumers-computed-p tbox)
        (compute-told-subsumers tbox)))

    (when *debug-p* 
      (terpri)

      (princ "   Computing classification order -> "))

    (compute-classification-order tbox)

    (when *debug-p*
      (princ "DONE!") (terpri)
      (if (in-definition-order-p tbox)
          (format t "  TBox is in Definition Order.~%")
        (format t "  TBox is *NOT* in Definition Order.~%")))


    ;;;
    ;;;
    ;;; 

    (setf (slot-value tbox 'prepared-p) t)

    ;;; wichtig - muss VOR get-language gesetzt werden, sonst Endlosschleife!
      
    (prepare (concept-store tbox) +dl+)
    (get-language tbox)

    (when *debug-p*
      (princ "  Computing language -> ")
      
      (princ (slot-value tbox 'language))
      (terpri)
      (terpri))

    ;;;
    ;;;
    ;;;

    tbox))

;;;
;;;
;;;

(defmethod taxonomy ((tbox tbox) &rest args &key recompute-p &allow-other-keys)
  (prepare tbox +dl+)

  (or (and (not recompute-p) 
           (slot-value tbox 'taxonomy))

      (let ((taxonomy 
             (make-dag :type 'taxonomy 
                       :tbox tbox
                       :name (format nil "Taxonomy of TBox ~A" (name tbox)))))
        (setf (slot-value tbox 'taxonomy)
              (apply #'classify taxonomy args))

        (unmark-all-dag-nodes taxonomy)
        
        (dolist (node (dag-nodes taxonomy))
          (when (defined-concept-p (concept node))
            (mark-dag-node node)))
        
        taxonomy)))


(defmethod taxonomy ((tbox null) &rest args )
  (error "Can't find TBox!"))

(defmethod taxonomy (tbox &rest args )
  (apply #'taxonomy (find-tbox tbox) args))

;;;
;;;
;;;

(defmethod classify ((tbox tbox) &rest args )
  (apply #'taxonomy tbox args))

(defmethod classify ((tbox symbol) &rest args )
  (apply #'taxonomy tbox args))

;;;
;;;
;;;

(defmethod compute-used-by-dag ((tbox tbox) &key 
                                (name #'original-description concept)
                                (definition 
                                 #'(lambda (concept) 
                                     (get-simple-axiom-with-left-side tbox concept)))                        
                                (root (make-top-concept))
                                (children 
                                 ;; voreingestellt für used-by-Relation! 
                                 #'(lambda (concept)
                                     (apply #'append
                                            (mapcar #'(lambda (x)
                                                        (get-all-atoms (left x)))
                                                    (remove-if-not #'(lambda (axiom)
                                                                       (member concept 
                                                                               (get-all-atoms (right axiom))))
                                                                   (get-simple-axioms tbox)))))))
  (let ((nodes nil)
        (dag (make-dag :type 'used-by-dag                       
                       :name (format nil "Used-by-relationship of TBox ~A" (name tbox))
                       :tbox tbox)))

    
    (labels ((expand (concept)               

               (or (find concept nodes :key #'concept)
                   
                   (let ((children (funcall children concept))
                         (node (make-dag-node :type 'taxonomy-node 
                                              :concept concept
                                              :name (funcall name concept)
                                              :definition (funcall definition concept)
                                              :concept concept)))

                     (push node nodes)
                     
                     (let ((children 
                            (mapcar #'expand children)))
                       (setf (dag-node-children node) children)
                       (dolist (child children)
                         (push node (dag-node-parents child)))

                       (insert-dag-node dag node))
                     
                     node))))

      (expand root)

      dag)))

;;;
;;;
;;;


(defmethod classify ((taxonomy taxonomy) &rest args)
  (let ((tbox (tbox taxonomy)))

    (let ((time
           (measure-time 
            (unless (apply #'tbox-sat-p tbox 
                           ;:debug-p t  
                           args)
              (format t "~%*** TBox ~A is incoherent!~%" tbox)
              (return-from classify nil)))))

      (format t "~%*** TIME NEEDED FOR TBOX COHERENCE CHECK: ~A SECONDS~%" time)
      (show-statistics))
    
    (format t "~%*** CLASSIFY CALLED WITH ARGUMENTS: ~A ~A~%" tbox args)

    (let ((time 

           (measure-time

            (let* ((concepts
                    (classification-order tbox))
                    
                   (nodes (mapcar #'(lambda (atomic-concept)
                                      (make-dag-node :type 'taxonomy-node
                                                     :name
                                                     (original-description atomic-concept)
                                                     :concept
                                                     atomic-concept
                                                     :definition
                                                     (get-simple-axiom-with-left-side atomic-concept)))

                                  (if *non-determinism-p* 
                                      (progn 
                                        (setf (in-definition-order-p tbox) nil)
                                        (reorder concepts))
                                    concepts)))
                    
                   (n (length nodes))
                   (count 0)
                   (pos 0)
                   (lastpos nil))
               
              (setf *nodes* nodes)
               
              (loop as i from 1 to 100 do (princ "-")) 
              (terpri)

              (unmark-all-atoms tbox)
        
              (dolist (node nodes)

                (setf *node*  node)
                 
                (apply #'classify-node node taxonomy args)
                 
                ;;(princ "+") 
                 
                (incf count)

                (setf pos (floor (+ 0.5 (* 100 (/ count n)))))
          
                (when (or (not lastpos)  
                          (not (= pos lastpos)))
                  (loop as i from (or lastpos 0) to (1- pos) do (princ "*"))
                  (setf lastpos pos)))
        
              (terpri)))))
    
      (format t "~%*** TIME NEEDED FOR TBOX CLASSIFICATION: ~A SECONDS~%" time)

      (show-statistics)))
  
  taxonomy)


(defmethod classify-node ((node taxonomy-node) (taxonomy taxonomy) &rest args)

  (setf *taxonomy* taxonomy)

  (when *debug-p* 
    (terpri)
    (princ ">>>>>>>>> Now classifying ")
    (princ node) (terpri))

  (let* ((tbox (tbox taxonomy))
         (args (append (list :tbox tbox)
                       (list :rbox (rbox tbox))
                       (list :type (get-abox-type-for-tbox tbox))
                       (list :debug-p (or *debug-p*
                                          (and *debug-p*
                                               (=> *debug-node*
                                                   (eq (dag-node-name node) *debug-node*)))))
                       (append args
                               (list :language (get-language tbox)))))

         (concept (concept node))
         
         (parents (apply #'compute-node-parents node taxonomy args))

         (children (progn 
                     (setf (dag-node-parents node) parents)

                     #|
                     (pprint 
                      (list node parents

                          ;(in-definition-order-p tbox)

                            (not (get-meta-constraints tbox))
                          ;(not (is-bottom-concept-p concept))
                          ;(not (is-top-concept-p concept))

                            (is-atomic-concept-p concept)

                            (multiple-value-bind (def axiom)
                                (get-simple-axiom-with-left-side concept)

                              (declare (ignorable def))

                              (and axiom
                                   (primitive-p axiom)
                                   (every #'(lambda (x) 
                                              (=> (positive-p x)
                                                  (marked-p x)))
                                          (told-subsumers (right axiom))))))) |#

                     ;;; wichtig, die muessen hier schon gesetzt werden!

                     (let ((condition  (and 

                                        (in-definition-order-p tbox)
                                        (not (get-meta-constraints tbox))
                          
                                        (not (is-top-concept-p concept))
                                        (not (is-bottom-concept-p concept))

                                        (is-atomic-concept-p concept)

                                        (multiple-value-bind (def axiom)
                                            (get-simple-axiom-with-left-side concept)

                                          (declare (ignorable def))

                                          (or (and (not axiom) 
                                                   (not (get-axioms-with-left-side concept)))

                                              (and axiom
                                                   (primitive-p axiom)
                                                   (every #'(lambda (x) 
                                                              (=> (positive-p x)
                                                                  (marked-p x)))
                                                          (typecase (right axiom)
                                                            (atomic-concept (list (right axiom)))
                                                            (and-concept (remove-if-not #'is-atomic-concept-p
                                                                                        (arguments (right axiom))))))))))))

                       
                       (if condition 

                           (progn 
                             (when *debug-p* 
                               (format t "**** SKIPPING BOTTOM SEARCH PHASE FOR ~A!~%" node))

                             (let ((children
                                    (or
                                     (mapcar #'(lambda (x) 
                                                 (find-node-for-concept1 taxonomy x))
                                             (remove-if-not #'marked-p (get-equivalent-names concept)))
                                     (list (find-node-for-concept1 taxonomy (parse-concept 'bottom))))))

                               children ))
                               

                         (let ((children 
                                (apply #'compute-node-children node taxonomy args))

                               (children2
                                (or 
                                 (mapcar #'(lambda (x) 
                                             (find-node-for-concept1 taxonomy x))
                                         (remove-if-not #'marked-p (get-equivalent-names concept)))
                                 (list (find-node-for-concept1 taxonomy (parse-concept 'bottom))))))
                         
                         
                           (when (and condition
                                      (not (set-equal children children2)))

                             (break "Error: Bottom Search Phase skipped?! ~A ~A ~A"  node children children2))
                         
                           children))))))
                         
         
    (if (and (set-equal parents children)
             parents
             (not (cdr parents))
             (not (cdr children)))

        (let ((equi-node (car parents)))
          (unless (eq equi-node node)
            (when *debug-p* (format t "**** FOUND EQUIVALENT NODE FOR ~A: ~A!~%" node (car parents)))
            
            (setf (slot-value equi-node 'equivalents)
                  (cons node (slot-value equi-node 'equivalents)))))
      
      (progn           
        (when *debug-p*
          (format t "**** PARENTS OF ~A: ~A!~%" node parents)
          (format t "**** CHILDREN OF ~A: ~A!~%" node children))
        
        (setf (dag-node-parents node) parents 
              (dag-node-children node) children)

        (setf (marked-p (concept node)) t)
        
        (insert-dag-node taxonomy node)))))

;;;
;;;
;;;

(defmethod subsumes1-p ((a concept) (b concept) &rest args)

  (let ((res    
         (if (and *logging-p* 
                  (=> *debug-node* 
                      (eq (dag-node-name *node*)
                          *debug-node*)))
             (progn 
               (apply #'subsumes-p a b 
                      :debug-p t args))
           
           (apply #'subsumes-p a b args))))
    
    (when *racer-validation-p*

      (let ((*print-pretty* t))
        (unless (eq (racer-user::concept-subsumes-p 
                     (let ((*package* (find-package :racer-user)))
                       (read-from-string (format nil "~A" 
                                                 a)))
                                       
                     (let ((*package* (find-package :racer-user)))
                       (read-from-string (format nil "~A" 
                                                 b)))
                     *racer-tbox*)
                    res)

          (with-logging 
           (progn
             (trace subsumes-p)
             (trace sat-p)
             (trace abox-sat-p)
             (trace consistent-p)
             (trace already-known-to-be-satisfiable-p)
             (trace already-known-to-be-inconsistent-p)
             
             (trace prover-init)
             (trace prover-main)
             (format t "~%~%!!!!!!!!!!!!!!!! ERROR !!!!!!!!!!!!!!!!!!!!!!~%~%")
           
             (apply #'subsumes-p a b :debug-p t :recompute-p t args)

             (format t "~%~%!!!!!!!!!!!!!!!! ERROR !!!!!!!!!!!!!!!!!!!!!!~%~%")
             (force-output *standard-output*)))
                                
          (error "Found mismatch for (subsumes? ~A ~A): Racer: ~A, Prover: ~A"
                 a b 
                 (not res) res))))

    
    (when *debug-p*
      (if res 
          (format t "~A SUBSUMES ~A~%" a b)
        (format t "~A DOES NOT SUBSUME ~A~%" a b)))

    res))

;;;
;;;
;;;

(defmethod get-told-non-subsumers ((concept concept))
  (mapcar #'get-negated-concept
          (remove-if-not #'negated-p
                         (told-subsumers concept))))

;;;
;;;
;;;


(defmethod compute-node-parents ((node taxonomy-node) (taxonomy taxonomy) &rest args)
    
  (labels ((mark-all-descendants (node)
             (unless (eq (dag-node-marked-p node) 'no)
               (mark-dag-node node 'no)
               (mapc #'mark-all-descendants (dag-node-children node))))
           
           (mark-all-ancestors (node)
             (unless (eq (dag-node-marked-p node) 'yes)
               (mark-dag-node node 'yes)
               (mapc #'mark-all-ancestors (dag-node-parents node))))
           
           (do-it (nodes)

             (loop while nodes do
                   
                   ;(princ (length nodes))
                   ;(terpri)
                              
                   (let ((current (pop nodes)))
                 
                     (unless (eq (dag-node-marked-p current) 'no)
                       (cond ((or (eq (dag-node-marked-p current) 'yes)
                                  (apply #'subsumes1-p (concept current) (concept node) args))

                              (mark-all-ancestors current)
                              (setf nodes (nconc nodes (copy-list (dag-node-children current)))))

                             (t (mark-all-descendants current))))))))

    (cond ((is-top-concept-p (concept node)) nil)
          
          ((is-bottom-concept-p (concept node)) 
           (list (find-node-for-concept1 taxonomy (parse-concept 'top))))
          
          (t 

           (unmark-all-dag-nodes taxonomy)


           (mapc #'mark-all-descendants  
                 (remove nil 
                         (mapcar #'(lambda (x) 
                                     (find-node-for-concept1 taxonomy x))
                                 (get-told-non-subsumers (concept node)))))

           (mapc #'mark-all-ancestors 
                 (remove nil
                         (mapcar #'(lambda (x) 
                                     (let ((node1
                                            (find-node-for-concept1 taxonomy x)))
                                       
                                       (when (and *debug-p* (not node1))
                                         (format t "*** WARNING: CAN'T FIND TOLD SUBSUMER ~A OF ~A!~%" x (concept node)))
                                       
                                       (when (and (in-definition-order-p (tbox taxonomy)) (not node1))
                                         ;;; nur dann ist das ein Fehler!
                                         (error "This TBox is not in definition order!"))

                                       node1))

                                 (remove-if-not #'positive-p 
                                                (told-subsumers (concept node))))))

           (do-it (list (find-node-for-concept1 taxonomy (parse-concept 'top))))
    
           (remove-if #'(lambda (q) 
                          (or (not (eq (dag-node-marked-p q) 'yes))
                              (some #'(lambda (child) 
                                        (eq (dag-node-marked-p child) 'yes))
                                    (dag-node-children q))))
                      (dag-nodes taxonomy))))))
    

(defmethod compute-node-children ((node taxonomy-node) (taxonomy taxonomy) &rest args)
  (labels ((mark-all-ancestors (node)
             (unless (eq (dag-node-marked-p node) 'no)
               (mark-dag-node node 'no)
               (mapc #'mark-all-ancestors (dag-node-parents node))))

           (mark-all-descendants (node)
             (unless (eq (dag-node-marked-p node) 'yes)
               (mark-dag-node node 'yes)
               (mapc #'mark-all-descendants (dag-node-children node))))


           (mark-descendants-with (node val)
             (unless (member val (dag-node-marked-p node))
               (push val (dag-node-marked-p node))
               (mapc #'(lambda (child)
                         (mark-descendants-with child val))
                     (dag-node-children node)))) 
             
           (do-it (nodes)

             (loop while nodes do
                   
                   (let ((current (pop nodes)))

                     (when (eq (dag-node-marked-p current) 'candidate)
                       
                       (cond ((or (eq (dag-node-marked-p current) 'yes)
                                  (apply #'subsumes1-p (concept node) (concept current) args))

                              (mark-all-descendants current)
                              (setf nodes (nconc nodes (copy-list (dag-node-parents current)))))

                             (t 
                              (mark-all-ancestors current))))))))

    (cond ((is-top-concept-p (concept node)) 
           ;(list (find-node-for-concept1 taxonomy (parse-concept 'top)))
           ; gibt es noch nicht! 

           nil)
          
          ((is-bottom-concept-p (concept node)) nil)
          
          (t 
           
           (dolist (node (dag-nodes taxonomy))
             (mark-dag-node node '()))
           
           (let* ((parents (dag-node-parents node))
                  (n (length parents))
                  (bottom (find-node-for-concept1 taxonomy (parse-concept 'bottom))))

             (dolist (parent parents)
               (mark-descendants-with parent parent))

             (dolist (node (dag-nodes taxonomy))
               (if (= n (length (dag-node-marked-p node)))
                   (mark-dag-node node 'candidate)
                 (mark-dag-node node 'no)))
             
             ;; (format t "Candidates for ~A: ~A" node candidates)
             ;; (terpri)

             (do-it (list bottom))

             #|
             (format t "Adter do-it: ~A" 
                     (mapcar #'(lambda (x)
                                 (list x
                                       (dag-node-marked-p x)))
                             (dag-nodes taxonomy)))
             (terpri)
             |#

             (remove-if #'(lambda (q) 
                            (or 
                             (not (eq (dag-node-marked-p q) 'yes))
                             (some #'(lambda (parent) 
                                       (eq (dag-node-marked-p parent) 'yes))
                                   (dag-node-parents q))))
                        (dag-nodes taxonomy)))))))

#|
            

(defmethod compute-node-parents ((node taxonomy-node) (taxonomy taxonomy) &rest args)
    
  (labels ((mark-all-descendants (node)
             (unless (eq (dag-node-marked-p node) 'no)
               (mark-dag-node node 'no)
               (mapc #'mark-all-descendants (dag-node-children node))))
           
           (mark-all-ancestors (node)
             (unless (eq (dag-node-marked-p node) 'yes)
               (mark-dag-node node 'yes)
               (mapc #'mark-all-ancestors (dag-node-parents node))))
           
           (do-it (current)

	     (unless (eq (dag-node-marked-p current) 'no)
                     
	       (cond ((or (when (eq (dag-node-marked-p current) 'yes)
                            ;;; 
                            t)
                          (apply #'subsumes1-p (concept current) (concept node) args))
		   
                      (mark-all-ancestors current)
		     
                      (dolist (child (dag-node-children current))
                        (do-it child)))
		     
                     (t 
                      (mark-all-descendants current))))))
    
    (cond ((is-top-concept-p (concept node)) nil)
          
          ((is-bottom-concept-p (concept node)) 
           (list (find-node-for-concept1 taxonomy (parse-concept 'top))))
          
          (t 

           (unmark-all-dag-nodes taxonomy)

           (mapc #'mark-all-ancestors 
                 (remove nil
                         (mapcar #'(lambda (x) 
                                     (let ((node1
                                            (find-node-for-concept1 taxonomy x)))
                                       
                                       (when (and *debug-p* (not node1))
                                         (format t "*** WARNING: CAN'T FIND TOLD SUBSUMER ~A OF ~A!~%" x (concept node)))
                                       
                                       (when (and (in-definition-order-p (tbox taxonomy)) (not node1))
                                         ;;; nur dann ist das ein Fehler!
                                         (error "This TBox is not in definition order!"))

                                       node1))

                                 (remove-if-not #'positive-p 
                                                (told-subsumers (concept node))))))

           (do-it (find-node-for-concept1 taxonomy (parse-concept 'top)))

           (remove-if #'(lambda (q) 
                          (or (not (eq (dag-node-marked-p q) 'yes))
                              (some #'(lambda (child) 
                                        (eq (dag-node-marked-p child) 'yes))
                                    (dag-node-children q))))
                      (dag-nodes taxonomy))))))
    

(defmethod compute-node-children ((node taxonomy-node) (taxonomy taxonomy) &rest args)
  (labels ((mark-all-ancestors (node)
             (unless (eq (dag-node-marked-p node) 'no)
               (mark-dag-node node 'no)
               (mapc #'mark-all-ancestors (dag-node-parents node))))

           (mark-all-descendants (node)
             (unless (eq (dag-node-marked-p node) 'yes)
               (mark-dag-node node 'yes)
               (mapc #'mark-all-descendants (dag-node-children node))))
           
           (mark-descendants-with (node val)

             (unless (member val (dag-node-marked-p node))
               (push val (dag-node-marked-p node))
               (mapc #'(lambda (child)
                         (mark-descendants-with child val))
                     (dag-node-children node))))           
             
           (do-it (current)

	     (unless (eq (dag-node-marked-p current) 'no)
	       
	       (when (eq (dag-node-marked-p current) 'candidate)
		   
                 (cond ((or (eq (dag-node-marked-p current) 'yes)
                            (apply #'subsumes1-p (concept node) (concept current) args))

                        (mark-all-descendants current)
                        (dolist (parent (dag-node-parents current))
                          (do-it parent)))

                       (t 
                        (mark-all-ancestors current)))))))
    
    (cond ((is-top-concept-p (concept node)) 
           ;(list (find-node-for-concept1 taxonomy (parse-concept 'top)))
           ; gibt es noch nicht! 

           nil)
          
          ((is-bottom-concept-p (concept node)) 

           nil)
          
          (t 
           
           (dolist (node (dag-nodes taxonomy))
             (mark-dag-node node '()))
           
           (let* ((parents (dag-node-parents node))
                  (n (length parents))
                  (bottom (find-node-for-concept1 taxonomy (parse-concept 'bottom))))

             (dolist (parent parents)
               (mark-descendants-with parent parent))

             (dolist (node (dag-nodes taxonomy))
               (if (= n (length (dag-node-marked-p node)))
                   (mark-dag-node node 'candidate)
                 (mark-dag-node node 'no)))
             
             (do-it bottom)

             (remove-if #'(lambda (q) 
                            (or 
                             (not (eq (dag-node-marked-p q) 'yes))
                             (some #'(lambda (parent) 
                                       (eq (dag-node-marked-p parent) 'yes))
                                   (dag-node-parents q))))
                        (dag-nodes taxonomy)))))))


|#

;;;
;;; Interface
;;;

(defmethod node-representation ((node taxonomy-node))
  (unparse 
   (cons (concept node) 
         (mapcar #'concept 
                 (equivalents node)))))

;;;
;;;
;;;

(defmethod parents ((concept concept))
  (let ((node (find-node-for-concept concept)))
    (when node
      (mapcar #'node-representation
              (dag-node-parents node)))))

(defmethod parents ((concept symbol))
  (parents (parse-concept concept)))

(defmethod children ((concept concept))
  (let ((node (find-node-for-concept concept)))
    (when node
      (mapcar #'node-representation 
              (dag-node-children node)))))

(defmethod children ((concept symbol))
  (children (parse-concept concept)))

(defmethod ancestors ((concept concept))
  (let ((node (find-node-for-concept concept)))
    (when node
      (mapcar #'node-representation
              (dag-node-ancestors node)))))

(defmethod ancestors ((concept symbol))
  (ancestors (parse-concept concept)))

(defmethod descendants ((concept concept))
  (let ((node (find-node-for-concept concept)))
    (when node 
      (mapcar #'node-representation
              (dag-node-descendants node)))))

(defmethod descendants ((concept symbol))
  (descendants (parse-concept concept)))

(defmethod equivalents ((concept concept))
  (let ((node (find-node-for-concept concept)))
    (when node
      (node-representation node))))
          
(defmethod synonyms ((concept concept))
  (equivalents concept))

(defmethod equivalents ((concept symbol))
  (equivalents (parse-concept concept)))

(defmethod synonyms ((concept symbol))
  (synonyms (parse-concept concept)))

;;;
;;;
;;;

(defmethod find-node-for-concept1 ((taxonomy taxonomy) (concept atomic-concept))
  (or (find concept (dag-nodes taxonomy) :key #'concept)
      (loop as node in (dag-nodes taxonomy) 
            when (member concept (equivalents node) :key #'concept)
            return node)))

(defun find-node-for-concept (concept)
  (find-node-for-concept1 (taxonomy (tbox concept)) concept))

;;;
;;;
;;;

(defmethod primitive-concept-p ((concept concept))
  nil)

(defmethod primitive-concept-p ((concept atomic-concept))
  (prepare (tbox concept) +dl+)
  (get-primitive-concept-definition concept))

(defmethod defined-concept-p ((concept concept))
  nil)

(defmethod defined-concept-p ((concept atomic-concept))
  (prepare (tbox concept) +dl+)
  (get-concept-definition concept))

;;;
;;; 
;;;

(defmethod primitive-concept-p (concept)
  (primitive-concept-p (parse-concept concept)))

(defmethod defined-concept-p (concept)
  (defined-concept-p (parse-concept concept)))

(defun get-primitive-concepts (tbox)
  (prepare tbox +dl+)
  (primitive-names (find-tbox tbox :error-p t)))

(defun get-defined-concepts (tbox)
  (prepare tbox +dl+)
  (defined-names (find-tbox tbox :error-p t)))

;;;
;;;
;;;

(defmethod note-tbox-modified ((tbox tbox))
  (reset-sat-status tbox))

;;;
;;;
;;;

(defmethod reset-sat-status ((tbox tbox))  
  (incf (tbox-version tbox))
  (setf (slot-value tbox 'prepared-p) nil)

  tbox)
    
(defmethod reset-sat-status ((tbox symbol))
  (reset-sat-status (find-tbox tbox :error-p t)))

;;;
;;;
;;;

(defun forget-tbox (tbox)
  (delete-tbox tbox))

(defun tbox-name (tbox)
  (name (find-tbox tbox :error-p t)))

;;;
;;;
;;;

(defmacro def* (left &optional (right 'top))
  (let ((cleft 
         (with-disabled-concept-store 
          (parse-concept left))))
    (if (not (and (is-atomic-concept-p cleft)
                  (not (negated-p cleft))))
        (error "Syntax error! ~A must be a concept name!" left)
      `(let ((*old-concept-p* t))
         (make-axiom (parse-concept ',left)
                     (parse-concept ',right)
                     :primitive-p t)))))

(defmacro def (left right)	
  (let ((cleft 
         (with-disabled-concept-store
          (parse-concept left))))
    (if (not (and (is-atomic-concept-p cleft)
                  (not (negated-p cleft))))
        (error "Syntax error! ~A must be a concept name!" left)
      `(let ((*old-concept-p* t))
         (make-axiom (parse-concept ',left)
                     (parse-concept ',right))))))

;;;
;;;
;;;

(defmacro impl (left right)	
  `(let ((*old-concept-p* t))
     (make-axiom (parse-concept ',left) (parse-concept ',right) :primitive-p t)))


(defmacro equi (left right)	
  `(let ((*old-concept-p* t))
     (make-axiom (parse-concept ',left) (parse-concept ',right))))

;;;
;;;
;;;

(defmethod taxonomy-list ((tbox tbox))
  (let* ((taxonomy (taxonomy tbox))
         (nodes 
          (sort (dag-nodes taxonomy)
                #'< 
                :key (lambda (x) (id (concept x))))))
    (tree-map #'(lambda (x)
                  (when x
                    (name (concept x))))
              (mapcar #'(lambda (node)
                          (list (if (equivalents node)                                    
                                    (cons node (equivalents node))
                                  node)
                                (mapcar #'(lambda (x) 
                                            (if (equivalents x)                                    
                                                (cons x (equivalents x))
                                              x))
                                        (dag-node-parents node))
                                (mapcar #'(lambda (x) 
                                            (if (equivalents x)                                    
                                                (cons x (equivalents x))
                                              x))                                
                                        (dag-node-children node))))
                      nodes))))

;;;
;;;
;;;

(defmethod tbox-sat-p :around ((tbox tbox) &rest args)
  (declare (ignorable args))
  (if (eq (slot-value tbox 'satisfiable) :not-tested)
      (setf (slot-value tbox 'satisfiable)
            (call-next-method))
    (slot-value tbox 'satisfiable)))

(defmethod tbox-sat-p ((tbox tbox) &rest args)

  (prepare tbox +dl+)

  (let* ((args (append (list :tbox tbox)
                       (list :rbox (rbox tbox))
                       (list :type (get-abox-type-for-tbox tbox))
                       (append args
                               (list :language (get-language tbox)))))
         (lastpos nil)
         (count 0)
         (pos nil)
         (concepts (classification-order tbox))
         (unsat nil)
         (n (length concepts)))

    (format t "~%*** TBOX-SAT-P CALLED WITH ARGUMENTS: ~A ~A~%" tbox args)
    
    (terpri)
    (loop as i from 1 to 100 do (princ "-"))
    (terpri)

    (dolist (concept concepts)
      
      (setf *concept* concept)

      (incf count)
               
      (setf pos (round (+ 0.5 (* 100 (/ count n)))))
      
      (let ((char (if (apply #'sat-p concept 
                             
                             :debug-p 

                             (or *debug-p*
                                 (and *logging-p* 
                                      (=> *debug-concept*
                                          (eq concept (parse-concept *debug-concept*)))))
                                  
                             args)
                      "+"
                    (progn 
                      (push concept unsat)
                      "-"))))
        
        (when (or (not lastpos)  
                  (not (= pos lastpos)))
          (loop as i from (or lastpos 0) to (1- pos) do (princ char))
          (setf lastpos pos))))
    
    (terpri)

    (let ((unsat (remove-if #'is-bottom-concept-p unsat)))
      (if unsat             
          (progn 
            (format t "~%*** WARNING: TBOX ~A IS INCOHERENT, UNSATISFIABLE CONCEPTS: ~A~%" tbox unsat)
            nil)
        t))))

(defmethod tbox-sat-p (tbox &rest args)
  (apply #'tbox-sat-p 
         (find-tbox tbox :error-p t)
         args))

;;;
;;; Eigene Macros
;;;

(defmacro tbox-sat? (abox &rest args)
  `(tbox-sat-p (quote ,abox) ,@args))

(defmacro tbox-sat*? (abox &rest args)
  `(tbox-sat-p ,abox ,@args))

;;;
;;;
;;;

(defmethod get-language ((tbox tbox) &optional recompute-p)
  (declare (ignorable recompute-p))

  (unless (prepared-p tbox)
    (prepare tbox +dl+))
    
  (setf (slot-value tbox 'language)
          
        (if (needs-blocking-p tbox)

            +alchf-rplus+

          (typecase (rbox tbox)

            (jepd-rolebox
             (if (member (rbox tbox)
                         (list +rcc1-rolebox+
                               +rcc2-rolebox+
                               +rcc3-rolebox+
                               +rcc5-rolebox+
                               +rcc8-rolebox+))
                 +alci-rcc+
               +alci-ra-jepd+))

            (rolebox 
             +alci-ra-minus+)

            (otherwise 
             (get-language (concept-store tbox)))))))
     
;;;
;;;
;;;


(in-tbox default-tbox)

(in-abox default-abox)




#|

(defun test ()

  (delete-all-tboxes)
  
  (def* a d1)
  (implies (or a (some r c)) d2)

  (prepare *cur-tbox* +alch+)

  (princ (get-simple-axioms *cur-tbox*))
  (terpri)
  (princ (get-meta-constraints *cur-tbox*))

  )


(defun test ()

  (delete-all-tboxes)
  
  (implies (and geometric-figure (some angles three))
           (some sides three))
  
  (implies geometric-figure figure)

  (prepare *cur-tbox* +alch+)

  (princ (get-simple-axioms *cur-tbox*))
  (terpri)
  (princ (get-meta-constraints *cur-tbox*))

  )



(defun test ()

  (delete-all-tboxes)
  
  (def* top d)
  (def* a d1)
  (def* a d2)

  (prepare *cur-tbox* +alch+)

  (princ (get-simple-axioms *cur-tbox*))
  (terpri)
  (princ (get-meta-constraints *cur-tbox*))

  )



(defun test ()

  (delete-all-tboxes)
  
  (def* top d)
  (def* a (and b (some r a)))
  (def* a x)

  (prepare *cur-tbox* +alch+)

  (princ (get-simple-axioms *cur-tbox*))
  (terpri)
  (princ (get-meta-constraints *cur-tbox*))

  )





(defun test ()

  (delete-all-tboxes)
  
  (def* a b)
  (def* a c)
  (def a xx)

  ;(def* c c)
  
  (setf *debug-p* t)
  (prepare *cur-tbox* +alch+ )

  (princ (get-simple-axioms *cur-tbox*))
  (terpri)
  (princ (get-meta-constraints *cur-tbox*))

  )
|#

