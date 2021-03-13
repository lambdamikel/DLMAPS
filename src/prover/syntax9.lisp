;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(defmacro with-disabled-concept-store (&body body)
  `(let ((*use-store-p* nil)
         (*cross-referencing-p* nil)
         (*insert-into-store-p* nil)
         (*create-negated-concept-p* nil))
     
     ,@body))

(defmacro with-enabled-concept-store (&body body)
  `(let ((*use-store-p* t)
         (*insert-into-store-p* t)
         (*create-negated-concept-p* t))
     
     ,@body))

(defmacro with-lookup-in-concept-store-enabled (&body body)
  `(let ((*use-store-p* t)
         (*insert-into-store-p* nil)
         (*create-negated-concept-p* nil))
     
     ,@body))

(defmacro with-protected-concept-store (&body body)
  `(let ((*dont-invalidate-store-p* t))
     
     ,@body))

;;;
;;;
;;;

(defpersistentclass role (description) ;;; abstrakt
  ((inverse-role :initform nil :initarg :inverse-role)
   (symmetric-role :reader symmetric-role :initform nil :initarg :symmetric-role)

   (id :reader id)      
   (parse-name :reader parse-name :initarg :parse-name :initform nil)

   (role-domain :accessor role-domain :initform nil :initarg :role-domain)
   (role-range  :accessor role-range  :initform nil :initarg :role-range)

   (superroles :reader superroles :initform nil :initarg :superroles)
   (subroles :reader subroles :initform nil :initarg :subroles)
   
   (all-superroles :reader all-superroles :initform nil :initarg :all-superroles)
   (all-subroles :reader all-subroles :initform nil :initarg :all-subroles)
   
   (textually-used-p :accessor textually-used-p :initform nil :initarg :textually-used-p)

   (rbox :reader rbox :initform nil :initarg :rbox)

   (concept-store :reader concept-store :initform nil :initarg :concept-store)
   (tbox :reader tbox :initform nil :initarg :tbox)

   ;;; es gibt komplexe Features! z.B. (and f g)! 
   (feature-p :reader feature-p :initform nil :initarg :feature-p)))


(defpersistentclass simple-role (role)
  ((inverse-p :reader inverse-p :initarg :inverse-p :initform nil)
   
   (transitive-p :reader transitive-p :initform nil :initarg :transitive-p)))


;;;
;;;
;;;

(defpersistentclass and/or-role (role) ;;; abstrakt
  ((arguments :reader arguments :initarg :arguments)))

(defpersistentclass and-role (and/or-role))

(defpersistentclass or-role (and/or-role))

;;;
;;;
;;;

(defmethod inverse-p ((role role))
  ;;; weil (inv (and r s))  -> 
  ;;;           (and (inv r) (inv s)) 
  ;;; gewandelt wird!
  nil)

(defmethod transitive-p ((role role))
  nil)

;;;
;;;
;;;

(defun or-role-p (role)
  (typep role 'or-role))

(defun and-role-p (role)
  (typep role 'and-role))

(defun simple-role-p (role)
  (typep role 'simple-role))

;;;
;;;
;;;

(defmethod print-object ((role role) stream)
  (if (feature-p role) 
      (format stream "#<FEATURE ~A ~A>" (type-of role) (textual-description role))
    (format stream "#<~A ~A>" (type-of role) (textual-description role))))

(defmethod print-object ((role simple-role) stream)
  (if *print-pretty*
      (format stream "~A" (textual-description role))
    (call-next-method)))
    

(defmethod print-object ((role and-role) stream)
  (if *print-pretty*
      (format stream "(AND~{ ~A~})" (arguments role))
    (call-next-method)))
    
    
(defmethod print-object ((role or-role) stream)
  (if *print-pretty*
      (format stream "(OR~{ ~A~})" (arguments role))
    (call-next-method)))
       
;;;
;;;
;;;

(defmethod underspecified-p ((role role) &rest args)
  nil)

(defmethod underspecified-p ((role or-role) &rest args)
  ;;; per Def. hat eine OR-ROLE mind. 2 Argumente! (or r) -> r durch Parser
  t)

;;;
;;;
;;;

(defpersistentclass concept (description) 
  ;;; Achtung - die von description geerbten Slots inconsistent, satsifiable, etc. 
  ;;; werden hier als offensichtlich inconsistent, satisfiable etc. gedeutet! 
  ((referenced-by :accessor referenced-by :initform nil :initarg :referenced-by)
   (referenced-by-ors :accessor referenced-by-ors :initform nil :initarg :referenced-by-ors)
   (referenced-by-ands :accessor referenced-by-ands :initform nil :initarg :referenced-by-ands)   

   (or-ref-counter :reader or-ref-counter :initform 0)
   (and-ref-counter :reader and-ref-counter :initform 0)

   (boolean-concept-p :reader boolean-concept-p :initform nil)
   
   (all-atoms)
   (all-store-roles)
   
   (marked-p :accessor marked-p :initform nil)
   
   (id :reader id)
   (old-p :reader old-p :initarg :old-p)

   (negated-concept :reader negated-concept :initform nil :initarg :negated-concept)

   (told-subsumers :accessor told-subsumers :initform nil)

   (cached-models :reader cached-models :initform nil)
   (instance-retrieval-model :accessor instance-retrieval-model :initform nil) 

   (language :initform nil :initarg :language)

   (constructors :accessor constructors :initform nil)

   (concept-store :reader concept-store :initform nil :initarg :concept-store)
   (tbox :reader tbox :initform nil :initarg :tbox)))
 
;;;
;;;
;;;

(defmethod reset-sat-status :after ((concept concept))
  (with-slots (cached-models instance-retrieval-model) concept

    (setf cached-models nil
          instance-retrieval-model nil)

    (dolist (ref (referenced-by concept))
      (when  (or (is-some-concept-p ref)
                 (is-or-concept-p ref)
                 (is-at-least-concept-p ref)
                 (is-all-concept-p ref)
                 (is-at-most-concept-p ref))

        (reset-sat-status ref)))))

;;;
;;;
;;;

(defmethod register-concept-model ((concept concept) model)
  (with-slots (cached-models) concept
    (push model cached-models)
    (register-concept-is-satisfiable concept)))

;;;
;;;
;;;

(defun make-weak-hash-table (&rest args)
  (let ((table (apply #'make-hash-table args)))
    ;(hcl:set-hash-table-weak table :key)
    table))

(defpersistentclass concept-store ()
  ((name :reader name :initarg :name)
   
   (prepared-p :reader prepared-p :initform nil)

   (tbox :reader tbox :initform nil :initarg :tbox)
   (language :initform nil :initarg :language)

   (role-hierarchies-p :accessor role-hierarchies-p :initform nil)
   (inverse-roles-p :accessor inverse-roles-p :initform nil)
   (transitive-roles-p :accessor transitive-roles-p :initform nil)
   (features-p :accessor features-p :initform nil)
   (complex-roles-p :accessor complex-roles-p :initform nil)
   

   (all-concepts :accessor all-concepts :initform nil)
   (all-store-roles :accessor all-store-roles :initform nil)

   (atoms :initform (make-weak-hash-table :size 1000 :rehash-size 1000 :test #'equal))

   (all-atoms      :accessor all-atoms :initform nil)
   (positive-atoms :accessor positive-atoms :initform nil)
   (negative-atoms :accessor negative-atoms :initform nil)

   (ands  :initform (make-weak-hash-table :size 10000 :rehash-size 10000 :test #'equal))
   (ors   :initform (make-weak-hash-table :size 10000 :rehash-size 10000 :test #'equal))
   (somes :initform (make-weak-hash-table :size 1000 :rehash-size 1000 :test #'equal))
   (alls  :initform (make-weak-hash-table :size 1000 :rehash-size 1000 :test #'equal))
   (roles :initform (make-weak-hash-table :size 100 :rehash-size 100 :test #'equal))
   
   (at-leasts  :initform (make-weak-hash-table :size 1000 :rehash-size 1000 :test #'equal))
   (at-mosts :initform (make-weak-hash-table :size 100 :rehash-size 100 :test #'equal))
   
   (id-counter :initform 0 :accessor id-counter)
   (role-counter :initform 0 :accessor role-counter)

   (constructors :accessor constructors :initform nil)))


(defun make-concept-store (&optional name tbox)
  (make-instance 'concept-store 
                 :tbox tbox
                 :name (or name 'anonymous-store)))

;;;
;;;
;;;

(defmethod reset-sat-status ((store concept-store))  
  (setf (slot-value store 'prepared-p) nil)
  store)
   
;;;
;;;
;;;

(defmethod print-object ((store concept-store) stream)
  (format stream "#<Store ~A>" (name store)))

(defmethod print-store-statistics ((store concept-store))
  (with-slots (id-counter all-concepts atoms ands ors somes alls at-leasts at-mosts roles) store
    (format t "*** Statistics of concept store ~A:~%
   ID COUNTER       : ~A
   ALL CONCEPTS     : ~A
   ATOMIC   CONCEPTS: ~A
   AND      CONCEPTS: ~A
   OR       CONCEPTS: ~A
   SOME     CONCEPTS: ~A
   ALL      CONCEPTS: ~A
   AT LEAST CONCEPTS: ~A
   AT MOST  CONCEPTS: ~A
   ROLES          : ~A~%~%"
            store
            id-counter
            (length all-concepts)
            (hash-table-count atoms)
            (hash-table-count ands)
            (hash-table-count ors)
            (hash-table-count somes)
            (hash-table-count alls)
            (hash-table-count at-leasts)
            (hash-table-count at-mosts)
            (hash-table-count roles))))

(defun statistics ()
  (print-store-statistics *cur-store*))

;;;
;;;
;;;

(defmacro with-concept-store ((store) &body body)
  `(with-slots (atoms ands ors somes alls 
                      at-leasts at-mosts roles) ,store
     (let ((*cur-store* ,store)
           (*atoms-store* atoms)
           (*and-store* ands)
           (*or-store* ors)
           (*some-store* somes)
           (*all-store* alls)
           (*at-least-store* at-leasts)
           (*at-most-store* at-mosts)
           (*roles-store* roles))
       ,@body)))


(defmacro in-concept-store (store)
  `(with-slots (atoms ands ors somes alls
                      at-leasts at-mosts
                      roles) ,store
     (setf *cur-store* ,store)
     (setf *atoms-store* atoms
           *and-store* ands
           *or-store* ors
           *some-store* somes
           *all-store* alls
           *at-least-store* at-leasts
           *at-most-store* at-mosts
           *roles-store* roles)
     *cur-store*))
       

;;;
;;;
;;;

(defmethod clear-store ((store concept-store))  
  #+:clim (reset-kinds)				; f. Graph Visualizer
  (with-slots (id-counter atoms 
                          all-atoms
                          positive-atoms 
                          negative-atoms
                          ands ors somes alls 
                          at-leasts at-mosts roles 
                          all-concepts
                          inverse-roles-p
                          transitive-roles-p
                          features-p 
                          role-hierarchies-p
                          complex-roles-p 
                          language
                          ) store
    (setf id-counter 0)

    (clrhash ands)
    (clrhash ors)
    (clrhash somes)
    (clrhash alls)
    (clrhash at-leasts)
    (clrhash at-mosts)
    (clrhash atoms)
    (clrhash roles)

    (setf positive-atoms nil
          negative-atoms nil
          all-atoms nil
          all-concepts nil
          language nil
          role-hierarchies-p nil
          complex-roles-p nil
          inverse-roles-p nil
          transitive-roles-p nil
          features-p nil))
   
  store)

;;;
;;;
;;; 

(defun find-role (description)
  (when *use-store-p*
    (with-concept-store (*cur-store*)    
      (gethash description *roles-store*))))
  

;;;
;;;
;;;

(defun parse-feature (description)
  (parse-role description :feature-p t))

(defun parse-role (description &key feature-p textually-used-p)
  (labels ((create-role (type description &rest args)
             (let ((role 
                    (apply #'make-instance type
                           :textual-description 
                           description
                           :old-p *old-concept-p*
                           :rbox *cur-rbox*
                           :constructor-sym 'reparse-role
                           :create-inverse-role-p *create-inverse-role-p*
                           :allow-other-keys t 
                           args)))

               role))

           (make-role (description &optional top-level-p)

             (unless description 
               (error "Bad NULL description given to parse-role!"))

             (let ((description 
                    (cond ((and *cur-rbox* 
                                (or (symbolp description)                                
                                    (and (consp description)
                                         (eq 'inv (first description)) 
                                         ;;; weil role in NF -> (second description) ist symbol  
                                         (symbolp (get-role *cur-rbox* description))
                                         ;;; symbolp, iff inverse rolle in role box verzeichnet
                                         )))
                           (get-role *cur-rbox* description))
                          
                          (t description))))

               (or (find-role description)
                   
                   (let ((role 

                          (if (symbolp description) 

                              (create-role 'simple-role
                                           description
                                           :feature-p (and top-level-p feature-p))
                            
                            (ecase (first description)

                              (inv 

                               (create-role 'simple-role
                                            description
                                            :feature-p (and top-level-p feature-p)
                                            :inverse-p t))

                              (or 
                               
                               ;; was spricht dagegen??
                               ;; (when feature-p
                               ;;  (error "*** BAD FEATURE SYNTAX: ~A" description))

                               (create-role 'or-role 
                                            description
                                            :feature-p (and top-level-p feature-p)
                                            :arguments (mapcar #'make-role (rest description))))
                              
                              (and 
                               
                               ;; was spricht dagegen??
                               ;; (when feature-p
                               ;;  (error "*** BAD FEATURE SYNTAX: ~A" description))
                                   
                               (create-role 'and-role 
                                            description
                                            :feature-p (and top-level-p feature-p)
                                            :arguments (mapcar #'make-role (rest description))))))))
                         
                     role))))

           (make-role-nf (description &optional inv-p)

             ;; (princ description) (terpri)

             (if (symbolp description)

                 (cond (*cur-rbox* 
                        (if inv-p 
                            (get-role *cur-rbox* `(inv ,description))
                          (get-role *cur-rbox* description)))
               
                       (t                         
                        (if inv-p 
                            `(inv ,description)
                          description)))
               
               (ecase (first description)

                 (inv                  
                  (make-role-nf (second description) 
                                (if inv-p nil t)))

                 (or
                  (let* ((args
                          (remove-duplicates 
                           (mapcar #'(lambda (x) 
                                       (make-role-nf x inv-p))
                                   (rest description))
                           :test #'equal))
                         (or-args
                          (apply #'append
                                 (mapcar #'rest
                                         (remove-if-not #'(lambda (x)
                                                            (and (consp x)
                                                                 (eq (first x) 'or)))
                                                        args))))

                         (atomic-args 
                          (append (sort (remove-if-not #'symbolp args)
                                        #'string-lessp 
                                        :key #'symbol-name)
                                  (sort (remove-if-not #'(lambda (x) 
                                                           (and (consp x) 
                                                                (eq (first x) 'inv)))
                                                       args)
                                        #'string-lessp 
                                        :key #'(lambda (x) (symbol-name (second x))))))

                         (and-args  (remove-if-not #'(lambda (x)
                                                       (and (consp x)
                                                            (eq (first x) 'and)))
                                                   args))
                         
                         (all-args (append atomic-args 
                                           or-args
                                           and-args)))

                    (if (cdr all-args)
                        (remove nil
                                `(or
                                  ,@atomic-args
                                  ,@or-args
                                  ,@and-args))
                      (first all-args))))

                 (and (let* ((args
                              (remove-duplicates 
                               (mapcar #'(lambda (x) 
                                           (make-role-nf x inv-p))
                                       (rest description))
                               :test #'equal))

                             (and-args
                              (apply #'append
                                     (mapcar #'rest
                                             (remove-if-not #'(lambda (x)
                                                                (and (consp x)
                                                                     (eq (first x) 'and)))
                                                            args))))

                             (or-args
                              (mapcar #'rest
                                      (remove-if-not #'(lambda (x)
                                                         (and (consp x)
                                                              (eq (first x) 'or)))
                                                     args)))

                              
                             (atomic-args 
                              (append (sort (remove-if-not #'symbolp args)
                                            #'string-lessp 
                                            :key #'symbol-name)
                                      (sort (remove-if-not #'(lambda (x) 
                                                               (and (consp x) 
                                                                    (eq (first x) 'inv)))
                                                           args)
                                            #'string-lessp 
                                            :key #'(lambda (x) (symbol-name (second x))))))
                              
                             (prod (newprod                                      
                                    (append or-args
                                            (mapcar #'list atomic-args)
                                            (mapcar #'list and-args)))))

                        (if (not (cdr prod))
                            (if (cdr (first prod))
                                `(and ,@(first prod))
                              (first (first prod)))
                          (if prod
                              `(or ,@(mapcar #'(lambda (prod)
                                                 `(and ,@prod))
                                             prod))
                            `(and ,@atomic-args ,@and-args)))))))))
    


    (let* ((role
            (if (is-role-p description)
                description
              (make-role (make-role-nf description) t))))

      (setf (textually-used-p role)
            (or (textually-used-p role)  
                textually-used-p))

      ;; einige Inverse werden nur "auf Vorrat" produziert, 
      ;; kommen aber nicht literal in der KB vor -> 
      ;; Problem fuer "get-language"

      (if *create-inverse-role-p*
          (values role (get-inverse-role role))
        role))))

;;;
;;;
;;;

(defmethod initialize-instance :after ((role role) &rest initargs &key dont-initialize-p)
  (unless dont-initialize-p 
    (with-concept-store (*cur-store*)
      (push role (all-store-roles *cur-store*))
      (setf (slot-value role 'id) (incf (role-counter *cur-store*)))
      (register-name-for-role role (textual-description role))
      (setf (slot-value role 'tbox) (tbox *cur-store*))
      (setf (slot-value role 'concept-store) *cur-store*))))

(defmethod register-name-for-role ((role role) name)
  (setf (gethash name *roles-store*) role))


;;;
;;;
;;;

(defmethod prepare ((store concept-store) (dl dl))
  (with-slots (prepared-p 
               all-concepts) store

    (unless prepared-p
      
      (dolist (concept all-concepts)
        (reset-sat-status concept))

      (compute-language store))

    store))


(defmethod compute-language ((store concept-store))
  (with-slots (language
               prepared-p
               all-store-roles 
               role-hierarchies-p
               inverse-roles-p
               complex-roles-p
               transitive-roles-p
               features-p
               all-concepts
               constructors) store

    (setf constructors nil
          role-hierarchies-p nil
          inverse-roles-p nil
          complex-roles-p nil
          transitive-roles-p nil
          features-p nil)

    (dolist (c all-concepts)
      (dolist (c (constructors c))
        (pushnew c (constructors *cur-store*))))

    (dolist (role all-store-roles)
      (let ((inv-role (get-inverse-role role)))
        (when (textually-used-p role)
          (when (is-and/or-role-p role)
            (setf complex-roles-p t))
          (when (superroles role)
            (setf role-hierarchies-p t))
          (when (textually-used-p inv-role)
            (setf inverse-roles-p t))
          (when (transitive-p role)
            (setf transitive-roles-p t))
          (when (feature-p role)
            (setf features-p t)))))

    (setf prepared-p t) 

    (with-concept-store (store)
        
                        (let* ((constructors (constructors store))
                               (inverse-roles-p (inverse-roles-p store))
                               (transitive-roles-p (transitive-roles-p store))
                               (features-p (features-p store))
                               (complex-roles-p (complex-roles-p store))
                               (role-hierarchies-p (role-hierarchies-p store))
                 
                               (i inverse-roles-p)
                               (f features-p)
                               (n (or (member 'unqualified-at-least constructors)
                                      (member 'unqualified-at-most constructors)))
                               (q (or (member 'qualified-at-least constructors)
                                      (member 'qualified-at-most constructors)))
                               (r transitive-roles-p))
    
                          (declare (ignorable role-hierarchies-p))

                          (setf language

                                (if (or complex-roles-p q)
           
                                    (to-be-implemented 'dl)

                                  (if i 

                                      (if r

                                          (if f 
                  
                                              (if n
                      
                                                  +alchifn-rplus+

                                                +alchif-rplus+)

                                            (if n
                
                                                +alchifn-rplus+

                                              +alchi-rplus+))

                                        (if f 
                  
                                            (if n
                      
                                                +alchifn-rplus+
                    
                                              +alchif-rplus+)

                                          (if n
                    
                                              +alchifn-rplus+

                                            +alchi+)))

                                    (if r

                                        (if f

                                            (if n 

                                                +alchfn-rplus+

                                              +alchf-rplus+)

                                          (if n 

                                              +alchfn-rplus+

                                            +alchf-rplus+))

                                      (if f
              
                                          (if n 
                  
                                              +alchfn-rplus+

                                            +alchf+)

                                        (if n 

                                            +alchn+

                                          +alch+))))))))
    store))

;;;
;;;
;;;

(defmethod register-inverses ((role role) (inv-role role))
  (setf (slot-value role 'inverse-role) inv-role      
        (slot-value inv-role 'inverse-role) role))

(defmethod get-inverse-role ((role role))
  (or (slot-value role 'inverse-role)
      (when *create-inverse-role-p* 
        (let* ((*create-inverse-role-p* nil)
               (inv-role 
                (parse-role (list 'inv (textual-description role)))))
          (register-inverses role inv-role)
          inv-role))))

;;;
;;;
;;;

(defmethod implies-p ((i role) (j role)  &rest args)
  ;;; fängt alle nicht spezifizierten Fälle ab
  nil)

;;;
;;;
;;;

(defmethod implies-p ((i simple-role) (j simple-role) &rest args)
  (or (eq i j)
      #|
      (equal (textual-description i) 
             (textual-description j))
      (some #'(lambda (j)
                (implies-p i j))
            (subroles j)) |#
      (member j (all-superroles i))))

(defmethod implies-p ((i simple-role) (j or-role) &rest args)
  (some #'(lambda (j) 
            (implies-p i j))
        (arguments j)))

(defmethod implies-p ((i simple-role) (j and-role) &rest args)
  (every #'(lambda (j) 
             (implies-p i j))
         (arguments j)))

;;;
;;;
;;;

(defmethod implies-p ((i or-role) (j or-role)  &rest args)
  (every #'(lambda (i)
             (implies-p i j))
         (arguments i)))


(defmethod implies-p ((i or-role) (j and-role)  &rest args)
  (every #'(lambda (j)
             (implies-p i j))
         (arguments j)))

;;;
;;;
;;;

(defmethod implies-p ((i and-role) (j simple-role)  &rest args)
  (some #'(lambda (i)
            (implies-p i j))
        (arguments i)))

(defmethod implies-p ((i and-role) (j and-role)  &rest args)
  (every #'(lambda (j)
             (implies-p i j))
         (arguments j)))

(defmethod implies-p ((i and-role) (j or-role)  &rest args)
  (some #'(lambda (j)
            (implies-p i j))
        (arguments j)))

;;;
;;;
;;;

(defpersistentclass atomic-concept (concept)
  ((name :reader name :initarg :name)
   (boolean-concept-p :initform t)
   (primitive-p :accessor primitive-p :initform nil)
   (cyclic-p :accessor cyclic-p :initform nil) ; f. blocking 
   (ts-cyclic-p :accessor ts-cyclic-p :initform nil) ; f. Told Subsumer Bestimmung!

   (negated-p :accessor negated-p :initform nil :initarg :negated-p)

   (references-atoms :accessor references-atoms :initform nil 
                     :initarg :references-atoms)

   (constructors :initform '(atoms))))
   
(defpersistentclass top-concept (atomic-concept)
  ((boolean-concept-p :initform t)
   ;; denn  TOP => ... -Axiome sind immer GCIs! 

   (inconsistent :initform nil)
   (satisfiable :initform t)
   (tautological :initform t)
   
   (negated-p :initform nil)
   (name :initform 'top)

   (constructors :initform '(top))))

(defpersistentclass bottom-concept (atomic-concept)
  ((boolean-concept-p :initform t)
   (inconsistent :initform t)
   (satisfiable :initform nil)
   (tautological :initform nil)
   
   (negated-p :initform nil)
   (name :initform 'bottom)

   (constructors :initform '(bottom))))

;;;
;;;
;;;

(defpersistentclass and/or-concept (concept)
  ((arguments :reader arguments :initarg :arguments)
   (arg-counter :reader arg-counter)
   
   (atomic-arguments :reader atomic-arguments)))

;;;
;;;
;;;

(defun no-of-atoms ()
  (hash-table-count *atoms-store*))

(defun no-of-ands ()
  (hash-table-count *and-store*))

(defun no-of-ors ()
  (hash-table-count *or-store*))

(defun no-of-somes ()
  (hash-table-count *some-store*))

(defun no-of-alls ()
  (hash-table-count *all-store*))

(defun no-of-at-leasts ()
  (hash-table-count *at-least-store*))

(defun no-of-at-mosts ()
  (hash-table-count *at-most-store*))

(defun no-of-concepts ()
  (+ (no-of-atoms)
     (no-of-ands)
     (no-of-ors)
     (no-of-somes)
     (no-of-alls)
     (no-of-at-leasts)
     (no-of-at-mosts)))

;;;
;;;
;;;

(defpersistentclass and-concept (and/or-concept)
  ((modal-successor-sets :reader modal-successor-sets :initform nil)

   (constructors :initform '(and))))

(defpersistentclass or-concept (and/or-concept)
  ((constructors :initform '(or))))

(defpersistentclass some/all-concept (concept)
  ((role :reader role :initarg :role)
   (qualification :reader qualification :initarg :qualification)
   (boolean-concept-p :initform nil)))


(defpersistentclass some-concept (some/all-concept))

(defpersistentclass attribute-exists-concept (some-concept))

#|
(defpersistentclass concrete-attribute-exists-concept (concept))
|#

(defpersistentclass at-least/most-concept (some/all-concept)
  ((n :reader n :initarg :n)))

(defpersistentclass at-least-concept (at-least/most-concept))

(defpersistentclass at-most-concept (at-least/most-concept))

(defpersistentclass all-concept (some/all-concept))

;;;
;;;

(defmethod reset-sat-status :after ((concept and-concept))
  (with-slots (arguments) concept
    (dolist (arg arguments)
      (reset-sat-status arg))))

;;;
;;;
;;;

(defmethod initialize-instance :after ((concept concept) &rest initargs &key dont-initialize-p)
  (unless dont-initialize-p  
    (setf (slot-value concept 'id) (incf (id-counter *cur-store*)))
    (setf (slot-value concept 'tbox) (tbox *cur-store*))
    (setf (slot-value concept 'concept-store) *cur-store*)
    (when *insert-into-store-p*
      (insert-into-store concept))))
  
(defmethod initialize-instance :after ((concept and/or-concept) &rest initargs &key dont-initialize-p)
  (unless dont-initialize-p  
    (setf (slot-value concept 'atomic-arguments)
          (remove-if-not #'is-atomic-concept-p (arguments concept))
          (slot-value concept 'boolean-concept-p)
          (every #'boolean-concept-p (arguments concept))
          (constructors concept)
          (delete-duplicates (reduce #'append (mapcar #'constructors (arguments concept))))
          (slot-value concept 'arg-counter) 
          (length (arguments concept)))))

(defmethod initialize-instance :after ((concept and-concept) &rest initargs &key dont-initialize-p)
  (unless dont-initialize-p  

    (when *syntactic-consistency-checking-p*

      (when *syntactic-consistency-checking-of-some/all-conjuncts-p*      
        (setf (slot-value concept 'modal-successor-sets)
              (let* ((args (arguments concept))
                     (alls (remove-if-not #'is-all-concept-p args))
                     (somes (remove-if-not #'is-some-concept-p args))
                     (res nil))
              
                (dolist (some somes)
                  (let* ((role (role some))
                         (alls (loop as all in alls when 
                                     (eq (role all) role)
                                     collect all))
                         (concept (make-and-concept 
                                   (list (qualification some) 
                                         (mapcar #'qualification alls)))))
                    (push (list role alls somes concept) res)))
                res)))

      (check-if-obviously-inconsistent-p concept))))

(defmethod initialize-instance :after ((concept or-concept) &rest initargs &key dont-initialize-p)
  (unless dont-initialize-p
    (when *syntactic-consistency-checking-p*
      (check-if-obviously-inconsistent-p concept))))

(defmethod initialize-instance :after ((concept some-concept) &rest initargs &key dont-initialize-p)
  (unless dont-initialize-p
    (when *syntactic-consistency-checking-p*
      (check-if-obviously-inconsistent-p concept))

    (setf (constructors concept)
          (remove-duplicates 
           (cons (if (is-top-concept-p (qualification concept))
                     'unqualified-some 
                   'qualified-some)
                 (constructors (qualification concept)))))))

(defmethod initialize-instance :after ((concept all-concept) &rest initargs &key dont-initialize-p)
  (unless dont-initialize-p
    (when *syntactic-consistency-checking-p*    
      (check-if-obviously-inconsistent-p concept))

    (setf (constructors concept)
          (remove-duplicates 
           (cons 'all
                 (constructors (qualification concept)))))))

(defmethod initialize-instance :after ((concept at-least-concept) &rest initargs &key dont-initialize-p)
  (unless dont-initialize-p
    (when *syntactic-consistency-checking-p*    
      (check-if-obviously-inconsistent-p concept))

    (setf (constructors concept)
          (remove-duplicates 
           (cons (if (is-top-concept-p (qualification concept))
                     'unqualified-at-least
                   'at-least)
                 (constructors (qualification concept)))))))

(defmethod initialize-instance :after ((concept at-most-concept) &rest initargs &key dont-initialize-p)
  (unless dont-initialize-p
    (when *syntactic-consistency-checking-p*    
      (check-if-obviously-inconsistent-p concept))

    (setf (constructors concept)
          (remove-duplicates 
           (cons (if (is-top-concept-p (qualification concept))
                     'unqualified-at-most                    
                   'at-most)
                 (constructors (qualification concept)))))))

;;;
;;;
;;;

(defun order (args)
  (sort args #'< :key #'id))

(defmethod insert-into-store ((concept concept))
  t)

(defmethod insert-into-store :after ((concept concept))
  
  (unless *dont-invalidate-store-p* 
    
    ;;; wird nur invalidiert, wenn 
    ;;; der Prover nicht laeuft, sonst ewiges
    ;;; reprepare, es werden ja temporaere Konzepte
    ;;; angelegt beim Beweisen! aber die Sprache
    ;;; aendert sich dann nciht mehr! 
    
    (reset-sat-status *cur-store*))

  (push concept (all-concepts *cur-store*)))

(defmethod insert-into-store ((concept atomic-concept))
  (setf (gethash (original-description concept) *atoms-store*) concept)
  (push concept (all-atoms (concept-store concept)))
  (if (negated-p concept)
      (push concept (negative-atoms (concept-store concept)))
    (push concept (positive-atoms (concept-store concept)))))

(defmethod insert-into-store ((concept and-concept))
  (let ((args (arguments concept)))
    (setf (gethash (arguments concept) *and-store*) concept)
    (when *cross-referencing-p*
      (mapc #'(lambda (arg)
                (pushnew concept (referenced-by arg))
                (unless (member concept (referenced-by-ands arg))
                  (incf (slot-value arg 'and-ref-counter))
                  (push concept (referenced-by-ands arg))))
            args))))

(defmethod insert-into-store ((concept or-concept))
  (let ((args (arguments concept)))
    (setf (gethash (arguments concept) *or-store*) concept)
    (when *cross-referencing-p*
      (mapc #'(lambda (arg)
                (pushnew concept (referenced-by arg))
                (unless (member concept (referenced-by-ors arg))
                  (incf (slot-value arg 'or-ref-counter))
                  (push concept (referenced-by-ors arg))))
            args))))

(defmethod insert-into-store ((concept some-concept))
  (let ((qual (qualification concept)))
    (when *cross-referencing-p* 
      (push concept (referenced-by qual)))
    (setf (gethash (list (role concept) qual) *some-store*) concept)))


(defmethod insert-into-store ((concept all-concept))
  (let ((qual (qualification concept)))
    (when *cross-referencing-p* 
      (push concept (referenced-by qual)))
    (setf (gethash (list (role concept) qual) *all-store*) concept)))


(defmethod insert-into-store ((concept at-least-concept))
  (let ((qual (qualification concept))
        (n (n concept)))
    (when *cross-referencing-p* 
      (push concept (referenced-by qual)))
    (setf (gethash (list n (role concept) qual) *at-least-store*) concept)))

(defmethod insert-into-store ((concept at-most-concept))
  (let ((qual (qualification concept))
        (n (n concept)))
    (when *cross-referencing-p* 
      (push concept (referenced-by qual)))
    (setf (gethash (list n (role concept) qual) *at-most-store*) concept)))

;;;
;;;
;;;

(defmethod print-object ((concept atomic-concept) stream)
  (if *print-pretty*
      (if (negated-p concept)
          (format stream "(NOT ~A)" (name concept))
        (format stream "~A" (name concept)))
    (if (negated-p concept)
        (format stream "#<(NOT ~A|~A)>" (name concept) (id concept))
      (format stream "#<~A|~A>" (name concept) (id concept)))))


(defmethod print-object ((concept top-concept) stream)
  (if *print-pretty* 
      (format stream "TOP")
    (format stream "#<TOP>")))

(defmethod print-object ((concept bottom-concept) stream)
  (if *print-pretty* 
      (format stream "BOTTOM")
    (format stream "#<BOTTOM>")))
  
(defmethod print-object ((concept and-concept) stream)
  (if *print-pretty*
      (format stream "(AND~{ ~A~})" (arguments concept))
    (format stream "#<(AND~{ ~A~}|~A)>" (arguments concept) (id concept))))

(defmethod print-object ((concept or-concept) stream)
  (if *print-pretty*
      (format stream "(OR~{ ~A~})" (arguments concept))
    (format stream "#<(OR~{ ~A~}|~A)>" (arguments concept) (id concept))))

(defmethod print-object ((concept some-concept) stream)
  (if *print-pretty*
      (format stream "(SOME ~A ~A)" (role concept) (qualification concept))
    (format stream "#<(SOME ~A ~A)|~A>" (role concept) (qualification concept) (id concept))))

(defmethod print-object ((concept all-concept) stream)
  (if *print-pretty*
      (format stream "(ALL ~A ~A)" (role concept) (qualification concept))
    (format stream "#<(ALL ~A ~A)|~A>" (role concept) (qualification concept) (id concept))))

(defmethod print-object ((concept at-least-concept) stream)
  (if *print-pretty*
      (format stream "(AT-LEAST ~A ~A ~A)" (n concept) (role concept) (qualification concept))
    (format stream "#<(AT-LEAST ~A ~A ~A)|~A>" (n concept) (role concept) (qualification concept) (id concept))))

(defmethod print-object ((concept at-most-concept) stream)
  (if *print-pretty*
      (format stream "(AT-MOST ~A ~A ~A)" (n concept) (role concept) (qualification concept))
    (format stream "#<(AT-MOST ~A ~A ~A)|~A>" (n concept) (role concept) (qualification concept) (id concept))))

;;;
;;;
;;;

(defmethod unparse ((item null))
  nil)

(defmethod unparse ((item cons))
  (cons (unparse (car item))
        (unparse (cdr item))))

(defmethod unparse ((role role))
  (let ((*print-pretty* t)
	(*package* (find-package :prover)))
    (if (not (inverse-p role))
        (read-from-string 
         (format nil "|~A|" (print-object role nil)))
      `(inv ,(unparse (slot-value role 'inverse-role))))))

(defmethod unparse ((concept concept))
  (let ((*print-pretty* t)
	(*package* (find-package :prover)))
    (read-from-string 
     (format nil "|~A|" (print-object concept nil)))))


;;;
;;;
;;;

(defmethod get-negated-description ((concept concept))
  (get-negated-concept concept))

;;;
;;;
;;;

(defmethod get-negated-concept :around ((concept concept))
  (with-concept-store ((concept-store concept))
    (let ((*create-negated-concept-p* nil))
      (call-next-method))))

(defmethod get-negated-concept ((concept bottom-concept))
  (or (negated-concept concept)
      (register-duals 
       (make-top-concept)
       concept)))

(defmethod get-negated-concept ((concept top-concept))
  (or (negated-concept concept)
      (register-duals 
       (make-bottom-concept)
       concept)))

(defmethod get-negated-concept ((concept atomic-concept))
  (or (negated-concept concept)
      (register-duals
       (make-atomic-concept (if (negated-p concept)
                                (name concept)
                              `(not ,(name concept))))
       concept)))

(defmethod get-negated-concept ((concept and-concept))
  (or (negated-concept concept)
      (register-duals
       (make-or-concept (mapcar #'get-negated-concept 
                                (arguments concept)))
       concept)))
  
(defmethod get-negated-concept ((concept or-concept))
  (or (negated-concept concept)
      (register-duals 
       (make-and-concept (mapcar #'get-negated-concept
                                 (arguments concept)))
       concept)))
  
(defmethod get-negated-concept ((concept some-concept))
  (or (negated-concept concept) 
      (register-duals 
       (make-all-concept  
        (role concept)
        (get-negated-concept (qualification concept)))
       concept)))

(defmethod get-negated-concept ((concept all-concept))
  (or (negated-concept concept) 
      (register-duals 
       (make-some-concept  
        (role concept)
        (get-negated-concept (qualification concept)))
       concept)))


(defmethod get-negated-concept ((concept at-least-concept))
  (or (negated-concept concept) 
      (register-duals 
       (make-at-most-concept  
        (1- (n concept))
        (role concept)
        (qualification concept))
       concept)))

(defmethod get-negated-concept ((concept at-most-concept))
  (or (negated-concept concept) 
      (register-duals 
       (make-at-least-concept  
        (1+ (n concept))
        (role concept)
        (qualification concept))
       concept)))

;;;
;;;
;;;


(defun register-duals (cona conb)
  (setf (slot-value cona 'negated-concept) conb
        (slot-value conb 'negated-concept) cona)
  cona)

;;;
;;;
;;;

(defun find-atomic-concept (expr)
  (when *use-store-p*
    (gethash expr *atoms-store*)))

(defun find-and-concept (args)  
  (when *use-store-p*
    (gethash args *and-store*)))

(defun find-or-concept (args)  
  (when *use-store-p*
    (gethash args *or-store*)))

(defun find-some-concept (role qual)  
  (when *use-store-p*
    (gethash (list role qual) *some-store*)))

(defun find-all-concept (role qual)  
  (when *use-store-p*
    (gethash (list role qual) *all-store*)))

(defun find-at-least-concept (n role qual)  
  (when *use-store-p*
    (gethash (list n role qual) *at-least-store*)))

(defun find-at-most-concept (n role qual)  
  (when *use-store-p*
    (gethash (list n role qual) *at-most-store*)))

;;;
;;;
;;;

(defun make-top-concept ()
  (with-concept-store (*cur-store*)    
    (let ((concept (or (find-atomic-concept 'top)
                       (make-instance 'top-concept
                                      :original-description 'top
                                      :old-p *old-concept-p*))))
      (if *create-negated-concept-p*
          (values concept 
                  (get-negated-concept concept))
        concept))))

(defun make-bottom-concept () 
  (with-concept-store (*cur-store*)    
    (let ((concept (or (find-atomic-concept 'bottom)
                       (make-instance 'bottom-concept
                                      :original-description 'bottom
                                      :old-p *old-concept-p*))))                        
      (if *create-negated-concept-p*
          (values concept 
                  (get-negated-concept concept))
        concept))))

;;;
;;;
;;;

(defun make-not-concept (arg)
  (unless (get-negated-concept arg)
    (error "Can't get negated concept of ~A!" arg))
  (values (negated-concept arg)
          arg))

(defun make-atomic-concept (expr)
  (with-concept-store (*cur-store*)    
    (if (atomic-concept*-p expr) 
        (let ((concept (or (find-atomic-concept expr)
                           (make-instance 'atomic-concept
                                          :original-description expr
                                          :name (if (symbolp expr)
                                                    expr
                                                  (second expr))
                                          :old-p *old-concept-p*
                                          :negated-p
                                          (if (symbolp expr)
                                              nil
                                            (and (consp expr)
                                                 (not (cddr expr))
                                                 (eq (first expr) 'not)))))))
          (if *create-negated-concept-p*
              (values concept 
                      (get-negated-concept concept))
            concept))
      (error "Bad concept: ~A!" expr))))

(defun make-and-concept (args)
  (with-concept-store (*cur-store*)    

    (if (every #'is-concept-p args)
      
        (let ((fargs nil))
        
          (dolist (arg args)
            (unless (is-top-concept-p arg)
              (if (is-and-concept-p arg)
                  (dolist (arg (arguments arg))
                    (unless (is-top-concept-p arg)
                      (push arg fargs)))
                (push arg fargs))))

          (let ((args (order (remove-duplicates fargs))))

            (if (not args)
                (make-top-concept)
              (if (cdr args)
                  (let ((concept 
                         (or (find-and-concept args)
                             (make-instance 'and-concept 
                                            :original-description `(and ,@args)
                                            :old-p *old-concept-p*
                                            :arguments args))))
                    (if *create-negated-concept-p*
                        (values concept 
                                (get-negated-concept concept))
                      concept))
                (if *create-negated-concept-p*
                    (values (first args)
                            (get-negated-concept (first args)))
                  (first args))))))
    
      (error "Bad concept: (AND ~A)!" args))))

(defun make-or-concept (args)
  (with-concept-store (*cur-store*)    

    (if (every #'is-concept-p args)
        (let ((fargs nil))

          (dolist (arg args)
            (unless (is-bottom-concept-p arg)
              (if (is-or-concept-p arg)
                  (dolist (arg (arguments arg))
                    (unless (is-bottom-concept-p arg)
                      (push arg fargs)))
                (push arg fargs))))

          (let ((args (order (remove-duplicates fargs))))

            (if (not args)
                (make-bottom-concept)
              (if (cdr args)
                  (let ((concept 
                         (or (find-or-concept args)
                             (make-instance 'or-concept
                                            :original-description `(or ,@args)
                                            :old-p *old-concept-p*
                                            :arguments args))))
                    (if *create-negated-concept-p*
                        (values concept 
                                (get-negated-concept concept))
                      concept))
                (if *create-negated-concept-p* 
                    (values (first args)
                            (get-negated-concept (first args)))
                  (first args))))))
    
      (error "Bad concept: (OR ~A)!" args))))


(defun make-some-concept (role qualification)
  (with-concept-store (*cur-store*)    
    (if (and (typep role 'role)
             (typep qualification 'concept))
        (let ((concept
               (or (find-some-concept role qualification)
                   (make-instance (if (feature-p role)
                                      'attribute-exists-concept
                                    'some-concept)
                                  :original-description `(some ,role ,qualification)
                                  :role role
                                  :old-p *old-concept-p*
                                  :qualification qualification))))
          (if *create-negated-concept-p*
              (values concept 
                      (get-negated-concept concept))
            concept))
      (error "Bad concept: (SOME ~A ~A)!" role qualification))))


(defun make-all-concept (role qualification)
  (with-concept-store (*cur-store*)    
    (if (and (typep role 'role)
             (typep qualification 'concept))
        (let ((concept
               (or (find-all-concept role qualification)
                   (make-instance 'all-concept 
                                  :original-description `(all ,role ,qualification)
                                  :role role
                                  :old-p *old-concept-p*
                                  :qualification qualification))))
          (if *create-negated-concept-p*
              (values concept 
                      (get-negated-concept concept))
            concept))
      (error "Bad concept: (ALL ~A ~A)!" role qualification))))

(defun make-at-least-concept (n role qualification)
  (with-concept-store (*cur-store*)    
    (if (and (typep role 'role)
             (integerp n)
             (not (minusp n))
             (typep qualification 'concept))
        (cond ((zerop n)
               (make-top-concept))
              ((zerop (1- n))
               (make-some-concept role qualification))
              (t 
               (let ((concept
                      (or (find-at-least-concept n role qualification)
                          (make-instance 'at-least-concept 
                                         :old-p *old-concept-p*
                                         :original-description `(at-least ,n ,role ,qualification)
                                         :role role
                                         :n n 
                                         :qualification qualification))))
                 (if *create-negated-concept-p*
                     (values concept 
                             (get-negated-concept concept))
                   concept))))
      (error "Bad concept: (AT-LEAST ~A ~A ~A)!" n role qualification))))


(defun make-at-most-concept (n role qualification)
  (with-concept-store (*cur-store*)    
    (if (and (typep role 'role)
             (integerp n)
             (not (minusp n))
             (typep qualification 'concept))
        (cond ((zerop n)
               (make-all-concept role (get-negated-concept qualification)))
              (t 
               (let ((concept
                      (or (find-at-most-concept n role qualification)
                          (make-instance 'at-most-concept 
                                         :old-p *old-concept-p*
                                         :original-description `(at-most ,n ,role ,qualification)
                                         :role role
                                         :n n 
                                         :qualification qualification))))
                 (if *create-negated-concept-p*
                     (values concept 
                             (get-negated-concept concept))
                   concept))))
      (error "Bad concept: (AT-MOST ~A ~A ~A)!" n role qualification))))
    
;;;
;;;
;;;

(defun atomic-concept*-p (term)
  (or (symbolp term)
      (and (consp term)
	   (eq (first term) 'not)
           (symbolp (second term)))))

(defun top-concept*-p (concept)
  (or (eq concept 'top)
      (eq concept '*top*)
      (eq concept '+top+)      
      (eq concept t)))

(defun bottom-concept*-p (concept)
  (or (eq concept 'bottom)
      (eq concept 'bot)      
      (eq concept '*bot*)      
      (eq concept '+bot+)      
      (eq concept '*bottom*)
      (eq concept nil)))

(defun all-concept*-p (concept)
  (and (consp concept) 
       (eq (first concept) 'all)
       (cddr concept)
       (not (cdddr concept))))

(defun at-least-concept*-p (concept)
  (and (consp concept) 
       (eq (first concept) 'at-least)
       ;; (cdddr concept)
       (not (cddddr concept))))

(defun at-most-concept*-p (concept)
  (and (consp concept) 
       (eq (first concept) 'at-most)
       ;; (cdddr concept)
       (not (cddddr concept))))

(defun exactly-concept*-p (concept)
  (and (consp concept) 
       (eq (first concept) 'exactly)
       ;; (cdddr concept)
       (not (cddddr concept))))


(defun or-concept*-p (concept)
  (and (consp concept)
       (eq (first concept) 'or)))

(defun and-concept*-p (concept)
  (and (consp concept) 
       (eq (first concept) 'and)))

(defun implies-concept*-p (concept)
  (and (consp concept) 
       (eq (first concept) '=>)
       (cddr concept)
       (not (cdddr concept))))

(defun implied-by-concept*-p (concept)
  (and (consp concept) 
       (eq (first concept) '<=)
       (cddr concept)
       (not (cdddr concept))))

(defun equivalent-concept*-p (concept)
  (and (consp concept) 
       (eq (first concept) '<=>)
       (cddr concept)
       (not (cdddr concept))))

(defun some-concept*-p (concept)
  (and (consp concept)
       (eq (first concept) 'some)
       (cddr concept)
       (not (cdddr concept))))

(defun cd-concept*-p (concept)
  (and (consp concept) 
       (let ((op (first concept)))
         (member op '(a an no string= string<> min max < > <= >= =)))))

;;;
;;;
;;;

(defun flatten (concept)
  (if (consp concept)
      (let ((op (first concept)))
	(case op
	  ((and or => <= <=>)
	   (if (and (consp concept)
		    (null (cddr concept)))
	       (flatten (second concept))
	     `(,op ,@(remove-duplicates
                      (mapcan #'(lambda (x)
                                  (let ((res (flatten x)))
                                    (if (and (consp res)
                                             (eq (first res) op))
                                        (rest res)
                                      (list res))))
                              (rest concept))))))
	  ((some all)
	   `(,op ,(second concept) 
		 ,(flatten (third concept))))
          ((at-least at-most exactly)
           `(,op ,(second concept) 
		 ,(third concept) 
		 ,(flatten (fourth concept))))
	  (not
	   `(,op ,(flatten (second concept))))))
    concept))

(defun parse-concept (concept)
  (labels ((do-it (concept)
             (cond ((top-concept*-p concept)
                    (make-top-concept))
                   ((bottom-concept*-p concept)
                    (make-bottom-concept))           
                   
                   ((and (consp concept) ;;; wichtig! reihenfolge! 
                         (eq (first concept) 'not))
                    (make-not-concept (do-it (second concept))))                   
                   ((atomic-concept*-p concept)
                    (make-atomic-concept concept))
                   ((and-concept*-p concept)
                    (make-and-concept (mapcar #'do-it (rest concept))))
                   ((or-concept*-p concept)
                    (make-or-concept (mapcar #'do-it (rest concept))))
                   
                   ((some-concept*-p concept)
                    (make-some-concept (parse-role (second concept)
                                                   :textually-used-p t)
                                       (do-it (third concept))))
                   ((all-concept*-p concept)
                    (make-all-concept (parse-role (second concept)
                                                  :textually-used-p t)
                                      (do-it (third concept))))
                   
                   ((at-least-concept*-p concept)
                    (make-at-least-concept (second concept) 
                                           (parse-role (third concept)
                                                       :textually-used-p t)
                                           (if (fourth concept)
                                               (do-it (fourth concept))
                                             (make-top-concept))))
                   ((at-most-concept*-p concept)
                    (make-at-most-concept (second concept) 
                                          (parse-role (third concept)
                                                      :textually-used-p t)
                                          (if (fourth concept)
                                              (do-it (fourth concept))
                                            (make-top-concept))))

                   ((exactly-concept*-p concept)
                    (do-it `(and (at-least ,@(rest concept))
                                 (at-most ,@(rest concept)))))
                   
                   ;;; =>, <=, <=>
        
                   ((implies-concept*-p concept)
                    (make-or-concept (list (make-not-concept (do-it (second concept)))
                                           (do-it (third concept)))))
                   ((implied-by-concept*-p concept)
                    (make-or-concept (list (make-not-concept (do-it (third concept)))
                                           (do-it (second concept)))))
                   ((equivalent-concept*-p concept)
                    (make-and-concept 
                     (list (make-or-concept (list (make-not-concept (do-it (second concept)))
                                                  (do-it (third concept))))
                           (make-or-concept (list (make-not-concept (do-it (third concept)))
                                                  (do-it (second concept)))))))

                   ;;; RacerPro Dummies

                   ((cd-concept*-p concept) 
                    (make-top-concept))

                   ;;;
                   ;;;
                   ;;
        
                   (t (error "Unable to parse ~A!" concept)))))

    (if (is-concept-p concept)
        
        concept

      (let ((concept (do-it concept)))

        (if *syntactic-consistency-checking-p* 
            (cond ((check-if-obviously-inconsistent-p concept)
                   (make-bottom-concept))
                  ((check-if-obviously-tautological-p concept)
                   (make-top-concept))
                  (t concept))
          concept)))))

;;;
;;;
;;;

(defmethod check-if-obviously-inconsistent-p :around ((concept concept))
  (when *syntactic-consistency-checking-p*
    (when (eq (slot-value concept 'inconsistent) :not-tested)
      (let ((res (call-next-method)))
        (when res ;; Achtung - der Algorithmus ist unvollstaendig!
          (setf (slot-value concept 'inconsistent) t)
          (setf (slot-value concept 'satisfiable) nil)
          (setf (slot-value concept 'tautological) nil)))))
  
  (eq (slot-value concept 'inconsistent) t))

(defmethod check-if-obviously-inconsistent-p ((concept bottom-concept))
  t)

(defmethod check-if-obviously-inconsistent-p ((concept top-concept))
  nil)

(defmethod check-if-obviously-inconsistent-p ((concept atomic-concept))
  nil)

(defmethod check-if-obviously-inconsistent-p ((concept and-concept))
  (or (some #'check-if-obviously-inconsistent-p (arguments concept))
      (some #'(lambda (atom) 
                (member (get-negated-concept atom)
                        (atomic-arguments concept)))
            (atomic-arguments concept))
      (some #'(lambda (modal-set) 
                (check-if-obviously-inconsistent-p (fourth modal-set)))
            (modal-successor-sets concept))))

(defmethod check-if-obviously-inconsistent-p ((concept or-concept))
  (every #'check-if-obviously-inconsistent-p (arguments concept)))


(defmethod check-if-obviously-inconsistent-p ((concept some-concept))
  (check-if-obviously-inconsistent-p (qualification concept)))

(defmethod check-if-obviously-inconsistent-p ((concept all-concept))
  nil)

(defmethod check-if-obviously-inconsistent-p ((concept at-least-concept))
  (check-if-obviously-inconsistent-p (qualification concept)))

(defmethod check-if-obviously-inconsistent-p ((concept at-most-concept))
  nil)

;;;
;;;
;;;

(defmethod check-if-obviously-tautological-p :around ((concept concept))
  (when *syntactic-consistency-checking-p*
    (when (eq (slot-value concept 'tautological) :not-tested)
      (let ((res (call-next-method)))
        (when res
          (setf (slot-value concept 'tautological) t)
          (setf (slot-value concept 'satisfiable) t)
          (setf (slot-value concept 'inconsistent) nil)))))
  
  (eq (slot-value concept 'tautological) t))

(defmethod check-if-obviously-tautological-p ((concept concept))
  (check-if-obviously-inconsistent-p (get-negated-concept concept)))

;;;
;;;
;;;

(defmethod already-known-to-be-inconsistent-p ((concept concept))
  (eq (slot-value concept 'inconsistent) t))

(defmethod already-known-to-be-tautological-p ((concept concept))
  (eq (slot-value concept 'tautological) t))

(defmethod already-known-to-be-satisfiable-p ((concept concept))
  (eq (slot-value concept 'satisfiable) t))

;;;
;;;
;;;

(defmethod register-concept-is-satisfiable ((concept concept))
  (unless (already-known-to-be-satisfiable-p concept)
    (with-slots (inconsistent satisfiable tautological) concept
      (setf inconsistent nil
            satisfiable t
            tautological :not-tested)

      (dolist (ref (referenced-by concept))
        (when  (or (is-some-concept-p ref)
                   (is-or-concept-p ref)
                   (is-at-least-concept-p ref)
                   (is-all-concept-p ref)
                   (is-at-most-concept-p ref))

          (register-concept-is-satisfiable ref))))))


(defmethod register-concept-is-satisfiable :after ((concept and-concept))
  (with-slots (arguments) concept
    (dolist (arg arguments)
      (register-concept-is-satisfiable arg))))

;;;
;;;
;;;


(defmethod register-concept-is-inconsistent ((concept concept))
  (unless (already-known-to-be-inconsistent-p concept)
    (with-slots (inconsistent satisfiable tautological) concept
      (setf inconsistent t
            satisfiable nil
            tautological :not-tested)

      (dolist (ref (referenced-by-ands concept))
        (register-concept-is-inconsistent ref))

      (dolist (ref (referenced-by concept))
        (when (or (is-some-concept-p ref)
                  (is-at-least-concept-p ref))
          (register-concept-is-inconsistent ref))))

    (register-concept-is-satisfiable (get-negated-concept concept))))


(defmethod register-concept-is-inconsistent :after ((concept or-concept))
  (with-slots (arguments) concept
    (dolist (arg arguments)
      (register-concept-is-inconsistent arg))))
  

;;;
;;;

(defmethod transitive-p ((all-concept all-concept))
  (transitive-p (role all-concept)))

;;;
;;;
;;;

(defun scramble-ids ()
  (when *cur-store* 
    (with-slots (atoms ands ors somes alls id-counter) *cur-store*
      (let ((ids (reorder (loop as i from 0 to id-counter collect i))))
        (dolist (slot '(atoms ands ors somes alls))
          (let ((table (slot-value *cur-store* slot)))
            (loop as concept being the hash-value of table do
                  (setf (slot-value concept 'id)
                        (pop ids))
                  (unless ids
                    (error "!")))))
        (princ ids)))))

;;;
;;;
;;;

(defmethod get-all-atoms :around ((concept concept))
  (if (slot-boundp concept 'all-atoms)
      (slot-value concept 'all-atoms)
    (setf (slot-value concept 'all-atoms)
          (call-next-method))))

(defmethod get-all-atoms ((concept top-concept))
  (list concept))

(defmethod get-all-atoms ((concept bottom-concept))
  (list concept))

(defmethod get-all-atoms ((concept atomic-concept))
  (list concept))

(defmethod get-all-atoms ((concept and/or-concept))
  (let ((res nil))
    (loop as arg in (arguments concept) do
          (if (is-atomic-concept-p arg)
              (push arg res)
            (dolist (arg (get-all-atoms arg))
              (push arg res))))
    (remove-duplicates res)))

(defmethod get-all-atoms ((concept some/all-concept))
  (get-all-atoms (qualification concept)))

;;;
;;;
;;;

(defmethod get-all-roles :around ((concept concept))
  (if (slot-boundp concept 'all-store-roles) ; auch die features etc. 
      (slot-value concept 'all-store-roles)
    (setf (slot-value concept 'all-store-roles)
          (call-next-method))))

(defmethod get-all-roles ((concept atomic-concept))
  nil)

(defmethod get-all-roles ((concept some/all-concept))
  (cons (role concept)
        (get-all-roles (qualification concept))))

(defmethod get-all-roles ((concept and/or-concept))
  (let ((res nil))
    (loop as arg in (arguments concept) do
          (dolist (role (get-all-roles arg))
            (push role res)))
    (remove-duplicates res)))


;;;
;;;
;;;

(defmethod get-language ((concept-store concept-store) &optional recompute-p)
  (declare (ignorable recompute-p))

  (prepare concept-store +dl+)
  (slot-value concept-store 'language))

(defmethod get-language ((concept concept) &optional recompute-p)
  (declare (ignorable recompute-p))

  (prepare (concept-store concept) +dl+)
  
  (when recompute-p ; wichtig
    (compute-language (concept-store concept)))

  (slot-value (concept-store concept) 'language))


