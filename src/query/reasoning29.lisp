;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

(defgeneric query-consistent-p (query &key &allow-other-keys)) ;; T, NIL, :DONT-KNOW

(defgeneric query-inconsistent-p (query &key &allow-other-keys))

(defgeneric query-tautological-p (query &key &allow-other-keys))

(defgeneric query-entails-p (query1 query2 &key &allow-other-keys))

;;;
;;;
;;;

(defun realize-abox-query-conjuncts (conjuncts &rest args)
  (let ((same-as-mapping 
         (compute-same-as-mapping conjuncts)))
    
    (when (eq same-as-mapping 'inconsistent) 
      (return-from realize-abox-query-conjuncts 'inconsistent))

    (multiple-value-bind (consistent-p new-conjuncts)
        (apply #'abox-query-conjuncts-consistent-p conjuncts
               :realize-p t 
               :same-as-mapping same-as-mapping 
               args)

      (if (not consistent-p)
          'inconsistent
        new-conjuncts))))

;;;
;;;
;;;

(defun adjust-package (sym)
  (change-package-of-description sym 
				 :racer-user
				 :keep-racer-symbols-p nil))

;;;
;;;
;;;

(defun abox-query-conjuncts-consistent-p (conjuncts &key 
                                                    query
                                                    realize-p 
                                                    same-as-mapping
                                                    (ignore-negated-conjuncts-p t)
                                                    ;;; wichtig! 
                                                    (tbox 'racer-user::default)
                                                    rule-con-pattern
                                                    &allow-other-keys)

  ;;;
  ;;; Konstruktion einer ABox aus den Query-Konjunkten: 
  ;;; 
  ;;; (?X C)                         ->  ?x : C
  ;;; (?X (NOT C))                   ->  ?x : (NOT C)

  ;;;
  ;;; PER DEFAULT WERDEN NAF-ATOME IGNORIERT!
  ;;; ALSO (IGNORE-NEGATED-CONJUNCTS-P T)
  ;;; KONSITENZ-CHECK IST UNVOLLSTAENDIG!
  ;;; 
  ;;; ACHTUNG: 
  ;;; FOLGENDES IST FALSCH:
  ;;; (NOT (?X C))  ->  (?X (NOT C))
  ;;;
  ;;; GEGENBEISPIEL: (AND (NOT (?X C)) (NOT (?X (NOT C)))) IST KONSISTENT!
  ;;; WIRKLICH! ABER MIT DER KONSTRUKTION WIRD DARAUS DIE ABOX
  ;;; { ?X : C, ?X : (NOT c) } -> INKONSITENT, IST ABER FALSCH!
  ;;;
  ;;; ES GILT: 
  ;;; 
  ;;; (?X C) -> (NOT (?X (NOT C)))
  ;;; ABER NUR IN DIESER RICHTUNG!
  ;;; (NOT (?X (NOT C))) -> (?X C) IST FALSCH!!!!
  ;;; 
  ;;; ZUDEM NATUERLICH: 
  ;;; (?X (NOT C))) -> (NOT (?X C))
  ;;; Analog fuer die Role Axioms! 

  ;;; (?X ?Y R)                      ->  (?x,?y) : R
  ;;; (?X ?Y (NOT R))                ->  { ?x : (ALL R (NOT MARKER)), ?y : MARKER, (?x,?y) : R }

  ;;; 
  ;;; ACHTUNG: 
  ;;; NAF-ATOME WERDEN IGNORIERT! S.O. 
  ;;; FOLGENDES IST FALSCH: 
  ;;; (NOT (?X ?Y R))                ->  { ?x : (ALL R (NOT MARKER)), ?y : MARKER, (?x,?y) : R }
  ;;;
  
  ;;; (?X (:HAS-KNOWN-SUCCESSOR R))  ->  (?X ?TEMP R)
  ;;; (SAME-AS ?X ?Y)                ->  Replace ?X <- ?Y 
  ;;;

  (labels ((negate-cd-symbol (x) 
             (case x
               (< '>=)
               (> '<=)
               (<= '>)
               (>= '<)
               (= '<>)
               (<> '=)
               (equal 'unequal)
               (unequal 'equal)
               (string= 'string<>)
               (string<> 'string=)
               (otherwise x)))

           (get-bit-vector (n pos)
             (unless (zerop pos)
               (multiple-value-bind (rest bit)
                   (floor n 2)
                 (let ((sym 
                        (intern
                         (format nil "BIT-~A" pos))))
                   (if (zerop bit)
                       (cons sym (get-bit-vector rest (1- pos)))
                     (cons `(not ,sym) (get-bit-vector rest (1- pos))))))))

           (all-concept-assertions-for-voi (x) 
             (loop as entry in conjuncts when 
                   (and (is-instance-retrieval-query-p entry)
                        (eq (get-rep (voi entry)) x)
                        (not (negated-p entry)))
                   collect (dl-concept entry)))

           (get-rep (x) 
             (let ((found (assoc x same-as-mapping)))
               (if found
                   (second found)
                 x))))

    
    (with-critical-section
     (without-unique-name-assumption

       (let* ((tbox #+:midelora 
                    (if (prover::is-tbox-p tbox)
                        (change-package-of-description (prover::tbox-name tbox)
                                                       :racer-user)
                      tbox)
                    #-:midelora 
                    tbox)
		
              (old-abox (current-abox))
              (old-tbox (current-tbox))
		
              (consistent-p :dont-know)
              (new-conjuncts nil)
		
              (orig-vois 
               (reduce #'append
                       (mapcar #'all-vois conjuncts)))
		
              (all-vois
               (remove-duplicates 
                (mapcar #'get-rep orig-vois)))

              (una-vois 
               (remove-if-not #'una-voi-p all-vois))
		
              (non-una-vois 
               (set-difference all-vois una-vois)))

         (declare (ignorable una-vois non-una-vois))

         (unwind-protect 
             (progn
		 
               (init-abox +secret-abox+ tbox)
		 
               (let ((marker (make-hash-table :test #'equal))
                     (add-to-all-individuals nil))
		   

                 (dolist (voi all-vois)
                   (add-concept-assertion +secret-abox+ 
                                          (adjust-package 
                                           (textual-description voi))
                                          'top))
		   
                 (dolist (conjunct conjuncts)

                   (when (or (not ignore-negated-conjuncts-p)
                             (not (negated-p conjunct)))

                     (typecase conjunct

                       (same-as-query
                        ;; hier muss NICHTS GETAN WERDEN!
                        ;; behindert nur, dass ich SAME-AS jedesmals
                        ;; rausfiltern muss
                        )
			 
                       (query-reference
                        (to-be-implemented 'query-reference-reasoning))
			 

                       (top-query
			  
                        (when (negated-p conjunct) 
                          (nrql-error "Bad negated conjunct ~A" conjunct)))                 
			 
                       (bottom-query
			  
                        (when (negated-p conjunct) 
                          (nrql-error "Bad negated conjunct ~A" conjunct))
			  
                        (add-concept-assertion +secret-abox+
                                               (adjust-package 
                                                (textual-description (get-rep (voi conjunct))))
                                               'bottom))
			 
                       (instance-retrieval-query

                        (add-concept-assertion +secret-abox+
                                               (adjust-package 
                                                (textual-description (get-rep (voi conjunct))))
                                               (adjust-package 
                                                (if (negated-p conjunct) 
                                                    `(not ,(dl-concept conjunct))
                                                  (dl-concept conjunct)))))

                         
                       #+:dlmaps
                       ((or spatial-substrate::map-rcc-edge-query)
                          
                        ;;; hat stets nur ein Disjunkt! Basisrelation! 

                          
                        (when spatial-substrate::*add-rcc-atoms-to-abox-for-reasoning*
                            
                          (let ((rcc (cdr (original-description conjunct))))
                              
                            (when (cdr rcc) 
                              (error "RCC non-base relation for reasoning not allowed!
Set *unfold-rcc-disjunctions-p* t!"))

                            (add-role-assertion
                               
                             +secret-abox+
                               
                             (adjust-package
                              (get-cor-voi (parser conjunct)
                                           (textual-description
                                            (get-rep
                                             (voi-from conjunct)))))

                             (adjust-package
                              (get-cor-voi (parser conjunct)
                                           (textual-description
                                            (get-rep
                                             (voi-to conjunct)))))
                               
                             (adjust-package (first rcc))))))
                         

                       ((or edge-retrieval-query 
                            has-known-successor-retrieval-query)

                        (let* ((role (dl-role conjunct))
                               (negated-p (negated-p conjunct))
                               (negated-p (if (negated-racer-role-p role)
                                              (not negated-p)
                                            negated-p)))

                          (unless (gethash role marker)
                            (setf (gethash role marker)
                                  #-:dlmaps 
                                  (create-tbox-internal-marker-concept tbox)
                                  #+(or :dlmaps  :midelora)
                                  (create-marker)))

                          (let ((role (if (negated-racer-role-p role)
                                          (second role)
                                        role)))

                            (if negated-p
				  
                                (let* ((marker (gethash role marker))
                                       (neg-marker `(racer-user::not ,marker))
                                       (concept `(all ,role ,marker)))
				    
                                  (unless marker (error "Marker concept not found!"))

                                  (typecase conjunct
                                    (has-known-successor-retrieval-query
				       
                                     (add-concept-assertion +secret-abox+  
                                                            (adjust-package 
                                                             (textual-description (get-rep (voi conjunct))))
                                                            (adjust-package 
                                                             concept))
                                     (push (list (textual-description (get-rep (voi conjunct))) neg-marker)
                                           add-to-all-individuals))
				      
                                    (edge-retrieval-query

                                     (add-concept-assertion +secret-abox+  
                                                            (adjust-package 
                                                             (textual-description (get-rep (voi-from conjunct))))
                                                            (adjust-package 
                                                             concept))
                                     (add-concept-assertion +secret-abox+  
                                                            (adjust-package 
                                                             (textual-description (get-rep (voi-to conjunct))))
                                                            (adjust-package
                                                             neg-marker)))))
				
                              (typecase conjunct
                                (has-known-successor-retrieval-query 
                                 (add-role-assertion +secret-abox+
                                                     (adjust-package (textual-description (get-rep (voi conjunct))))
                                                     (adjust-package (create-marker 'temp-ind))
                                                     (adjust-package role)))
                                (edge-retrieval-query 
                                 (add-role-assertion +secret-abox+
                                                     (adjust-package (textual-description (get-rep (voi-from conjunct))))
                                                     (adjust-package (textual-description (get-rep (voi-to conjunct))))
                                                     (adjust-package role))))))))
			 
                       (racer-cd-edge-retrieval-query 
                        (let* ((from-attrib (from-attribute conjunct))
                               (to-attrib (to-attribute conjunct))
                               (negated-p (negated-p conjunct))
                               (predicate (constraint conjunct))
                               (predicate (if negated-p 
                                              (if (listp predicate)
                                                  (tree-map #'negate-cd-symbol predicate)
                                                (negate-cd-symbol predicate))
                                            predicate))

                               (from (textual-description (get-rep (voi-from conjunct))))
                               (to (textual-description (get-rep (voi-to conjunct))))

                               (from-cd-obj (intern (format nil "~A-~A" from from-attrib)))
                               (to-cd-obj (intern (format nil "~A-~A" to to-attrib)))

                               (constraint
                                (if (consp predicate)
                                    (tree-map #'(lambda (x)
                                                  (cond ((eq x from) from-cd-obj)
                                                        ((eq x to) to-cd-obj)
                                                        (t x)))
                                              predicate)
                                  ;;; alte Syntax, einfaches Praedikatssymbol
                                  (list predicate from-cd-obj to-cd-obj))))

                          (add-attribute-assertion +secret-abox+ 
                                                   (adjust-package from)
                                                   (adjust-package from-cd-obj)
                                                   (adjust-package from-attrib))

                          (add-attribute-assertion +secret-abox+ 
                                                   (adjust-package to)
                                                   (adjust-package to-cd-obj)
                                                   (adjust-package to-attrib))

                          (add-constraint-assertion +secret-abox+                                                            
                                                    (adjust-package constraint)))))))

		   
                 (dolist (ind-and-concept add-to-all-individuals)
                   (let ((ind (first ind-and-concept))
                         (concept (second ind-and-concept)))
                     (dolist (ind2 (all-individuals +secret-abox+))
                       (unless (same-abox-individual-p ind2 ind)
                         (add-concept-assertion +secret-abox+  
                                                (adjust-package ind2)
                                                (adjust-package concept))))))

                 (when rule-con-pattern
                   (evaluate-rule-consequence query rule-con-pattern))
		   
                 ;;; (princ (all-concept-assertions +secret-abox+))
                 ;;; (terpri)
                 ;;; (princ (all-role-assertions +secret-abox+))
		   
                 (setf consistent-p (abox-consistent-p +secret-abox+))
		   
                 #|
		   (terpri)
		   (terpri)
		   (pprint  (setf *x* (all-concept-assertions)))
		   (pprint (setf *y* (all-role-assertions)))
		   
		   (terpri) 
		   (princ consistent-p)
		   (break) |#
		   
		   
                 (setf new-conjuncts 
                       (when (and realize-p consistent-p)
                         (realize-abox +secret-abox+)

                         (let* ((msis
                                 (remove nil
                                         (mapcar #'(lambda (voi) 
                                                     (let ((msis
                                                            (mapcar #'first
                                                                    ;; eines aus jeder Aequivalenzklasse (das erste)
                                                                    ;; reicht ja!
                                                                    (most-specific-instantiators 
                                                                     (textual-description (get-rep voi))
                                                                     +secret-abox+)))
                                                           (acas (all-concept-assertions-for-voi voi)))

                                                       (unless (equal msis '(*top*))
                                                         (list voi 
                                                               (let ((res 
                                                                      (remove-duplicates `(and ,@acas ,@msis)
                                                                                         :test #'equal)))
                                                                 (if (cddr res)
                                                                     res
                                                                   (second res)))))))
                                                 all-vois))))
			 
                           msis)))

                 (values consistent-p 
                         (if consistent-p 
                             new-conjuncts
                           (list (first orig-vois) 'bottom)))))
	     
	     
           (when (find-abox +secret-abox+)
             (forget-abox +secret-abox+))
	     
           (if (eq *package* (find-package :ts))
               (progn (set-current-tbox old-tbox) 
                 (set-current-abox old-abox))
             (progn (set-current-tbox (change-package-of-description 
                                       old-tbox
                                       :racer-user))
               (set-current-abox (change-package-of-description 
                                  old-abox
                                  :racer-user))))))))))          


(defun simple-description-node-query-conjuncts-consistent-p (conjuncts &rest args 
                                                                       &key
                                                                       (tbox 'racer-user::default) 
                                                                       (package 'racer-user)
                                                                       &allow-other-keys)

  ;;;
  ;;; Hier werden simple-descriptions-queries (:and ...) (:or ...) von
  ;;; Knoten auf Konsistenz geprueft dazu wird Racer als SAT-Checker
  ;;; verwendet, MIT TBox!
  ;;;
  
  (declare (ignore args))

  (let ((clusters nil))
    (loop while conjuncts do
          (let* ((qa (first conjuncts))
                 (cluster
                  (cons qa
                        (remove-if-not #'(lambda (qb)
                                           (equal (type-of qa) 
                                                  (type-of qb))
                                           (equal (vois qa)
                                                  (vois qb)))
                                       (rest conjuncts)))))
            (setf conjuncts
                  (set-difference conjuncts cluster))
            (push cluster clusters)))

    (every #'(lambda (cluster)
               (let ((descr
                      (change-package-of-description 
                       `(and ,@(mapcar #'(lambda (q)
                                           (if (negated-p q)
                                               (typecase q
                                                 (simple-conjunctive-description-query 
                                                  `(not (and ,@(ensure-list (textual-description q)))))
                                                 (simple-disjunctive-description-query 
                                                  `(not (or ,@(ensure-list (textual-description q))))))
                                             (typecase q
                                               (simple-conjunctive-description-query 
                                                `(and ,@(ensure-list (textual-description q))))
                                               (simple-disjunctive-description-query 
                                                `(or ,@(ensure-list (textual-description q)))))))
                                       cluster))
                       package)))

                 ;;; hier wird RACER nur als Sat-Checker fuer Boolsche Formeln verwendet

                 (racer:concept-satisfiable-p descr tbox)))

           clusters)))



(defun simple-description-edge-query-conjuncts-consistent-p (conjuncts &rest args 
                                                                       &key
                                                                       (package 'racer-user)
                                                                       &allow-other-keys)
  ;;;
  ;;; Hier werden simple-descriptions-queries (:and ...) (:or ...) von Kanten auf Konsistenz geprueft
  ;;; dazu wird Racer als SAT-Checker verwendet, aber *ohne* TBox!
  ;;; 

  (declare (ignore args))

  (let ((clusters nil))
    (loop while conjuncts do
          (let* ((qa (first conjuncts))
                 (cluster
                  (cons qa
                        (remove-if-not #'(lambda (qb)
                                           (equal (type-of qa) 
                                                  (type-of qb))
                                           (equal (vois qa)
                                                  (vois qb)))
                                       (rest conjuncts)))))
            (setf conjuncts
                  (set-difference conjuncts cluster))
            (push cluster clusters)))

    (every #'(lambda (cluster)
               (let ((descr
                      (change-package-of-description 
                       `(and ,@(mapcar #'(lambda (q)
                                           (if (negated-p q)
                                               (typecase q
                                                 (simple-conjunctive-description-query 
                                                  `(not (and ,@(ensure-list (textual-description q)))))
                                                 (simple-disjunctive-description-query 
                                                  `(not (or ,@(ensure-list (textual-description q))))))
                                             (typecase q
                                               (simple-conjunctive-description-query 
                                                `(and ,@(ensure-list (textual-description q))))
                                               (simple-disjunctive-description-query 
                                                `(or ,@(ensure-list (textual-description q)))))))
                                       cluster))
                       package)))

                 ;;; hier wird RACER nur als Sat-Checker fuer Boolsche Formeln verwendet

                 (racer:concept-satisfiable-p descr nil)))

           clusters)))



(defun racer-description-node-query-conjuncts-consistent-p (conjuncts 
                                                            &rest args
                                                            &key (tbox 'racer-user::default) &allow-other-keys)

  ;;; Hier werden RACER-Descriptions auf Konsistenz geprueft
  ;;; (?X (:RACER (SOME R C))) (?X (:RACER (ALL R (NOT C)))) -> NIL !
  ;;; die Negationen (eigentlich Negation as Failure!) werden nach innen gezogen
  ;;; das ist eine zulaessige Inferenz (aber uvollstaedig!) (NOT (?X C)) -> (?X (NOT C)), 
  ;;; aber *NICHT* (?X (NOT C)) -> (NOT (?X C))! 

  (declare (ignore args))

  (let ((clusters nil))

    (loop while conjuncts do
          (let* ((qa (first conjuncts))
                 (cluster
                  (cons qa
                        (remove-if-not #'(lambda (qb)
                                           (equal (type-of qa) 
                                                  (type-of qb))
                                           (equal (vois qa)
                                                  (vois qb)))
                                       (rest conjuncts)))))
            (setf conjuncts
                  (set-difference conjuncts cluster))
            (push cluster clusters)))

    ;;(setf *x* clusters) (break)

    (every #'(lambda (cluster)
               (let ((descr
                      `(and ,@(mapcar #'(lambda (q)
                                          (if (negated-p q)        
                                              `(not ,(dl-concept q))
                                            (dl-concept q)))                                      
                                      cluster))))
                 (racer:concept-satisfiable-p descr tbox)))
           clusters)))

;;;
;;;
;;;

(defmethod evaluate-rule-consequence ((query nrql-query) (consequence list))
  (with-slots (parser) query
    (labels ((process (var) 
               (cond ((new-ind-op-p parser var)
                      (let* ((bindings (cddr var))
                             (key (cons (second var) bindings)))
                        (intern (format nil "~A~{-~A~}"
                                        (first key)
                                        (rest key)))))
                     (t var))))

      (let ((abox (mapcar #'(lambda (x)     
                              (let ((op (first x)))
                                (case op
                                  (racer:instance 
                                   (let ((var (second x))
                                         (concept (third x)))
                                     `(,op ,(process var) ,concept)))
                                  ((racer:related racer:constrained)
                                   (let ((from (second x))
                                         (to (third x))
                                         (role (fourth x)))
                                     `(,op
                                       ,(process from)
                                       ,(process to)
                                       ,role)))
                                  (racer:constraints 
                                    `(,op
                                      ,@(mapcar #'(lambda (constraint) 
                                                    (list (first constraint)
                                                          (process (second constraint))
                                                          (process (third constraint))))
                                                (rest x)))))))
                          consequence)))

        (dolist (assertion abox)
          ;;; (eval assertion)
          (ecase (first assertion)
            (instance 
             (apply #'add-concept-assertion +secret-abox+ 
		    (mapcar #'adjust-package (rest assertion))))
            (related 
             (apply #'add-role-assertion +secret-abox+ 
		    (mapcar #'adjust-package (rest assertion))))
            (constrained
             (apply #'add-attribute-assertion +secret-abox+ 
		    (mapcar #'adjust-package (rest assertion))))
            (constraints
              (dolist (constraint (rest assertion))
                (add-constraint-assertion +secret-abox+ (adjust-package constraint))))))))))

;;;
;;;
;;;

(defmethod abox-conjuncts-consistent-p ((from-query and-query) (conjuncts list) &rest args &key &allow-other-keys)
  ;;; Instance / Edge-Retrieval-Queries werden hier geprueft
  (=> conjuncts 
      (apply #'abox-query-conjuncts-consistent-p 
             conjuncts
	     
             :tbox (tbox (substrate (parser from-query)))
	     
             :query from-query
             args)))




#+:dlmaps
(defmethod substrate-conjuncts-consistent-p ((from-query and-query) (conjuncts list) &rest args &key &allow-other-keys)

  (and (let ((conjuncts 
              (remove-if-not #'is-substrate-simple-node-query-p conjuncts)))
         (=> conjuncts
             (apply #'simple-description-node-query-conjuncts-consistent-p  
                    conjuncts
                    args)))
       
       (let ((conjuncts 
              (remove-if-not #'is-substrate-simple-edge-query-p conjuncts)))
         (=> conjuncts
             (apply #'simple-description-edge-query-conjuncts-consistent-p  
                    conjuncts
                    args)))
       
       (let ((conjuncts (remove-if-not #'is-substrate-racer-node-query-p conjuncts)))
         (=> conjuncts  
             (apply #'racer-description-node-query-conjuncts-consistent-p  
                    conjuncts
                    args)))))

;;;
;;;
;;;       

(defmethod query-consistent-p ((query query) &key &allow-other-keys)
  (subclass-responsibility 'query-consistent-p))

(defmethod query-inconsistent-p ((query query) &rest args)
  (eq (apply #'query-consistent-p query args) nil))

;;;
;;;
;;;

(defmethod query-consistent-p :around ((query query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (with-slots (query-satisfiable) query
    (when (eq query-satisfiable :not-tested)
      (setf query-satisfiable (call-next-method)))

    query-satisfiable))

;;;
;;;
;;;

(defmethod query-tautological-p :around ((query query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (with-slots (query-tautological) query
    (when (eq query-tautological :not-tested)
      (setf query-tautological (call-next-method)))

    query-tautological))

;;;
;;;
;;;


(defmethod query-tautological-p ((query query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (eq (query-consistent-p 
       (prep-query `(not ,(unparse-query query))
                   (substrate query)))
      nil))


;;;
;;;
;;;

(defmethod query-consistent-p :before ((query query) &key &allow-other-keys)
  (unless (in-dnf-p query)
    (nrql-error "Query consistency works only for queries in DNF")))

;;;
;;;
;;;

(defmethod query-consistent-p ((query same-as-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  t)

#+:dlmaps
(defmethod query-consistent-p ((query atomic-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (consistent-p (if (negated-p query)
                    (get-negated-description query)
                  query)))

(defmethod query-consistent-p ((query dl-prover-query) &rest args &key &allow-other-keys)
  (apply #'abox-query-conjuncts-consistent-p 
         (list query) 
         :tbox (tbox (substrate (parser query)))
         :query query
         args))

(defmethod query-consistent-p ((query has-known-successor-retrieval-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  t)

(defmethod query-consistent-p ((query binary-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  t)

#+:dlmaps
(defmethod query-consistent-p ((query substrate-predicate-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  :dont-know)

#+:dlmaps
(defmethod query-consistent-p ((query virtual-predicate-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  :dont-know)

(defmethod query-consistent-p ((query top-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  t)

(defmethod query-consistent-p ((query bottom-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  nil)

(defmethod query-consistent-p ((query true-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  t)

(defmethod query-consistent-p ((query false-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  nil)

;;;
;;; 
;;;

(defun prep-query (query substrate &rest args &key &allow-other-keys)
  (apply #'prepare-query query 
         nil
         substrate
         :bind-specials-p nil
         :optimize-p nil
         :generate-code-p nil
         :report-inconsistent-queries-p nil
         :report-tautological-queries-p nil
         :use-repository-p nil
         :rewrite-semantically-p nil
         :rewrite-to-dnf-p t 
         args))

;;;
;;; 
;;;

(defun compute-same-as-mapping (conjuncts) 
  (let* ((positive-same-as-conjuncts
          (remove-if-not #'(lambda (x) 
                             (and (not (negated-p x))
                                  (is-same-as-query-p x)))
                         conjuncts))
	 
         (negative-same-as-conjuncts 
          (remove-if-not #'(lambda (x) 
                             (and (negated-p x)
                                  (is-same-as-query-p x)))
                         conjuncts))

         (same-as-mapping nil))
    
    (dolist (saq positive-same-as-conjuncts)
      (let ((var (voi-from saq)))
        (when (assoc var same-as-mapping)
          ;;; (and (same-as ?x betty) (same-as ?x charles)) ? -> NIL
          (return-from compute-same-as-mapping 'inconsistent))
        (push (list var (voi-to saq)) same-as-mapping)))
    
    (dolist (saq negative-same-as-conjuncts)
      (let* ((var (voi-from saq))
             (found (assoc var same-as-mapping)))
        (when found
          (when (eq (second found)
                    (voi-to saq))
            ;;; (and (same-as ?x betty) (not (same-as ?x betty))) ? -> NIL 
            (return-from compute-same-as-mapping 'inconsistent)))))

    same-as-mapping))

;;;
;;; Primary Method!
;;;


(defmethod query-consistent-p ((query and-query) &rest args &key
                               #+:dlmaps (check-conjuncts-also-individually-p nil)
                               &allow-other-keys)
  
  (let ((conjuncts (subqueries query)))

    (when (some #'is-bottom-query-p (subqueries query))
      (return-from query-consistent-p nil))
    
    #+:dlmaps
    (when check-conjuncts-also-individually-p 
      (when (some #'(lambda (query) 
                      (apply #'query-inconsistent-p query args))
                  conjuncts)
        (return-from query-consistent-p nil)))

    (let ((same-as-mapping 
           (compute-same-as-mapping conjuncts)))

      (when (eq same-as-mapping 'inconsistent) 
        (return-from query-consistent-p nil))

      (let* ((abox        
              (apply #'abox-conjuncts-consistent-p 
                     query
		     (remove-if-not #'is-dl-prover-query-p conjuncts)
                     ;;;(remove-if-not #'is-racer-abox-query-p conjuncts)
                     
                     :same-as-mapping same-as-mapping
                     args))

             (substrate 
              #+:dlmaps
              (apply #'substrate-conjuncts-consistent-p 
                     query
                     (remove-if-not #'is-substrate-query-p conjuncts)
                     :same-as-mapping same-as-mapping
                     args)
              #-:dlmaps
              t)
             
             #-:dlmaps
             (data
              (apply #'data-conjuncts-consistent-p 
                     query
                     (remove-if-not #'is-data-substrate-query-p conjuncts)
                     :same-as-mapping same-as-mapping
                     args)))
	
        (if (or (eq abox nil)
                (eq substrate nil)
                #-:dlmaps (eq data nil)
                #+:dlmaps nil)
            nil
          (if (or (eq abox :dont-know)
                  (eq substrate :dont-know)
                  #-:dlmaps (eq data :dont-know)
                  #+:dlmaps nil)
              :dont-know        
            t))))))


(defmethod query-consistent-p ((query or-query) &rest args &key &allow-other-keys)
  (let ((res nil))
    (or (some #'(lambda (query) 
                  (let ((subres (apply #'query-consistent-p query args)))
                    (when (eq subres :dont-known)
                      (setf res :dont-know))
                    (eq subres t)))
              (subqueries query))
        res)))

;;;
;;;
;;;

(defmethod query-entails-p :around ((query1 query) (query2 query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (when (not (and (in-dnf-p query1)
                  (in-dnf-p query2)))
    
    (nrql-error "Query entailment works only for queries in DNF"))

  (call-next-method))

;;;
;;;
;;;

(defmethod query-entails-p ((query1 query) (query2 query) &rest args &key &allow-other-keys)
  (apply #'query-entails-query-p query1 query2 args))

;;;
;;; f. Klassifikation im DAG benoetigt
;;;

(defmethod query-entails-p ((query1 query) (query2 master-top-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  t)

(defmethod query-entails-p ((query1 master-top-query) (query2 query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  nil)

(defmethod query-entails-p ((query1 master-top-query) (query2 master-top-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  t)

;;;
;;;
;;;

(defmethod query-entails-p ((query1 master-bottom-query) (query2 query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  t)

(defmethod query-entails-p ((query1 query) (query2 master-bottom-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  nil)

(defmethod query-entails-p ((query1 master-bottom-query) (query2 master-bottom-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  t)

;;;
;;;
;;;


(defmethod query-equivalent-p ((query1 query) (query2 query) &rest args &key &allow-other-keys)
  (and (apply #'query-entails-p query1 query2 args)
       (apply #'query-entails-p query2 query1 args)))


;;;
;;; Reduktion auf Unerfuellbarkeit
;;;

(defmethod query-entails-query-p ((query1 query) (query2 query) &rest args
                                  &key enforce-same-arity-p &allow-other-keys)
  (declare (ignorable args))

  (let ((sq1 (cons query1 (all-subqueries query1)))
        (sq2 (cons query2 (all-subqueries query2))))

    (when (and (not (some #'negated-p sq1))
               (not (some #'is-query-reference-p sq1))
               (not (some #'negated-p sq2))
               (not (some #'is-query-reference-p sq2)))

      (when (and (=> enforce-same-arity-p 
                     (variable-vectors-compatible-p query2 query1 :superset))
                 
                 (and (eq (substrate query1)
                          (substrate query2))))
        
        (let* ((substrate (substrate query1))

               (substitution-list 
                (mapcar #'(lambda (sub)
                            (mapcar #'textual-description sub))
                        (remove-if #'(lambda (sub)
                                       (member nil sub))
                                   
                                   (apply #'append
                                          (mapcar #'(lambda (x y)
                                                      (let ((svoi1 (first x))
                                                            (avoi1 (second x))
                                                            (svoi2 (first y))
                                                            (avoi2 (second y)))
                                                        (list (list svoi1 svoi2)
                                                              (list avoi1 avoi2))))
                                                  
                                                  (all-paired-vois query2)
                                                  (all-paired-vois query1)
                                                  )))))

               (query1 (unparse-query query1))
               (query2 (unparse-query query2))
               
               (nquery2 (substitute-vois-in query2 substitution-list)))
          
          (when *debug-p* 
            (format t "~%---------------------------------------------------------------------------------------")
            (format t "~%ENTAILS ~A ~A?~%" query1 query2)
            (terpri) 
            (princ "Substitution list: ")
            (princ substitution-list)
            (terpri)
            (princ "Query1:")
            (pprint query1)
            (terpri)(terpri)
            (princ "Query2:")
            (pprint query2)
            (terpri)(terpri)
            (princ "nQuery2:")
            (pprint nquery2)
            (terpri)(terpri)
            (princ "(AND Query1 (NOT Query2)):")
            (pprint `(and ,query1 (not ,query2)))
            (terpri) (terpri))
          
          (let* ((query (prep-query
                         `(and ,query1 (not ,nquery2))
                         substrate
                         :add-same-as-conjuncts-p nil))
                 (res (not (query-consistent-p query :ignore-negated-conjuncts-p nil))))
	    
            (when *debug-p*
              (princ "REWRITTEN: ") 
              (terpri) (terpri)

              (pprint (unparse-query query))

              (format t "~%REDUCTION TO INCONSISTENCY CHECK - RESULT: ~A~%" res))

            res))))))

