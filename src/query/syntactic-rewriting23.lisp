;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;;
;;;

#+:dlmaps
(defmethod syntactically-rewrite-atomic-query (query (parser simple-parser) &rest args
                                                     &key
                                                     replace-inds-with-vars-p
                                                     negated-p inverse-p &allow-other-keys)
  (declare (ignorable args))
  
  (labels ((gvi (ind)
             (if replace-inds-with-vars-p
                 (get-var-for-ind parser ind)
               ind)))
    
    (let* ((inds (get-inds-from parser query))

           (query
          
            (cond ((top-query-p parser query)
                   (prog1
                       (if negated-p                     
                           `(:bottom ,(gvi (second query)))
                         `(:top ,(gvi (second query))))
                     (setf negated-p nil)))
               
                  ((bottom-query-p parser query)
                   (prog1
                       (if negated-p                     
                           `(:top
                             ,(gvi (second query)))
                         `(:bottom
                           ,(gvi (second query))))
                     (setf negated-p nil)))

                  ((true-query-p parser query)
                   (prog1
                       (if negated-p                     
                           :false-query
                         :true-query)
                     (setf negated-p nil)))

                  ((false-query-p parser query)
                   (prog1
                       (if negated-p                     
                           :true-query
                         :false-query)
                     (setf negated-p nil)))

                  (t query))))

      (values 
       (if negated-p 
           (if inverse-p 
               `(:not (:inv ,query))
             `(:not ,query))
         (if inverse-p
             `(:inv ,query)
           query))
       inds))))



(defmethod syntactically-rewrite-atomic-query (query (parser nrql-abox-query-parser) 
                                                     &rest args &key negated-p inverse-p 
                                                     replace-inds-with-vars-p
                                                     additional-head-inds
                                                     &allow-other-keys)

  (declare (ignorable args))
  
  ;;;
  ;;; fuers RACER-Dummy-Substrate:
  ;;; 
  ;;; (?x ?y   (:constraint (has-father age) (has-mother age) =)) ->
  ;;; (and (?x $?x-has-father) 
  ;;;      (?y $?y-has-mother)
  ;;;      ($?x-has-father $?y-has-mother (:constraint age age =)))
  ;;; (?x NIL R) -> (not (?x (:has-known-successor R)))
  ;;; (NIL ?x R) -> (not (?x (:has-known-successor (INV R))))
  ;;; 

  (labels ((make-some-chain (feature-list)
             (if feature-list
                 `(some ,(first feature-list)
                        ,(make-some-chain (rest feature-list)))
               'top))

           (gvi (ind)
             (if replace-inds-with-vars-p
                 (get-var-for-ind parser ind)
               ind)))

    (let* ((inds (append (get-inds-from parser query)
                         additional-head-inds))

           (racer-rewritten-p nil)
           
           (query 
         
            (cond ((top-query-p parser query)
                   (prog1
                       (if negated-p                     
                           `(:bottom
                             ,(gvi 
                               (second query)))
                         `(:top
                           ,(gvi 
                             (second query))))
                     (setf negated-p nil)))
               
                  ((bottom-query-p parser query)
                   (prog1
                       (if negated-p                     
                           `(:top
                             ,(gvi 
                               (second query)))
                         `(:bottom
                           ,(gvi 
                             (second query))))
                     (setf negated-p nil)))
                  
                  ;;;
                  ;;;
                  ;;;
                  
                  ((true-query-p parser query)
                   (prog1
                       (if negated-p                     
                           :false-query
                         :true-query)
                     (setf negated-p nil)))

                  ((false-query-p parser query)
                   (prog1
                       (if negated-p                     
                           :true-query
                         :false-query)
                     (setf negated-p nil)))

                  ;;;
                  ;;;

                  ((bind-individual-query-p parser query)
                   ;;;
                   ;;; Bind-Individual Queries werden in syntactically-rewrite-QUERY wegtransformiert!
                   ;;; hier ist nur wichtig, dass ?x -> ?*x ersetzt wird
                   ;;; 
                   `(:bind-individual ,(second query)))

                  ;;;
                  ;;;
                  ;;;

                  ((same-as-query-p parser query)

                   (cond (negated-p  

                          (cond ((and (var-p parser (second query))
                                      (var-p parser (third query))
                                      (not (aux-var-p parser (second query)))
                                      (not (aux-var-p parser (third query))))
                                 
                                 (setf negated-p nil)

                                 `(:and
                                   (:top ,(second query))
                                   (:top ,(third query))))

                                ((and (var-p parser (second query))
                                      (var-p parser (third query)))

                                 (setf negated-p nil)
                                      
                                 `(:and
                                   (:top ,(second query))
                                   (:top ,(third query))
                                   (:not (:same-as ,(second query) ,(third query)))))
                                
                                ((and (var-p parser (second query))
                                      (ind-p parser (third query)))

                                 `(:same-as 
                                   ,(second query)
                                   ,(third query)))

                                ((and (ind-p parser (second query))
                                      (var-p parser (third query)))

                                 `(:same-as 
                                   ,(third query)
                                   ,(second query)))))
                         
                         (t
                          
                          (cond ((and (aux-var-p parser (second query))
                                      (aux-var-p parser (third query)))

                                 `(:and
                                   (:top ,(second query)) 
                                   (:top ,(third query))
                                   (:same-as 
                                    ,(second query)
                                    ,(third query))))

                                ((and (var-p parser (second query))
                                      (ind-p parser (third query)))

                                 `(:same-as 
                                   ,(second query)
                                   ,(third query)))

                                ((and (ind-p parser (second query))
                                      (var-p parser (third query)))

                                 `(:same-as 
                                   ,(third query)
                                   ,(second query)))

                                ((and (ind-p parser (second query))
                                      (ind-p parser (third query)))
                                 
                                 (if (same-individual-p (second query) (third query))
                                     :true-query
                                   :false-query))
                                 
                                (t 

                                 `(:and
                                   (:bottom ,(second query))
                                   (:bottom ,(third query))))))))

                  ;;;
                  ;;;
                  ;;;
                 
                  ((has-known-successor-retrieval-query-p parser query)
                   `(,(gvi 
                       (first query))
                     ,(second query)))

                  ((loom-no-role-filler-query-p parser query)
                   ;; not a binary-query-p, wegen NIL!
                   (setf negated-p (not negated-p))
                   (setf inverse-p nil)

                   (if (not (first query)) ; (NIL ?x R) ?
                    
                       `(,(gvi (second query))
                         (:has-known-successor (inv ,(third query))))
                  
                     `(,(gvi (first query))
                       (:has-known-successor ,(third query)))))

                  ;;;
                  ;;;
                  ;;;

                  ((unary-query-p parser query)
                   (if (abox-thing-p parser (first query))

                       (if (symbolp (second query))
                           
                           (let ((rewritten 
                                  (and *rewrite-defined-concepts-p*
                                       #-:dlmaps
                                       (rewrite-concept (second query) 
                                                        (tbox (substrate parser))
                                                        (first query)))))
                             
                             (if (or (null rewritten) (symbolp rewritten))
                                            
                                 `(,(gvi 
                                     (first query))
                                   ,(replace-syntactic-concept-expression-sugar parser (second query)))

                               
                               (progn 
                                 (setf racer-rewritten-p t)
                                 rewritten)))

                         `(,(gvi 
                             (first query))
                           ,(replace-syntactic-concept-expression-sugar parser (second query))))
                         

                     `(,(gvi 
                         (first query))
                       ,(second query))))
                     
                  ((binary-query-p parser query)
                
                   (cond ((valid-original-constraint-description-p parser (third query))

                          ;;; (?x ?y (:constraint (has-father has-name) (has-name) string=))

                          (let* ((from 
                                  (gvi
                                   (if inverse-p (second query) (first query))))
                                 (to 
                                  (gvi
                                   (if inverse-p (first query) (second query))))

                                 (constraint (rest (third query)))

                                 (from-chain (ensure-list (if inverse-p (second constraint) (first constraint))))
                                 (to-chain (ensure-list (if inverse-p (first constraint) (second constraint))))
                                 
                                 (from-attrib (first (last from-chain)))
                                 (to-attrib (first (last to-chain)))
                                 ;; sind entweder Attributes oder Datatype Properties!

                                 (from-is-attrib-p (cd-attribute-p from-attrib (tbox (substrate parser))))
                                 (to-is-attrib-p (cd-attribute-p to-attrib (tbox (substrate parser))))

                                 ;; entweder Attribute oder Datatype-Property!
                                 ;; bei Datatype-Properties darf die letzte Rolle nicht als Role Atom 
                                 ;; angelegt werden, weil die nicht unter (all-role-assertions) von
                                 ;; Racer zurueckgegeben werden                                 
                                 
                                 (from-features 
                                  (if from-is-attrib-p
                                      (butlast from-chain)
                                    from-chain))

                                 (to-features
                                  (if to-is-attrib-p
                                      (butlast to-chain)
                                    to-chain))

                                 (from-attrib
                                  (if from-is-attrib-p
                                      from-attrib
                                    (get-attribute-for-datatype-property parser from-attrib)))

                                 (to-attrib
                                  (if to-is-attrib-p
                                      to-attrib
                                    (get-attribute-for-datatype-property parser to-attrib)))

                                 (from-vois (mapcar #'(lambda (from1 from2)
                                                        (make-abox-var parser 
                                                                       (format nil "~A-~A" from1 from2) t))
                                                    (cons from from-features)
                                                    from-features))

                                 (to-vois (mapcar #'(lambda (to1 to2)
                                                      (make-abox-var parser
                                                                     (format nil "~A-~A" to1 to2) t))
                                                  (cons to to-features)
                                                  to-features))
                                 
                                 (predicate (third constraint))
                                 
                                 (predicate 
                                  (if inverse-p 
                                      (case predicate
                                        (< '>)
                                        (> '<)
                                        (<= '>=)
                                        (>= '<=)
                                        (otherwise predicate))
                                    predicate)))

                            (declare (ignorable from-is-attrib-p to-is-attrib-p))
                         
                            (prog1 
                                (simplify-boolean-expression
                                 (let ((pred                               
                                        `(,(if from-vois
                                               (first (last from-vois))
                                             from)
                                          ,(if to-vois
                                               (first (last to-vois))
                                             to)
                                          (:constraint ,from-attrib ,to-attrib ,predicate))))
                                
                                   (if negated-p 
                                       `(:or ,@(when from-features
                                                 `((:not (,from  ,(make-some-chain from-features)))))
                                         ,@(when to-features
                                             `((:not (,to ,(make-some-chain to-features)))))
                                      
                                         (:and
                                       
                                          ,@(mapcar #'(lambda (a b feature)
                                                        `(,a ,b ,feature))
                                                    (cons from from-vois)
                                                    from-vois
                                                    from-features)
                                       
                                          ,@(mapcar #'(lambda (a b feature)
                                                        `(,a ,b ,feature))
                                                    (cons to to-vois)
                                                    to-vois
                                                    to-features)
                                       
                                          (not ,pred)))
                                  
                                     `(:and
                                    
                                       ,@(mapcar #'(lambda (a b feature)
                                                     `(,a ,b ,feature))
                                                 (cons from from-vois)
                                                 from-vois
                                                 from-features)
                                    
                                       ,@(mapcar #'(lambda (a b feature)
                                                     `(,a ,b ,feature))
                                                 (cons to to-vois)
                                                 to-vois
                                                 to-features)
                                    
                                       ,pred)))
                                 t)
                           
                              (setf negated-p nil
                                    inverse-p nil))))
                      
                         (t 
                         
                          (if inverse-p 
                              `(,(gvi (second query))
                                ,(gvi (first query))
                                (inv ,(third query)))
                            `(,(gvi (first query))
                              ,(gvi (second query))
                              ,(third query))))))
                
                  (t query))))

      (values
       (if negated-p
           `(:not ,query)
         query)
       inds
       racer-rewritten-p))))
        
;;;
;;;
;;;

(defmethod syntactically-rewrite-query (query (parser simple-parser) &rest args 
                                              &key 
                                              (add-same-as-conjuncts-p t)
                                              (replace-inds-with-vars-p t)
                                              &allow-other-keys)

  ;;; 
  ;;; Bringt Query in NNF, sorgt dafuer dass alle Disjunkte gleiche Stelligkeit haben
  ;;; Bsp.: (OR (?X C) (?Y D)) -> (OR (AND (?X C) (TOP ?Y)) (AND (?Y D) (?X TOP)))
  ;;; 
  ;;; Zudem: (betty woman) -> (and ($?x-betty woman) (:same-as $?x-betty betty))
  ;;; (:bind-individual betty) -> (and (:same-as $?x-betty betty))
  ;;; Auffaltung von definierten Queries: 
  ;;; Wenn (defquery mother (?x ?y) (and (?x woman) (?x ?y has-child))), 
  ;;; dann (:substitute (mother ?x ?child)) -> (and (?x woman) (?x ?child has-child))
  ;;; 

  (labels ((normalize-disjunction (expr)
             (let* ((expr (simplify-boolean-expression expr t))
                    (all-vois 
                     (get-vois-from parser expr))
                    (subexpressions
                     (mapcar #'(lambda (expr)
                                 (let* ((vois (get-vois-from parser expr))
                                        (diff (set-difference all-vois vois)))
                                   (if diff
                                       `(:and ,@(mapcar #'(lambda (voi)
                                                            `(:top ,voi))
                                                        diff)
                                         ,expr)
                                     expr)))
                             (get-subexpressions parser expr))))

               `(:or ,@subexpressions)))

           (gvi (ind)
             (if replace-inds-with-vars-p
                 (get-var-for-ind parser ind)
               ind))
           
           (add-same-as-conjuncts (rexpr inds negated-p)
             (if (not negated-p) 
                 `(:and ,@(mapcar #'(lambda (ind)
                                      `(:same-as ,(gvi ind) ,ind))
                                  inds)
                   ,rexpr)
               
               (normalize-disjunction 
                `(:or ,@(mapcar #'(lambda (ind)
                                    `(:not (:same-as ,(gvi ind) ,ind)))
                                inds)
                  ,rexpr))))

           (do-it (expr &optional inverse-p negated-p defs pvois)

             (terpri) (write expr) (terpri)

             (if (not (consp expr))
                 
                 (cond ((or (true-query-p parser expr)
                            (false-query-p parser expr))

                        (apply #'syntactically-rewrite-atomic-query expr parser 
                               :negated-p negated-p 
                               args))

                       (t expr))
               
               (cond ((not-query-p parser expr)
                      
                      (do-it (second expr) inverse-p (not negated-p) defs
                             pvois))

                     ((inv-query-p parser expr)
                      (do-it (second expr)
                             (not inverse-p)
                             negated-p
                             defs
                             pvois))

                     ((projection-operator-p parser expr)
                      (let* ((pvois

                              (second expr))
                              
                             #| (if pvois ; falsch! 
                                    (intersection 
                                     (second expr)
                                     pvois)
                                  (second expr)) |# 

                             (expr (do-it (third expr)
                                          inverse-p
                                          nil 
                                          defs
                                          pvois))

                             (res

                              (if (projection-operator-p parser expr)
                            
                                  (if (null pvois)

                                      ;;; war entweder schon (project-to () (...)), oder durch
                                      ;;; (project-to (?x) (project-to (?y) (....)))

                                      `(:project-to nil ,expr)
                                
                                    expr)

                                (let ((vois-in-query (get-vois-from parser 
                                                                    expr
                                                                    :stop-at-projections-p t)))

                                  ;;; (and (?x c) (project-to (?y) (and (?y d) (?z e)))) ->
                                  ;;; vois-in-query = (?x ?y), NICHT auch ?z ! 
                            
                                  (format t "~A ~A~%" expr vois-in-query)

                                  (if (set-equal vois-in-query 
                                                 pvois)

                                      ;;; (project-to (?x) (?x c)) -> (?x c)
                                      ;;; (Projektion redundant) 

                                      expr

                                    (if (subsetp pvois vois-in-query)

                                        ;;; weniger Variablen in der Projektionsliste, 
                                        ;;; als im Query Body, z.B. 
                                        ;;; (project-to (?x) (and (?x c) (?y d)) -> okay 
                                  
                                        `(:project-to ,pvois 
                                          ,(substitute-vois-in expr
                                                               (mapcar #'(lambda (x)
                                                                           (list x x))
                                                                       pvois)
                                                               :dont-anonymize-inds-p t))
                                      (let ((missing
                                             (set-difference pvois vois-in-query)))

                                        ;;; Variablen in der Projektionsliste, die nicht
                                        ;;; im Body der Query auftauchen! -> Error 

                                        (parser-error 
                                         (if (cdr missing)
                                             (format nil "Objects ~A not mentioned in query body ~A" 
                                                     missing
                                                     expr)
                                           (format nil "Object ~A not mentioned in query body ~A" 
                                                   (first missing)
                                                   expr))))))))))

                        (if negated-p
                            `(not ,res)
                          res)))

                     ;;;
                     ;;; Defined Queries
                     ;;; 

                     ((defined-query-p parser expr)
                      (let* ((name (first (second expr)))
                             (vars (rest (second expr)))
                             (def (get-variable-substituted-query (substrate parser)
                                                                  name
                                                                  vars)))

                        (if (member name defs)
                            (parser-error 
                             (format nil "Cyclic definition ~A detected" name))
                        
                          (do-it def inverse-p negated-p 
                                 (cons name defs)
                                 pvois))))

                     ((probably-defined-query-p parser expr)
                      
                      ;;; neue Syntax: (?x ?y ?z defined-query)!
                      ;;; von Ralf gewuenscht
                      
                      (do-it `(:substitute (,(first (last expr))
                                            ,@(butlast expr)))
                             inverse-p negated-p 
                             defs
                             pvois))
                     
                     ;;;
                     ;;; Spez. Syntaxten
                     ;;; 
                           
                     ((bind-individual-query-p parser expr)
                      (multiple-value-bind (rexpr inds)
                          (apply #'syntactically-rewrite-atomic-query expr parser 
                                 :negated-p negated-p 
                                 :inverse-p inverse-p 
                                 :replace-inds-with-vars-p 
                                 replace-inds-with-vars-p 
                                 args)
                        (declare (ignore rexpr))

                        (if negated-p
                            `(:not (:same-as 
                                    ,(gvi (first inds))
                                    ,(first inds)))
                          `(:same-as 
                            ,(gvi (first inds))
                            ,(first inds)))))

                     ((same-as-query-p parser expr)

                      (multiple-value-bind (rexpr inds)
                          (apply #'syntactically-rewrite-atomic-query expr parser 
                                 :negated-p negated-p 
                                 :inverse-p inverse-p 
                                 :replace-inds-with-vars-p 
                                 replace-inds-with-vars-p 
                                 args)
                        (declare (ignore inds))

                        rexpr))

                     ;;;
                     ;;;
                     ;;;
                    
                     ((or (top-query-p parser expr)
                          (bottom-query-p parser expr))
                          
                      (multiple-value-bind (rexpr inds)
                          (apply #'syntactically-rewrite-atomic-query expr parser 
                                 :negated-p negated-p 
                                 :inverse-p inverse-p 
                                 :replace-inds-with-vars-p 
                                 replace-inds-with-vars-p 
                                 args)

                        (if (and add-same-as-conjuncts-p inds)
                            (add-same-as-conjuncts rexpr inds negated-p)
                          rexpr)))

                     ;;;
                     ;;; AND/OR
                     ;;;

                     ((or (and (not negated-p) 
                               (or-query-p parser expr))
                          (and negated-p
                               (and-query-p parser expr)))

                      ;; OR
                           
                      (let* ((subexpressions 
                              (remove nil
                                      (mapcar #'(lambda (expr) 
                                                  (do-it expr inverse-p negated-p defs pvois))
                                              (get-subexpressions parser expr))))
                             (subexpressions 
                              (remove :false-query subexpressions)))

                        (if (not subexpressions)

                            :false-query

                          (if (member :true-query subexpressions)
                              
                              :true-query
                              
                            (let ((expr (simplify-boolean-expression
                                         `(:or ,@subexpressions))))
                              
                              (if (eq (first expr) :or)
                                  (normalize-disjunction expr)

                                expr))))))
                          
                     ((or (and (not negated-p)
                               (and-query-p parser expr))
                          (and negated-p
                               (or-query-p parser expr)))
                      ;; AND
                           
                      (let* ((subexpressions 
                              (remove nil 
                                      (mapcar #'(lambda (expr) 
                                                  (do-it expr inverse-p negated-p defs pvois))
                                              (get-subexpressions parser expr))))
                             (subexpressions 
                              (remove :true-query subexpressions))

                             (same-as-subexpressions
                              (remove-if-not #'(lambda (x)  ; (same-as $?i-var i)
                                                 (and (same-as-query-p parser x)
                                                      (ind-p parser (third x))))
                                             subexpressions))
                             (subexpressions 
                              (set-difference subexpressions same-as-subexpressions)))

                        ;; hier wird sichergestellt, dass die Bindungen fuer die repraesentativen
                        ;; Variablen der Individuen bereits etabliert sind 

                        (if (not subexpressions)

                            :true-query

                          (if (member :false-query subexpressions)

                              :false-query
                            
                            (let ((expr (simplify-boolean-expression
                                         `(:and
                                           ,@same-as-subexpressions
                                           ,@subexpressions))))
                              
                              expr)))))
                     
                     ;;;
                     ;;; Unary / Binary Atoms 
                     ;;; 

                     ((or (unary-query-p parser expr)
                          (binary-query-p parser expr))
                           
                      (multiple-value-bind (rexpr inds racer-rewritten-p)
                          (apply #'syntactically-rewrite-atomic-query expr parser 
                                 :negated-p negated-p 
                                 :inverse-p inverse-p 
                                 :replace-inds-with-vars-p 
                                 replace-inds-with-vars-p 
                                 args)

                        (if racer-rewritten-p 
                            
                            (do-it rexpr
                                   inverse-p negated-p
                                   defs
                                   pvois)


                          (if (and add-same-as-conjuncts-p inds)
                              (add-same-as-conjuncts rexpr inds negated-p)
                            (if (eq (first rexpr) :or)
                                (normalize-disjunction rexpr)
                              rexpr)))))

                     ;;;
                     ;;; unbekannt -> so lassen 
                     ;;;  
                           
                     (t expr)))))


    (simplify-boolean-expression
     (do-it query) t)))
      
;;;
;;;
;;;

(defmethod get-dnf-query ((query list))
  (get-boolean-dnf query))

(defmethod get-dnf-query ((query (eql :true-query)))
  query)

(defmethod get-dnf-query ((query (eql :false-query)))
  query)

(defmethod get-dnf-query ((query query))
  (let ((query (parse-query 
                (get-boolean-dnf (unparse-query query))
                (copy (parser query)))))
    (dolist (sq (cons query (all-subqueries query)))
      (setf (slot-value sq 'in-dnf-p) t))
    query))

;;;
;;; 
;;;

(defmethod in-dnf-p ((query atomic-query))
  t)

(defmethod in-dnf-p ((query query-reference))
  t)

(defmethod in-dnf-p ((query and-query))
  (every #'(lambda (x) 
             (or (is-atomic-query-p x)
                 (is-false-query-p x)
                 (is-true-query-p x)
                 (is-query-reference-p x)))
         (subqueries query)))

(defmethod in-dnf-p ((query or-query))
  (and (every #'in-dnf-p (subqueries query))
       (every #'(lambda (x) 
                  (or (is-and-query-p x)
                      (is-false-query-p x)
                      (is-true-query-p x)
                      (is-atomic-query-p x)))
              (subqueries query))))

(defmethod in-dnf-p :around ((query atomic-query))
  (and (in-nnf-p query)
       (call-next-method)))


;;;
;;;
;;;

(defmethod in-nnf-p ((query atomic-query))
  t)

(defmethod in-nnf-p ((query complex-query))
  (and (not (negated-p query))
       (every #'in-nnf-p (subqueries query))))

;;;
;;;
;;;

(defun get-ano-var (x)
  (intern (format nil "~A-ANO~A" 
                  x 
                  (incf *ano-counter*))))

(defun substitute-vois-in (expr subs &key dont-anonymize-inds-p)
  (let* ((parser (make-instance 'simple-parser))
         (subs 
          (remove-duplicates 
           (append subs
                   (mapcar #'(lambda (x) 
                               (let ((from (first x))
                                     (to (second x)))
                                 (list (get-corresponding-voi parser from)
                                       (get-corresponding-voi parser to))))
                           subs))
           :test #'equal)))
    
    (labels ((subs (expr)             
               (let* ((expr1 (ensure-list expr))
                      (res
                       (mapcar #'(lambda (x)                                                
                                   (let ((found (assoc x subs)))
                                     (if (and found
                                              ;;; NIL moeglich -> Variable wird auch
                                              ;;; anonymisiert! 
                                              (second found))
                                         (second found)
                                       (if (and dont-anonymize-inds-p
                                                (ind-p parser x))
                                           x
                                         (let ((var 
                                                (get-ano-var x)))
                                           ;;; natuerlich muessen die 
                                           ;;; konsistent umbenannt
                                           ;;; werden!!! 
                                           (push (list x var) subs)
                                           (push (list (get-corresponding-voi parser x)
                                                       (get-corresponding-voi parser var))
                                                 subs)
                                           var)))))
                               expr1)))
                 
                 (if (consp expr)
                     res
                   (first res))))
             
             (do-it (expr)
               ;;; ich muss hier die VOLLE SYNTAX zulassen, 
               ;;; also nicht nur NOT, sondern auch NEG etc.

               (when (consp expr)
                 (let ((op (first expr)))
                   (cond
                    ((member op '(neg not inv :neg :not :inv))
                     `(,op ,(do-it (second expr))))

                    ((member op '(:substitute substitute))                   
                     `(,op (,(first (second expr))                            
                            ,@(subs (rest (second expr))))))
                  
                    ((member op '(bind-individual :bind-individual 
                                                  top :top
                                                  bottom :bottom))
                     `(,op ,(subs (second expr))))
                  
                    ((unary-query-p parser expr)
                     `(,(subs (first expr))
                       ,(second expr)))
                  
                    ((binary-query-p parser expr)
                     `(,(subs (first expr))
                       ,(subs (second expr))
                       ,(third expr)))

                    ((complex-query-p parser expr)
                     `(,op
                       ,@(mapcar #'do-it
                                 (get-subexpressions parser expr))))

                    ((projection-operator-p parser expr)
                     `(,op 
                       ,(subs (second expr))
                       ,(do-it (third expr))))

                    ((same-as-query-p parser expr)
                     `(,op
                       ,(subs (second expr))
                       ,(subs (third expr))))
                    
                    (t (nrql-error 
                        (format nil "Found unexpected expression ~A" expr))))))))


      (values (do-it expr) subs))))
              
                                          
