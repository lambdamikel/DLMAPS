;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;;
;;;
                    
(defconstant +continuation-marker+ 'cont-123)

(defgeneric evaluate-tester (query continuation &rest args &key &allow-other-keys))

(defgeneric evaluate-enumerator (query continuation &rest args &key &allow-other-keys))

(defgeneric evaluate-from-bound-enumerator (query continuation &rest args &key &allow-other-keys))

(defgeneric evaluate-to-bound-enumerator (query continuation &rest args &key &allow-other-keys))

;;;
;;;
;;;

(defgeneric compile-query (query &rest args))

(defgeneric compile-subquery (query remaining-conjuncts &rest args))

;;;
;;;
;;;

#+:midelora
(defun same-abox-individual-p (a b)
  (if (and a b 
           (is-midelora-substrate-p *running-substrate*))
      (eq (prover::find-node *running-substrate* a)
          (prover::find-node *running-substrate* b))
    (eq a b)))

#-:midelora
(defun same-abox-individual-p (a b)
  (eq a b))

(defun same-role-p (a b)
  (equal a b))
     
(defun same-concept-p (a b)
  (equal a b))
   
(defun same-constraint-p (a b)
  (equal a b))

(defun same-individual-p (a b)
  (eq a b))

;;;
;;;
;;;

(defmethod individual-exists-p ((ind abox-individual))
  (dl-prover-individual-exists-p  *running-substrate* 
                                  (individual ind)))

(defmethod individual-exists-p ((ind substrate-individual))
  (individual ind))

;;;
;;;
;;;      

(defmethod bind-voi ((voi query-individual) bind-to continuation &key &allow-other-keys)
  (declare (ignorable bind-to continuation))
  (nrql-error "Runtime error: Can't bind individual ~A" voi))

(defmethod get-binding-code ((voi substrate-variable) cand-name body &key (una-p *use-unique-name-assumption-p*))
  ;;; fuer Compiler 
  (let* ((cor-voi (corresponding-voi voi))
        
         (name (textual-description voi))
         
         (cor-name 
          (when cor-voi
            (textual-description cor-voi)))

         (una-voi-p (una-voi-p voi))
         (una-cor-voi-p 
          (when cor-voi
            (una-voi-p cor-voi))))
       
    `(let* ((,name ,cand-name)
            ,@(when cor-name
                `((,cor-name
                   (when ,name
                     (get-associated-abox-individual *running-substrate*
                                                     ,name))))))
       
       (when (and ,name
                  (=> (and ,una-p ,una-voi-p)
                      (not (marked-p ,name)))

                  ,@(when cor-name 

                      `(,cor-name
                        (dl-individual-p *running-substrate* ,cor-name)

                        (=> (and ,una-p ,una-cor-voi-p)
                            (not (member ,cor-name *candidates* :test #'same-abox-individual-p))))))

         (unless ,name
           (nrql-error "Runtime error: Can't bind ~A to ~A" ,voi ,name))

         (setf (bound-to ,voi) ,name)
           
         ,@(when cor-name
             `((setf (bound-to ,cor-voi) ,cor-name)))
         
         ,@(when una-voi-p
             `((mark ,name)))

         ,@(when body
         
             `((unwind-protect 
                   (let ((*candidates* 
                          ,(if (and cor-name una-cor-voi-p)
                               `(cons ,cor-name *candidates*)
                             '*candidates*)))
                     ,body)
                
                 ,@(when una-voi-p
                     `((unmark ,name)))
                
                 (setf (bound-to ,voi) nil)           
                
                 ,@(when cor-name
                     `((setf (bound-to ,cor-voi) nil))))))))))



(defmethod bind-voi ((voi substrate-variable) bind-to continuation &key (una-p *use-unique-name-assumption-p*))
  ;;; fuer Runtime

  (unless bind-to
    (nrql-error "Runtime error: Can't bind ~A to ~A" voi bind-to))

  (let* ((cor-voi (corresponding-voi voi))
         (una-voi-p (una-voi-p voi))
          
         (una-cor-voi-p 
          (when cor-voi
            (una-voi-p cor-voi)))
         
         (cor-bind-to 
          (when cor-voi
            (get-associated-abox-individual *running-substrate*
                                            bind-to))))

    (when (and (=> (and una-p una-voi-p)
                   (not (marked-p bind-to)))

               (=> cor-voi 
                   (and (dl-individual-p *running-substrate* cor-bind-to)
                        (=> (and una-p una-cor-voi-p)
                            (not (member cor-bind-to 
                                         *candidates* :test #'same-abox-individual-p))))))

      (setf (bound-to voi) bind-to)
           
      (when cor-voi
        (setf (bound-to cor-voi) cor-bind-to))

      (when continuation
         
        (when una-voi-p
          (mark bind-to))
         
        (unwind-protect 
            (let ((*candidates* 
                   (if (and cor-voi una-cor-voi-p)
                       (cons cor-bind-to *candidates*)
                     *candidates*)))
              (apply continuation nil))
           
          (when una-voi-p
            (unmark bind-to))
           
          (setf (bound-to voi) nil)       
           
          (when cor-voi
            (setf (bound-to cor-voi) nil)))))))



(defmethod carefully-bind-voi ((voi substrate-variable) bind-to continuation 
                               &key (una-p *use-unique-name-assumption-p*))
  ;;; fuer Runtime

  (let* ((bound-to (bound-to voi))
         (cor-voi (corresponding-voi voi))
         (cor-bound-to
          (when cor-voi 
            (bound-to cor-voi)))
         (cor-bind-to
          (when cor-voi
            (get-associated-abox-individual *running-substrate* bind-to))))
    
    (if bound-to
        (when (same-individual-p bound-to bind-to)
          (if cor-bound-to
              (when (same-abox-individual-p cor-bound-to cor-bind-to)
                (apply continuation nil))
            (apply continuation nil)))

      (bind-voi voi bind-to continuation :una-p una-p))))

#|

(defmethod get-code-carefully-bind-voi ((voi substrate-variable) bind-to body 
                                        &key (una-p *use-unique-name-assumption-p*))

  `(let* ((bound-to (bound-to ,voi))
          (cor-voi (corresponding-voi ,voi))
          (cor-bound-to
           (when cor-voi 
             (bound-to cor-voi)))
          (cor-bind-to
           (when cor-voi
             (get-associated-abox-individual *running-substrate* ,bind-to))))
    
     (if bound-to
         (when (same-individual-p bound-to ,bind-to)
           (if cor-bound-to
               (when (same-abox-individual-p cor-bound-to cor-bind-to)
                 ,body)
             (progn ,body)))
      
       ,(get-binding-code voi bind-to body :una-p una-p))))

|#

;;;
;;;
;;;

(defmethod get-binding-code ((voi abox-variable) cand-name body &key (una-p *use-unique-name-assumption-p*))
  ;;; fuer Compiler
  (let* ((cor-voi (corresponding-voi voi))
        
         (name (textual-description voi))
         
         (cor-name 
          (when cor-voi
            (textual-description cor-voi)))

         (una-voi-p (una-voi-p voi))
         (una-cor-voi-p 
          (when cor-voi
            (una-voi-p cor-voi))))

    `(let* ((,name ,cand-name)            
            ,@(when cor-name
                `((,cor-name
                   (when ,name
                     (get-associated-substrate-node *running-substrate* ,name))))))
       
       (when (and ,name
                  (=> (and ,una-p ,una-voi-p)
                      (not (member ,name *candidates* :test #'same-abox-individual-p)))
                  ,@(when cor-name 
                      `(,cor-name
                  
                        (=> (and ,una-p ,una-cor-voi-p)
                            (not (marked-p ,cor-name))))))
   
         (unless ,name
           (nrql-error "Runtime error: Can't bind ~A to ~A" ,voi ,name))

         (setf (bound-to ,voi) ,name)
       
         ,@(when cor-name
             `((setf (bound-to ,cor-voi) ,cor-name)

               ,@(when (and una-cor-voi-p body)
                   `((mark ,cor-name)))))

         ,@(when body

             `((unwind-protect 
                   (let ((*candidates* 
                          ,(if una-voi-p 
                               `(cons ,name *candidates*)
                             '*candidates*)))
         
                     ,body)
           
                 ,@(when cor-name
                     `((setf (bound-to ,cor-voi) nil)

                       ,@(when una-cor-voi-p
                           `((unmark ,cor-name) ))))
           
                 (setf (bound-to ,voi) nil))))))))


(defmethod bind-voi ((voi abox-variable) bind-to continuation &key (una-p *use-unique-name-assumption-p*))
  ;;; fuer Runtime

  (unless bind-to
    (nrql-error "Runtime error: Can't bind ~A to ~A" voi bind-to))

  (let* ((cor-voi (corresponding-voi voi))
         (una-voi-p (una-voi-p voi))
          
         (una-cor-voi-p 
          (when cor-voi
            (una-voi-p cor-voi)))
         
         (cor-bind-to 
          (when cor-voi
            (get-associated-substrate-node *running-substrate* bind-to))))

    (declare (ignorable una-cor-voi-p))
    
    (when (and (=> (and una-p una-cor-voi-p)
                   (not (marked-p cor-bind-to)))
               
               (=> (and una-p una-voi-p)
                   (not (member bind-to 
                                *candidates* :test #'same-abox-individual-p))))


      (setf (bound-to voi) bind-to)
           
      (when cor-voi
        (setf (bound-to cor-voi) cor-bind-to))
         
      (when una-cor-voi-p
        (mark cor-bind-to))

      (when continuation
         
        (unwind-protect 
            (let ((*candidates* 
                   (if una-voi-p
                       (cons bind-to *candidates*)
                     *candidates*)))
              (apply continuation nil))
           
          (when una-cor-voi-p
            (unmark cor-bind-to))
           
          (setf (bound-to voi) nil)           
           
          (when cor-voi
            (setf (bound-to cor-voi) nil)))))))



(defmethod carefully-bind-voi ((voi abox-variable) bind-to continuation &key (una-p *use-unique-name-assumption-p*))
  ;;; fuer Runtime

  (let* ((bound-to (bound-to voi))
         (cor-voi (corresponding-voi voi))
         (cor-bound-to
          (when cor-voi 
            (bound-to cor-voi)))
         (cor-bind-to
          (when cor-voi
            (get-associated-substrate-node *running-substrate* bind-to))))
    
    (if bound-to
        (when (same-abox-individual-p bound-to bind-to)
          (if cor-bound-to
              (when (same-individual-p cor-bound-to cor-bind-to)
                (apply continuation nil))
            (apply continuation nil)))

      (bind-voi voi bind-to continuation :una-p una-p))))

#|

(defmethod get-code-carefully-bind-voi ((voi abox-variable) bind-to body &key (una-p *use-unique-name-assumption-p*))

  `(let* ((bound-to (bound-to ,voi))
          (cor-voi (corresponding-voi ,voi))
          (cor-bound-to
           (when cor-voi 
             (bound-to cor-voi)))
          (cor-bind-to
           (when cor-voi
             (get-associated-substrate-node *running-substrate* ,bind-to))))
    
     (if bound-to
         (when (same-abox-individual-p bound-to ,bind-to)
           (if cor-bound-to
               (when (same-individual-p cor-bound-to cor-bind-to)
                 ,body)
             (progn ,body)))
      
       ,(get-binding-code voi bind-to body :una-p una-p))))

|#

;;;
;;;
;;;

(defmacro with-disabled-cache-entries ((query) &rest body)
  (let ((superset (gensym))
        (exact (gensym))
        (subset (gensym)))
    `(with-slots (superset-cache-reference
                  subset-cache-reference
                  exact-cache-reference) ,query
       (let ((,superset superset-cache-reference)
             (,subset subset-cache-reference)
             (,exact exact-cache-reference))
         (setf superset-cache-reference nil
               subset-cache-reference nil
               exact-cache-reference nil)
         (unwind-protect
             (progn 
               ,@body)
           (setf superset-cache-reference ,superset
                 subset-cache-reference ,subset
                 exact-cache-reference ,exact))))))

(defmacro with-superset-cache-as-exact-cache ((query) &rest body)
  (let ((var (gensym)))
    `(with-slots (superset-cache-reference
                  exact-cache-reference) ,query
       (let ((,var exact-cache-reference))
         (setf exact-cache-reference
               superset-cache-reference)
         (unwind-protect
             (progn 
               ,@body)
           (setf exact-cache-reference ,var))))))


(defmacro with-subset-cache-as-exact-cache ((query) &rest body)
  (let ((var (gensym)))
    `(with-slots (subset-cache-reference
                  exact-cache-reference) ,query
       (let ((,var exact-cache-reference))
         (setf exact-cache-reference
               subset-cache-reference)
         (unwind-protect
             (progn 
               ,@body)
           (setf exact-cache-reference ,var))))))

;;;
;;;
;;;
;;; Ende erreicht: Tuple registrieren etc. 
;;;



(defun get-object-name (ind)
  (if (symbolp ind)
      (symbol-name ind)
    (name ind)))

(defmethod query-fn ((query null))
  #'(lambda ()
      (with-slots (result-bindings
                   last-result-bindings-item
                   result-bindings-hash
                   result-vois
                   counter how-many
                   abort-search-p 
                   answer-pattern 
                   counter 
                   bindings-found-p) *running-query*

        (setf bindings-found-p t) ;;; wichtig fuer Queries ohne Answer-Pattern! -> Antwort "t"

        (when (and (is-rule-p *running-query*)
                   (not answer-pattern))
          (register-bindings *running-substrate* *running-query* nil nil))
          
        (unless answer-pattern
          (throw 'query-execution 'done))

        (let* ((result-binding (mapcar #'bound-to result-vois))               
               (already-present-p  
                (if (not *exclude-permutations-p*)
                    (gethash result-binding result-bindings-hash)
                  (let ((new-binding (copy-list (sort result-binding #'string-lessp :key #'get-object-name))))
                    (gethash new-binding result-bindings-hash)))))
          
          (unless already-present-p 
                    
            (let ((new-binding (get-tuple-for-pattern answer-pattern)))

              (setf (gethash result-binding result-bindings-hash) t)

              (incf counter)
                
              (register-bindings *running-substrate* *running-query* answer-pattern new-binding)
                    
              (if (not last-result-bindings-item)
                  (progn 
                    (setf result-bindings (list new-binding))
                    (setf last-result-bindings-item (last result-bindings)))
                (progn 
                  (setf (cdr last-result-bindings-item)
                        (list new-binding))
                  (setf last-result-bindings-item
                        (cdr last-result-bindings-item))))

              (when (and how-many (= counter how-many))
                (setf abort-search-p t)
                (throw 'query-execution 'done))))))))

;;;
;;; fuer den Compiler
;;;

(defmethod compile-query :before ((query query) &rest args)
  (declare (ignorable args))
  (dolist (voi (vois (parser query)))
    (unless (is-query-individual-p voi)
      (setf (bound-to voi) nil))))

#|
(defmethod compile-query ((query nrql-query) &rest args)
  (when (two-phase-processing-p query)
    (to-be-implemented 'compile-query))
  (apply #'compile-subquery query nil args)
  query)
|#

(defmethod compile-query ((query nrql-query) &rest args)
  (let* ((body 
          (apply #'compile-subquery query nil args))
         (body 
          `(let* ((query ,query)
                  (tir-p (told-information-reasoning-p query)))
             (when (or tir-p 
                       (two-phase-processing-p query))
               (let ((*told-information-reasoning-p* t))
                 (setf (slot-value query 'told-information-reasoning-p) t)
                 ,body))
             (unless tir-p 
               (note-phase-two-starts query)
               (wait-for-request-or-abort query)
               (let ((*told-information-reasoning-p* nil))
                 (setf (slot-value query 'told-information-reasoning-p) nil)
                 ,body))))
         (function `(lambda () ,body)))
    
    (declare (ignorable function))

    ;; (pprint function) 

    (setf (slot-value query 'source) body)

    #-:only-runtime-evaluation
    ;;; Compiler must not see eval / compile
    (setf (slot-value query 'query-fn)
          (if *compile-queries-p*
              (compile nil function)
            (eval function)))

    query))


(defmethod compile-query ((query query) &rest args)
  (apply #'compile-subquery query nil args)
  query)

;;;
;;; Runtime
;;;

(defmethod evaluate-query :before ((query query))
  (declare (ignorable args))
  (dolist (voi (vois (parser query)))
    (unless (is-query-individual-p voi)
      (setf (bound-to voi) nil))))
    
(defmethod evaluate-query ((query query))
  (evaluate-subquery query nil))

(defmethod evaluate-query ((query nrql-query))
  (let ((tir-p (told-information-reasoning-p query)))
    (when (or tir-p 
              (two-phase-processing-p query))
      (let ((*told-information-reasoning-p* t))
        (setf (slot-value query 'told-information-reasoning-p) t)
        (evaluate-subquery query nil)))

    (unless tir-p 
      (note-phase-two-starts query)
      (wait-for-request-or-abort query)
      (let ((*told-information-reasoning-p* nil))
        (setf (slot-value query 'told-information-reasoning-p) nil)
        (evaluate-subquery query nil)))))

;;;
;;;
;;;

(defmethod compile-subquery :around ((query query) remaining-conjuncts &rest args)
  (declare (ignorable args))
  (let* ((body
          (if (not (is-active-p query))
              `(progn
                 ,(format nil "COMMENT: SKIP ~A" query)
                 ,+continuation-marker+)

            (let ((x (call-next-method)))
              ;; (pprint x) 
              x)))

         (body
          (if (tree-find body +continuation-marker+)
              (let ((continuation
                     (if (is-active-p query)
                         (with-vois-marked-as-bound (slot-value query 'vois)
                           (compile-subquery
                            (first remaining-conjuncts) 
                            (rest remaining-conjuncts)))
                       (compile-subquery
                        (first remaining-conjuncts) 
                        (rest remaining-conjuncts)))))
                
                (tree-map #'(lambda (x)                         
                              (if (eq x +continuation-marker+)                                     
                                  `(progn                                    
                                     ,@(when (and (cache-bindings-p query)
                                                  ;; QUERY IST IN DNF!!!
                                                  ;; weil SUBQUERY, kann es keine
                                                  ;; OR-QUERY SEIN! 
                                                  ;; NUR AND ODER ATOM! 
                                                  (not (is-and-query-p query)))
                                         ;; BINDINGS VOM ATOM-KONJUNKT CACHEN
                                         `(,(get-cache-binding-code query)))
                                     
                                     ,@(when (last-conjunct-p query)
                                         (let ((query (subquery-of query)))
                                           (when (cache-bindings-p query)
                                             `(,(get-cache-binding-code query)))))

                                     ,(if *compile-inline-p*
                                          continuation
                                        `(let ((*previous-conjunct* ,query))
                                           (funcall (query-fn ,(first remaining-conjuncts))))))
                                x))
                          body))
            body))
         
         (function `(lambda () ,body)))

    (declare (ignorable function))

    (setf (slot-value query 'source) body)

    #-:only-runtime-evaluation
    ;;; Compiler must not see eval / compile
    (setf (slot-value query 'query-fn)
          (if *compile-queries-p*
              (compile nil function)
            (eval function)))

    #+:only-runtime-evaluation
    (nrql-error "Compiler error: Can't compile queries, since :only-runtime-evaluation is on *features*")

    body))

(defmacro continuation-marker ()
  `(let ((*previous-conjunct* query))

     (setf (slot-value query 'bindings-found-p) t)

     (cache-binding-if-required query)

     (evaluate-subquery (first remaining-conjuncts)
                        (rest  remaining-conjuncts))))

(defmacro continuation ()
  `#'(lambda (&rest args)
       (declare (ignorable args))
       (continuation-marker)))

;;;
;;;
;;;

(defun cache-binding-if-required (query)
  (when (and (cache-bindings-p query)
             (not (is-and-query-p query)))
    (cache-binding query))
      
  (when (and (is-atomic-query-p query) 
             (last-conjunct-p query))

    (let ((query (subquery-of query)))
      (when (cache-bindings-p query)
        (cache-binding query)))))

;;;
;;;
;;;

(defmethod evaluate-subquery :around ((query query) remaining-conjuncts)
  (declare (ignorable remaining-conjuncts))
  (if (not (is-active-p query))
      (progn 
        (continuation-marker))
    (call-next-method)))

;;;
;;;
;;; 

(defmethod compile-subquery ((query null) (remaining-conjuncts null) &rest args)
  (declare (ignorable remaining-conjuncts args))
  `(funcall (query-fn nil)))


(defmethod evaluate-subquery ((query null) (remaining-conjuncts null))
  (declare (ignorable remaining-conjuncts))
  (funcall (query-fn nil)))

;;;
;;;
;;;

(defmethod compile-subquery ((query true-query) remaining-conjuncts &rest args)
  (declare (ignorable remaining-conjuncts args))
  (continuation-marker))

(defmethod evaluate-subquery ((query true-query) remaining-conjuncts)
  (declare (ignorable remaining-conjuncts))
  (continuation-marker))

;;;
;;;
;;;

(defmethod compile-subquery ((query false-query) remaining-conjuncts &rest args)
  (declare (ignorable remaining-conjuncts args))
  nil)

(defmethod evaluate-subquery ((query false-query) remaining-conjuncts)
  (declare (ignorable remaining-conjuncts))
  nil)

;;;
;;;
;;;

(defmethod compile-subquery ((query and-query) remaining-conjuncts &rest args)
  (let* ((first (first (subqueries query)))
         (rest (append (rest (subqueries query))
                       remaining-conjuncts)))

    (with-slots (exact-cache-reference 
                 superset-cache-reference
                 subset-cache-reference) query

      (labels ((do-it (bindings &optional check-p)
                 (with-vois-marked-as-bound (all-vois query)
                   `(abortable-dolist (binding ,bindings)
                      (when (or ,(not check-p)
                                (=> ,subset-cache-reference
                                    (not (gethash binding
                                                  (bindings-hash ,subset-cache-reference)))))

                        (loop as voi in ',(all-vois query)
                              as val in binding do 
                              (bind-voi voi val nil))
                        
                        ,(apply #'compile-subquery
                                first 
                                rest
                                args))))))
    
        (cond (exact-cache-reference 
             
               (deactivate-all-subqueries query)
               (do-it `(bindings ,exact-cache-reference)))

              ((or superset-cache-reference 
                   subset-cache-reference)

               `(progn 

                  ,@(with-subset-cache-as-exact-cache (query)
                      (when exact-cache-reference
                        (deactivate-all-subqueries query)
                        (prog1
                            (list 
                             (do-it `(bindings ,exact-cache-reference)))
                          (activate-all-subqueries query))))

                  ,(if superset-cache-reference
                                    
                       (do-it `(bindings ,superset-cache-reference) t)
                 
                     (apply #'compile-subquery first rest args))))
          
              (t (apply #'compile-subquery first rest args)))))))
     
(defmethod evaluate-subquery ((query and-query) remaining-conjuncts)
  (let* ((first (first (subqueries query)))
         (rest (append (rest (subqueries query))
                       remaining-conjuncts)))

    (with-slots (exact-cache-reference 
                 superset-cache-reference
                 subset-cache-reference) query
    
      (labels ((do-it (bindings &optional check-p)
                 (with-saved-bindings (all-vois query)
                   (abortable-dolist (binding bindings)
                     (when (or (not check-p)
                               (=> subset-cache-reference
                                   (not (gethash binding
                                                 (bindings-hash subset-cache-reference)))))
                       (loop as voi in (all-vois query)
                             as val in binding do 
                             (bind-voi voi val nil))

                       (evaluate-subquery first rest))))))

        (cond (exact-cache-reference 
               (deactivate-all-subqueries query)
               (do-it (bindings exact-cache-reference)))

              ((or subset-cache-reference 
                   superset-cache-reference)

               (with-subset-cache-as-exact-cache (query)
                 (when exact-cache-reference
                   (deactivate-all-subqueries query)
                   (do-it (bindings exact-cache-reference))
                   (activate-all-subqueries query)))

               (if superset-cache-reference
                   (do-it (bindings superset-cache-reference) t)
                 (evaluate-subquery first rest)))

              (t (evaluate-subquery first rest)))))))

;;;
;;;
;;;

(defmethod compile-subquery ((query query-reference) remaining-conjuncts &rest args)
  (declare (ignorable args remaining-conjuncts))
 
  (with-vois-marked-as-bound (result-vois query) 
                    
    `(let ((all-inds nil)
           (all-nodes nil))
        
       (labels ((continue ()
                  (progn 
                    ,(apply #'compile-subquery
                            (first remaining-conjuncts)
                            (rest remaining-conjuncts)
                            args)))
                    
                (bind-vois-to (vois vals)

                  ;; (princ vois) 

                  (let ((voi (first vois))
                        (val (first vals)))
                    (when (and voi val)
                      (if (and (cdr vois)
                               (cdr vals))
                          (carefully-bind-voi voi val 
                                              #'(lambda ()
                                                  (bind-vois-to (rest vois)
                                                                (rest vals))))
                        (carefully-bind-voi voi val 
                                            #'(lambda ()
                                                (continue)))))))
                                                   
           
                (neg-bind-vois-to (vois all-vois)

                  ;; (princ vois) 

                  (let ((voi (first vois)))
                    (when voi
                      (dolist (val (if (is-abox-voi-p voi)
                                       (or all-inds
                                           (setf all-inds 
                                                 (dl-prover-all-individuals ,(substrate query))))
                                     (or all-nodes
                                         (setf all-nodes 
                                               (get-nodes ,(substrate query))))))
                        (if (cdr vois)
                            (carefully-bind-voi voi val 
                                                #'(lambda ()
                                                    (neg-bind-vois-to (rest vois)
                                                                      all-vois)))
                       
                          (carefully-bind-voi voi val
                                              #'(lambda (&rest args)
                                                  (declare (ignorable args))
                                             
                                                  (let* ((result-binding 
                                                          (mapcar #'bound-to all-vois))
                                                         (already-present-p  
                                                          (gethash result-binding
                                                                   (result-bindings-hash 
                                                                    ,(referenced-query query)))))
                                                    (unless already-present-p
                                                      (continue)))))))))))

         ,(cond ((result-vois query)
                     
                 (cond ((negated-p query)

                        `(cond ((not (tuple-at-a-time-p ,(referenced-query query)))
                              
                                (neg-bind-vois-to (result-vois ,query) 
                                                  (result-vois ,query)))
                             
                               (t ;; hier habe ich keine Chance - das kann nicht
                                  ;; iterativ beantwortet werden!
                                
                                  (get-answer ,(referenced-query query))
                             
                                  (neg-bind-vois-to (result-vois ,query) 
                                                    (result-vois ,query) ))))
                   
                       (t
                      
                        `(cond ((not (tuple-at-a-time-p ,(referenced-query query)))
                           
                                (abortable-dolist (binding (result-bindings 
                                                            ,(referenced-query query)))
                                  (bind-vois-to (result-vois ,query) 
                                                binding)))

                               (t (loop
                                   (let ((binding (get-next-tuple ,(referenced-query query))))
                                     (cond ((consp binding)
                                            (bind-vois-to (result-vois ,query)
                                                          (mapcar #'second binding))
                                            (return))

                                           ((eq binding :warning-expensive-phase-two-starts)
                                            ;; noch ein Durchlauf!
                                            )
                                           (t (return))))))))))
              
                (t

                 `(progn
                    (when (tuple-at-a-time-p ,(referenced-query query))
                      (get-next-tuple ,(referenced-query query)))

                    ,(if (negated-p query)
                      
                         `(when (not (bindings-found-p ,(referenced-query query)))
                            (continue))
                           
                    
                       `(when (bindings-found-p ,(referenced-query query))
                          (continue))))))))))

  
  
(defmethod evaluate-subquery ((query query-reference) remaining-conjuncts)
  (declare (ignorable remaining-conjuncts))
  
  (let ((all-inds nil)
        (all-nodes nil))
        
    (labels ((bind-vois-to (vois vals)

               ;; (princ vois) 

               (let ((voi (first vois))
                     (val (first vals)))
                 (when (and voi val)
                   (if (and (cdr vois)
                            (cdr vals))
                       (carefully-bind-voi voi val 
                                           #'(lambda ()
                                               (bind-vois-to (rest vois)
                                                             (rest vals))))
                     (carefully-bind-voi voi val (continuation))))))
           
             (neg-bind-vois-to (vois all-vois)

               ;; (princ vois) 

               (let ((voi (first vois)))
                 (when voi
                   (dolist (val (if (is-abox-voi-p voi)
                                    (or all-inds
                                        (setf all-inds 
                                              (dl-prover-all-individuals (substrate query))))
                                  (or all-nodes
                                      (setf all-nodes 
                                            (get-nodes (substrate query))))))
                     (if (cdr vois)
                         (carefully-bind-voi voi val 
                                             #'(lambda ()
                                                 (neg-bind-vois-to (rest vois)
                                                                   all-vois)))
                       
                       (carefully-bind-voi voi val
                                           #'(lambda (&rest args)
                                               (declare (ignorable args))
                                             
                                               (let* ((result-binding 
                                                       (mapcar #'bound-to all-vois))
                                                      (already-present-p  
                                                       (gethash result-binding
                                                                (result-bindings-hash 
                                                                 (referenced-query query)))))
                                                 (unless already-present-p
                                                   (funcall (continuation))))))))))))
      (cond ((result-vois query)
        
             (cond ((negated-p query)

                    (cond ((not (tuple-at-a-time-p (referenced-query query)))

                           (neg-bind-vois-to (result-vois query) 
                                             (result-vois query)))

                          (t ;; hier habe ich keine Chance - das kann nicht
                             ;; iterativ beantwortet werden!

                             (get-answer (referenced-query query))
                             
                             (neg-bind-vois-to (result-vois query) 
                                               (result-vois query) ))))
                   
                   (t

                    (cond ((not (tuple-at-a-time-p (referenced-query query)))
                           
                           (abortable-dolist (binding (result-bindings 
                                                       (referenced-query query)))
                             (bind-vois-to (result-vois query) 
                                           binding)))

                          (t (loop
                              (let ((binding (get-next-tuple (referenced-query query))))
                                (cond ((consp binding)
                                       (bind-vois-to (result-vois query)
                                                     (mapcar #'second binding))
                                       (return))
                                      ((eq binding :warning-expensive-phase-two-starts)
                                       ;; noch ein Durchlauf!
                                       )
                                      (t (return))))))))))

            (t
             (when (tuple-at-a-time-p (referenced-query query))
               (get-next-tuple (referenced-query query)))

             (if (negated-p query)

                 (when (not (bindings-found-p (referenced-query query)))
                   (funcall (continuation)))

               (when (bindings-found-p (referenced-query query))
                 (funcall (continuation)))))))))

;;;
;;;
;;;

(defmethod compile-subquery ((query or-query) remaining-conjuncts &rest args)
  (when (or (exact-cache-reference query)
            (superset-cache-reference query)
            (subset-cache-reference query))
    (to-be-implemented 'use-cache-for-or))

  `(progn 
     ,@(mapcar #'(lambda (disjunct)
                   (let ((code (apply #'compile-subquery disjunct remaining-conjuncts args)))
                     (if *compile-inline-p*
                         code
                       `(funcall (query-fn ,disjunct)))))
               (subqueries query))

     ,@(when (cache-bindings-p query)
         `(,(get-cache-binding-code query)))))

(defmethod evaluate-subquery ((query or-query) remaining-conjuncts)
  (when (or (exact-cache-reference query)
            (superset-cache-reference query)
            (subset-cache-reference query))
    (to-be-implemented 'use-cache-for-or))

  (dolist (disjunct (subqueries query))
    (evaluate-subquery disjunct remaining-conjuncts))
  
  (when (cache-bindings-p query)
    (cache-binding query)))

;;;
;;; spezielle atomare Queries
;;;


(defmethod compile-subquery ((query top-query) remaining-conjuncts &rest args)
  (declare (ignorable args remaining-conjuncts))
  ;;; bisher gibt es nur unaere Top-Queries
  (let ((voi (voi query))
        (substrate (substrate query)))
    (if (bound-to voi)
        `(progn ,+continuation-marker+)
      `(abortable-dolist (cand ,(if (is-abox-voi-p voi)
                                    `(dl-prover-all-individuals ,substrate)
                                  `(get-nodes ,substrate)))
         ,(get-binding-code voi 'cand +continuation-marker+)))))

(defmethod evaluate-subquery ((query top-query) remaining-conjuncts)
  (let ((voi (voi query))
        (substrate (substrate query)))
    (if (bound-to voi)
        (continuation-marker)
      (abortable-dolist (cand (if (is-abox-voi-p voi)
                                  (dl-prover-all-individuals substrate)
                                (get-nodes substrate)))
        (bind-voi voi cand (continuation))))))

(defmethod compile-subquery ((query bottom-query) remaining-conjuncts &rest args)
  (declare (ignorable args remaining-conjuncts))
  nil)

(defmethod evaluate-subquery ((query bottom-query) remaining-conjuncts)
  (declare (ignorable remaining-conjuncts))
  nil)

;;;
;;;
;;;

(defmethod compile-subquery ((query unary-query) remaining-conjuncts &rest args)
  (declare (ignorable args remaining-conjuncts))
  (if (bound-to (voi query))
      (get-tester-code query +continuation-marker+)
    `(catch 'abort-enumerator
       ,(get-enumerator-code query
                             (get-binding-code (voi query) 'cand +continuation-marker+)
                             :var 'cand))))

(defmethod evaluate-subquery ((query unary-query) remaining-conjuncts)
  (if (bound-to (voi query))
      (evaluate-tester query (continuation))
    (catch 'abort-enumerator
      (evaluate-enumerator query
                           #'(lambda (&rest args &key var (una-p *use-unique-name-assumption-p*) &allow-other-keys)  
                               (declare (ignorable args))
                               (bind-voi (voi query) var 
                                         (continuation)
                                         :una-p una-p))))))

(defmethod compile-subquery ((query binary-query) remaining-conjuncts &rest args)
  (declare (ignorable args remaining-conjuncts))
  (if (bound-to (voi-from query))
      (if (bound-to (voi-to query))
          (get-tester-code query +continuation-marker+)

        `(catch 'abort-enumerator
           ,(get-from-bound-enumerator-code query
                                            (get-binding-code (voi-to query) 'cand-to +continuation-marker+)
                                            :to 'cand-to)))

    (if (bound-to (voi-to query))
        `(catch 'abort-enumerator
           ,(get-to-bound-enumerator-code query
                                          (get-binding-code (voi-from query) 'cand-from +continuation-marker+)
                                          :from 'cand-from))

      ;;; beide unbound 

      (if (eq (voi-from query)
              (voi-to query))

          `(catch 'abort-enumerator
             ,(get-enumerator-code query 
                                   (get-binding-code (voi-from query) 'cand-from +continuation-marker+)
                                   :from 'cand-from
                                   :to 'cand-to))
          
        `(catch 'abort-enumerator
           ,(get-enumerator-code query 
                                 (get-binding-code (voi-from query) 'cand-from
                                                   (get-binding-code (voi-to query) 'cand-to +continuation-marker+))
                                 :from 'cand-from
                                 :to 'cand-to))))))

;;; wichtig: f. same-as muss ueberladen werden, 
;;; wegen UNA-VOI-p!

(defmethod compile-subquery ((query same-as-query) remaining-conjuncts &rest args)
  (declare (ignorable args remaining-conjuncts))
  (if (bound-to (voi-from query))
      (if (bound-to (voi-to query))
          (get-tester-code query +continuation-marker+)

        (get-from-bound-enumerator-code query
                                        (get-binding-code (voi-to query) 'cand-to +continuation-marker+ :una-p nil)
                                        :to 'cand-to))
      
    (if (bound-to (voi-to query))
        (get-to-bound-enumerator-code query
                                      (get-binding-code (voi-from query) 'cand-from +continuation-marker+ :una-p nil)
                                      :from 'cand-from)

      ;;; beide unbound 

      (nrql-error "Bad same-as query conjunct detected: ~A" query))))


(defmethod evaluate-subquery ((query binary-query) remaining-conjuncts)
  (if (bound-to (voi-from query))
      (if (bound-to (voi-to query))
          (evaluate-tester query (continuation))

        (catch 'abort-enumerator
          (evaluate-from-bound-enumerator query
                                          #'(lambda (&rest args
                                                           &key to (una-p *use-unique-name-assumption-p*)
                                                           &allow-other-keys)
                                              (declare (ignorable args))
                                              (bind-voi (voi-to query) to 
                                                        (continuation)
                                                        :una-p una-p)))))
    
    (if (bound-to (voi-to query))
        (catch 'abort-enumerator
          (evaluate-to-bound-enumerator query
                                        #'(lambda (&rest args &key from (una-p *use-unique-name-assumption-p*) 
                                                         &allow-other-keys)
                                            (declare (ignorable args))
                                            (bind-voi (voi-from query) from
                                                      (continuation)
                                                      :una-p una-p))))
      (if (eq (voi-from query) 
              (voi-to query))
    
          (catch 'abort-enumerator
            (evaluate-enumerator query 
                                 #'(lambda (&rest args &key from to (una-p *use-unique-name-assumption-p*) &allow-other-keys)
                                     (declare (ignorable args))
                                     ;;; der Compiler macht das an der entsp. Stelle 
                                     ;;; im Enumerator-Code!!! *nicht* vergessen!
                                     (when (same-individual-p from to)
                                       (bind-voi (voi-from query) from 
                                                 (continuation)
                                                 :una-p una-p)))))
            
        (catch 'abort-enumerator
          (evaluate-enumerator query 
                               #'(lambda (&rest args &key from to (una-p *use-unique-name-assumption-p*) &allow-other-keys)
                                   (declare (ignorable args))                                                 
                                   (bind-voi (voi-from query) from
                                             #'(lambda (&rest args &key &allow-other-keys)
                                                 (declare (ignorable args))
                                                 (bind-voi (voi-to query) to
                                                           (continuation)
                                                           :una-p una-p))
                                             :una-p una-p))))))))

;;;
;;; AROUND-METHODEN - CACHE-Beruecksichtigung
;;;

(defmethod get-tester-code :around ((query query) body &rest args)
  (with-slots (exact-cache-reference
               subset-cache-reference
               superset-cache-reference) query
    
    (cond (exact-cache-reference 

           (apply #'get-code-check-if-cache-member-p query :exact body args))

          ((or superset-cache-reference 
               subset-cache-reference)
         
           `(progn 
              ,(with-subset-cache-as-exact-cache (query)
                 (when exact-cache-reference
                   (apply #'get-code-check-if-cache-member-p query :exact body args)))
                
              ,(if superset-cache-reference
                   (apply #'get-code-check-if-cache-member-p query :superset body args)
                 (call-next-method))))
          
          (t (call-next-method)))))

(defmethod evaluate-tester :around ((query query) continuation &rest args &key &allow-other-keys)
  (declare (ignorable args))

  (with-slots (exact-cache-reference
               subset-cache-reference
               superset-cache-reference) query
    
    (cond (exact-cache-reference 
           (check-if-cache-member-p query :exact continuation))

          ((or superset-cache-reference 
               subset-cache-reference)
           
           (with-subset-cache-as-exact-cache (query)
             (when exact-cache-reference
               (check-if-cache-member-p query :exact continuation)))

           (if superset-cache-reference
               (check-if-cache-member-p query :superset continuation)
             (call-next-method)))

          (t (call-next-method)))))


(defmethod get-enumerator-code :around ((query query) body &rest args &key &allow-other-keys)
  (declare (ignorable args))

  (with-slots (exact-cache-reference
               subset-cache-reference
               superset-cache-reference) query

    (cond (exact-cache-reference 
           (apply #'get-code-return-cache-members query :exact body args))

          ((or superset-cache-reference 
               subset-cache-reference)
         
           `(progn 
              ,(with-subset-cache-as-exact-cache (query)
                 (when exact-cache-reference
                   (apply #'get-code-return-cache-members query :exact body args)))
              
              ,(if superset-cache-reference
                   (apply #'get-code-return-cache-members query :superset body args)
                 (call-next-method))))

          (t (call-next-method)))))

(defmethod evaluate-enumerator :around ((query query) continuation &rest args &key &allow-other-keys)
  (declare (ignorable args))

  (with-slots (exact-cache-reference
               subset-cache-reference
               superset-cache-reference) query

    (cond (exact-cache-reference 
           (return-cache-members query :exact continuation))

          ((or superset-cache-reference 
               subset-cache-reference)

           (with-subset-cache-as-exact-cache (query)
             (when exact-cache-reference
               (return-cache-members query :exact continuation)))

           (if superset-cache-reference
               (return-cache-members query :superset continuation)
             (call-next-method)))

          (t (call-next-method)))))

(defmethod get-from-bound-enumerator-code :around ((query query) body &rest args &key &allow-other-keys)
  (with-slots (exact-cache-reference
               subset-cache-reference
               superset-cache-reference) query

    (cond (exact-cache-reference 
           (apply #'get-code-return-cache-members-from-bound query :exact body args))

          ((or superset-cache-reference 
               subset-cache-reference)

           `(progn 
              ,(with-subset-cache-as-exact-cache (query)
                 (when exact-cache-reference
                   (apply #'get-code-return-cache-members-from-bound query :exact body args)))

              ,(if superset-cache-reference
                   (apply #'get-code-return-cache-members-from-bound query :superset body args)
                 (call-next-method))))
           
          (t (call-next-method)))))

(defmethod evaluate-from-bound-enumerator :around ((query query) continuation &rest args &key &allow-other-keys)
  (declare (ignorable args))

  (with-slots (exact-cache-reference
               subset-cache-reference
               superset-cache-reference) query
  
    (cond (exact-cache-reference 
           (return-cache-members-from-bound query :exact continuation))

          ((or superset-cache-reference 
               subset-cache-reference)

           (with-subset-cache-as-exact-cache (query)
             (when exact-cache-reference
               (return-cache-members-from-bound query :exact continuation)))

           (if superset-cache-reference
               (return-cache-members-from-bound query :superset continuation)
             (call-next-method)))
           
          (t (call-next-method)))))

(defmethod get-to-bound-enumerator-code :around ((query query) body &rest args &key &allow-other-keys)
  (with-slots (exact-cache-reference
               subset-cache-reference
               superset-cache-reference) query

    (cond (exact-cache-reference 
           (apply #'get-code-return-cache-members-to-bound query :exact body args))

          ((or superset-cache-reference 
               subset-cache-reference)
           
           `(progn 
              ,(with-subset-cache-as-exact-cache (query)
                 (apply #'get-code-return-cache-members-to-bound query :exact body args))
              
              ,(if superset-cache-reference
                   (apply #'get-code-return-cache-members-to-bound query :superset body args)
                 (call-next-method))))
          
          (t (call-next-method)))))

(defmethod evaluate-to-bound-enumerator :around ((query query) continuation &rest args &key &allow-other-keys)
  (declare (ignorable args))

  (with-slots (exact-cache-reference
               subset-cache-reference
               superset-cache-reference) query

    (cond (exact-cache-reference
           (return-cache-members-to-bound query :exact continuation))
          
          ((or superset-cache-reference 
               subset-cache-reference)
           
           (with-subset-cache-as-exact-cache (query)
             (when exact-cache-reference
               (return-cache-members-to-bound query :exact continuation)))
           
           (if superset-cache-reference
               (return-cache-members-to-bound query :superset continuation)
             (call-next-method)))

          (t (call-next-method)))))

;;;
;;; Substrate Queries 
;;; Achtung: dieser Code ist generisch, aber nicht maximal effizient, da
;;; er die Methode "matches-p" (s. "runtime.lisp") zum Abgleich waerend der Laufzeit 
;;; verwendet! Fuer maximale Effizienz sollte "compile-subquery" ueberladen werde, z.B. fuer
;;; "simple-node-query" und dann statt matches "equal" direkt eincodiert werden, 
;;; wodurch dann auf "matches-p" verzichtet werden kann
;;; 

#+:dlmaps
(defquery-code (substrate-node-query)
  (:tester 
   (:compiler (body &rest args &key var &allow-other-keys)
    (declare (ignorable args))  
    (with-slots (negated-p voi) query
      `(,(if negated-p
             'unless
           'when)
        (matches-p ,query ,(or var `(bound-to ,voi)))
        ,body)))
   (:runtime
    (continuation &rest args &key var &allow-other-keys)
    ;;; Achtung: nur bei den Testern koennen per key 
    ;;; zu ueberpruefende Vars angebeben werden! 
    ;;; gilt nur fuer Runtime! 
    (with-slots (negated-p voi) query
      (let ((var (or var (bound-to voi))))
        (if negated-p
            (unless (matches-p query var )
              (apply continuation :var var args))
          (when (matches-p query var)
            (apply continuation :var var args)))))))
  (:enumerator
   (:compiler
    (body &rest args &key var &allow-other-keys)
    (declare (ignorable args))
    (unless var 
      (nrql-error "Compiler error: No var given"))
    (with-slots (negated-p substrate) query 
      (if negated-p
          `(abortable-dolist (,var (get-nodes ,substrate))
             (unless (matches-p ,query ,var)
               ,body))
        `(abortable-dolist (,var (retrieve-matching-objects ,substrate ,query))
           ,body))))
   (:runtime 
    (continuation &rest args &key &allow-other-keys)
    ;; Achtung - hier kann man *keine* Vars uebergeben! 
    (with-slots (negated-p substrate) query    
      (if negated-p
          (abortable-dolist (var (get-nodes substrate))
            (unless (matches-p query var)
              (apply continuation :var var args)))
        (abortable-dolist (var (retrieve-matching-objects substrate query))
          (apply continuation :var var args)))))))

#+:dlmaps
(defquery-code (substrate-edge-query)
  (:tester
   (:compiler
    (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    (with-slots (negated-p substrate voi-from voi-to) query 
      `(,(if negated-p
             'unless
           'when)
        (some #'(lambda (edge)
                  (matches-p ,query edge))
              (get-edges-between ,substrate
                                 ,(or from `(bound-to ,voi-from))
                                 ,(or to `(bound-to ,voi-to))))
        ,body)))
   (:runtime
    (continuation &rest args &key from to &allow-other-keys)
    (with-slots (negated-p substrate voi-from voi-to) query
      (let ((from (or from (bound-to voi-from)))
            (to (or to (bound-to voi-to))))
        (if negated-p
            (unless (some #'(lambda (edge)
                              (matches-p query edge))
                          (get-edges-between substrate from to))
              (apply continuation :from from :to to args))
          (when (some #'(lambda (edge)
                          (matches-p query edge))
                      (get-edges-between substrate from to))
            (apply continuation :from from :to to args)))))))

  (:from-bound-enumerator 
   (:compiler 
    (body &rest args &key to &allow-other-keys)
    (declare (ignorable args))
    (unless to 
      (nrql-error "Compiler error: No var given"))    
    (with-slots (negated-p substrate voi-from) query
      (if negated-p
          `(abortable-dolist (,to (get-nodes ,substrate))
             (unless (some #'(lambda (edge)
                               (matches-p ,query edge))
                           (get-edges-between ,substrate (bound-to ,voi-from) ,to))
               ,body))
        `(abortable-dolist (edge (outgoing (bound-to ,voi-from)))
           (let ((,to (to edge)))
             (declare (ignorable ,to))
             (when (matches-p ,query edge) 
               ,body))))))
   (:runtime 
    (continuation &rest args &key &allow-other-keys)
    (with-slots (negated-p substrate voi-from) query    
      (if negated-p
          (abortable-dolist (var (get-nodes substrate))
            (unless (some #'(lambda (edge)
                              (matches-p query edge))
                          (get-edges-between substrate (bound-to voi-from) var))
              (apply continuation :to var args)))
        (abortable-dolist (edge (outgoing (bound-to voi-from)))
          (when (matches-p query edge) 
            (apply continuation :to (to edge) args)))))))

  (:to-bound-enumerator
   (:compiler 
    (body &rest args &key from &allow-other-keys)
    (declare (ignorable args))
    (unless from
      (nrql-error "Compiler error: No var given"))
    (with-slots (negated-p substrate voi-to) query
      (if (negated-p query)
          `(abortable-dolist (,from (get-nodes ,substrate))
             (unless (some #'(lambda (edge)
                               (matches-p ,query edge))
                           (get-edges-between ,substrate ,from (bound-to ,voi-to)))
               ,body))
        `(abortable-dolist (edge (incoming (bound-to ,voi-to)))
           (let ((,from (from edge)))
             (declare (ignorable ,from))
             (when (matches-p ,query edge)
               ,body))))))
   (:runtime
    (continuation &rest args &key &allow-other-keys)
    (with-slots (negated-p substrate voi-to) query       
      (if negated-p
          (abortable-dolist (var (get-nodes substrate))
            (unless (some #'(lambda (edge)
                              (matches-p query edge))
                          (get-edges-between substrate var (bound-to voi-to)))
              (apply continuation :from var args)))
        (abortable-dolist (edge (incoming (bound-to voi-to)))
          (when (matches-p query edge)
            (apply continuation :from (from edge) args)))))))

  (:enumerator
   (:compiler 
    (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    (unless (and from to)
      (nrql-error "Compiler error: No vars given"))
    (with-slots (negated-p substrate) query
      (if negated-p
          (if (eq (voi-from query)
                  (voi-to query))
              `(abortable-dolist (,from (get-nodes ,substrate))
                 (unless (some #'(lambda (edge)
                                   (matches-p ,query edge))
                               (get-edges-between ,substrate ,from ,from))
                   ,body))
            `(abortable-dolist (,from (get-nodes ,substrate))
               (abortable-dolist (,to (get-nodes ,substrate))
                 (unless (some #'(lambda (edge)
                                   (matches-p ,query edge))
                               (get-edges-between ,substrate ,from ,to))
                   ,body))))
        (if (eq (voi-from query)
                (voi-to query))
            `(abortable-dolist (,from (get-nodes ,substrate))
               (when (some #'(lambda (edge)
                               (matches-p ,query edge))
                           (get-edges-between ,substrate ,from ,from))
                 ,body))
          `(abortable-dolist (edge (retrieve-matching-objects ,substrate ,query))
             (let ((,from (from edge))
                   (,to (to edge)))
               (declare (ignorable ,from ,to))
               ,body))))))
   (:runtime
    (continuation &rest args &key &allow-other-keys)
    (with-slots (negated-p substrate) query
      (if negated-p
          (if (eq (voi-from query)
                  (voi-to query))
              (abortable-dolist (from (get-nodes substrate))
                (unless (some #'(lambda (edge)
                                  (matches-p query edge))
                              (get-edges-between substrate from from))
                  (apply continuation :from from :to from args)))
            (abortable-dolist (from (get-nodes substrate))
              (abortable-dolist (to (get-nodes substrate))
                (unless (some #'(lambda (edge)
                                  (matches-p query edge))
                              (get-edges-between substrate from to))
                  (apply continuation :from from :to to args)))))
        (if (eq (voi-from query)
                (voi-to query))        
            (abortable-dolist (from (get-nodes substrate))
              (when (some #'(lambda (edge)
                              (matches-p query edge))
                          (get-edges-between substrate from from))
                (apply continuation :from from :to from args)))
          (abortable-dolist (edge (retrieve-matching-objects substrate query))
            (apply continuation :from (from edge) :to (to edge) args))))))))

;;;
;;; ABOX Queries (Racer + MiDeLoRa) 
;;; 

(defquery-code (instance-retrieval-query)
  (:tester
   (:compiler (body &rest args &key var &allow-other-keys)
    (apply #'get-code-dl-prover-check-individual-instance-p 
           (or var `(bound-to ,(voi query)))
           (dl-concept query)
           body 
           :query query
           :negated-p (negated-p query)
           args))
   (:runtime (continuation &rest args &key var &allow-other-keys)
    (apply #'evaluate-dl-prover-check-individual-instance-p 
           (or var (bound-to (voi query)))
           (dl-concept query)
           continuation
           :query query
           :negated-p (negated-p query)
           args)))
  (:enumerator
   (:compiler (body &rest args &key var &allow-other-keys)
    (unless var (nrql-error "Compiler error: No var given"))
    (apply #'get-code-dl-prover-retrieve-concept-instances 
           (dl-concept query)
           body
           :query query
           :negated-p (negated-p query)
           args))
   (:runtime  (continuation &rest args &key &allow-other-keys)
    (apply #'evaluate-dl-prover-retrieve-concept-instances 
           (dl-concept query)
           continuation
           :query query
           :negated-p (negated-p query)
           args))))

(defquery-code (has-known-successor-retrieval-query)
  (:tester 
   (:compiler (body &rest args &key var &allow-other-keys)
    (apply #'get-code-dl-prover-check-has-known-successor-p 
           (or var `(bound-to ,(voi query)))
           (dl-role query)
           body 
           :query query
           :negated-p (negated-p query)
           args))
   (:runtime (continuation &rest args &key var &allow-other-keys)
    (apply #'evaluate-dl-prover-check-has-known-successor-p 
           (or var (bound-to (voi query)))
           (dl-role query)
           continuation
           :query query
           :negated-p (negated-p query)
           args)))
  (:enumerator 
   (:compiler (body &rest args &key var &allow-other-keys)
    (unless var (nrql-error "Compiler error: No var given"))
    (apply #'get-code-dl-prover-retrieve-has-known-successor-instances 
           (dl-role query)
           body
           :query query
           :negated-p (negated-p query)
           args))
   (:runtime (continuation &rest args &key &allow-other-keys)
    (apply #'evaluate-dl-prover-retrieve-has-known-successor-instances 
           (dl-role query)
           continuation
           :query query
           :negated-p (negated-p query)
           args))))

(defquery-code (edge-retrieval-query)
  (:tester 
   (:compiler (body &rest args &key from to &allow-other-keys)
    (apply #'get-code-dl-prover-check-individuals-related-p 
           (or from `(bound-to ,(voi-from query)))
           (or to `(bound-to ,(voi-to query)))
           (dl-role query)
           body
           :query query
           :negated-p (negated-p query)
           args))
   (:runtime  (continuation &rest args &key from to &allow-other-keys)
    (apply #'evaluate-dl-prover-check-individuals-related-p 
           (or from (bound-to (voi-from query)))
           (or to (bound-to (voi-to query)))
           (dl-role query)
           continuation
           :query query
           :negated-p (negated-p query)
           args)))

  (:from-bound-enumerator 
   (:compiler (body &rest args &key to &allow-other-keys)
    (unless to (nrql-error "Compiler error: No var given"))
    (apply #'get-code-dl-prover-retrieve-individual-fillers
           `(bound-to ,(voi-from query))
           (dl-role query)
           body 
           :query query
           :negated-p (negated-p query)
           :to to
           args))
   (:runtime (continuation &rest args &key &allow-other-keys)
    (apply #'evaluate-dl-prover-retrieve-individual-fillers
           (bound-to (voi-from query))
           (dl-role query)
           continuation
           :query query
           :negated-p (negated-p query)
           args)))
  
  (:to-bound-enumerator
   (:compiler (body &rest args &key from &allow-other-keys)
    (unless from (nrql-error "Compiler error: No var given"))
    (apply #'get-code-dl-prover-retrieve-individual-fillers 
           `(bound-to ,(voi-to query))
           (dl-role query)
           body 
           :query query
           :inverse-p t ;;; wichtig!
           :negated-p (negated-p query)
           :to from ;;; wichtig! 
           args))
   (:runtime (continuation &rest args &key &allow-other-keys)
    (apply #'evaluate-dl-prover-retrieve-individual-fillers 
           (bound-to (voi-to query))
           (dl-role query)
           continuation 
           :query query
           :inverse-p t
           :negated-p (negated-p query)
           args)))

  (:enumerator 
   (:compiler (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    (let ((body 
           (if (eq (voi-from query) 
                   (voi-to query))
               `(when (same-individual-p ,from ,to)
                  ,body)
             body)))
      (apply #'get-code-dl-prover-retrieve-related-individuals (dl-role query)
             body 
             :query query
             :negated-p (negated-p query)
             args)))
   (:runtime (continuation &rest args &key &allow-other-keys)
    (apply #'evaluate-dl-prover-retrieve-related-individuals 
           (dl-role query)
           continuation 
           :query query
           :negated-p (negated-p query)
           args))))

;;;
;;; momentan nur f. Racer
;;; 

(defquery-code (racer-cd-edge-retrieval-query)
  (:tester
   (:compiler (body &rest args &key from to &allow-other-keys)
    (apply #'get-code-racer-check-individuals-cd-related-p 
           (or from `(bound-to ,(voi-from query)))
           (or to `(bound-to ,(voi-to query)))
           (from-attribute query)
           (to-attribute query)
           (constraint query)
           body
           :query query
           :negated-p (negated-p query)
           args))
   (:runtime (continuation &rest args &key from to &allow-other-keys)
    
    (apply #'evaluate-racer-check-individuals-cd-related-p 
           (or from (bound-to (voi-from query)))
           (or to (bound-to (voi-to query)))
           (from-attribute query)
           (to-attribute query)
           (constraint query)
           continuation
           :query query
           :negated-p (negated-p query)
           args)))

  (:from-bound-enumerator
   (:compiler (body &rest args &key to &allow-other-keys)
    (unless to (nrql-error "Compiler error: No var given"))
    
    (with-slots (negated-p substrate) query
      (if (not negated-p) 
          (apply #'get-code-dl-prover-check-individual-instance-p 
                 `(bound-to ,(voi-from query))
                 `(an ,(from-attribute query))
                 (apply #'get-code-dl-prover-retrieve-concept-instances                      
                        `(an ,(to-attribute query))
                        (apply #'get-tester-code query body :to to args)
                        :var to
                        :query query
                        args)
                 :query query
                 args)

        `(abortable-dolist (,to (dl-prover-all-individuals ,substrate))
           ,(apply #'get-tester-code query body :to to args)))))

   (:runtime  (continuation &rest args &key &allow-other-keys)
    
    (with-slots (negated-p substrate) query
      (if (not negated-p) 
          (apply #'evaluate-dl-prover-check-individual-instance-p 
                 (bound-to (voi-from query))
                 `(an ,(from-attribute query))
                 #'(lambda (&rest args)
                     (apply #'evaluate-dl-prover-retrieve-concept-instances                      
                            `(an ,(to-attribute query))
                            #'(lambda (&rest args &key var &allow-other-keys)
                                (apply #'evaluate-tester query continuation :to var args))
                            :query query
                            args))
                 :query query
                 args)

        (abortable-dolist (to (dl-prover-all-individuals substrate))
          (apply #'evaluate-tester query continuation :to to args ))))))

  (:to-bound-enumerator
   (:compiler (body &rest args &key from &allow-other-keys)
    (unless from (nrql-error "Compiler error: No var given"))

    (with-slots (negated-p substrate) query  

      (if (not negated-p) 
          (apply #'get-code-dl-prover-check-individual-instance-p 
                 `(bound-to ,(voi-to query))
                 `(an ,(to-attribute query))
                 (apply #'get-code-dl-prover-retrieve-concept-instances                      
                        `(an ,(from-attribute query))
                        (apply #'get-tester-code query body :from from args)
                        :var from
                        :query query
                        args)
                 :query query
                 args)

        `(abortable-dolist (,from (dl-prover-all-individuals ,substrate))
           ,(apply #'get-tester-code query body :from from args)))))
        
   (:runtime (continuation &rest args &key &allow-other-keys)

    (with-slots (negated-p substrate) query  
    
      (if (not negated-p) 
          (apply #'evaluate-dl-prover-check-individual-instance-p 
                 (bound-to (voi-to query))
                 `(an ,(to-attribute query))
                 #'(lambda (&rest args)
                     (apply #'evaluate-dl-prover-retrieve-concept-instances                      
                            `(an ,(from-attribute query))
                            #'(lambda (&rest args &key var &allow-other-keys)
                                (apply #'evaluate-tester query continuation :from var args))
                            :query query
                            args))
                 :query query
                 args)

        (abortable-dolist (from (dl-prover-all-individuals substrate))
          (apply #'evaluate-tester query continuation :from from args ))))))

  (:enumerator
   (:compiler (body &rest args &key from to &allow-other-keys)
    (unless (and from to) (nrql-error "Compiler error: No vars given"))
  
    (with-slots (negated-p substrate) query  

      (if (not negated-p) 
   
          (if (eq (voi-from query)
                  (voi-to query))
        
              (apply #'get-code-dl-prover-retrieve-concept-instances
                     `(and (an ,(from-attribute query))
                           (an ,(to-attribute query)))
                     (apply #'get-tester-code query body 
                            :from from
                            :to from
                            args)
                     :var from
                     :query query
                     args)
      
            (apply #'get-code-dl-prover-retrieve-concept-instances
                   `(an ,(from-attribute query))
                   (apply #'get-code-dl-prover-retrieve-concept-instances                      
                          `(an ,(to-attribute query))
                          (apply #'get-tester-code query body 
                                 args)
                          :var to
                          :query query
                          args)
                   :var from         
                   :query query
                   args))

        (if (eq (voi-from query)
                (voi-to query))

            `(abortable-dolist (,from (dl-prover-all-individuals ,substrate))
               ,(apply #'get-tester-code query body :from from :to from args))

          `(abortable-dolist (,from (dl-prover-all-individuals ,substrate))
             (abortable-dolist (,to (dl-prover-all-individuals ,substrate))
               ,(apply #'get-tester-code query body :from from :to to args)))))))
             
   (:runtime (continuation &rest args &key &allow-other-keys)

    (let ((from (voi-from query))
          (to (voi-to query)))
      
      (with-slots (negated-p substrate) query  

        (if (not negated-p) 

            (if (eq from to)
        
                (apply #'evaluate-dl-prover-retrieve-concept-instances
                       `(and (an ,(from-attribute query))
                             (an ,(to-attribute query)))
                       #'(lambda (&rest args &key var &allow-other-keys)
                           (apply #'evaluate-tester query continuation :from var :to var args))
                       :query query
                       args)
      
              (apply #'evaluate-dl-prover-retrieve-concept-instances
                     `(an ,(from-attribute query))
                     #'(lambda (&rest args &key var &allow-other-keys)
                         (let ((from var))
                           (apply #'evaluate-dl-prover-retrieve-concept-instances
                                  `(an ,(to-attribute query))
                                  #'(lambda (&rest args &key var &allow-other-keys)
                                      (let ((to var))
                                        (apply #'evaluate-tester query continuation :from from :to to args)))
                                  :query query
                                  args)))
                     :query query
                     args))

          (if (eq from to)
              
              (abortable-dolist (from (dl-prover-all-individuals substrate))
                (apply #'evaluate-tester query continuation :from from :to from args))

            (abortable-dolist (from (dl-prover-all-individuals substrate))
              (abortable-dolist (to (dl-prover-all-individuals substrate))
                (apply #'evaluate-tester query continuation :from from :to to args))))))))))

;;;
;;;
;;;

(defquery-code (same-as-query)
  
  (:tester 
   (:compiler (body &rest args &key &allow-other-keys)
    (declare (ignorable args))
    (let ((from1 (voi-from query))
          (to1 (voi-to query))
          (negated-p (negated-p query)))
      (if negated-p 
          `(when (and (=> ,(is-abox-voi-p from1)
                          (not (same-abox-individual-p (bound-to ,from1)
                                                       (bound-to ,to1))))
                      (=> ,(is-substrate-voi-p from1)
                          (not (same-individual-p (bound-to ,from1)
                                                  (bound-to ,to1)))))
             ,body)
        `(when (and (=> ,(is-abox-voi-p from1)
                        (same-abox-individual-p (bound-to ,from1)
                                                (bound-to ,to1)))
                    (=> ,(is-substrate-voi-p from1) 
                        (same-individual-p (bound-to ,from1)
                                           (bound-to ,to1))))
           ,body))))

   (:runtime  (continuation &rest args &key &allow-other-keys)
    (let ((from1 (voi-from query))
          (to1 (voi-to query))
          (negated-p (negated-p query)))
      (if negated-p 
          (when (and (=> (is-abox-voi-p from1)
                         (not (same-abox-individual-p (bound-to from1)
                                                      (bound-to to1))))
                     (=> (is-substrate-voi-p from1)
                         (not (same-individual-p (bound-to from1)
                                                 (bound-to to1)))))
            (apply continuation args))
        (when (and (=> (is-abox-voi-p from1)
                       (same-abox-individual-p (bound-to from1)
                                               (bound-to to1)))
                   (=> (is-substrate-voi-p from1) 
                       (same-individual-p (bound-to from1)
                                          (bound-to to1))))
          (apply continuation args))))))

  (:from-bound-enumerator
   
   ;;; (same-as $?x $?y), (same-as {$}?x i), 
   ;;; (same-as i {$}?x) gibt es nicht! (s. syntactic-rewriting)

   (:compiler (body &rest args &key to &allow-other-keys)
    (declare (ignorable args))
    (let ((from (voi-from query)))

      (if (negated-p query)
          `(abortable-dolist (,to ,(if (is-abox-voi-p from)
                                       `(dl-prover-all-individuals *running-substrate*)
                                     `(get-nodes *running-substrate*)))
             (when (and (=> ,(is-abox-voi-p from)
                            (not (same-abox-individual-p (bound-to ,from) ,to)))
                        (=> ,(is-substrate-voi-p from)
                            (not (same-individual-p (bound-to ,from) ,to))))
               ,body))

        `(let ((,to (bound-to ,from)))
           ,body))))
   
   (:runtime (continuation &rest args &key &allow-other-keys)

    (let ((from (voi-from query)))
      (if (negated-p query)
          (abortable-dolist (cand (if (is-abox-voi-p from)
                                      (dl-prover-all-individuals *running-substrate*)
                                    (get-nodes *running-substrate*)))

            (when (and (=> (is-abox-voi-p from)
                           (not (same-abox-individual-p (bound-to from) cand)))
                       (=> (is-substrate-voi-p from)
                           (not (same-individual-p (bound-to from) cand))))
              
              (apply continuation 
                     :una-p nil
                     :to cand
                     args)))

        (let ((cand (bound-to from)))
          (apply continuation 
                 :una-p nil
                 :to cand
                 args))))))
  
  (:to-bound-enumerator

   ;;; (same-as $?x $?y), (same-as {$}?x i)

   (:compiler (body &rest args &key from &allow-other-keys)
    (declare (ignorable args))
    (let ((to (voi-to query)))
      (if (negated-p query)
          `(abortable-dolist (,from ,(if (is-abox-voi-p to)
                                         `(dl-prover-all-individuals *running-substrate*)
                                       `(get-nodes *running-substrate*)))
             (when (and (=> ,(is-abox-voi-p to)
                            (not (same-abox-individual-p (bound-to ,to) ,from)))
                        (=> ,(is-substrate-voi-p to)
                            (not (same-individual-p (bound-to ,to) ,from))))
               ,body))

        (if (is-abox-individual-p to)

            `(when (individual-exists-p ,to)
               (abortable-dolist (,from ,(if (is-abox-voi-p to)
                                             `(dl-prover-all-individuals *running-substrate*)
                                           `(get-nodes *running-substrate*)))
                 (when (and (=> ,(is-abox-voi-p to)
                                (same-abox-individual-p (bound-to ,to) ,from))
                            (=> ,(is-substrate-voi-p to)
                                (same-individual-p (bound-to ,to) ,from)))
                   ,body)))

          `(let ((,from (bound-to ,to)))
             ,body)))))
   
   
   (:runtime (continuation &rest args &key &allow-other-keys)
    (let ((to (voi-to query)))
      (if (negated-p query)
          (abortable-dolist (cand (if (is-abox-voi-p to)
                                      (dl-prover-all-individuals *running-substrate*)
                                    (get-nodes *running-substrate*)))
            (when (and (=> (is-abox-voi-p to)
                           (not (same-abox-individual-p (bound-to to) cand)))
                       (=> (is-substrate-voi-p to)
                           (not (same-individual-p (bound-to to) cand))))
              
              (apply continuation 
                     :una-p nil
                     :from cand
                     args)))

        (if (is-abox-individual-p (voi-to query))
            (when (individual-exists-p to)
              (abortable-dolist (cand (if (is-abox-voi-p to)
                                          (dl-prover-all-individuals *running-substrate*)
                                        (get-nodes *running-substrate*)))
                (when (and (=> (is-abox-voi-p to)
                               (same-abox-individual-p (bound-to to) cand))
                           (=> (is-substrate-voi-p to)
                               (same-individual-p (bound-to to) cand)))
            
                  (apply continuation 
                         :una-p nil
                         :from cand
                         args))))

          (let ((cand (bound-to to)))
            (apply continuation 
                   :una-p nil
                   :from cand
                   args)))))))

  ;;; :enumerator  
  ;;; gibt es nicht, weil durch syntactic-rewriting sichergestellt wird, dass
  ;;; (same-as $?x $?y) -> (and (top $?x) (same-as $?x $?y) (top $?y)) vorliegt
  ;;; das funktioniert auch bei ausgeschaltetem Optimizer, und der Optimizer sichert
  ;;; zu, dass die Reihenfolgen nicht umgedreht werden. 

  )


