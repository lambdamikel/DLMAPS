;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;;
;;;


(defmethod check-abox-consistency ((substrate substrate) &key &allow-other-keys)
  t)


(defmethod check-abox-consistency ((substrate racer-substrate) &key racer-wuna-p) 
  (with-critical-section
    (with-slots (tbox abox) substrate
      (unless (abox-consistent-p abox)
        (nrql-error "ABox ~A is inconsistent! Querying denied." abox))
      (when (and racer-wuna-p
                 (not (abox-una-consistent-p abox)))
        (nrql-error "ABox ~A is UNA inconsistent! Querying denied." abox)))))

#+:midelora
(defmethod check-abox-consistency ((substrate midelora-substrate) &key &allow-other-keys)
  (with-critical-section
    (unless (prover::abox-consistent-p substrate)
      (nrql-error "ABox ~A is inconsistent! Querying denied." substrate))))

;;;
;;;
;;;

(defmethod prepare-substrate1 ((substrate racer-substrate))
  (without-timeout 

   ;;; reset-substrate ist die einzige Funktion, die Zeit kostet; 
   ;;; daher soll nur dort ein Timeout erlaubt sein 

   (with-slots (new-inds-hash needs-filler-reasoning-p tbox abox package) substrate
     (clrhash new-inds-hash)
    
     (if (is-racer-tbox-mirror-substrate-p substrate)
         (setf needs-filler-reasoning-p nil)
       (let ((language (get-abox-language abox)))
         (setf needs-filler-reasoning-p
               (or needs-filler-reasoning-p
                   (find #\f language)
                   (find #\n language)
                   (find #\q language)))))))
    
  (when (substrate-needs-reset-p substrate)                  

    ;;; hat sich die ABox geaendert seitdem die Substrate-Instanz
    ;;; erzeugt wurde? -> reset-substrate. 
    ;;; reset-substrate ruft compute-abox-mirror bzw.
    ;;; compute-tbox-mirror!
    
    (reset-substrate substrate)))


#+:midelora
(defmethod prepare-substrate1 ((substrate midelora-substrate))
  (without-timeout 

   ;;; reset-substrate ist die einzige Funktion, die Zeit kostet; 
   ;;; daher soll nur dort ein Timeout erlaubt sein 

   (with-slots (new-inds-hash needs-filler-reasoning-p tbox) substrate

     (clrhash new-inds-hash)
    
     (if (is-midelora-tbox-mirror-substrate-p substrate)
         (setf needs-filler-reasoning-p nil)
       (let ((language (prover::get-abox-language substrate)))
         (setf needs-filler-reasoning-p
               (or needs-filler-reasoning-p
                   (find #\f language)
                   (find #\n language)
                   (find #\q language)))))))
    
  (when (substrate-needs-reset-p substrate)                  

    ;;; hat sich die ABox geaendert seitdem die Substrate-Instanz
    ;;; erzeugt wurde? -> reset-substrate. 
    ;;; reset-substrate ruft compute-abox-mirror bzw.
    ;;; compute-tbox-mirror!
    
    (reset-substrate substrate)))


;;;
;;; Bevor die Query gestartet wird, wird (vom evtl. neuen Query-Prozess) 
;;; prepare-substrate-for-query-execution gerufen - das spiegelt dann
;;; erst Teile der ABox (oder die ganze)
;;; 

(defmethod prepare-substrate-for-query-execution ((substrate dl-prover-substrate) (query query))
  t)

(defmethod prepare-substrate-for-query-execution :after ((substrate racer-substrate) (query query))
  (with-critical-section
    (with-slots (check-abox-consistency-p
                 ensure-tbox-classification-p) query
      (with-slots (tbox abox) substrate
	  
        (unless (is-tbox-mirror-substrate-p substrate)
          (when check-abox-consistency-p
            (check-abox-consistency substrate))

          (when ensure-tbox-classification-p
            (unless (dl-tbox-classified-p substrate tbox)
              (dl-classify-tbox substrate tbox)
              (unless (dl-tbox-coherent-p substrate tbox)
                (nrql-warning "TBox ~A is incoherent!")))))

        (prepare-substrate1 substrate)))))


(defmethod prepare-substrate-for-query-execution ((substrate tbox-mirror-substrate) (query query))
  (with-critical-section
      
    (with-slots (check-abox-consistency-p) query

      (setf check-abox-consistency-p nil))))


;;;
;;;
;;;

(defmethod prepare-substrate-for-query-execution :after ((substrate dl-prover-substrate) (query nrql-query))
  (with-critical-section
    (with-slots (added-premise-axioms
                 premise) query
       
      (setf added-premise-axioms nil)

      (when premise
        (let* ((*told-information-reasoning-p* t)
               (*dont-add-abox-duplicates-p* t))

          (setf added-premise-axioms
                (add-abox-assertions substrate premise)))

        ;;; darf nicht unter *told-information-reasoning-p* asugefuehrt werden!!!
        ;;; sonst Bugs!

        (prepare-substrate1 substrate)))))

;;;
;;;
;;;

(defmethod prepare-query-for-execution ((query query))
  (with-slots (counter abort-search-p) query
    
    (dolist (sq (cons query (all-subqueries query)))
      (with-slots (cache-bindings-p
                   bindings-found-p bindings result-bindings) sq
        
        (setf bindings-found-p nil
              bindings nil
              result-bindings nil)))
    
    (setf counter 0)

    (setf abort-search-p nil)))


(defmethod invalidate-cached-bindings ((query query))
  (dolist (sq (cons query (all-subqueries query)))
    (setf (slot-value sq 'bindings) nil)))

;;;
;;;
;;;

(defmethod prepare-to-run-query ((query query))      
  (without-timeout
    
   (with-slots (env-setup-fn iterator-id) query
     
     (setf env-setup-fn 
           #'(lambda (query)

               (with-slots (initial-abox-mirroring-p
                            exclude-permutations-p
                            timeout 
                            substrate
                            answer-pattern
                            timeout-p 
                            abort-search-p
                            parser
                            query-fn
                            result-bindings-hash
                            iterator-id
                            use-repository-p
                            put-into-repository-p
                            source) query

                 (without-signature-checks
                  (with-dl-prover-state (query)

                    ;;; Specials binden - diese werden nur verwendet, 
                    ;;; um bei Modulen die nicht direkt das Query Objekt als
                    ;;; Parameter bekommen (racer-dummy-substrate... / 
                    ;;; racer-code, ...) nicht jedesmal entsp. Parameter mit-
                    ;;; uebergeben zu muessen! ansonsten sind alle relevanten
                    ;;; Parameterwerte im Query-Objekt selbst gespeichert!
	      
                    (let* ((*running-query* query)
                           (*running-substrate* substrate)                         
                           (*process-id* iterator-id)
                           (*previous-conjunct* nil)
			   
			   (*package* (if #+:midelora (is-midelora-substrate-p *running-substrate*)
					#-:midelora nil
                                        (find-package :prover)
					*package*))

                           (*running-abox* (typecase *running-substrate*
                                             (racer-substrate
                                              (when (abox *running-substrate*)
                                                (with-critical-section
                                                  (find-abox (abox *running-substrate*)))))
                                             #+:midelora 
                                             (midelora-substrate
                                              *running-substrate*)))
                           (*running-tbox* (typecase *running-substrate* 
                                             (racer-substrate
                                              (when (tbox *running-substrate*)
                                                (with-critical-section 
                                                  (find-tbox (tbox *running-substrate*)))))
                                             #+:midelora 
                                             (midelora-substrate
                                              (tbox *running-substrate*))))

                           (*initial-abox-mirroring-p* initial-abox-mirroring-p)
                           (*exclude-permutations-p* exclude-permutations-p))
                      
                      (prepare-substrate-for-query-execution *running-substrate* *running-query*)
                      
                      (unless source ; ansonsten einkompiliert! 
                        (when use-repository-p
                          (compute-cache-references *running-query* put-into-repository-p)))
                      
                      (with-new-marking-context        

                        (prepare-query-for-execution query)
		  
                        ;; Call Hook 
                        (querying-started *running-substrate* query)

                        (unwind-protect
                            (catch 'query-execution 
                           
                              (with-timeout-cleanup ; f. uebergeordneten Timeout! 
                               (if timeout                                 
                                   (with-timeout
                                       ;;; zumindest die Racer-API-Funktionen
                                       ;;; werden niemals ein with-timeout 
                                       ;;; an dieser Stelle aufsetzen; s. with-racer-timeout
                                       ;;; in den racer-api-Funktionsdefinitionen
                                       ;;; das timeout hier ist ein "historisches Feature" 
                                       (timeout
                                        (setf abort-search-p t
                                              timeout-p t))
                                     (funcall query-fn))
                              
                                 ;;; Racer-API in diesen Zweig
                                 (funcall query-fn))

                               (progn 
                                 (setf abort-search-p t
                                       timeout-p t)

                                 (substrate-needs-reset *running-substrate*))))
		    
                          ;; Call Hook 
                          (querying-ended *running-substrate* query)
		    
                          (if (or (not answer-pattern) 
                                  ;; in diesem Falle wurde nur eine Binding erzeugt!
                                  ;; nicht als Cache nutzbar! 
                                  abort-search-p)
                              (invalidate-cached-bindings query)
		      
                            (setf (slot-value query 'valid-qbox-entry-p) t))
		    
                          'done))))))))
    

     (unless (bottom-up-component-query-p query)
       (if (is-rule-p query) 
           (progn 
             (push query *all-rules*)
             (push query *ready-rules*))
         (progn 
           (push query *all-queries*)
           (push query *ready-queries*))))
    
     (list iterator-id :ready-to-run))))



(defmethod prepare-to-run-query ((query nrql-query))      
  (without-timeout
   (with-slots (iterator-id env-setup-fn) query
     (setf env-setup-fn 
           #'(lambda (query)
	  
               (with-slots (told-information-reasoning-p
                            exclude-permutations-p
                            continuation-based-instance-retrieval-p 
                            ensure-tbox-classification-p
                            initial-abox-mirroring-p
                            timeout 
                            substrate
                            answer-pattern
                            counter 
                            timeout-p 
                            abort-search-p
                            parser
                            query-fn
                            iterator-id
                            source
                            put-into-repository-p
                            use-repository-p) query

                 (without-signature-checks
                  (with-dl-prover-state (query)
	      
                    (let* ((*running-query* query)
                           (*running-substrate* substrate)                         
                           (*process-id* iterator-id)
                           (*previous-conjunct* nil)

                           (*ensure-tbox-classification-p* ensure-tbox-classification-p)
                         
                           (*told-information-reasoning-p* told-information-reasoning-p)
			   
                           (*continuation-based-instance-retrieval-p* continuation-based-instance-retrieval-p)
			   
			   (*package* (if #+:midelora (is-midelora-substrate-p *running-substrate*)
					#-:midelora nil
                                        (find-package :prover)
					*package*))
		     
                           (*running-abox* (typecase *running-substrate*
                                             (racer-substrate
                                              (when (abox *running-substrate*)
                                                (with-critical-section
                                                  (find-abox (abox *running-substrate*)))))
                                             #+:midelora 
                                             (midelora-substrate
                                              *running-substrate*)))
                           (*running-tbox* (typecase *running-substrate* 
                                             (racer-substrate
                                              (when (tbox *running-substrate*)
                                                (with-critical-section 
                                                  (find-tbox (tbox *running-substrate*)))))
                                             #+:midelora 
                                             (midelora-substrate
                                              (tbox *running-substrate*))))

                           (*initial-abox-mirroring-p* initial-abox-mirroring-p)
                           (*exclude-permutations-p* exclude-permutations-p))

                      (prepare-substrate-for-query-execution *running-substrate* *running-query*)

                      (unless source ;; ansonsten einkompiliert
                        (when use-repository-p
                          (compute-cache-references *running-query* put-into-repository-p)))
		
                      (with-new-marking-context                     

                        (prepare-query-for-execution query)
		  
                        ;; Call Hook 
                        (querying-started *running-substrate* query)
		  
                        (unwind-protect
                            (catch 'query-execution 
                           
                              (with-timeout-cleanup
                            
                               (if timeout       
                                   (with-timeout 
                                       (timeout
                                        (when (iterator-id query)
                                          (nrql-warning "Timeout for query ~A!" (iterator-id query)))
                                        (setf abort-search-p t
                                              timeout-p t))
                                     (funcall query-fn))
                              
                                 (funcall query-fn))
                            
                               (progn 
                                 (setf abort-search-p t
                                       timeout-p t)

                                 (substrate-needs-reset *running-substrate*))))
                            
                        
                          ;; Call Hook 
                          (querying-ended *running-substrate* query)
		    
                          (if (or (not answer-pattern) 
                                  abort-search-p)
                              (invalidate-cached-bindings query)
                            (setf (slot-value query 'valid-qbox-entry-p) t))
		    
                          'done))))))))
    
     (unless (bottom-up-component-query-p query)
       (if (is-rule-p query)
           (progn 
             (push query *all-rules*)
             (push query *ready-rules*))
         (progn 
           (push query *all-queries*)
           (push query *ready-queries*))))
    
     (list iterator-id :ready-to-run))))

(defmethod prepare-to-run-rule ((query query))      
  (prepare-to-run-query query))

;;;
;;;
;;;      

(defmethod new-ind-op-p ((parser simple-parser) op)
  (and (consp op)
       (member (to-keyword (first op))
               '(:new-ind :new-individual :individual :create-individual
                 :new-symbol :new-atom :new-concept))
       (symbolp (second op))
       (every #'(lambda (x) 
                  (voi-p parser x))
              (cddr op))))

;;;
;;;
;;;

(defmethod semantically-rewrite-query ((query query) (parser simple-parser) 
                                       &rest args &key &allow-other-keys)
  (declare (ignorable args))

  query)

;;;
;;;
;;;


(defmethod register-kb-id ((query query))
  t)

(defmethod register-kb-id ((query nrql-query))
  (with-slots (substrate) query
    (setf (slot-value query 'kb-id) 
          (get-kb-id substrate))))

;;;
;;;
;;;

(defmethod get-kb-id ((substrate dl-prover-substrate))
  (list (get-tbox-id substrate)
        (get-abox-id substrate)))

(defmethod get-kb-id ((substrate tbox-mirror-substrate))
  (when (slot-boundp substrate 'mirror-of-tbox)
    (list (get-tbox-id substrate)
          nil)))

;;;
;;;
;;;

(defmethod get-abox-id ((substrate racer-substrate))
  (get-abox-version (abox substrate)))

#+:midelora 
(defmethod get-abox-id ((substrate midelora-substrate))
  (prover::get-abox-version (abox substrate)))

;;;
;;;
;;;


(defmethod get-tbox-id ((substrate racer-substrate))
  (get-tbox-version (tbox substrate)))

(defmethod get-tbox-id ((substrate racer-tbox-mirror-substrate))
  (when (slot-boundp substrate 'mirror-of-tbox)
    (get-tbox-version (mirror-of-tbox substrate))))

#+:midelora 
(defmethod get-tbox-id ((substrate midelora-substrate))
  (prover::get-tbox-version (tbox substrate)))

#+:midelora
(defmethod get-tbox-id ((substrate midelora-tbox-mirror-substrate))
  (prover::get-tbox-version (mirror-of-tbox substrate)))
      
;;;
;;;
;;;

(defmethod prepare-query :around ((query t) (result-vois list) (substrate substrate) &rest args
                                  &key
                                  parser 
                                  (parser-class (get-parser-class-for-substrate substrate)) 
                                  &allow-other-keys)
  (with-critical-section
    (let* ((*ano-counter* 0)
           (parser (or parser 
                       (apply #'make-instance parser-class
                              :original-description (list result-vois query args)
                              :substrate substrate
                              :allow-other-keys t
                              args)))
           (inds nil)
           (result-vois
            (mapcar #'(lambda (x)                       
                        (if (ind-p parser x)
                            (progn 
                              (push x inds)
                              (get-var-for-ind parser x))
                          x))
                    result-vois)))

      (apply #'call-next-method 
             query
             result-vois
             substrate
             :parser parser
             :additional-head-inds inds
             args))))


(defmethod prepare-query :around ((query t) (result-vois list) (substrate racer-dummy-substrate) &rest args
                                  &key
                                  parser 
                                  (parser-class (get-parser-class-for-substrate substrate)) rule-p 
                                  &allow-other-keys)
  (with-critical-section

    (if rule-p 
        (call-next-method)
      (let* ((*ano-counter* 0)
             (parser (or parser 
                         (apply #'make-instance parser-class
                                :original-description (list result-vois query args)
                                :substrate substrate
                                :allow-other-keys t 
                                args)))
             (inds nil)

             (head (mapcar #'(lambda (x)                       
				 
                               (when (ind-p parser x)
                                 (push x inds))
				 
                               (cond ((is-attribute-abox-projector-thing-p parser x)
                                      (let ((attrib (convert-to-dl-prover-attribute-expression substrate
                                                                                               (first x))))
                                        `(,attrib
                                          ,(if (ind-p parser (second x))
                                               (progn
                                                 (push (second x) inds)
                                                 (get-var-for-ind parser (second x)))
                                             (second x)))))
				       
                                     ((is-told-value-abox-projector-thing-p parser x)
                                      (let ((attrib (convert-to-dl-prover-attribute-expression substrate
                                                                                               (first (second x)))))
                                        `(,(first x)
                                          (,attrib
                                           ,(if (ind-p parser (second (second x)))
                                                (progn 
                                                  (push (second (second x)) inds)
                                                  (get-var-for-ind parser (second (second x))))
                                              (second (second x)))))))

                                     ((voi-p parser x) 
                                      x)
				       
                                     (t (parser-error 
                                         (format nil "Bad head entry ~A" x)))))
			     
                           result-vois))
             (body #| (remove nil
                              `(and ,query
                                    ,@(when inds
                                        (mapcar #'(lambda (ind)
                                                    `(:same-as ,(get-var-for-ind parser ind) ,ind))
                                                inds))))
	                            ;;; aendert die Semantik!!!
				    |# 
		     
		     query))
	  
        (apply #'call-next-method 
               body 
               head
               substrate
               :additional-head-inds inds
               :parser parser
               args)))))

(defmethod prepare-query :around ((query t) (result-vois list) (substrate tbox-mirror-substrate) &rest args
                                  &key parser (parser-class (get-parser-class-for-substrate substrate)) &allow-other-keys)
  (with-critical-section
    (let ((*racer-check-if-atoms-defined-p* nil))    
      (let* ((*ano-counter* 0)
             (parser (or parser 
                         (apply #'make-instance parser-class 
                                :original-description (list result-vois query args)                           
                                :substrate substrate 
                                :allow-other-keys t 
                                args))))

        (dolist (voi result-vois)
          (unless (or (substrate-thing-p parser voi)
                      (abox-thing-p parser voi))
            (parser-error 
             (format nil "Bad head entry ~A" voi))))
	  
        (apply #'call-next-method 
               query
               result-vois
               substrate
               :parser parser
               args)))))

;;;
;;;
;;;       

(defmethod reprepare-query ((query query))
  (with-critical-section

    (delete-query query)
    (delete-rule query)
    
    (when (substrate-needs-reset-p (substrate query))
      (reset-substrate (substrate query)))

    (if (tbox-has-changed-since-parsing-p query)
       
        (if (is-rule-p query)
            (progn
              (warn-tbox-has-changed)
              (let* ((orig (original-description (parser query)))
                     (rule2
                      (apply #'prepare-rule 
                             (second orig) 
                             (first orig) 
                             (substrate query)
                             (cons :id
                                   (cons (iterator-id query)
                                         (third orig))))))
                (values (prepare-to-run-rule rule2)
                        rule2)))

          (progn
            (warn-tbox-has-changed)
            (let* ((orig (original-description (parser query)))
                   (query2
                    (apply #'prepare-query 
                           (second orig) 
                           (first orig) 
                           (substrate query)
                           (cons :id
                                 (cons (iterator-id query)
                                       (third orig))))))
              (values (prepare-to-run-query query2)
                      query2))))
      
      (progn
        (dolist (q (cons query (all-subqueries query)))
          (dolist (slot '(bindings-queue 
                          last-queue-item get-next-tuple-p
                          new-abox-assertions
                          abox-assertions-to-add
                          result-bindings last-result-bindings-item
                          kb-changed-token-delivered-p))
            (setf (slot-value q slot) nil)))
	
        (register-kb-id query)

        (values (prepare-to-run-query query)
                query)))))

;;;
;;;
;;;

(defmethod reprepare-rule ((query query))  
  (with-critical-section
    (reprepare-query query)))


(defun process-rule-con-pattern (rule-con-pattern parser &key replace-p)
  (let ((result-vois nil)
        (new-ind-ops nil))

    (labels ((process (var x &optional no-new-ind-p cd-p)

               (cond ((voi-p parser var)   

                      (if replace-p
                          (find-voi parser var)
                        
                        (progn
                          (pushnew var result-vois)
                          var)))

                     ((and (not no-new-ind-p)
                           (new-ind-op-p parser var))

                      (let* ((inds (mapcar #'(lambda (var )
                                               (process var x no-new-ind-p cd-p))
                                           (cddr var)))
                             (var `(:new-ind ,(second var)
                                    ,@inds)))
                         
                        (pushnew var new-ind-ops :test #'equal)
                        var))

                     ((and cd-p 
                           (is-attribute-abox-projector-thing-p parser var :told-value-p t)
                           ;; :told-value-p t wichtig!
                           )
                      `(,(first var) 
                        ,(process (second var) x no-new-ind-p cd-p)))

                     ((and cd-p 
                           (is-told-value-abox-projector-thing-p parser var))
                      `(:told-value
                        ,(process (second var) x no-new-ind-p cd-p)))
                      
                     ((or (numberp var) 
                          (stringp var))

                      var)

                     ((and (consp var) 
                           (not (cdddr var)))

                      ;; z.B. (+ (told-value (age ?x)) 30) 
                      
                      `(,(first var)
                        ,(process (second var) x no-new-ind-p cd-p)
                        ,(process (third var) x no-new-ind-p cd-p)))

                     (t (parser-error 
                         (format nil "Bad rule pattern entry ~A" x))))))
      (let ((res

             (mapcar #'(lambda (x)     
                         (let ((op
                                (intern (format nil "~A" (first x)) 
                                        #-:midelora :racer
                                        #+:midelora :prover)))
                           (case (to-keyword op)
                              
                             ((:instance)
                              (let ((var (second x))
                                    (concept (third x)))
                                 
                                `(,op ,(process var x)
                                      ,(if (new-ind-op-p parser concept)
					   `(:new-symbol 
                                             ,(second concept)
                                             ,@(let* ((inds (mapcar #'(lambda (var)
                                                                        (process var x t))
                                                                    (cddr concept))))
                                                 inds))
					 concept))))

                             ((:forget-concept-assertion)
                              (let ((var (second x))
                                    (concept (third x)))
                                 
                                `(,op ,(process var x t)
                                      ,concept)))
                              
                             ((:related)
                              (let ((from (second x))
                                    (to (third x))
                                    (role (fourth x)))
                                `(,op
                                  ,(process from x)
                                  ,(process   to x)
                                  ,role)))

                             ((:forget-role-assertion)
                              (let ((from (second x))
                                    (to (third x))
                                    (attrib (fourth x)))
                                `(,op
                                  ,(process from x t)
                                  ,(process   to x t)
                                  ,attrib)))

                             #-:midelora 
                             ((:constrained)
                              (let ((ind (second x))
                                    (object (third x))
                                    (attrib (fourth x)))
                                `(,op
                                  ,(process ind x)
                                  ,(process object x)
                                  ,attrib)))

                             #-:midelora 
                             ((:forget-constrained-assertion)
                              (let ((ind (second x))
                                    (object (third x))
                                    (attrib (fourth x)))
                                `(,op
                                  ,(process ind x t)
                                  ,(process object x t)
                                  ,attrib)))

                             #-:midelora 
                             ((:forget-constraint)
                              x)

                             #-:midelora 
                             ((:constraints constraint)
                              `(,op
                                ,@(mapcar #'(lambda (constraint) 
                                              (list (first constraint)
                                                    (process (second constraint) constraint nil t)
                                                    (process  (third constraint) constraint nil t)))
                                          (rest x))))

                             (otherwise 
                              (parser-error 
                               (format nil "Bad rule pattern entry ~A" x))))))

                     rule-con-pattern)))

        (values res result-vois new-ind-ops)))))


(defmethod prepare-rule ((query t) (rule-con-pattern list) (substrate dl-prover-substrate) &rest args
                         &key parser (parser-class (get-parser-class-for-substrate substrate)) &allow-other-keys)
  
  (with-critical-section
      
    (let* ((parser (or parser
                       (apply #'make-instance parser-class
                              :substrate substrate 
                              :allow-other-keys t 
                              args))))

      (multiple-value-bind (rule-con-pattern result-vois new-ind-ops)
          (process-rule-con-pattern rule-con-pattern parser)                                   

        (setf (slot-value parser 'original-description)
              (list rule-con-pattern query args))
	
        (unless rule-con-pattern
          (nrql-error "Parser error: no rule consequence given!"))

        (apply #'prepare-query
               query result-vois 
               substrate
               :parser parser
               :rule-p t
               :new-ind-ops new-ind-ops
               :rule-con-pattern rule-con-pattern
               args)))))

;;;
;;;
;;;

(defmethod prepare-query ((query t) (result-vois list) (substrate substrate) &rest args
                          &key
			  
                          (parser-class (get-parser-class-for-substrate substrate))

                          parser
                          
                          (bind-specials-p t)

                          &allow-other-keys)

  (when bind-specials-p
    (setf *last-query* nil))

  (let* ((parser (or parser 
                     (apply #'make-instance parser-class
                            :original-description (list result-vois query args)
                            :substrate substrate
                            :allow-other-keys t
                            args)))

         (p-vois (get-vois-from-answer-pattern result-vois parser))
	     
         (query1 `(project-to ,p-vois ,query)) 

         ;; die oberste Projektion entspricht genau den result-vois

         (query2 (apply #'syntactically-rewrite-query query1 parser 
                        args))

         (query3 
          (if (projection-operator-p parser query2)
              query2
            `(project-to ,p-vois ,query2))))

    (apply #'prepare-query-bunch
           (get-toplevel-queries parser query3)
           :parser parser
           :result-vois result-vois
           :original-query query3
           args)))


(defun prepare-query-bunch (query-bunch &rest args
                                        &key
			    
                                        (bind-specials-p t)
			    
                                        original-query 

                                        rule-con-pattern
                                        new-ind-ops 

                                        premise
			    
                                        (generate-code-p *generate-code-p*)
			    
                                        (optimize-p *optimize-p*)
			    
                                        (rewrite-semantically-p *rewrite-semantically-p*)
			    
                                        (rewrite-to-dnf-p *rewrite-to-dnf-p*)
			    
                                        (report-inconsistent-queries-p *report-inconsistent-queries-p*)

                                        (report-tautological-queries-p *report-tautological-queries-p*)
			    
                                        (use-repository-p *use-repository-p*)
			    
                                        (put-into-repository-p *put-into-repository-p*)
			    
                                        id 

                                        parser

                                        result-vois

                                        &allow-other-keys)

  (unless parser
    (error "!"))

  (when (cdr query-bunch)
    (setf (query-hash parser)
          (make-hash-table :size 10 
                           :rehash-size 10)))

  (let* ((toplevel-query 
          (first (last query-bunch)))
         (queries
          (mapcar #'(lambda (oquery)

                      (let* ((query-name 
                              (first oquery))
			       
                             (result-vois  
                              (second oquery))
			       
                             (query (third oquery))

                             (query0 
                              (if rewrite-to-dnf-p
                                  (get-dnf-query query)
                                query))
			       
                             (query (apply #'parse-query
                                           query0 
                                           parser
					;:original-query query
                                           args))

                             ;;; es macht Sinn, die BUQ-Query-Komponennten
                             ;;; individuell semantisch umzuschreiben (bzw.
                             ;;; den Query Realizer darauf laufen zu lassen, 
                             ;;; denn die werden ja individuell als Queries
                             ;;; ablaufen!
                             ;;; das gleiche gilt fuer die Berechnung von 
                             ;;; Cache-Referenzen

                             (query 
                              (if rewrite-semantically-p
				    
                                  (apply #'semantically-rewrite-query query parser 
                                         :combine-disjunctive-atoms-p nil
                                         :combine-conjunctive-atoms-p t
                                         :reasoning-p t
                                         args)

                                query)))

                        (setf (slot-value query 'original-query)
                              query0)
			  
                        (when (query-hash parser)
                          (setf (gethash query-name
                                         (query-hash parser))
                                query))

                        (register-answer-pattern query result-vois)
			  
                        ;;;
                        ;;; Optimierung
                        ;;;

                        ;;; wichtig: optimize-query darf nicht nach register-answer-pattern
                        ;;; gerufen werden, da register-answer-pattern-p (result-voi-p voi) -> T setzt
                        ;;; fuer die result-vois 
			  
                        (when optimize-p
                          (unless (in-dnf-p query)
                            (nrql-error "Optimizer error: Optimizer works only for queries in DNF! Enable DNF!"))      
                          (apply #'optimize-query query args)
			    
                          ;;; kann ebenfalls nur fuer Queries in DNF funktionieren! 
			    
                          (compute-existential-variables query))
                         
                        (mark-last-conjuncts-of-ands query)

                        ;;;
                        ;;; Code Generation
                        ;;;
			  
                        (when generate-code-p       
			    
                          ;;; 
                          ;;; dieses Flag wird von Callern gesetzt
                          ;;; wenn nicht beabsichtigt ist, dass die
                          ;;; Query auch wirklich ausgefuehrt wird
                          ;;; 

                          (setf (slot-value query 'use-repository-p) use-repository-p
                                (slot-value query 'put-into-repository-p) put-into-repository-p)

                          (when (and (not *runtime-evaluation-p*)
                                     (eq oquery toplevel-query)
                                     use-repository-p)

                            #+:racer-server (when *server-timeout* 
                                              (nrql-error "Bad *server-timeout* detected!"))
			      
                            ;;;
                            ;;; wenn compiliert wird, muss 
                            ;;; ich schon hier die Cache-Referenzen
                            ;;; brechnen, da sonst der generierte 
                            ;;; Code das nicht beruecksichtigt!
                            ;;; fuer *runtime-evaluation-p* 
                            ;;; werden die Cache-Referenzen erst direkt
                            ;;; in prepare-to-run-query berechnet
                            ;;;

                            (prepare-substrate-for-query-execution 
                             (substrate query) query)
                      
                            (compute-cache-references query put-into-repository-p))

                          ;;; (setf *x* query)
			    
                          (if *runtime-evaluation-p* 
                              (setf (slot-value query 'query-fn)
                                    #'(lambda ()
                                        (evaluate-query query)))
                            (apply #'compile-query query args)))

                        query))

                  query-bunch)))

    ;; (setf *x* queries)  (setf *y* query-bunch) (break)
      
    (let* ((toplevel-query (first (last queries)))

           (id 
            (cond (id
                   (if (or (if rule-con-pattern
                             (find-rule id)
                             (find-query id))
                           (string-equal (subseq (symbol-name id) 0 (min 5 (length (symbol-name id)))) "QUERY"))
                       (nrql-error "~A ~A already exists! Use a different query ID." (if rule-con-pattern "Rule" "Query") id)
                     id))
                  (t
                   (get-query-iterator-id toplevel-query))))

           (tq-for-reasoning 
            (when (or report-inconsistent-queries-p
                      report-tautological-queries-p)
              (apply #'parse-query
                     (get-dnf-query 
                      (apply #'syntactically-rewrite-query
                             (remove-projections 
                              parser
                              original-query)
                             parser 
                             args))
                     parser
                     args))))

      (setf (slot-value toplevel-query 'iterator-id) id)
      (setf (slot-value toplevel-query 'original-query)
            original-query)

      ;;;
      ;;; Reasoning
      ;;; 
	
      (when report-inconsistent-queries-p
        (when (query-inconsistent-p tq-for-reasoning
                                    :rule-con-pattern
                                    rule-con-pattern)
	    
          (setf (slot-value toplevel-query 'query-satisfiable) nil)
          (warn-inconsistent toplevel-query)))
	
      (when (and report-tautological-queries-p 
                 (query-tautological-p tq-for-reasoning))

        (setf (slot-value toplevel-query 'query-tautological) t)
        (warn-tautological toplevel-query))

      ;;; 
      ;;; KB-Ids setzen
      ;;; Achtung: da prepare-query in with-critical-section ausgefuehrt wird
      ;;; (s. around-Methoden!), kann sich TBox/ABox version waehrend des
      ;;; Parsens nicht geaendert haben! 
      ;;; 
	
      ;;; im wesentlichen muss die TBox-Id gespeichert werden - 
      ;;; wenn sich z.B. eine Rollendefinition aendert, muss
      ;;; die Query bei Ausfuehrung reprepared werden
      ;;; 

      (register-kb-id toplevel-query)

      (register-answer-pattern toplevel-query result-vois)         
	
      ;;;
      ;;; Regel-Pattern / Premisse etc. registrieren
      ;;;
	
      (when (typep toplevel-query 'nrql-query)
        
        (setf (slot-value toplevel-query 'rule-con-pattern) 
              ;; hier werden lediglich die VOI-Symbole gegen VOI-Objekte ersetzt
              (process-rule-con-pattern rule-con-pattern parser 
                                        :replace-p t))
                                        
        (setf (slot-value toplevel-query 'new-ind-ops) new-ind-ops)
        (setf (slot-value toplevel-query 'premise) premise))

      ;;;
      ;;; Bottum-Up Evaluation Plan registrieren
      ;;;

      (when (cdr queries)
        (setf (slot-value toplevel-query 'bottom-up-evaluation-plan)
              (butlast queries)))

      (let ((count 0))
        (dolist (buq (butlast queries))
          (incf count)
          ;; f. (not (project-to ...)) -> Bindungen muessen aufbewahrt werden
          (setf (slot-value buq 'bottom-up-component-query-p) t
                (slot-value buq 'iterator-id) 
                (to-keyword (format nil "~A-ANO-BUQ-~A" id count)))))
	
      (when bind-specials-p
        (setf *last-query* 
              toplevel-query))

      toplevel-query)))

;;;
;;;
;;;      

(defmethod mark-last-conjuncts-of-ands ((query query))
  (dolist (and-query (remove-if-not #'is-and-query-p (cons query (all-subqueries query))))
    (setf (slot-value (first (last (subqueries and-query))) 'last-conjunct-p) t)))

;;;
;;;
;;;             

(defmethod get-vois-from-answer-pattern ((pattern list) (parser simple-parser))
  (loop as voi in pattern 
        collect
        (cond ((is-attribute-abox-projector-thing-p parser voi)
               (second voi))

              ((is-told-value-abox-projector-thing-p parser voi)
               (second (second voi)))

              ((or (abox-thing-p parser voi)                            
                   (substrate-thing-p parser voi))
               voi)

              (t 
               (parser-error (format nil "Bad head entry ~A" voi))))))

;;;
;;;
;;;             

(defmethod register-answer-pattern ((query query) (result-vois list))
  (let ((parser (parser query))
        (vois nil))
    (setf (slot-value query 'answer-pattern)
          (mapcar #'(lambda (voi) 
                      (cond #-:midelora 
                            ((is-attribute-abox-projector-thing-p parser voi)

                             (let ((x (find-voi parser (second voi))))

                               (unless x
                                 (parser-error 
                                  (format nil "Bad head entry ~A" voi)))
			   
                               (setf (result-voi-p x) t)     

                               (push x vois)
			   
                               (list (convert-to-dl-prover-attribute-expression (substrate parser)
                                                                                (first voi))
                                     x)))

                            #-:midelora 
                            ((is-told-value-abox-projector-thing-p parser voi)

                             (let ((x (find-voi parser (second (second voi)))))

                               (unless x
                                 (parser-error 
                                  (format nil "Bad head entry ~A" voi)))

                               (setf (result-voi-p x) t)         
			   
                               (push x vois)
			   
                               (list :told-value
                                     (list (convert-to-dl-prover-attribute-expression (substrate parser)
                                                                                      (first (second voi)))
                                           x))))

                            ((or (abox-thing-p parser voi)                            
                                 (substrate-thing-p parser voi))

                             (let ((x (find-voi parser voi)))

                               (unless x
                                 (parser-error 
                                  (format nil "Bad head entry ~A" voi)))
			   
                               (setf (result-voi-p x) t)

                               (push x vois)
			   
                               x))

                            (t 
			 
                             (parser-error (format nil "Bad head entry ~A" voi)))))

                  result-vois))

    (setf (slot-value query 'result-vois)
          (sort-vois vois))))

;;;
;;; existential Variables sollen solche sein, fuer die ich nur
;;; eine Bindungsmoeglichkeit enummerieren muss...
;;; das nur fuer Variablen, die $?x sind, und nicht in Role 
;;; Chains in einer Konjunktion auftauchen, etc.
;;; Hier ginge es: (project-to (?y) (and ($?x) (?y ?z r) (?z ?u s)))
;;; $?x, ?z, ?u sind existentiell... 
;;;

(defmethod compute-existential-variables ((query true-query))
  t)

(defmethod compute-existential-variables ((query false-query))
  t)

(defmethod compute-existential-variables ((query or-query))
  (dolist (subquery (subqueries query))
    (compute-existential-variables subquery)))


(defmethod compute-existential-variables ((query atomic-query))
  (with-slots (existential-vois) query
    (case (current-role query)
      ((:enumerator 
        :from-is-bound-enumerator
        :to-is-bound-enumerator)
     
       (dolist (voi (get-ALL-vois query))
         (when (and (not (result-voi-p voi))
                    (=> (corresponding-voi voi)
                        (not (result-voi-p (corresponding-voi voi)))))
           (push voi existential-vois)))))))


(defmethod compute-existential-variables ((query and-query))
  (with-slots (existential-vois) query
    
    (let ((existential-subqueries nil))
      
      (dolist (subquery (subqueries query))
      
        (when (and (is-active-p subquery)
                   (not (is-same-as-query-p subquery)))

          (case (current-role subquery)
            (:tester)
            ((:enumerator
              :from-is-bound-enumerator
              :to-is-bound-enumerator)

             (dolist (voi (get-all-vois subquery))
               (when (and (not (result-voi-p voi))
                          (=> (corresponding-voi voi)
                              (not (result-voi-p (corresponding-voi voi)))))

                 (let ((using-queries 
                        (remove-if-not #'is-active-p
                                       (remove subquery
                                               (intersection (remove nil 
                                                                     (append (used-by voi)
                                                                             (when (corresponding-voi voi)
                                                                               (used-by (corresponding-voi voi)))))
                                                             (subqueries query))))))
                 
                   ;; (break "~A ~A" voi using-queries)
                 
                   (unless using-queries
               
                     ;;; kein anderes Atom im selben And benutzt das Voi! 
                     ;;; -> existentiell
               
                     (push voi existential-vois)

                     (push voi (existential-vois subquery))

                     (push subquery existential-subqueries)))))))))

      ;;; reordering required!!!
      ;;; sonst kann man die Optimierung nicht ausnutzen.
      ;;; Bsp.: (retrieve (?x) (and (?x woman) (?y woman)))
      ;;; evtl. erzeugt der Optimierer den Plan 
      ;;; ((?y woman) (?x woman))
      ;;; nun wird (?y woman) als existentiell (= nur ein Kandidate wird gesucht!)
      ;;; markiert. Da ?y nun nur auf eine Art und Weise gebunden wird,
      ;;; fehlen der entsp. Kandidate fuer (?x woman)! 
      ;;; Loesung: die existentiellen Konjunkte muessen ans Ende des Planes,
      ;;; sonst werden die "vollen enumeratoren" behindert.
      
      (dolist (esq existential-subqueries)
        (setf (slot-value query 'subqueries)
              (delete esq (slot-value query 'subqueries))))

      (setf (slot-value query 'subqueries)
            (append (subqueries query)
                    existential-subqueries)))))

;;;
;;;
;;;

(defmethod get-toplevel-queries ((parser simple-parser) (query symbol))
  query)

(defmethod get-toplevel-queries ((parser simple-parser) (query list))
  (let ((queries nil)
        (counter 0))
    
    (labels ((do-it (query)
               (cond ((projection-operator-p parser query)
                      (let* ((res (do-it (third query)))
                             (name (to-keyword
                                    (format nil "ANO-QUERY-~A" (incf counter))))
                             (new-query 
                              `( ,name
                                 ,(second query)
                                 ,res))
                             (ref-query 
                              `(:bindings-from
                                ,name
                                ,(second query))))

                        (push new-query queries)

                        ref-query))
                     
                     ((complex-query-p parser query)
                      `(,(first query)
                        ,@(mapcar #'do-it (rest query))))

                     ((not-query-p parser query)
                      `(not 
                        ,(do-it (second query))))
                     
                     ((inv-query-p parser query)
                      `(inv 
                        ,(do-it (second query))))
                     
                     (t query))))
      
      (do-it query)

      (reverse queries))))

;;;
;;;
;;;

(defmethod remove-projections ((parser simple-parser) (query symbol))
  query)

(defmethod remove-projections ((parser simple-parser) (query list))
  (labels ((do-it (query)
             (cond ((projection-operator-p parser query)
                    (do-it (third query)))
		   
                   ((complex-query-p parser query)
                    `(,(first query)
                      ,@(mapcar #'do-it (rest query))))

                   ((not-query-p parser query)
                    `(not 
                      ,(do-it (second query))))
		   
                   ((inv-query-p parser query)
                    `(inv 
                      ,(do-it (second query))))
		   
                   (t query))))
    
    (do-it query)))



