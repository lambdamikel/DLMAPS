;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(defvar *prover-main-start-time* nil)

(defvar *prover-init-start-time* nil)

(defmacro announce (message &rest args)
  `(progn 
     (when (or *debug-p* *announce-p*)
       (terpri)
       (format t ,message ,@args))
     t))

(defmacro announce1 (message &rest args)
  `(progn 
     (terpri)
     (format t ,message ,@args)
     t))

(defmacro defrule (expansion-type (language-type abox-type &key inherit-from args) &body body)    
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (progn 

       (defvar1 ,(intern (format nil "*TIME-SPEND-IN-~A*" expansion-type)) 0)

       (defmethod ,(intern (string-upcase (format nil "get-code-for-~A" expansion-type)))                
                  ((language ,language-type) (abox ,abox-type) 
                   &rest args
                   &key positive-code negative-code after-successful-clash-repair-code body-code
                   ,@args
                   &allow-other-keys)

         (declare (ignorable positive-code negative-code after-successful-clash-repair-code body-code
                             ,@args))

       
         ,(if inherit-from

              `(apply              
                (symbol-function ',(intern (string-upcase (format nil "get-code-for-~A" expansion-type))))
                (make-language ',inherit-from)
                (make-instance ',abox-type :dont-initialize t)
                args)
 
            `(let* ((code 
                     (tree-map #'(lambda (item)
                                   (case item 
                                     ;; ,@(mapcar #'(lambda (x) 
                                     ;;              `(,x (or ,x ',x)))
                                     ;;          args)
                                     (+insert-positive-code+ `(progn 
                                                                (incf ,(intern (format nil "*TIME-SPEND-IN-~A*" ',expansion-type))
                                                                      (- (get-internal-run-time) *start-time*))
                                                                ,@(or positive-code (list t))))
                                     (+insert-negative-code+ `(progn 
                                                                (incf ,(intern (format nil "*TIME-SPEND-IN-~A*" ',expansion-type))
                                                                      (- (get-internal-run-time) *start-time*))
                                                                ,@(or negative-code (list t))))
                                     (+insert-after-successful-clash-repair-code+ 
                                      `(progn ,@after-successful-clash-repair-code))
                                     (+insert-body-code+ `(progn 
                                                            (incf ,(intern (format nil "*TIME-SPEND-IN-~A*" ',expansion-type))
                                                                  (- (get-internal-run-time) *start-time*))
                                                            ,@(or body-code (list t))))
                                     (otherwise item)))

                               ',`(let ((*start-time* (get-internal-run-time)))
                                    (declare (ignorable *start-time*))
                                    ,@body)
             
                             ; ',(cons 'progn body)
                               ))

                    (let-list (list ,@(mapcar #'(lambda (x) 
                                                  `(list ',x ,x))
                                              args))))
                     

               `(progn 
                  (announce "~%>>> Now performing ~A (Level ~A)~%" ',',expansion-type level)
                
                  (let ,let-list

                    (declare (ignorable ,@(mapcar #'first let-list)))

                    ,code)))))

       ;;; 2. Method f. (perform! ...) 

       (defmethod ,(intern (string-upcase (format nil "get-code-for-~A!" expansion-type)))                
                  ((language ,language-type) (abox ,abox-type) 
                   &rest args
                   &key positive-code negative-code after-successful-clash-repair-code body-code
                   ,@args
                   &allow-other-keys)

         (declare (ignorable positive-code negative-code after-successful-clash-repair-code body-code
                             ,@args))
       
         ,(if inherit-from

              `(apply              
                (symbol-function ',(intern (string-upcase (format nil "get-code-for-~A!" expansion-type))))
                (make-language ',inherit-from)
                (make-instance ',abox-type :dont-initialize t)
                args)
 
            `(let* ((code 
                     (tree-map #'(lambda (item)
                                   (case item 
                                     ,@(mapcar #'(lambda (x) 
                                                   `(,x (or ,x ',x)))
                                               args)
                                     (+insert-positive-code+ `(progn 
                                                                (incf ,(intern (format nil "*TIME-SPEND-IN-~A*" ',expansion-type))
                                                                      (- (get-internal-run-time) *start-time*))
                                                                ,@(or positive-code (list t))))
                                     (+insert-negative-code+ `(progn 
                                                                (incf ,(intern (format nil "*TIME-SPEND-IN-~A*" ',expansion-type))
                                                                      (- (get-internal-run-time) *start-time*))
                                                                ,@(or negative-code (list t))))
                                     (+insert-after-successful-clash-repair-code+ 
                                      `(progn ,@after-successful-clash-repair-code))
                                     (+insert-body-code+ `(progn 
                                                            (incf ,(intern (format nil "*TIME-SPEND-IN-~A*" ',expansion-type))
                                                                  (- (get-internal-run-time) *start-time*))
                                                            ,@(or body-code (list t))))
                                     (otherwise item)))
                               ',`(let ((*start-time* (get-internal-run-time)))
                                    (declare (ignorable *start-time*))
                                    ,@body))))
             
               `(progn 
                  (announce "~%>>> Now performing ~A (Level ~A)~%" ',',expansion-type level)
                
                  ,code)))))))


#|

(defun show-incarnation-info (type)
  
  (format t "~%~%***** ~A OF ~A : ~A, TBOX ~A
LANGUAGE:           ~A
NODES:              ~A
EDGES:              ~A
----------------------
BLOCKING ENABLED:   ~A
DYNAMIC BLOCKING:   ~A
----------------------
COMBINED-SOME-ALL:  ~A 
DELETE NODES:       ~A
REUSE NODES:        ~A
ALL COMPLETIONS:    ~A
MAINTAIN STATE:     ~A
KEEP DET. ASSERT.:  ~A
----------------------
MODEL CACHING:      ~A
SUBTABLEAU CACHING: ~A
USE CACHED MODELS:  ~A
USE UNSAT CACHE:    ~A
----------------------
COMPUTE CORE MODEL: ~A
STORE IND MODELS:   ~A
STORE RETR.MODEL:   ~A
----------------------
SEMANTIC BRANCHING: ~A
NON DETERMINISM:    ~A
----------------------
TBOX:               ~A
STORE:              ~A
META CONSTRAINTS:   ~A~%~%"

          type
          *cur-abox* (type-of *cur-abox*) (tbox *cur-abox*) 


          (type-of *language*)
          (no-of-nodes *cur-abox*)
          (no-of-edges *cur-abox*)
			       
          *blocking-enabled-p*
          *dynamic-blocking-p* 

          *combined-some-all-rule-p*
          *delete-nodes-p*
          *reuse-nodes-p*
          *compute-all-completions-p*
          *maintain-prover-state-p*
          *keep-det-assertions-p*
			       
          *cache-models-p*
          *subtableau-caching-p*
          *use-cached-models-p*
          *use-unsat-cache-p*
			       
          *compute-core-model-p*
          *store-ind-models-p*
          *store-instance-retrieval-model-p*
			       
          *semantic-branching-p*
          *non-determinism-p*
                               
          *cur-tbox*
          *cur-store*
          *meta-constraints*))


|#



(defun show-incarnation-info (type)
  
  (format t "~%~%***** ~A OF ~A : ~A, TBOX ~A
LANGUAGE:           ~A
NODES:              ~A
EDGES:              ~A
-------------------------------------------------------------"
          type
          *cur-abox* (type-of *cur-abox*) (tbox *cur-abox*) 


          (type-of *language*)
          (no-of-nodes *cur-abox*)
          (no-of-edges *cur-abox*))

  (dolist (slot *prover-specials*)
    (when (and (boundp slot)
               (or (member (symbol-value slot) '(t nil))
                   (member slot '(*strategy*))
                   (numberp (symbol-value slot))))
      (format t "~%~57,A : ~A" 
              slot
              (symbol-value slot)))))


(defmacro define-prover ( ( (prover-name language-type abox-type) &body keylist) &body body)

  (when (and keylist (not (eq (first keylist) '&key)))
    (error "Bad syntax! ~A" keylist))

  (let* ((init-code
          (rest (assoc :init body)))
         (rollback-code
          (rest (assoc :rollback body)))
         (main-code
          (rest (assoc :main body)))
         (success-code
          (rest (assoc :success body)))
         (keys (remove :init
                       (remove :main
                               (remove :rollback
                                       (remove :success
                                               (mapcar #'first (mapcar #'ensure-list (rest keylist)))))))))

    
    (setf *language-dispatcher* (make-language language-type)
          *abox-dispatcher*     (make-instance abox-type :dont-initialize t))

    `(progn 
       
       ,@(when init-code 
           `(,@(unless (member (first init-code) '(:copy :inherit-from :call-next-method))
                 `((defmethod get-init-code ((name (eql (quote ,prover-name))) (language ,language-type)
                                             (abox ,abox-type))
                     (quote ,init-code))))

             ,@(when (eq (first init-code) :call-next-method)
                 `((defmethod get-init-code ((name (eql (quote ,prover-name))) (language ,language-type)
                                             (abox ,abox-type)) 
                     (call-next-method))))

             (defvar1 ,(intern (format nil "*TIME-SPEND-IN-PROVER-INIT-~A-~A*" 
                                       prover-name language-type)) 
                      0)


             (defvar1 ,(intern (format nil "*TIME-SPEND-IN-PROVER-INIT-BEFORE-MAIN-~A-~A*" 
                                       prover-name language-type)) 
                      0)

             (defvar1 ,(intern (format nil "*TIME-SPEND-IN-PROVER-MAIN-~A-~A*" 
                                       prover-name language-type)) 
                      0)

             ;;;
             ;;; INIT 
             ;;;
             
             (defmethod prover-init ((name (eql (quote ,prover-name))) (language ,language-type) (abox ,abox-type)
                                     &key 
                                     
                                     (meta-constraints nil meta-constraints-specified-p)
                                     (tbox nil tbox-specified-p)

                                     (how-many nil how-many-specified-p)
                                     (debug-p nil debug-specified-p)
                                     (break-p nil break-specified-p)
                                     (visualize-p nil visualize-specified-p)
                                     (completion-found-hook nil completion-found-hook-specified-p)
                                     (reuse-nodes-p nil reuse-nodes-specified-p)
                                     (compute-all-completions-p nil compute-all-completions-specified-p)

                                     (maintain-prover-state-p nil maintain-prover-state-specified-p)
                                     (blocking-enabled-p nil blocking-enabled-specified-p)
                                     
                                     (delete-nodes-p nil delete-nodes-specified-p)
                                     (semantic-branching-p nil semantic-branching-specified-p)
                                     (non-determinism-p nil non-determinism-specified-p)
                                     
				     (use-cached-models-p nil use-cached-models-specified-p)
                                     (use-unsat-cache-p nil use-unsat-cache-specified-p)
                                     (cache-models-p nil cache-models-specified-p)
				     
				     (store-ind-models-p nil store-ind-models-specified-p)
				     (compute-core-model-p nil compute-core-model-specified-p)
                                     (keep-det-assertions-p nil keep-det-assertions-specified-p)

				     (store-instance-retrieval-model-p nil store-instance-retrieval-model-specified-p)

                                     (subtableau-caching-p nil subtableau-caching-specified-p)
                                     
                                     ,@(rest keylist) &allow-other-keys)
               
               (declare (ignorable ,@keys))

               (with-prover-standard-settings 
                
                (let* ((level 'prover-init)
                      
                       (start-time (get-internal-run-time))
                       
                       (rollback-to-action nil)
                       
                       ;;;
                       ;;;
                       ;;;

                       (*dont-invalidate-store-p* t)

                       (*maintain-history-p* t)

                       ;;;
                       ;;;
                       ;;;

                       (*cur-abox* abox)
                       
                       (*cur-tbox* 
                        (or tbox
                            (tbox *cur-abox*)))
                       
                       (*cur-store* 
                        (concept-store *cur-tbox*))
                       
                       (*cur-rbox* (rbox *cur-tbox*))
                         
                       #+:use-membership-tables 
                       (*unexpanded-table* (unexpanded-table *cur-abox*))
                       #+:use-membership-tables 
                       (*expanded-table* (expanded-table *cur-abox*))
                         
                       (*language* language))

                  (declare (ignorable level rollback-to-action))

                  (setf *prover-main-start-time* nil
                        *prover-init-start-time* (get-internal-run-time))

                  (with-concept-store (*cur-store*) 
                    
                                      (let* ((*meta-constraints*
                                              (if meta-constraints-specified-p
                                                  (append meta-constraints 
                                                          (get-meta-constraints *cur-tbox*))
                                                (get-meta-constraints *cur-tbox*)))
                           
                                             (*blocking-enabled-p* 
                                              (when 
                                                  (if blocking-enabled-specified-p
                                                      blocking-enabled-p
                                                    (or *blocking-enabled-p*
                                                        *meta-constraints*
                                                        (needs-blocking-p *cur-tbox*)
                                                        (get-all-transitive-roles *cur-tbox*)))
                                                t))
                       
                                             (*dynamic-blocking-p* 
                                              (and *blocking-enabled-p*
                                                   (is-dl-with-inverse-roles-p *language*)))
                       
                                             (*combined-some-all-rule-p* 
                                              (is-dl-with-combined-some-all-rule-p *language*))
                 
                                             (*propagation-of-transitive-all-concepts-p* 
                                              (and (is-dl-with-transitive-roles-p *language*)
                                                   (not *combined-some-all-rule-p*)
                                                   (get-all-transitive-roles *cur-tbox*)))

                                             ;;;
                                             ;;;
                                             ;;;
                       
                                             (*maintain-prover-state-p* 
                                              (if maintain-prover-state-specified-p 
                                                  maintain-prover-state-p
                                                *maintain-prover-state-p*))
 
                                             (*delete-nodes-p*
                                              (if delete-nodes-specified-p
                                                  delete-nodes-p
                                                *delete-nodes-p*))

                                             (*reuse-nodes-p* 
                                              (or (if reuse-nodes-specified-p 
                                                      reuse-nodes-p
                                                    *reuse-nodes-p*)
                                                  (is-dl-with-rolebox-p language)))
                           
                                             ;;;
                                             ;;;
                                             ;;;
                      
                                             (*semantic-branching-p*
                                              (if semantic-branching-specified-p
                                                  semantic-branching-p
                                                *semantic-branching-p*))
                           
                                             (*non-determinism-p*
                                              (if non-determinism-specified-p
                                                  non-determinism-p 
                                                *non-determinism-p*))
                           
                                             ;;;
                                             ;;;
                                             ;;;

                          
                                             (*use-cached-models-p*
                                              (if use-cached-models-specified-p
                                                  use-cached-models-p
                                                *use-cached-models-p*))
                           
                                             (*use-unsat-cache-p*
                                              (if use-unsat-cache-specified-p
                                                  use-unsat-cache-p
                                                *use-unsat-cache-p*))
                           
                                             (*cache-models-p*
                                              (if cache-models-specified-p
                                                  cache-models-p
                                                *cache-models-p*))

                                             (*subtableau-caching-p*
                                              (if subtableau-caching-specified-p
                                                  subtableau-caching-p 
                                                *subtableau-caching-p*))

                                             (*store-ind-models-p*
                                              (if store-ind-models-specified-p
                                                  store-ind-models-p
                                                *store-ind-models-p*))

                                             (*store-instance-retrieval-model-p*
                                              (if store-instance-retrieval-model-specified-p 
                                                  store-instance-retrieval-model-p
                                                *store-instance-retrieval-model-p*))

                                             (*compute-core-model-p* 
                                              (or (if compute-core-model-specified-p
                                                      compute-core-model-p
                                                    *compute-core-model-p*)
                                                  *store-instance-retrieval-model-p*))

                                             (*keep-det-assertions-p*
                                              (or *compute-core-model-p*
                                                  (if keep-det-assertions-specified-p
                                                      keep-det-assertions-p
                                                    *keep-det-assertions-p*)))

                           
                                             ;;;
                                             ;;;
                                             ;;;
                      
                                             (*how-many* 
                                              (if how-many-specified-p
                                                  how-many
                                                *how-many*))
                      
                                             (*debug-p* 
                                              (if debug-specified-p
                                                  debug-p
                                                *debug-p*))
                      
                                             (*break-p* 
                                              (if break-specified-p
                                                  break-p
                                                *break-p*))
                      
                                             (*visualize-p* 
                                              (if visualize-specified-p
                                                  visualize-p
                                                *visualize-p*))

                                             (*compute-all-completions-p* 
                                              (if compute-all-completions-specified-p
                                                  compute-all-completions-p
                                                *compute-all-completions-p*))
                      
                                             (*completion-found-hook* 
                                              (if completion-found-hook-specified-p 
                                                  completion-found-hook
                                                *completion-found-hook*))
                           
                                             ;;;
                                             ;;;
                                             ;;;

                                             (next-round-p nil)
		                             
                                             ,@(when keys 
                                                 (mapcar #'(lambda (key)
                                                             (list
                                                              (intern (format nil "*~A*" key))
                                                              key))
                                                         keys)))
                       
                                        (declare (special ,@(mapcar #'(lambda (key)
                                                                        (intern (format nil "*~A*" key)))
                                                                    keys)))
                      
                                        (declare (ignorable ,@(mapcar #'(lambda (key)
                                                                          (intern (format nil "*~A*" key)))
                                                                      keys)))

                                        (declare (ignorable next-round-p level))


                                        (when (and *keep-det-assertions-p* *maintain-prover-state-p*)
                                          (error "Both *keep-det-assertions-p* and *maintain-prover-state-p* specified."))
			    
                                        (when (and *compute-core-model-p* *delete-nodes-p*)
                                          (error "Both *compute-core-model-p* and *delete-noeds-p* specified."))


                                        (when (not (or *maintain-prover-state-p*
                                                       *keep-det-assertions-p*))
				
                                          (setf (action-timestamp-counter *cur-abox*) 0
                                                (current-action *cur-abox*) nil
                                                (choice-point-counter *cur-abox*) 0
                                                rollback-to-action (get-current-action)))
                   
                                        (mapc #'delete-abox *completions*)
                   
                                        (setf *completions* nil)
                   
                                        (when (or *debug-p* *show-prover-incarnations-p*)
                                          (show-incarnation-info 'sat-testing)

                                          (describe-object abox t)
                                          (loop-over-abox-nodes (node abox)
                                            (describe-object node t) (terpri) (terpri)))

                                        (unless (prepared-p *cur-store*)
                                          (error "Store ~A is not prepared!" *cur-store*))
                                        (unless (prepared-p *cur-tbox*)
                                          (error "TBox ~A is not prepared!" *cur-tbox*))
                                        (unless (prepared-p *cur-abox*)
                                          (error "ABox ~A is not prepared!" *cur-abox*))
                                       
                   
                                        (catch 'exit
                                          
                                          (let ((res

                                                 (multiple-value-list
                                                  (progn 

                                                    ,@(cond ((member (first init-code) '(:copy :call-next-method))
                                                             (get-init-code prover-name *language-dispatcher* *abox-dispatcher*))
                                                            
                                                            ((eq (first init-code) :inherit-from)
                                                             (get-init-code prover-name
                                                                            (make-language (second init-code))
                                                                            (make-instance (third init-code) :dont-initialize t)))
                                                            
                                                            (t init-code))))))
                                            
                                            (when *prover-main-start-time* 
                                              (incf ,(intern (format nil "*TIME-SPEND-IN-PROVER-MAIN-~A-~A*" 
                                                                     prover-name language-type))
                                                    (- (get-internal-run-time) *prover-main-start-time*)))

                                            (when (first res) ; erfuellbar? 

                                              (let ((*maintain-history-p* nil))

                                                (cond (*keep-det-assertions-p* 

                                                       (announce "Now computing deterministic assertions")

                                                       (delete-non-det-assertions *cur-abox*)

                                                       (when *compute-core-model-p* 

                                                         (announce "Now computing core model")

                                                         (compute-core-model *cur-abox*))

                                                       (when *store-instance-retrieval-model-p* 

                                                         (let ((root-node (find-node *cur-abox* 1)))

                                                           (unless root-node
                                                             (error "root node?!"))

                                                           (announce "Now making instance retrieval models for ~A" 
                                                                     (first (last (told-concepts root-node))))

                                                           (unless (core-model root-node)
                                                             (error "no core model for root node?"))

                                                           (announce "Adding ~A (core model of root node) as instance retrieval model of concept ~A" 
                                                                     (core-model root-node) 
                                                                     (first (last (told-concepts root-node))))
						  
                                                           (setf (instance-retrieval-model 
                                                                  (first (last (told-concepts root-node))))
                                                                 (core-model root-node))))

                                                       ;;; ist richtig, denn die beibehaltenen Assertionen
                                                       ;;; erscheinen nun auf TOLD CONCEPTS!

                                                       (loop-over-abox-nodes (node abox)
                                                         (setf (slot-value node 'choice-points) nil)
                                                         (setf (slot-value node 'initial-concept) nil)
                                                         (reset-label (description node))))

                                                      ((not *maintain-prover-state-p*)

                                                       ,(if rollback-code
                                                            `(progn ,@rollback-code)
                                                          `(rollback))))))

                                            (incf ,(intern (format nil "*TIME-SPEND-IN-PROVER-INIT-~A-~A*" 
                                                                   prover-name language-type))
                                                  (- (get-internal-run-time) start-time))

                                            (values-list res))))))))))

       ;;;
       ;;; MAIN 
       ;;;

       
       ,@(when main-code
           `(,@(unless (member (first main-code) '(:copy :inherit-from :call-next-method))
                 `((defmethod get-main-code ((name (eql (quote ,prover-name))) (language ,language-type)
                                             (abox ,abox-type))
                     (quote ,main-code))))
             
             ,@(when (eq (first main-code) :call-next-method)
                 `((defmethod get-main-code ((name (eql (quote ,prover-name))) (language ,language-type)
                                             (abox ,abox-type))
                     (call-next-method))))
             
             (defmethod prover-main ((name (eql (quote ,prover-name))) (language ,language-type) (abox ,abox-type) 
                                     &key 
                                     (level 0)
                                     ,@(rest keylist) &allow-other-keys)
               (declare (ignorable ,@keys))
               
               (setf *abox* abox)      

               (unless *prover-main-start-time* ; bei der ersten Inkarnation setzen 
                 (setf *prover-main-start-time* (get-internal-run-time))

                 (unless *prover-init-start-time* (error "No prover init start time?!"))

                 (unless ,(intern (format nil "*TIME-SPEND-IN-PROVER-INIT-BEFORE-MAIN-~A-~A*" 
                                          prover-name language-type))
                   (error "!"))
                 
                 (incf ,(intern (format nil "*TIME-SPEND-IN-PROVER-INIT-BEFORE-MAIN-~A-~A*" 
                                        prover-name language-type))
                       (- (get-internal-run-time) *prover-init-start-time*)))
                 
               
               (let ((level (1+ level))
                     (next-round-p t)
                     (round 0)
                     (return-value nil))

                 (declare (ignorable next-round-p level))


                 (loop while next-round-p do 

                       (incf round)

                       (setf next-round-p nil)
                 
                       (when (or *debug-p* (and *show-prover-incarnations-p* (= 1 level)))
                         (show-incarnation-info 'prover-main))
                       
                       (when *debug-p*
                         (format t "~%~%-----------------------------------------------------------------------------------")
                         (format t "~%**** INCARNATION LEVEL ~A, ROUND ~A OF PROVER ~A *****~%~%" level round ',language-type)
                         (describe-object abox t)
                         
                         (loop-over-abox-nodes (node abox)
                           (describe-object node t) (terpri) (terpri)))
                       
                       (setf return-value
                             (multiple-value-list

                              (progn

                                ,@(cond ((member (first main-code) '(:copy :call-next-method))
                                         (get-main-code prover-name *language-dispatcher* *abox-dispatcher*))
                                        ((eq (first main-code) :inherit-from)
                                         (get-main-code prover-name
                                                        (make-language (second main-code))
                                                        (make-instance (third main-code) :dont-initialize t)))                         
                                        (t main-code)))))

                       finally return (values-list return-value)

                       )))))
           
       ;;;
       ;;; SUCCESS 
       ;;;

       
       ,@(when success-code 
           `(,@(unless (member (first success-code) '(:copy :inherit-from :call-next-method))
                 `((defmethod get-success-code ((name (eql (quote ,prover-name))) (language ,language-type) 
                                                (abox ,abox-type))
                     (quote ,success-code))))

             ,@(when (eq (first success-code) :call-next-method)
                 `((defmethod get-success-code ((name (eql (quote ,prover-name))) (language ,language-type) 
                                                (abox ,abox-type))
                     (call-next-method))))

             (defmethod prover-success ((name (eql (quote ,prover-name))) (language ,language-type) (abox ,abox-type)
                                        &key 
                                        ,@(rest keylist) &allow-other-keys)
               (declare (ignorable ,@keys))

               (setf (slot-value abox 'satisfiable) t)

               ,@(cond ((member (first success-code) '(:copy :call-next-method))
                        (get-success-code prover-name *language-dispatcher* *abox-dispatcher*))
                       ((eq (first success-code) :inherit-from)
                        (get-success-code prover-name
                                          (make-language (second success-code))
                                          (make-instance (third success-code) :dont-initialize t)))
                       (t success-code))))))))

;;;
;;;
;;;

(defmacro perform ( ( expansion-type &rest args &key abox-type language-type &allow-other-keys)  &body body )
  (let ((positive-code
         (rest (assoc :positive body)))
        (negative-code
         (rest (assoc :negative body)))
        (body
         (rest (assoc :body body)))
        (after-successful-clash-repair
         (rest (assoc :after-successful-clash-repair body)))

        (abox (if abox-type 
                  (make-instance abox-type :dont-initialize t)
                *abox-dispatcher*))
        
        (language (if language-type 
                      (make-instance language-type :dont-initialize t)
                    *language-dispatcher*)))

    `(unless next-round-p
       (block prover       
         ,(apply (symbol-function (intern (string-upcase (format nil "get-code-for-~A" expansion-type))))
                 language abox 
                 :positive-code positive-code
                 :negative-code negative-code
                 :after-successful-clash-repair-code after-successful-clash-repair
                 :body-code body
                 args)))))

(defmacro perform! ( ( expansion-type &rest args &key abox-type language-type &allow-other-keys)  &body body )
  (let ((positive-code
         (rest (assoc :positive body)))
        (negative-code
         (rest (assoc :negative body)))
        (body
         (rest (assoc :body body)))
        (after-successful-clash-repair
         (rest (assoc :after-successful-clash-repair body)))

        (abox (if abox-type 
                  (make-instance abox-type :dont-initialize t)
                *abox-dispatcher*))
        
        (language (if language-type 
                      (make-instance language-type :dont-initialize t)
                    *language-dispatcher*)))

    `(unless next-round-p
       (block prover       
         ,(apply (symbol-function (intern (string-upcase (format nil "get-code-for-~A!" expansion-type))))
                 language abox 
                 :positive-code positive-code
                 :negative-code negative-code
                 :after-successful-clash-repair-code after-successful-clash-repair
                 :body-code body
                 args)))))

;;;
;;;
;;;

(defmacro rollback () 
  `(rollback-to abox rollback-to-action))

(defmacro success ()
  `(prover-success name language abox))

(defmacro start-main ()
  `(prover-main name language abox))

(defmacro restart-main ()
  `(prover-main name language abox :level level))

(defmacro next-round () 
  `(setf next-round-p t))

;;;
;;;
;;;

(defmacro completion-found ()
  `(progn 

     (announce "COMPLETION FOUND!~%~%")
     
     (let ((bad-nodes 
            (remove-if #'(lambda (x) 
                           (or (not (active-p x))
                               (complete-p x)
                               (blocked-p x)
                               (cache-satisfiable-p x)))
                       (get-nodes abox))))

       (when (some #'(lambda (x)
                       (and (active-p x)
                            (blocked-p x)))
                   (get-nodes abox))

         (error "Strategy error!"))
                            

       (when bad-nodes
         (dolist (node bad-nodes)
           (describe-object node t))
         (error "Strategy error: Nodes ~A are not complete" bad-nodes)))
     
      
     (cond (*visualize-p*      
            (visualize abox)
            (break))
           (*completion-found-hook* 
            (funcall *completion-found-hook* abox)))

     (when (and *break-p* 
                (not *visualize-p*))
       (break))

     (when *compute-all-completions-p*
       (push (copy abox) *completions*)
       (when (and *how-many* 
                  (>= (length *completions*)
                      *how-many*))
         (throw 'exit t)))

     t))


