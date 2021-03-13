;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(defconstant +marker+ (gensym "M"))

(defun error-abox-inconsistent (abox)
  (error "ABox ~A is inconsistent!" abox))

(defvar *time-spend-in-individual-instance-rollback* 0)

(defvar *time-spend-in-individual-instance-rollback2* 0)

(defvar *time-spend-in-global-rollback* 0)

(defvar *time-spend-in-individual-instance-register-told-concepts* 0)

(defvar *time-spend-in-individual-instance-init-abox* 0)

(defvar *time-spend-in-individual-instance-abox-sat* 0)

(defvar *retrieval-context* nil)

(defvar *rollback-to* nil)

(defvar *rollback-from* nil)

;;;
;;;
;;;

(defmacro with-abox-retrieval ((abox) &body body)
  `(let ((*retrieval-context* :prepare)
         (*rollback-to* nil))
     (unwind-protect
         (progn ,@body)

       (when (eq *retrieval-context* :established)
         (global-rollback ,abox)))))


(defmethod global-rollback ((abox abox) &key 
                            (rollback-from *rollback-from*)
                            (rollback-to *rollback-to*))
         
  (announce "Leaving Abox retrieval context, global rollback!")
         
  (with-timing (*time-spend-in-global-rollback*)
         
    (let ((*MAINTAIN-UNEXPANDED-SOME-CONCEPTS1-HEAP-P*  nil)
          (*MAINTAIN-UNEXPANDED-AT-LEAST-CONCEPTS-HEAP-P* nil)
          (*MAINTAIN-UNEXPANDED-ATOMIC-CONCEPTS1-HEAP-P* nil)
          (*MAINTAIN-DEACTIVATED-NODES-P* nil)
          (*MAINTAIN-LEAF-NODES-P* nil)
          (*MAINTAIN-UNEXPANDED-AT-LEAST-CONCEPTS1-HEAP-P* nil)
          (*MAINTAIN-UNEXPANDED-ATTRIBUTE-EXISTS-CONCEPTS-HEAP-P* nil)

          (*MAINTAIN-CACHE-SAT-NODES-P* nil)

          (*MAINTAIN-UNEXPANDED-ATOMIC-CONCEPTS-HEAP-P* nil)
          (*MAINTAIN-UNEXPANDED-OR-CONCEPTS1-HEAP-P* nil)
          (*MAINTAIN-OLD-NODES-P* nil)
          (*MAINTAIN-UNEXPANDED-AND-CONCEPTS-HEAP-P* nil)
          (*MAINTAIN-UNEXPANDED-OR-CONCEPTS-HEAP-P* nil)
          (*MAINTAIN-ACTIVE-NODES-P* nil)
          (*MAINTAIN-UNEXPANDED-AND-CONCEPTS-HEAP1-P* nil)
          (*MAINTAIN-UNEXPANDED-ATTRIBUTE-EXISTS-CONCEPTS1-HEAP-P* nil)
          (*MAINTAIN-UNEXPANDED-SOME-CONCEPTS-HEAP-P* nil)
          (*MAINTAIN-BLOCKED-NODES-P* nil))

      (setf (slot-value abox 'current-action) rollback-from)

      (rollback-to abox rollback-to))))


(define-prover ((concept-instances dl abox) &key concept continuation)
  (:rollback :no)

  (:init  
     
   (announce "CONCEPT INSTANCES PROVER")

   (let* ((res nil)
          (negated (get-negated-concept concept))
          (action nil)
          (init-performed-p nil)
          (same-loop-abox-init-done-p nil)
          (count 0)
          (pos 0)
          (nodes nil)
          (n 1)
          (lastpos nil)
          (instance nil)
          (instance-found nil))

     (labels ((do-it ()
		  
                (block prover 
                  (with-clash-handler
                                      
                   (dolist (node nodes)
			
                     (incf *all-instance-tests*)

                     (when *debug-p* 
                       (princ "CHECKING NODE ") (princ node) (terpri)
                       (princ concept)
                       (terpri) 
                       (describe-object node t)
                       (describe-object abox t))
			
                     (incf count)
                     (setf pos (floor (+ 0.5 (* 30 (/ count n)))))
                     (setf clashes nil)
                     (setf instance nil)

                     (cond ((obvious-non-instance abox node concept)

                            (incf *obvious-non-instance-hits*)
			       
                            t)

                           ((obvious-instance abox node concept)
                            
                            (incf *obvious-instance-hits*)
			    
                            (setf instance t)
                            (push node res))

                           (t
			                              
                            (labels ((abox-sat ()
                                       
                                       (loop-over-abox-nodes (node abox)
                                         (when (initial-concept node) 
                                           (break "initial concept 1 : ~A!" node)))

                                       (let ((action (get-current-action)))
                    
                                         (prog1 
                        
                                             (or (let ((added ; macht den einen Knoten wieder aktiv! 
                                                        (register-as-unexpanded negated
                                                                                :node node 
                                                                                :new-choice-point 0)))
                              
                                                   (check-for-clash node added)

                                                   clashes)
                            
                                                 (with-timing (*time-spend-in-individual-instance-abox-sat*)
                                                   (incf *individual-instance-proofs*)
                                                   (prover-init
                                                    'abox-sat
                                                    language
                                                    abox
                                                    :compute-core-model-p nil  
                                                    :store-ind-models-p t
                                                    :keep-det-assertions-p nil
                                                    :maintain-prover-state-p t)))

                                           (announce ">>>> CONCEPT INSTANCES: Now rolling back!")

                                           (with-timing (*time-spend-in-individual-instance-rollback*)
                                             (let ((*maintain-active-nodes-p* t))

                                               (rollback-to abox action)))


                                           (loop-over-abox-nodes (node abox)
                                             (when (initial-concept node) 
                                               (break "initial concept2 ~A !" node)))
                                         
                                           (reset-cached-sat-result abox)))))
                              
                              (when (if (or (not *abox-init-required-p*)
                                            (eq *retrieval-context* :established)
                                            same-loop-abox-init-done-p )

                                        (with-strategy (+abox-saturation-strategy+)
                                          (not (abox-sat)))
                                          
                                      (let ((*abox-init-required-p* nil))

                                        (when (eq *retrieval-context* :prepare)
                                          (setf *retrieval-context* :established)
                                          (setf *rollback-to* (get-current-action)))
                                        
                                        (setf same-loop-abox-init-done-p t
                                              init-performed-p t)
                                      
                                        (setf action (get-current-action))

                                        ;(visualize abox) (break)
                                            
                                        (announce ">>>> CONCEPT INSTANCES: Now performing initial deterministic expansion")

                                        (with-timing (*time-spend-in-individual-instance-register-told-concepts*)
                                          (register-told-concepts abox language)
                                          (prepare-index-structures abox))

                                        (loop-over-abox-nodes (node abox)
                                          (when (initial-concept node) 
                                            (break "initial concept!")))
                                        
                                        
                                        (announce ">>>> CONCEPT INSTANCES: Initial deterministic expansion")
                                        
                                        (let ((start-time1 
                                               (get-internal-run-time)))
                                          
                                          (with-strategy (+abox-saturation-strategy+)
                                            (perform (deterministic-expansion :language-type dl)
                                              (:body                               
                                               (announce "Done!")
                                               ;(visualize abox) (break)
                                               (incf *time-spend-in-individual-instance-init-abox* 
                                                     (- (get-internal-run-time) start-time1))

                                               (setf *rollback-from* (get-current-action))
                       
                                               (if clashes
                                                   ;;; kann eigentlich nicht vorkommen!
                                                   (error-abox-inconsistent abox)
                                                 (not (abox-sat)))))))))
                                      
                                (setf instance t)
                                
                                (push node res)))))

                     
                     (when (and instance continuation)
                       (funcall continuation node))

                     (setf instance-found (or instance-found instance))
			
                     (when (and t	; *debug-p* 
                                (or (not lastpos)  
                                    (not (= pos lastpos))))
                       (loop as i from (or lastpos 0) to (1- pos) do 
                             (if instance-found  
                                 (princ "+") 
                               (princ "-")))
			  
                       (setf instance-found nil)
                       (setf lastpos pos))) 
                   
                   (when t		; *debug-p* 
                     (terpri))
                   
                   (when (and init-performed-p 
                              (not *retrieval-context*))
                     (global-rollback abox :rollback-to action))
                   
                   res))))
	 
       (loop-over-abox-nodes (node abox) 
         (when (old-p node) 
           (push node nodes)))
	 
       (setf n (length nodes))

       (when t			; *debug-p*  
         (terpri)
         (loop as i from 0 to 29 do (princ "="))
         (terpri))

       (do-it)))))



(define-prover ((individual-instance-p dl abox) &key node concept)
  (:rollback :no)

  (:init  
     
   (announce "INDIVIDUAL INSTANCE?")

   (incf *all-instance-tests*)

   (let* ((negated (get-negated-concept concept))
          (action nil)
          (init-performed-p nil)
          (instance nil))

     (labels ((do-it ()
		  
                (block prover 
                  (with-clash-handler
			
                   (setf clashes nil)
                   (setf instance nil)
                   
                   (cond ((obvious-non-instance abox node concept)
			     
                          (incf *obvious-non-instance-hits*)
			    
                          t)

                         ((obvious-instance abox node concept)

                          (incf *obvious-instance-hits*)
			     
                          (setf instance t))
                         
                         (t (labels ((abox-sat ()
          
                                       (let ((action (get-current-action)))
                    
                                         (prog1 
                        
                                             (or (let ((added ; macht den einen Knoten wieder aktiv! 
                                                        (register-as-unexpanded negated
                                                                                :node node 
                                                                                :new-choice-point 0)))
                              
                                                   (check-for-clash node added)

                                                   clashes)
                            
                                                 (with-timing (*time-spend-in-individual-instance-abox-sat*)
                                                   (incf *individual-instance-proofs*)
                                                   (prover-init
                                                    'abox-sat
                                                    language
                                                    abox
                                                    :compute-core-model-p nil  
                                                    :store-ind-models-p nil
                                                    :keep-det-assertions-p nil
                                                    :maintain-prover-state-p t)))

                                           (announce ">>>> INDIVIDUAL INSTANCE-P: Now rolling back!")

                                           (with-timing (*time-spend-in-individual-instance-rollback*)
                                             (let ((*maintain-active-nodes-p* t))
                                               (rollback-to abox action)))
                                         
                                           (reset-cached-sat-result abox)))))

                              (when (if (or (not *abox-init-required-p*)
                                            (eq *retrieval-context* :established))
                                            
                                        (not (abox-sat))
                                          
                                      (let ((*abox-init-required-p* nil)) 

                                      ; (visualize abox) (break)
                                        
                                        (when (eq *retrieval-context* :prepare)
                                          (setf *retrieval-context* :established)
                                          (setf *rollback-to* (get-current-action)))

                                        (announce ">>>> INDIVIDUAL INSTANCE-P: Now performing initial deterministic expansion")

                                        (setf init-performed-p t)
                                        
                                        (setf action (get-current-action))
                                        
                                        (with-timing (*time-spend-in-individual-instance-register-told-concepts*)
                                          (register-told-concepts abox language)
                                          (prepare-index-structures abox))
                                        
                                        (announce ">>>> INDIVIDUAL INSTANCE-P: Initial deterministic expansion")
                                        
                                        (let ((start-time1 
                                               (get-internal-run-time)))
                                          
                                          (with-strategy (+abox-saturation-strategy+)
                                            (perform (deterministic-expansion :language-type dl)
                                              (:body                               
                                               (announce "Done!")
                                             
                                               (incf *time-spend-in-individual-instance-init-abox* 
                                                     (- (get-internal-run-time) start-time1))

                                               (setf *rollback-from* (get-current-action))
                       
                                               (if clashes
                                                   ;;; kann eigentlich nicht vorkommen!
                                                   (error-abox-inconsistent abox)
                                                 (not (abox-sat)))))))))
                                      
                                (setf instance t)))))

                   (when (and init-performed-p 
                              (not *retrieval-context*))
                     (global-rollback abox :rollback-to action))
                   		      
                   instance))))

       (do-it)))))


#| 

(define-prover ((simple-individual-instance-p dl abox) &key node concept)
  (:rollback :no)

  (:init  
     
   (announce "INDIVIDUAL INSTANCE?")

   (let* ((negated (get-negated-concept concept))
          (instance nil))

     (labels ((do-it ()
                
                (setf instance nil)
                
                (cond ((obvious-non-instance abox node concept)
			     
                       t)

                      ((obvious-instance abox node concept)
			     
                       (setf instance t))
                         
                      (t (labels ((abox-sat ()

                                    (let ((action nil))
                    
                                      (prog1 
                        
                                          (or (let ((added ; macht den einen Knoten wieder aktiv! 
                                                     (register-as-unexpanded negated
                                                                             :node node 
                                                                             :new-choice-point 0)))

                                                (setf action (get-current-action))
                                                   
                                                (block prover ; wird nicht benutzt als Ausgang 
                                                  (with-clash-handler
                                                   (check-for-clash node added)

                                                   clashes)))
                            
                                              (with-timing (*time-spend-in-individual-instance-abox-sat*)
                                                (prover-init
                                                 'abox-sat
                                                 language
                                                 abox
                                                 :compute-core-model-p nil  
                                                 :store-ind-models-p nil
                                                 :keep-det-assertions-p nil
                                                 :maintain-prover-state-p nil)))

                                        (undo-action abox action)
                                         
                                        (reset-cached-sat-result abox)))))

                           (unless (abox-sat)
                             (setf instance t)))))

                instance))

       (do-it)))))

|#


;;;
;;;
;;;


(defun related-p (abox from to role &rest args)

  (let* ((abox (find-abox abox :error-p t))
         (from (find-node abox from :error-p t))
         (to   (find-node abox   to :error-p t)))

      
    (with-abox* (abox)

      (if (apply #'prepare-abox-for-querying-and-sat-p abox args)
            
          (let* ((role (parse-role role))
                 (marker 
                  (with-protected-concept-store
                   (get-negated-concept 
                    (parse-concept +marker+))))
                 (concept 
                  (with-protected-concept-store
                   (parse-concept `(all ,(unparse role) ,+marker+)))))
	    
            (node-instance from concept) 
            (node-instance to marker)

            (prog1 
                (not (apply #'abox-sat-p abox args))

              (node-forget from concept)
              (node-forget to marker)))

        (error-abox-inconsistent abox)))))

;;;
;;;
;;;

(defmethod obvious-instance ((abox abox) node concept) 
  (cond ((is-top-concept-p concept)
         
         t)

        ((is-bottom-concept-p concept)

         nil)
	
        (t

         (cond ((member concept (told-concepts node))

                t)
               
               ((member (negated-concept concept) (told-concepts node))

                nil)

               (t 

                (let ((negated (negated-concept concept)))

                  (unless (core-model node)
                    (break "no core models for ~A?" node))

                  (unless (instance-retrieval-model negated)
                    (break "no instance retrieval model for ~A?" negated))

                  (models-surely-not-mergeable-p +dl+
                                                 (core-model node) 
                                                 (instance-retrieval-model negated))))))))



(defmethod obvious-non-instance ((abox abox) node concept)  
  (cond ((is-bottom-concept-p concept)

         t)

        ((is-top-concept-p concept)

         nil)

        ((member concept (told-concepts node))
         
         nil)
	
        (t

         (let ((negated (negated-concept concept)))

           (if (member negated (told-concepts node))

               t

             (progn 
               
               (unless (ind-models node)
                 (break "no ind models for ~A?" node))

               (unless (cached-models negated)
                 (break "no concept models for ~A?" negated))

               (or (some #'(lambda (ma) 
                             (some #'(lambda (mb)
                                       (models-mergeable-p +alchn+ ma mb))
                                   (cached-models negated)))
                         (ind-models node))

                   (obvious-instance abox node negated))))))))

;;;
;;;
;;;

(defun instance-p (abox node concept &rest args &key language)

  (let* ((abox (find-abox abox :error-p t))
         (node (find-node abox node :error-p t)))

    (with-protected-concept-store 

     (with-abox* (abox)

       (let* ((concept (parse-concept concept))
              (language (or language (get-language concept t)))
              (res
               (apply #'compute-models-and-sat-p concept args))
              (abox-sat-p 
               (apply #'prepare-abox-for-querying-and-sat-p abox args)))
          
         (if abox-sat-p
          
             (ecase res
               (:top t)
               (:yes (cond ((obvious-non-instance abox node concept)
                            nil)
                           ((obvious-instance abox node concept)
                            t)
                           (t
                            (apply #'prover-init
                                   'individual-instance-p 
                                   (make-language language)
                                   abox
                                   :debug-p nil ; t
                                   :node node
                                   :concept concept
                                   args))))
               (:no nil))

           (error-abox-inconsistent abox)))))))

(defun retrieve-concept-instances (abox concept &rest args 
                                        &key language continuation
                                        &allow-other-keys)
  

  (let* ((abox (find-abox abox :error-p t)))

    (with-protected-concept-store 

     (with-abox* (abox)

       (let* ((concept (parse-concept concept))
              (language (or language (get-language concept t)))
              (res
               (apply #'compute-models-and-sat-p concept args))
              (abox-sat-p 
               (apply #'prepare-abox-for-querying-and-sat-p abox args)))
          
         (if abox-sat-p

             (case res
                
               (:top 
                (let ((res nil))
                  (loop-over-old-nodes (node abox)
                                       (when (old-p node)
                                         (when continuation
                                           (funcall continuation node))
                                         (push node res)))
                  res))
                
               (:yes (let ((res
                            (apply #'prover-init
                                   'concept-instances 
                                   (make-language language)
                                   abox
                                   :concept concept
                                   :continuation continuation
                                   args)))

                       res))

               (:no nil))
                
           (error-abox-inconsistent abox)))))))



(defun retrieve-role-fillers (abox ind role &rest args 
                                   &key language continuation
                                   &allow-other-keys)
  
  (let* ((abox (find-abox abox :error-p t))
         (node (find-node abox ind :error-p t))
         (language (or language (get-language abox))))

    (with-protected-concept-store 

     (with-abox* (abox)

       (let* ((role (parse-role role))
              (marker 
               (parse-concept +marker+))
              (concept 
               (parse-concept `(all ,(unparse role) ,+marker+))))
        
         (node-instance node concept)
        
         (prog1
            
             (apply #'retrieve-concept-instances abox marker 
                    :continuation continuation
                    :language language
                    args)

           (node-forget node concept)))))))

;;;
;;;
;;;

(defmethod compute-models-and-sat-p ((concept concept) &rest args)
  (announce "Compute models and sat-p!")
  
  (let ((negated (get-negated-concept concept)))

    (if (or (already-known-to-be-satisfiable-p concept) 
            (cached-models concept)
            (apply #'sat-p concept
                   :store-instance-retrieval-model-p t
                   :store-ind-models-p t
                   :maintain-prover-state-p nil
                   :recompute-p t
                   args))

        (progn 

          (when (or (not (instance-retrieval-model concept))
                    (not (cached-models concept)))

            (apply #'sat-p concept
                   :store-instance-retrieval-model-p t
                   :maintain-prover-state-p nil
                   :store-ind-models-p t
                   :recompute-p t
                   args))
          
          (if (or (already-known-to-be-satisfiable-p negated) 
                  (cached-models negated)
                  (apply #'sat-p negated
                         :store-instance-retrieval-model-p t
                         :maintain-prover-state-p nil
                         :store-ind-models-p t
                         :recompute-p t
                         args))

              (progn 

                (when (or (not (instance-retrieval-model negated))
                          (not (cached-models negated)))

                  (apply #'sat-p negated
                         :maintain-prover-state-p nil
                         :store-instance-retrieval-model-p t
                         :store-ind-models-p t
                         :recompute-p t
                         args))

                :yes)
            :top))

      :no)))

;;;
;;;
;;;


(defmethod prepare-abox-for-querying-and-sat-p ((abox abox) &rest args)
  (announce "Prepare abox for querying and sat-p!")

  (if (slot-value abox 'ready-for-retrieval)
      t
    (prog1 
        (apply #'abox-consistent-p abox
               :recompute-p t
               :store-ind-models-p t
               :compute-core-model-p t
               :maintain-prover-state-p nil
               args)
      (setf (slot-value abox 'ready-for-retrieval) t))))
      

  #|

(progn
  (full-reset)
  (with-kb (test test :delete-if-exists-p t)
         
           (instance a (or (all r c) (all r d)))

           (related a b r)
           (prepare-abox-for-querying-and-sat-p *cur-abox*)

           (with-abox-retrieval (*cur-abox*)
            (princ (retrieve-concept-instances *cur-abox* '(or c d)))
            (princ (individual-instance? a (or (all r (or c d)) x)))
            (princ (retrieve-concept-instances *cur-abox* '(or c d ))))))
          

|#

  #|

(defun abox-queries-test ()
  (delete-all-tboxes)

  (instance i c)
  (instance j d)
  (related i j r)
  
  (pprint (ts::midelora-retrieve (?x ?y) (and (?x c) (?x ?y r) (?y d))))

  (pprint (ts::midelora-retrieve (?x) (and (i ?x r) (?x d)))))

|#



