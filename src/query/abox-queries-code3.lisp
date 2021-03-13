;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;; 
;;; Code fuer DL-Prover (Racer/MiDeLoRa) - Queries
;;; 

;;;
;;; Individual Instance Checking
;;; 


(defun get-code-dl-prover-check-individual-instance-p (ind dl-concept body &rest args  
                                                           &key negated-p 
                                                           &allow-other-keys)
  (declare (ignorable args))

  (if (not negated-p)
      `(let ((concept (quote ,dl-concept)))
         
         (declare (ignorable concept))
         
         (when (dl-prover-individual-instance-p *running-substrate* ,ind concept)

           ,body))

    `(unless (dl-prover-individual-instance-p *running-substrate* ,ind ',dl-concept)
         
       ,body)))
    

(defun evaluate-dl-prover-check-individual-instance-p (ind concept continuation &rest args  
                                                           &key negated-p &allow-other-keys)
  (declare (ignorable args))

  (if (not negated-p)
      (when (dl-prover-individual-instance-p *running-substrate* ind concept)
        
        (apply continuation :var ind args))
    
    (unless (dl-prover-individual-instance-p *running-substrate* ind concept)

      (apply continuation :var ind args))))

;;;
;;; Individuals Related Checking
;;; 

(defun get-code-dl-prover-check-individuals-related-p (from to role body &rest args)
  `(let ((from ,from)
         (to ,to)
         (role (quote ,role)))
     
     (declare (ignorable from to role))
                          
     ,(apply #'get-code-dl-prover-retrieve-individual-fillers from role 
             `(when (same-abox-individual-p cand-to to) 
                ,body)
             :to 'cand-to
             args)))

(defun evaluate-dl-prover-check-individuals-related-p (from to role continuation &rest args)
  (let ((to2 to))    
    (apply #'evaluate-dl-prover-retrieve-individual-fillers from role 
           #'(lambda (&rest args &key to &allow-other-keys)
               (declare (ignorable args))
               (when (same-abox-individual-p to2 to)
                 (apply continuation args)))
           args)))


;;;
;;; Concept Instances Enumeration
;;; 

(defun get-code-dl-prover-retrieve-concept-instances (dl-concept body 
                                                                 &rest args 
                                                                 &key negated-p var
                                                                 query
                                                                 &allow-other-keys)
  (declare (ignorable args))
  
  (unless var (nrql-error "Compiler error: no var given"))

  (if (not negated-p)
      `(let ((known (dl-prover-retrieve-known-concept-instances *running-substrate* (quote ,dl-concept))))
        
         (abortable-dolist (,var known)
           (progn 
             (setf (bindings-found-p ,query) nil)
             ,body
             (when ,(when (and (is-unary-query-p query)
                               (member (voi query) (existential-vois query)))
                      t)
               (when (bindings-found-p ,query)
                 (throw 'abort-enumerator t)))))

         (if *continuation-based-instance-retrieval-p*
        
             (dl-prover-retrieve-concept-instances *running-substrate* (quote ,dl-concept)
                                                   :continuation
           
                                                   #'(lambda (,var)
                                                       
                                                       (setf (bindings-found-p ,query) nil)
                                                       ,body
                                                       (when ,(when (and (is-unary-query-p query)
                                                                         (member (voi query) (existential-vois query)))
                                                                t)
                                                         (when (bindings-found-p ,query)
                                                           (throw 'abort-enumerator t)))))

           (abortable-dolist (,var 
                              (dl-prover-retrieve-concept-instances *running-substrate* (quote ,dl-concept)))
             (progn 
               (setf (bindings-found-p ,query) nil)
               ,body
               (when ,(when (and (is-unary-query-p query)
                                 (member (voi query) (existential-vois query)))
                        t)
                 (when (bindings-found-p ,query)
                   (throw 'abort-enumerator t)))))))
      
    `(abortable-dolist (,var 
                        (dl-prover-all-individuals *running-substrate*))

       (unless (dl-prover-individual-instance-p *running-substrate* ,var (quote ,dl-concept))
         (setf (bindings-found-p ,query) nil)
         ,body
         (when ,(when (and (is-unary-query-p query) 
                           (member (voi query) (existential-vois query)))
                  t)
           (when (bindings-found-p ,query)
             (throw 'abort-enumerator t)))))))


(defun evaluate-dl-prover-retrieve-concept-instances (dl-concept continuation 
                                                                 &rest args 
                                                                 &key negated-p query
                                                                 &allow-other-keys)

  (labels ((pos-body (var)
             
             (apply continuation :var var args)))
  
    (if (not negated-p)

        (let ((known (dl-prover-retrieve-known-concept-instances *running-substrate* dl-concept)))
            
          (abortable-dolist (var known)
            (setf (bindings-found-p query) nil)
            (pos-body var)
            (when (and (is-unary-query-p query) 
                       (bindings-found-p query)
                       (member (voi query) (existential-vois query)))
              (throw 'abort-enumerator t)))
            
          (if *continuation-based-instance-retrieval-p*

              (dl-prover-retrieve-concept-instances *running-substrate* dl-concept
                                                    :continuation
                                                    #'(lambda (var)
                                                        (setf (bindings-found-p query) nil)
                                                        (pos-body var)
                                                        (when (and (is-unary-query-p query) 
                                                                   (bindings-found-p query)
                                                                   (member (voi query) (existential-vois query)))
                                                          (throw 'abort-enumerator t))))

            (abortable-dolist (var (dl-prover-retrieve-concept-instances *running-substrate* dl-concept))
              (setf (bindings-found-p query) nil)
              (pos-body var)
              (when (and (is-unary-query-p query) 
                         (bindings-found-p query)
                         (member (voi query) (existential-vois query)))
                (throw 'abort-enumerator t)))))

      
      (abortable-dolist (var (dl-prover-all-individuals *running-substrate*))
        (setf (bindings-found-p query) nil)
        (unless (dl-prover-individual-instance-p *running-substrate* var dl-concept)
          (apply continuation :var var args)
          (when (and (is-unary-query-p query) 
                     (bindings-found-p query)
                     (member (voi query) (existential-vois query)))
            (throw 'abort-enumerator t)))))))

;;;
;;; Related Individuals Enumeration (Komplex! Optimiert Dl-Provers Routinen!) 
;;; Liefert Paare! (From To) 
;;; 

(defun get-code-dl-prover-retrieve-related-individuals (role body &rest args &key from to query &allow-other-keys)
  (unless (and from to)
    (nrql-error "Compiler error: no vars given"))
  
  `(abortable-dolist (,from (dl-prover-all-individuals *running-substrate*))
     (progn 
       (setf (bindings-found-p ,query) nil)
       ,(apply #'get-code-dl-prover-retrieve-individual-fillers from role 
               body
               :to to 
               args)
       (when ,(when (and (member (voi-from query) (existential-vois query))
                         (member (voi-to query) (existential-vois query)))
                t)
         (when (bindings-found-p ,query)
           (throw 'abort-enumerator t))))))
         

(defun evaluate-dl-prover-retrieve-related-individuals (role continuation &rest args &key query &allow-other-keys)
  (declare (ignorable args))

  (abortable-dolist (from (dl-prover-all-individuals *running-substrate*))
    (setf (bindings-found-p query) nil)
    (apply #'evaluate-dl-prover-retrieve-individual-fillers from role 
           #'(lambda (&rest args &key to &allow-other-keys) 
               (apply continuation :from from :to to args))
           args)
    (when (and (member (voi-to query) (existential-vois query))
               (member (voi-from query) (existential-vois query))
               ; es muessen tatsaechlich BEIDE rein existentiell sein! 
               (bindings-found-p query))
      (throw 'abort-enumerator t))))

;;;
;;; Individual Role Successor Enumeration - Optimiert Racers Routine, 
;;; bzgl. Behandlung transitiver Roller (kein Reasoning notwendig)
;;; 

(defun get-code-dl-prover-retrieve-individual-fillers (from role body
                                                            &rest args 
                                                            &key  
                                                            query
                                                            only-one-p 
                                                            negated-p
                                                            ;;; bei (NOT (?*X ?*Y R))
                                                            ;;; AChtung: das ist nicht das Gleiche
                                                            ;;; wie (?*x ?*y (NOT R)), s. unten!

                                                            inverse-p to &allow-other-keys)

  (declare (ignorable args))

  (unless to (nrql-error "Compiler error: no var given"))

  (let ((only-one-p 
         (or only-one-p
             (and query 
                  (member (voi-to query) 
                          (existential-vois query))))))

    (if (negated-dl-role-p role)

        ;;;
        ;;; (?*x ?*y (NOT R)), (NOT (?*x ?*y (NOT R)))
        ;;; 
    
        (if negated-p

            ;;;
            ;;; (NOT (?*x ?*y (NOT R))) ?
            ;;; 

            `(let* ((inds (dl-prover-all-individuals *running-substrate*))
                    (negated-succs (copy-list inds)))

               (declare (ignorable inds negated-succs))
                 
               (abortable-dolist (to inds)
                 ,(apply #'get-code-dl-prover-check-individuals-related-p from 'to role
                         `(setf negated-succs (delete to negated-succs))
                         :negated-p nil
                         :inverse-p nil 
                         :query query
                         nil))

               (abortable-dolist (,to negated-succs)
                 ,body))
      
          ;;;
          ;;; (?*x ?*y (NOT R)) ?
          ;;; 
          ;;; Ist (?*x ?*y (not R)) entailed?
          ;;; Ja, wenn ich (from to R) hinzufuege und
          ;;; einen Widerspruch kriege!
          ;;; 
          ;;; Achtung: funktioniert erst, wenn bei Racer
          ;;; die UNA ausgeschaltet werden kann; 
          ;;; sonst kann die ABox inkonsistent werden 
          ;;; durch Hinzufuegen von Attributen 
          ;;; z.B. (a b has-father) -> 
          ;;; sowohl durch (a c has-father) inkonsistent ->
          ;;; (a c (NOT has-father)) wird ausgerechnet!
          ;;; was natuerlich falsch ist
          ;;; Racer muesste b und c mergen -> Racer 1.8! 
          ;;; 
          


          (let ((equal-role-p (eq (second role) +equal-role+)))
            
            `(let* ((role ',(if equal-role-p nil (second role)))
                    (role ,(if equal-role-p 
                               nil
                             (if inverse-p 
				 `(dl-prover-atomic-role-inverse *running-substrate* role)
			       `role))))
           
               (declare (ignorable role)) 
             
               (dolist (,to (dl-prover-all-individuals *running-substrate*))
           
                 (let ((abox-consistent-p nil)
                       (added-p nil))

                   (with-critical-section

                     ,@(cond (equal-role-p 

                              (to-be-implemented 'negated-equal-role))

                             (t
               
                              `((unless (loop as role-axiom in 
                                              (dl-prover-all-role-assertions-for-individual-in-domain 
                                               *running-substrate* ,from)
                                              as role1 = (second role-axiom)
                                              as to1 = (second (first role-axiom))
                                              thereis (and (same-abox-individual-p to1 ,to)
                                                           (same-role-p role1 role)))
                                  (setf added-p t)
                                  (dl-add-role-assertion *running-substrate* *running-abox* ,from ,to role))
                            
                                (setf abox-consistent-p
                                      (if (is-racer-substrate-p *running-substrate*)
                                          (without-unique-name-assumption
                                            (dl-prover-abox-consistent-p *running-substrate*
                                                                         *running-abox*))
                                    
                                        ;;; noch inkorrekt fuer Midelora! 
                                        (dl-prover-abox-consistent-p *running-substrate*
                                                                     *running-abox*)))
                            
                                (when added-p 
                                  (dl-forget-role-assertion *running-substrate* *running-abox* ,from ,to role))))))

                   ;;; da der Effekt ja rueckgaengig gemacht wird, 
                   ;;; muessen die Caches nicht updated werden!
             
                   (unless abox-consistent-p 
               
                     ,body))))))
       
      ;;;
      ;;; (?*x ?*y R), (NOT (?*x ?*y R))
      ;;; 

      (let ((equal-role-p (eq role +equal-role+)))
    
        `(let* ((role ',(if equal-role-p nil role))
                (role ,(if equal-role-p 
                           nil
                         (if inverse-p 
			     `(dl-prover-atomic-role-inverse *running-substrate* role)
			   `role)))
                (inv-role ,(if equal-role-p 
                               nil
                             `(dl-prover-atomic-role-inverse *running-substrate* role))))

           (declare (ignorable role inv-role))
      
           (let* ((from ,from))

             (declare (ignorable from to role inv-role))

             ,(if negated-p
              
                  ;;;
                  ;;; (NOT (?*x ?*y R))
                  ;;; 
    
                  `(let* ((inds (dl-prover-all-individuals *running-substrate*))
                          (negated-succs (copy-list inds)))

                     (declare (ignorable inds negated-succs))
                 
                     (abortable-dolist (to inds)
                       ,(apply #'get-code-dl-prover-check-individuals-related-p from 'to role 
                               `(setf negated-succs (delete to negated-succs))
                               :negated-p nil
                               :inverse-p nil 
                               :query query
                               nil))

                     (abortable-dolist (,to negated-succs)
                       ,body))

                ;;;
                ;;; (?*x ?*y R)
                ;;; 
              
            
                `(let* ((role-descendants1 
                         ,(if equal-role-p 
                              nil
                            `(dl-prover-atomic-role-descendants *running-substrate* role)))

                        (role-descendants2 
                         ,(if equal-role-p 
                              nil
                            `(dl-prover-atomic-role-descendants *running-substrate* inv-role)))

                        (done nil)

                        (transitive-p
                         ,(if equal-role-p 
                              nil
                            `(dl-prover-transitive-role-p *running-substrate* role)))

                        (succs (dl-prover-all-role-successors *running-substrate* from role))
                    
                        (generated-succs nil))
               
                   (declare (ignorable role-descendants1 role-descendants2 succs transitive-p))
                             
                   (labels ((do-it (current &key continue-p first-p)
                          
                              (unless first-p 
                            
                                ;;; found a successor!

                                (when transitive-p 
                                  (push current generated-succs))
                            
                                (with-critical-section
                                  (unless (dl-prover-role-successors-cache-complete-for-role-p *running-substrate* from role)
                                    (unless (dl-prover-is-known-role-successor-of-p *running-substrate* current from role)
                                      (dl-prover-register-role-successor *running-substrate* current from role))))
                              
                                (let ((,to current))
                                  (declare (ignorable ,to))

                                  ,@(when only-one-p 
                                      `((setf done t)))
                              
                                  ,body))
                                      
                              (when (and (or continue-p first-p)
                                         (not done))

                                ,@(cond (equal-role-p

                                         `((dolist (to1 (dl-prover-individual-synonyms *running-substrate* current))

                                             (do-it to1 :continue-p nil))))

                                        (t
                                       
                                         `((loop as role-axiom in 
                                                 (dl-prover-all-role-assertions-for-individual-in-domain *running-substrate* 
                                                                                                         current)
                                                 as role1 = (second role-axiom)
                                                 as to1 = (second (first role-axiom))
                                                 when (with-critical-section
                                                        (and (=> transitive-p 
                                                                 (not (member to1 generated-succs)))
                                                             (or (equal role1 role)
                                                                 (member role1 role-descendants1 :test #'equal)
                                                                 ;;; New: Reasoning required!
                                                                 (and (not *told-information-reasoning-p*)
                                                                      (needs-filler-reasoning-p *running-substrate*)
                                                                      (if (symbolp role) 
                                                                          (dl-prover-internal-individuals-related-p *running-substrate*
                                                                                                                    current
                                                                                                                    to1
                                                                                                                    role
                                                                                                                    (abox *running-substrate*))
                                                                        (dl-prover-internal-individuals-related-p *running-substrate*
                                                                                                                  to1 
                                                                                                                  current
                                                                                                                  inv-role
                                                                                                                  (abox *running-substrate*)))))))
                                                 do 
                                                 (do-it to1 :continue-p transitive-p))
                  
                                           (loop as role-axiom in 
                                                 (dl-prover-all-role-assertions-for-individual-in-range *running-substrate* 
                                                                                                        current)
                                                 as role1 = (second role-axiom)
                                                 as from1 = (first (first role-axiom))
                                                 when (with-critical-section
                                                        (and (=> transitive-p 
                                                                 (not (member from1 generated-succs)))
                                                             (or (equal role1 inv-role)
                                                                 (member role1 role-descendants2 :test #'equal)
                                                                 ;;; New: Reasoning required!
                                                                 (and (not *told-information-reasoning-p*)
                                                                      (needs-filler-reasoning-p *running-substrate*)
                                                                      (if (symbolp inv-role) 
                                                                          (dl-prover-internal-individuals-related-p *running-substrate*
                                                                                                                    from1
                                                                                                                    current
                                                                                                                    inv-role
                                                                                                                    (abox *running-substrate*))
                                                                        (dl-prover-internal-individuals-related-p *running-substrate* 
                                                                                                                  current
                                                                                                                  from1
                                                                                                                  role
                                                                                                                  (abox *running-substrate*)))))))
                                                 do 
                                                 (do-it from1 :continue-p transitive-p))))))))

                     (if (not (eq succs :unknown))
                         (dolist (to succs)
                           (do-it to :first-p nil))
                       (prog1 (do-it from :first-p t)
                         (when (and (not ,(when only-one-p t))
                                    (=> (needs-filler-reasoning-p *running-substrate*)
                                        ;; sonst unvollstaendig!
                                        (not *told-information-reasoning-p*)))
                           (with-critical-section
                             (unless (dl-prover-role-successors-cache-complete-for-role-p *running-substrate* from role)
                               ;;; evtl. wurde der Cache durch einen parallelen Prozess bereits komplettiert!
                               (dl-prover-register-role-successors-cache-is-complete-for-role *running-substrate* from role)))))))))))))))



(defun evaluate-dl-prover-retrieve-individual-fillers (from role continuation
                                                            &rest args 
                                                            &key  
                                                            query
                                                            only-one-p 
                                                            negated-p
                                                            inverse-p &allow-other-keys)
  (declare (ignorable args))

  ;; (unless query
  ;;  (nrql-error "Runtime error: query missing"))

  (let ((only-one-p 
         (or only-one-p
             (and query
                  (member (voi-to query) 
                          (existential-vois query))))))

    (if (negated-dl-role-p role)

        ;;;
        ;;; (?*x ?*y (NOT R)), (NOT (?*x ?*y (NOT R)))
        ;;;     

        (if negated-p 

            ;;;
            ;;; (NOT (?*x ?*y (not R))) ?
            ;;; 

            (let* ((inds (dl-prover-all-individuals *running-substrate*))
                   (negated-succs (copy-list inds)))

              (declare (ignorable inds negated-succs))
            
              (abortable-dolist (to inds)
                (apply #'evaluate-dl-prover-check-individuals-related-p from to role
                       #'(lambda (&rest args)
                           (declare (ignorable args))
                           (setf negated-succs (delete to negated-succs)))
                       :negated-p nil
                       :inverse-p nil
                       :query query
                       nil))
            
              (abortable-dolist (var negated-succs)
                (if inverse-p 
                    (apply continuation :from var args)
                  (apply continuation :to var args))))
          
          ;;;
          ;;; (?*x ?*y (not R))
          ;;; 

          (let* ((role (second role))
                 (equal-role-p (eq role +equal-role+))
                 (role (if equal-role-p 
                           +equal-role+
                         (if inverse-p 
                             (dl-prover-atomic-role-inverse *running-substrate* role)
                           role))))
        
            (dolist (to (dl-prover-all-individuals *running-substrate*))
           
              (let ((abox-consistent-p nil)
                    (added-p nil))

                (with-critical-section
                
                  (cond (equal-role-p 
                         
                         (to-be-implemented 'negated-equal-role))

                        (t

                         (unless (loop as role-axiom in 
                                       (dl-prover-all-role-assertions-for-individual-in-domain 
                                        *running-substrate* from)
                                       as role1 = (second role-axiom)
                                       as to1 = (second (first role-axiom))
                                       thereis (and (same-abox-individual-p to1 to)
                                                    (same-role-p role1 role)))
                           (setf added-p t)
                           (dl-add-role-assertion *running-substrate* *running-abox* from to role))
              
                         (setf abox-consistent-p
                               (if (is-racer-substrate-p *running-substrate*)
                                   (without-unique-name-assumption
                                     (dl-prover-abox-consistent-p *running-substrate* *running-abox*))

                                 (dl-prover-abox-consistent-p *running-substrate* *running-abox*)))
                  
                         (when added-p 
                           (dl-forget-role-assertion *running-substrate* *running-abox* from to role)))))
             
                (unless abox-consistent-p 
                  (apply continuation :to to args))))))
   
      ;;;
      ;;; (?*x ?*y R), (NOT (?*x ?*y R))
      ;;; 
    
      (let* ((equal-role-p (eq role +equal-role+))

             (role (if equal-role-p 
                       +equal-role+
                     (if inverse-p 
                         (dl-prover-atomic-role-inverse *running-substrate* role)
                       role)))

             (inv-role (if equal-role-p 
                           +equal-role+
                         (dl-prover-atomic-role-inverse *running-substrate* role))))

        (if negated-p
              
            ;;;
            ;;; (NOT (?*x ?*y R))
            ;;; 
          
            (let* ((inds (dl-prover-all-individuals *running-substrate*))
                   (negated-succs (copy-list inds)))

              (declare (ignorable inds negated-succs))
                 
              (abortable-dolist (to inds)
                (apply #'evaluate-dl-prover-check-individuals-related-p from to role 
                       #'(lambda (&rest args)
                           (declare (ignorable args))
                           (setf negated-succs (delete to negated-succs)))
                       :negated-p nil
                       :inverse-p nil
                       :query query
                       nil))
            
              (abortable-dolist (var negated-succs)
                (if inverse-p 
                    (apply continuation :from var args)
                  (apply continuation :to var args))))

          ;;;
          ;;; (?*x ?*y R)
          ;;; 
          
          (let* ((role-descendants1 
                  (unless equal-role-p
                    (dl-prover-atomic-role-descendants *running-substrate* role)))

                 (role-descendants2 
                  (unless equal-role-p
                    (dl-prover-atomic-role-descendants *running-substrate* inv-role)))

                 (done nil)

                 (transitive-p
                  (and (not equal-role-p)
                       (dl-prover-transitive-role-p *running-substrate* role)))
                 
                 (succs (dl-prover-all-role-successors *running-substrate* from role))

                 (generated-succs nil))
                          
            (labels ((do-it (current &key continue-p first-p)
                       
                       (unless first-p 
                                            
                         ;;; found a successor! 
                       
                         (when transitive-p
                           (push current generated-succs))

                         (with-critical-section
                           (unless (dl-prover-role-successors-cache-complete-for-role-p *running-substrate* from role)
                             (unless (dl-prover-is-known-role-successor-of-p *running-substrate* current from role)
                               (dl-prover-register-role-successor *running-substrate* current from role))))
                         
                         (when only-one-p 
                           (setf done t))
                       
                         (if inverse-p 
                             (apply continuation :from current args)
                           (apply continuation :to current args)))
                           
                       (when (and (or continue-p first-p)
                                  (not done))

                         (cond ((equal role +equal-role+)

                                (dolist (to1 (dl-prover-individual-synonyms *running-substrate* current))

                                  (do-it to1 :continue-p nil)))

                               (t
                       
                                (loop as role-axiom in 
                                      (dl-prover-all-role-assertions-for-individual-in-domain *running-substrate* current)
                                      as role1 = (second role-axiom)
                                      as to1 = (second (first role-axiom))
                                      when (with-critical-section
                                             (and (=> transitive-p 
                                                      (not (member to1 generated-succs)))
                                                  (or (equal role1 role)
                                                      (member role1 role-descendants1 :test #'equal)
                                                      ;;; New: Reasoning required!
                                                      (and (not *told-information-reasoning-p*)
                                                           (needs-filler-reasoning-p *running-substrate*)
                                                           (if (symbolp role) 
                                                               (dl-prover-internal-individuals-related-p *running-substrate*
                                                                                                         current
                                                                                                         to1
                                                                                                         role
                                                                                                         (abox *running-substrate*))
                                                             (dl-prover-internal-individuals-related-p *running-substrate*
                                                                                                       to1 
                                                                                                       current
                                                                                                       inv-role
                                                                                                       (abox *running-substrate*)))))))
                                      do 
                                      (do-it to1 :continue-p transitive-p))
                  
                                (loop as role-axiom in 
                                      (dl-prover-all-role-assertions-for-individual-in-range *running-substrate* current)
                                      as role1 = (second role-axiom)
                                      as from1 = (first (first role-axiom))
                                      when (with-critical-section
                                             (and (=> transitive-p 
                                                      (not (member from1 generated-succs)))
                                                  (or (equal role1 inv-role)
                                                      (member role1 role-descendants2 :test #'equal)
                                                      ;;; New: Reasoning required!
                                                      (and (not *told-information-reasoning-p*)
                                                           (needs-filler-reasoning-p *running-substrate*)
                                                           (if (symbolp inv-role) 
                                                               (dl-prover-internal-individuals-related-p *running-substrate*
                                                                                                         from1
                                                                                                         current
                                                                                                         inv-role
                                                                                                         (abox *running-substrate*))
                                                             (dl-prover-internal-individuals-related-p *running-substrate*
                                                                                                       current
                                                                                                       from1
                                                                                                       role
                                                                                                       (abox *running-substrate*)))))))
                                      do 
                                      (do-it from1 :continue-p transitive-p)))))))
                   
              (if (not (eq succs :unknown))
                  (dolist (to succs)
                    (do-it to :first-p nil))
                (prog1
                    (do-it from :first-p t)
                  (when (and (not only-one-p) ; sonst nicht vollstaendig! 
                             (=> (needs-filler-reasoning-p *running-substrate*)
                                 ;; sonst unvollstaendig!
                                 (not *told-information-reasoning-p*)))
                    (with-critical-section
                      (unless (dl-prover-role-successors-cache-complete-for-role-p *running-substrate* from role)
                        (dl-prover-register-role-successors-cache-is-complete-for-role *running-substrate* from role)))))))))))))

;;;
;;; Concrete Domain :CONSTRAINT Checking
;;; Enumeration steckt in defquery-code!!!! 
;;; Fuer den Fall, dass Queries wie (?x ?y (:constraint age age =)) ankommmen ->
;;; wenn keine Feature-Ketten muss doch enumeriert werden!
;;; hatte ich vergessen (sonst machen die Role-Atoms der Features die Enumeration)
;;; 

(defun get-code-racer-check-individuals-cd-related-p (from to from-attrib to-attrib constraint body 
                                                           &rest args
                                                           &key negated-p query
                                                           &allow-other-keys)
  (declare (ignorable args))

  (let ((from-name (if (equal from-attrib to-attrib) 
                       (intern (format nil "~A-1" from-attrib)
                               (racer-package (substrate query)))
                     from-attrib))
        (to-name (if (equal from-attrib to-attrib)
                     (intern (format nil "~A-2" to-attrib)
                             (racer-package (substrate query)))
                   to-attrib)))

    (when (or (from-is-datatype-property-p query)
              (to-is-datatype-property-p query))
      (to-be-implemented 'owl-datatype-properties-in-constraint-queries))

    `(let* ((from ,from)
            (to ,to))

       (declare (ignorable from to))

       (if (not (and (dl-prover-individual-instance-p *running-substrate* from (quote (an ,from-attrib)))
                     (dl-prover-individual-instance-p *running-substrate* to (quote (an ,to-attrib)))))
          
           ;;; Attribute fehlen? -> negatives :constraint erfuellt!
         
           (when ,negated-p 
             ,body)
        
         ;;; Attribute implizit vorhanden

         (with-critical-section
           (unless (dl-prover-retrieve-individual-attribute-fillers *running-substrate* from (quote ,from-attrib))
             (dl-prover-add-attribute-assertion *running-substrate*
                                                from 
                                                (intern (format nil "~A-~A" from ',from-attrib)
                                                        ',(racer-package (substrate query)))
                                                ',from-attrib))
           
           (unless (dl-prover-retrieve-individual-attribute-fillers *running-substrate* to (quote ,to-attrib))
             (dl-prover-add-attribute-assertion *running-substrate*
                                                to 
                                                (intern (format nil "~A-~A" to ',to-attrib)
                                                        ',(racer-package (substrate query)))
                                                ',to-attrib))

           (let* ((from-cd (first (dl-prover-retrieve-individual-attribute-fillers *running-substrate* from
                                                                                   (quote ,from-attrib))))
                  ;;;
                  ;;; sind Attribute, selbst wenn es mehrere Fillers gibt, muessen die ja die selben sein
                  ;;; ab hier koennen die RACER-Funktionen verwendet werden; die neuen Filler
                  ;;; werden in die Hash-Tabellen eingetragen (Seiteneffekt)
                  ;;; 
                  (to-cd (first (dl-prover-retrieve-individual-attribute-fillers *running-substrate* to 
                                                                                 (quote ,to-attrib))))
              
                  (constraint
                   ,(if (consp constraint)
                        ;;; neue Syntax, z.B. 
                        ;;; (:constraint age age (< (+ 50 age-1) age-2))
                        `(tree-map #'(lambda (x)
                                       (cond ((eq x ',from-name) from-cd)
                                             ((eq x ',to-name) to-cd)
                                             (t x)))
                                   ',constraint)
                      ;;; alte Syntax, einfaches Praedikatssymbol
                      `(list ',constraint from-cd to-cd))))
             
             (unless (and from-cd to-cd)
               (nrql-error "Compiler error: internal error"))
         
             (,(if negated-p 'unless 'when)
              
              (dl-prover-constraint-entailed-p *running-substrate* constraint)

              ,body)))))))
      

(defun evaluate-racer-check-individuals-cd-related-p (from to from-attrib to-attrib constraint continuation 
                                                           &rest args
                                                           &key query negated-p &allow-other-keys)
  (declare (ignorable args))

  (let ((from-name (if (equal from-attrib to-attrib) 
                       (intern (format nil "~A-1" from-attrib) 
                               (racer-package *running-substrate*))
                     from-attrib))
        (to-name (if (equal from-attrib to-attrib)
                     (intern (format nil "~A-2" to-attrib)
                             (racer-package *running-substrate*))
                   to-attrib)))

    (if (not (and (dl-prover-individual-instance-p *running-substrate* from 
                                                   (replace-syntactic-concept-expression-sugar 
                                                    ;;; notwendig, weil from-attrib auch ein
                                                    ;;; Datatype Property sein kann!
                                                    (parser query)
                                                    `(racer-user::an ,from-attrib)))
                  (dl-prover-individual-instance-p *running-substrate* to 
                                                   (replace-syntactic-concept-expression-sugar 
                                                    (parser query)
                                                    `(racer-user::an ,to-attrib)))))

        (when negated-p 
          (apply continuation :from from :to to args))

      (with-critical-section
       
        (let ((from-attrib 
               (if (from-is-datatype-property-p query)
                   (get-attribute-for-datatype-property (parser query)
                                                        from-attrib)
                 from-attrib))
             
              (to-attrib 
               (if (to-is-datatype-property-p query)
                   (get-attribute-for-datatype-property (parser query)
                                                        to-attrib)
                 to-attrib)))

          (unless (dl-prover-retrieve-individual-attribute-fillers *running-substrate* from from-attrib)
            (dl-prover-add-attribute-assertion *running-substrate*
                                               from 
                                               (intern (format nil "~A-~A" from from-attrib)
                                                       (racer-package *running-substrate*))
                                               from-attrib))
    
          (unless (dl-prover-retrieve-individual-attribute-fillers *running-substrate* to to-attrib)
            (dl-prover-add-attribute-assertion *running-substrate*
                                               to 
                                               (intern (format nil "~A-~A" to to-attrib)
                                                       (racer-package *running-substrate*))
                                               to-attrib))

          (let* ((from-cd (first (dl-prover-retrieve-individual-attribute-fillers *running-substrate* from from-attrib)))
                 (to-cd (first (dl-prover-retrieve-individual-attribute-fillers *running-substrate* to to-attrib)))
                 (constraint 
                  (if (consp constraint)
                      (tree-map #'(lambda (x)
                                    (cond ((eq x from-name) from-cd)
                                          ((eq x to-name) to-cd)
                                          (t x)))
                                constraint)
                    (list constraint from-cd to-cd))))

            (unless (and from-cd to-cd)
              (nrql-error "Internal runtime error"))
      
            (if negated-p 
                (unless (dl-prover-constraint-entailed-p *running-substrate* constraint)
                  (apply continuation :from from :to to args))

              (when (dl-prover-constraint-entailed-p *running-substrate* constraint)              
              
                (apply continuation :from from :to to args)))))))))

;;;
;;; Has-Known-Successor Instance Checking
;;; 


(defun get-code-dl-prover-check-has-known-successor-p (from role body &rest args &key negated-p &allow-other-keys)
  `(let ((successor-found-p nil))
     ,(apply #'get-code-dl-prover-retrieve-individual-fillers
             from 
             role 
             `(setf successor-found-p t)
             :to 'temp
             :negated-p nil
             :only-one-p t
             args)
     (,(if negated-p 
           'unless 
         'when)
      successor-found-p 
      ,body)))


(defun evaluate-dl-prover-check-has-known-successor-p (from role continuation &rest args &key negated-p &allow-other-keys)
  (let ((successor-found-p nil))
    (apply #'evaluate-dl-prover-retrieve-individual-fillers
           from 
           role 
           #'(lambda (&rest args &key to &allow-other-keys)
               (declare (ignorable to args))
               (setf successor-found-p t))
           :negated-p nil
           :only-one-p t
           args)
    (if negated-p
        (unless successor-found-p 
          (apply continuation :var from args))
      (when successor-found-p 
        (apply continuation :var from args)))))

;;;
;;; Has-Known-Successor Instance Enumeration
;;; 

(defun get-code-dl-prover-retrieve-has-known-successor-instances (role body &rest args &key var &allow-other-keys)
  `(abortable-dolist (,var (dl-prover-all-individuals *running-substrate*))
     ,(apply #'get-code-dl-prover-check-has-known-successor-p var role body
             args)))

(defun evaluate-dl-prover-retrieve-has-known-successor-instances (role continuation &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (abortable-dolist (var (dl-prover-all-individuals *running-substrate*))
    (apply #'evaluate-dl-prover-check-has-known-successor-p var role continuation args)))



