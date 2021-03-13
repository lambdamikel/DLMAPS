;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(defvar *TIME-SPEND-IN-MAKE-CREATE-ACTION* 0)
(defvar *TIME-SPEND-IN-MAKE-PUT-TO-UNEXPANDED-ACTION* 0)
(defvar *TIME-SPEND-IN-MAKE-MOVE-FROM-UNEXPANDED-TO-EXPANDED* 0)
(defvar *TIME-SPEND-IN-MAKE-REGISTER-ADDITIONAL-DEPENDENCIES* 0)
(defvar *TIME-SPEND-IN-MAKE-CHANGE-STATE-ACTION* 0)

;;;
;;;
;;; 

(defun get-current-choice-point ()
  (choice-point-counter *cur-abox*))

(defun get-new-choice-point ()
  (incf (choice-point-counter *cur-abox*)))


;;;
;;; Choice Point (Dependenz) Verwaltung
;;;

(timed-defmethod register-choice-points ((concept concept) (choice-points list) &key node (error-p t) &allow-other-keys)

  (unless node 
    (error "Need a node!"))

  (when *reflexive-checks-p*
    (when (and error-p (get-choice-points concept :node node :error-p nil))
      (error " ~A : ~A already has choice points!~%" node concept)))

  (unless choice-points
    (error "~A : ~A no choice points given!~%" node concept))

  #+:use-avl-trees-for-choice-points
  (avl-tree-insert (list concept (copy-list choice-points)) (slot-value node 'choice-points))
  
  #-:use-avl-trees-for-choice-points
  (push (list concept (copy-list choice-points)) (slot-value node 'choice-points)))
  

(timed-defmethod register-choice-points ((edge abox-edge) (choice-points list) &key (error-p t) &allow-other-keys)

  (when *reflexive-checks-p*
    (when (and error-p (get-choice-points edge :error-p nil))
      (error "~A already has choice points!" edge)))

  ;;; die Kanten haben immer einfach Listen

  (setf (slot-value edge 'choice-points) (copy-list choice-points)))

(defmethod register-choice-points ((objects list) (choice-points list) &rest args)
  (dolist (object objects)
    (apply #'register-choice-points object choice-points args)))

;;;
;;;
;;;

(timed-defmethod get-choice-points ((concept concept) &key node error-p)

  (unless node
    (error "Need a node!"))

  (or #+:use-avl-trees-for-choice-points
      (second (avl-tree-find concept (slot-value node 'choice-points)))
      #-:use-avl-trees-for-choice-points
      (second (find concept (slot-value node 'choice-points) :key #'first))
      
      (when error-p 
        (describe-object node t)
        (error "No choice points for ~A : ~A!" node concept))))

(timed-defmethod get-choice-points ((edge abox-edge) &key error-p &allow-other-keys)
  (or (slot-value edge 'choice-points)
      (when error-p (error "No choice points for ~A!" edge))))

(defmethod get-choice-points ((objects list) &rest args &key error-p &allow-other-keys)
  (reduce #'append (mapcar #'(lambda (object) 
                               (if (and (consp object)
                                        (is-abox-node-p (first object)))
                                   (apply #'get-choice-points (second object) 
                                          :node (first object)
                                          :error-p error-p 
                                          args)
                                 (apply #'get-choice-points object :error-p error-p args)))
                           objects)))

;;;
;;;
;;;

(timed-defmethod delete-choice-points ((concept concept) &key node)

  (unless node 
    (error "Need a node!"))

  (setf (slot-value node 'choice-points)

        #+:use-avl-trees-for-choice-points
        (avl-tree-delete concept (slot-value node 'choice-points))
        #-:use-avl-trees-for-choice-points
        (delete concept (slot-value node 'choice-points) :key #'first)))
                        
(defmethod delete-choice-points ((edge edge) &key &allow-other-keys)
  (setf (slot-value edge 'choice-points) nil))

(defmethod delete-choice-points ((objects list) &rest args)
  (dolist (object objects)
    (apply #'delete-choice-points object args)))

;;;
;;;
;;;


(defmethod get-clash-culprits ((node abox-node) (concept bottom-concept))
  (list concept))

(defmethod get-clash-culprits ((node abox-node) (concept atomic-concept))
  (list concept (get-negated-concept concept)))

(defmethod get-clash-culprits ((node abox-node) (concept and-concept))
  (list concept (get-negated-concept concept)))

(defmethod get-clash-culprits ((node abox-node) (concept or-concept))
  (append (list concept (get-negated-concept concept))
          (mapcar #'get-negated-concept (arguments concept))))

(defmethod get-clash-culprits ((node abox-node) (concept some/all-concept))
  (list concept (get-negated-concept concept)))

(defmethod get-clash-culprits ((node abox-node) (concept at-least-concept))
  (list concept (get-negated-concept concept)))

(defmethod get-clash-culprits ((node abox-node) (concept at-most-concept))
  (list concept (get-negated-concept concept)))

;;;
;;;
;;;

#+:use-dependency-directed-backtracking
(defmethod get-preconditioned-actions ((concept-assertion list))
  (let ((node (first concept-assertion))
        (concept (second concept-assertion)))
    #+:use-avl-trees-for-actions 
    (second (avl-tree-find concept (slot-value node 'precondition-for-actions)))
    #-:use-avl-trees-for-actions 
    (second (find concept (slot-value node 'precondition-for-actions) :key #'first))))

#+:use-dependency-directed-backtracking
(defmethod get-preconditioned-actions ((edge abox-edge))
  (slot-value edge 'precondition-for-actions))

;;;
;;;
;;;

#+:use-dependency-directed-backtracking
(defmethod get-postconditioned-actions ((concept-assertion list))
  (let ((node (first concept-assertion))
        (concept (second concept-assertion)))
    #+:use-avl-trees-for-actions
    (second (avl-tree-find concept (slot-value node 'postcondition-for-actions)))
    #-:use-avl-trees-for-actions
    (second (find concept (slot-value node 'postcondition-for-actions) :key #'first))))

#+:use-dependency-directed-backtracking
(defmethod get-postconditioned-actions ((edge abox-edge))
  (slot-value edge 'postcondition-for-actions))

;;;
;;;
;;;


(defstruct action
  type
  det-action-p
  next-action 
  previous-action
  triggered-by
  triggers   
  comment   
  timestamp)


(defun get-current-action ()
  ;;; wird *nur* für den Aufbau der zeitlichen Ordnung verwendet!!!
  (current-action *cur-abox*))

(defun set-to-current-action (action)
  (setf (current-action *cur-abox*) action))

;;;
;;;
;;;

(defmacro undo-action1 (abox action)
  `(let ((abox ,abox)
         (action ,action))
     (declare (ignorable abox))
     (with-slots (next-action previous-action) action
       (when next-action 
         (setf (slot-value next-action 'triggered-by)
               (delete action (slot-value next-action 'triggered-by))))
    
       (when previous-action       
         (setf (slot-value previous-action 'triggers)
               (delete action (slot-value previous-action 'triggers))))

       (when next-action 
         (setf (slot-value next-action 'previous-action) previous-action))

       (when previous-action 
         (setf (slot-value previous-action 'next-action) next-action))
    
       (when (eq action (current-action *cur-abox*))
         ;;; wurde die letzte Aktion gelöscht = zeitlich spaeteste? 
         ;;; => *current-action* auf zeitlichen Vorgaenger setzen
         ;;; wenn es keine previous-action gibt, wird *current-action* NIL! okay!
         (setf (current-action *cur-abox*) previous-action)))))

;;;
;;;
;;;

(defmacro init-action (action)
  `(let ((action ,action))
     (when (current-action *cur-abox*)
       (setf (slot-value action 'previous-action) (current-action *cur-abox*)
             (slot-value (current-action *cur-abox*) 'next-action) action))
     (setf (slot-value action 'timestamp)
           (incf (action-timestamp-counter *cur-abox*)))
     (setf (current-action *cur-abox*) action)))

;;;
;;;
;;;

(defstruct (object-action (:include action))
  object
  pre)

(defmethod print-object ((action object-action) stream)
  (with-slots (timestamp comment object pre) action
    (format stream "#<~A @ ~A ~A:~%   OBJECT ~A~%   PRE ~A>" 
            (type-of action) timestamp comment object pre)))

(defstruct (create-edge-action (:include object-action)))

(defstruct (create-node-action (:include object-action)))

(defstruct (change-state-action (:include object-action))
  state)

#|

(defstruct (delete-edge-action (:include object-action)))

(defstruct (delete-node-action (:include object-action)))

|#


;;;
;;;
;;;

(defmacro register-action-in-slot-of-constraint (action slot-name constraint)
  `(let ((action ,action)
         (constraint ,constraint))
     (etypecase constraint
       (edge 
        (push action (slot-value (description constraint) 
                                 ,slot-name)))
       (cons
        (let ((node (first constraint))
              (concept (second constraint)))

          #+:use-avl-trees-for-actions 
          (let ((item (avl-tree-find concept 
                                     (slot-value (description node) 
                                                 ,slot-name))))
            (if item
                (setf (second item) (push action (second item)))
              (avl-tree-insert (list concept (list action))
                               (slot-value (description node) ,slot-name))))
      
          #-:use-avl-trees-for-actions
          (let ((item (find concept (slot-value (description node) 
                                                ,slot-name) 
                            :key #'first)))
            (if item ; (concept (action1 ... actionn))
                (setf (second item) (push action (second item)))
              (push (list concept (list action))                
                    (slot-value (description node) ,slot-name)))))))))


(defmacro unregister-action-in-slot-of-constraint (action slot-name constraint)
  `(let ((action ,action)
         (constraint ,constraint))
     (etypecase constraint
       (edge 
        (setf (slot-value (description constraint) ,slot-name)
              (delete action (slot-value (description constraint) ,slot-name))))
       (cons
        (let ((node (first constraint))
              (concept (second constraint)))
      
          #+:use-avl-trees-for-actions
          (let ((item (avl-tree-find concept (slot-value (description node) 
                                                         ,slot-name))))
            (when item 
              (setf (second item) (delete action (second item)))
              (unless (second item) 
                (avl-tree-delete concept (slot-value (description node) ,slot-name)))))
      
          #-:use-avl-trees-for-actions
          (let ((item (find concept (slot-value (description node) 
                                                ,slot-name)
                            :key #'first)))
            (when item 
              (setf (second item) (delete action (second item)))
              (unless (second item) 
                (setf (slot-value (description node) ,slot-name)
                      (delete concept (slot-value (description node) ,slot-name) :key #'first))))))))))

;;;
;;;
;;;

(defmacro init-object-action (action) 
  `(let ((action ,action))
     (init-action action)
     #+:use-dependency-directed-backtracking     
     (with-slots (pre object triggered-by) action
       (dolist (constraint pre)
         (register-action-in-slot-of-constraint action 'precondition-for-actions constraint)
         (dolist (postconditioned-by-action 
                  (get-postconditioned-actions constraint))
           (push postconditioned-by-action 
                 triggered-by)
           (push action 
                 (slot-value postconditioned-by-action 'triggers)))))
     ))

;;;
;;;
;;;

(defstruct (constraint-action (:include action))
  pre
  post
  state)

(defmethod print-object ((action constraint-action) stream)
  (with-slots (timestamp comment pre post det-action-p) action
    (format stream "~%#<~A @ ~A, DET: ~A, COM: ~A:~%   PRE: ~A~%   POST: ~A>" (type-of action) timestamp det-action-p comment pre post)))
   
(defstruct (move-from-unexpanded-to-expanded-action (:include constraint-action)))

(defstruct (put-to-unexpanded-action (:include constraint-action)))

(defstruct (register-additional-dependencies-action (:include constraint-action)))

;;;
;;;
;;;

(defmacro init-constraint-action (action)
  `(let ((action ,action))
     (init-action action)
     #+:use-dependency-directed-backtracking
     (with-slots (pre post triggered-by) action
       (dolist (constraint pre)
         (register-action-in-slot-of-constraint action 'precondition-for-actions constraint)
         (dolist (postconditioned-by-action 
                  (get-postconditioned-actions constraint))
           (push postconditioned-by-action 
                 triggered-by)
           (push action (slot-value postconditioned-by-action 'triggers))))
       (dolist (constraint post)
         (register-action-in-slot-of-constraint action 'postcondition-for-actions constraint)))
     ))

;;;
;;;
;;;


(defmacro get-state-vector-1 (object)
  `(list ,object 
         (get-state-vector ,object)))

(defmacro register-action (action precondition-constraints postcondition-constraints 
                                  &key state-object comment det-action-p)

  `(when *maintain-history-p* 

     (announce "Register action ~A" ',action)

     (let ((precondition-constraints 
            ,precondition-constraints)
           (postcondition-constraints
            ,postcondition-constraints)
           (state-object ,state-object)
           (comment ,comment)
           (det-action-p ,det-action-p))
     
       (declare (ignorable precondition-constraints
                           postcondition-constraints
                           state-object
                           det-action-p
                           comment))

       ,(ecase action

          ;;; object actions: POST ist i.d.R. das erzeugte/geloeschte/modifizierte Objekt! 
    
          (change-state 
           `(with-timing (*time-spend-in-make-change-state-action*)
              (let ((action
                     (make-change-state-action
                      :comment comment
                      :object postcondition-constraints
                      :pre precondition-constraints
                      :state (get-state-vector-1 (or state-object postcondition-constraints)))))

                (init-object-action action)
                action)))

          (create 
           `(with-timing (*time-spend-in-make-create-action*)
              (let ((action
                     (etypecase postcondition-constraints                
                       (abox-edge 
                        (make-create-edge-action
                         :comment comment
                         :object postcondition-constraints
                         :pre precondition-constraints))

                       (abox-node
                        (make-create-node-action
                         :comment comment
                         :object postcondition-constraints
                         :pre precondition-constraints)))))
            
                (init-object-action action)
                action)))
        
          #|
        (delete 
         `(let ((action
                 (etypecase precondition-constraints                 
                   (abox-edge 
                    (make-delete-edge-action
                     :comment comment
                     :object precondition-constraints
                     :pre precondition-constraints))

                   (abox-node
                    (make-delete-node-action
                     :comment comment
                     :object precondition-constraints
                     :pre precondition-constraints)))))
            
            (init-object-action action)
            action))
|#

          ;;; constraint actions: PRE und POST
    
          (put-to-unexpanded 
           `(with-timing (*time-spend-in-make-put-to-unexpanded-action*)
              (let ((action 
                     (make-put-to-unexpanded-action
                      :comment comment
                      :pre precondition-constraints
                      :post postcondition-constraints
                      :state (get-state-vector-1 state-object)
                      :det-action-p det-action-p)))
                (init-constraint-action action)
                action)))

          (move-from-unexpanded-to-expanded
           `(with-timing (*time-spend-in-make-move-from-unexpanded-to-expanded*)
              (let ((action 
                 
                     (if (null precondition-constraints) ;;; keine Vorbedingungen! 
                     
                         (make-move-from-unexpanded-to-expanded-action
                          :comment comment
                          :pre postcondition-constraints ;;; KEIN Fehler!!!
                          :post postcondition-constraints
                          :state (get-state-vector-1 state-object)
                          :det-action-p det-action-p )
                   
                       (make-move-from-unexpanded-to-expanded-action
                        :comment comment
                        :pre precondition-constraints
                        :post postcondition-constraints
                        :state (get-state-vector-1 state-object)))))

                (init-constraint-action action)
                action)))


          (register-additional-dependencies 
           `(with-timing (*time-spend-in-make-register-additional-dependencies*)
              (let ((action 

                     (if (null precondition-constraints) ;;; keine Vorbedingungen! 
                     
                         (make-register-additional-dependencies-action
                          :comment comment
                          :pre postcondition-constraints ;;; KEIN Fehler!!!
                          :post postcondition-constraints
                          :state (when state-object 
                                   (get-state-vector-1 state-object)))
               
                       (make-register-additional-dependencies-action
                        :comment comment
                        :pre precondition-constraints
                        :post postcondition-constraints
                        :state  (when state-object 
                                  (get-state-vector-1 state-object))))))
                ;;; (break "~A" action)
                (init-constraint-action action)
                action)))))))
     
;;;
;;;
;;; 

(defmacro undo-object-action (abox action)
  `(progn 
     (undo-action1 ,abox ,action)
     #+:use-dependency-directed-backtracking
     (with-slots (pre object) ,action
       (dolist (constraint pre)
         (unregister-action-in-slot-of-constraint ,action 'precondition-for-actions constraint)))))


(defmacro undo-constraint-action (abox action)
  `(progn 
     (undo-action1 ,abox ,action)
     #+:use-dependency-directed-backtracking
     (with-slots (pre post) ,action
       (dolist (constraint pre)
         (unregister-action-in-slot-of-constraint action 'precondition-for-actions constraint))
       (dolist (constraint post)
         (unregister-action-in-slot-of-constraint action 'postcondition-for-actions constraint)))))

;;;
;;;
;;;

(defvar *time-spend-in-undo-create-edge-action* 0)

(defvar *time-spend-in-undo-create-node-action* 0)

(defvar *time-spend-in-undo-change-state-action* 0)

(defvar *time-spend-in-undo-put-to-unexpanded-action* 0)

(defvar *time-spend-in-undo-move-from-unexpanded-to-expanded-action* 0)

(defvar *time-spend-in-undo-register-additional-dependencies-action* 0)

;;;
;;;
;;;

(defmacro undo-action (abox action &optional to-node)
  `(let ((*start-time* (get-internal-run-time))
         (*rollback-active-p* t)
         (*maintain-history-p* nil))
     
     (incf *no-of-undos*)

     (prog1
     
         (etypecase ,action

           ;; create
     
           (create-edge-action 
            (with-timing (*time-spend-in-undo-create-edge-action*)
              (delete-edge ,abox (slot-value ,action 'object)
                           ;; wichtig
                           :delete-inverse-p nil)
              (undo-object-action ,abox ,action)))


           (create-node-action 
            (with-timing (*time-spend-in-undo-create-node-action*)
              (let ((node (slot-value ,action 'object)))
        
                (delete-node ,abox node
                             ;; wichtig; "create-edge"
                             ;; erzeugt ja auch eine 
                             ;; protokollierende Aktion; 
                             ;; beim Undo gibt es sonst
                             ;; einen Error, wenn das Loeschen
                             ;; des Knotens eine automatisch 
                             ;; erzeugte reflexive Kante schon
                             ;; mitloescht!
                             :delete-edges-p nil)
      
                (when *use-unsat-cache-p*
                  (when (and (initial-concept node)
                             (if ,to-node 
                                 (or (is-successor-p node ,to-node)
                                     (eq node (cur-clash-node abox)))
                               (eq node (cur-clash-node abox))))
            
                    (announce "Found unsatisfiable concept ~A" (initial-concept node))

                    (register-already-known-to-be-inconsistent abox node))))
      
              (undo-object-action ,abox ,action)))

           #|
           ;;; delete

           (delete-edge-action 
            
            (let ((edge (slot-value ,action 'object)))
              (register-edge edge))

            (undo-object-action ,abox ,action))

           (delete-node-action 

            (let ((node (slot-value ,action 'object)))
              (register-node node))
      
            (undo-object-action ,abox ,action))
|#
           ;; change state
     
           (change-state-action 
            (with-timing (*time-spend-in-undo-change-state-action*)
              (with-slots (object state) action
                (let* ((state-object (first state))
                       (state (second state)))
                  (set-state-vector state-object state)))))

           ;; labels

           (put-to-unexpanded-action 
            (with-timing (*time-spend-in-undo-put-to-unexpanded-action*)
              (with-slots (post state) ,action
                (dolist (post post)
                  (let ((concept (second post))
                        (node (first post)))
                    (delete-from-unexpanded node concept)
                    (delete-choice-points concept :node node)))
                (undo-constraint-action ,abox ,action)
                (let* ((state-object (first state))
                       (state (second state)))
                  (set-state-vector state-object state)))))
           
           (move-from-unexpanded-to-expanded-action
            (with-timing (*time-spend-in-undo-move-from-unexpanded-to-expanded-action*)
              (with-slots (post state) ,action
                (dolist (post post)
                  (let ((concept (second post))
                        (node (first post)))
                    (put-from-expanded-to-unexpanded concept node)))
                (undo-constraint-action ,abox ,action)
                (let* ((state-object (first state))
                       (state (second state)))
                  (set-state-vector state-object state)))))

           ;; deps (kaum verwendet) 

           (register-additional-dependencies-action
            (with-timing (*time-spend-in-undo-register-additional-dependencies-action*)
              (with-slots (pre post) ,action

                (let ((pre-choice-points (get-choice-points pre)))
                  (dolist (post post)
                    (let* ((concept (second post))
                           (node (first post))
                           (points (get-choice-points concept :node node)))
              
                      (delete-choice-points concept :node node)
              
                      (register-choice-points concept 
                                              (remove-duplicates (cons 0 (set-difference points pre-choice-points)))
                                              :node node)))))

              (undo-constraint-action ,abox ,action))))

       (incf *time-spend-in-undo* 
             (- (get-internal-run-time) *start-time*)))))

;;;
;;; Chronologisches Backtracking
;;;


(defmethod get-reverse-history ((abox abox))
  (let* ((current (current-action abox)))
    (when current
      (loop while current 
            collect current
            do
            (setf current (slot-value current 'previous-action))))))


(defmethod get-history ((abox abox))
  (let* ((current (current-action abox)))

    (when current
      (loop while (slot-value current 'previous-action)
            do
            (setf current (slot-value current 'previous-action)))
    
      (loop while current 
            collect current
            do
            (setf current (slot-value current 'next-action))))))



(defmethod rollback-to ((abox abox) (action null) &optional to-node)
  (declare (ignore to-node))
  (announce "Rolling back from ~A to ~A" (current-action abox) action)
  (undo-history abox (get-reverse-history abox)))
                        
(defmethod rollback-to ((abox abox) (action action) &optional to-node)
  (announce "Rolling back from ~A to ~A" (current-action abox) action)

  (unless (current-action abox)
    (error "No current action!"))

  (let* ((current (current-action abox))
         (history
          (loop until (eq action current) 
                collect current
                do
                (setf current (slot-value current 'previous-action)))))

    (undo-history abox history to-node)))
                      
(defmethod undo-history ((abox abox) (history list) &optional to-node)
  (announce "Undoing history of length ~A" (length history))
  
  (dolist (action history)
    (announce "Undoing ~A" action)
    (undo-action abox action to-node))

  (setf (cur-clash-node abox) nil)

  (announce "Done."))

;;;
;;;
;;;

#+:use-dependency-directed-backtracking
(defun get-all-actions-triggered-by (constraint)
  (let ((actions nil))
    (labels ((do-it (action)
               (unless (member action actions)
                 (push action actions)
               
                 (dolist (triggered-action (slot-value action 'triggers))
                   (do-it triggered-action)))))
      
      (dolist (action (get-preconditioned-actions constraint))
        (do-it action))
      
      (sort actions #'> :key #'timestamp))))

#+:use-dependency-directed-backtracking
(defmethod rollback-all-depending-on ((abox abox) constraint)
  
  (announce "Rollback all actions triggered by ~A" constraint)

  (let ((actions (get-all-actions-triggered-by constraint)))
 
    (undo-history abox actions)))

;;;
;;; 
;;;

(timed-defmethod set-choice-points ((edge abox-edge) choice-points &key &allow-other-keys)
  (setf (slot-value edge 'choice-points) (ensure-list choice-points)))

(timed-defmethod add-a-choice-point ((edge abox-edge) choice-point &key &allow-other-keys)
  (push choice-point (slot-value edge 'choice-points)))

(timed-defmethod add-choice-points-for-edges ((abox abox) edges)
  (let ((choice-points nil))
    (dolist (edge edges)
      (let ((point (get-new-choice-point)))
        (push point choice-points)
        (add-a-choice-point edge point)))

    choice-points))

;;;
;;;
;;;

(timed-defmethod register-as-expanded ((concept concept) &key node depends-on comment)

  (unless node
    (error "Need a node!"))

  (when *reflexive-checks-p*
    (unless (unexpanded-p node concept)
      (describe-object node t)
    
      (error "Before putting ~A : ~A to EXPANDED, put it to UNEXPANDED!"
             node concept))

    (unless (get-choice-points concept :node node)
      (describe-object node t) 
      (error "Register as expanded - no choice points for ~A : ~A!" node concept)))
    
  (announce "I have expanded ~A : ~A" node concept)

  (when depends-on
    (announce "Depends on: ~A" depends-on)
    
    (register-additional-dependencies concept 
                                      :node node
                                      :depends-on depends-on
                                      :error-p nil
                                      :comment comment))
    
  (register-action move-from-unexpanded-to-expanded 
                   depends-on
                   (list (list node concept))
                   :state-object node
                   :comment comment)

  (put-from-unexpanded-to-expanded concept node))
   

;;;
;;;
;;;

(timed-defmethod register-as-unexpanded ((concept concept) &key node new-choice-point depends-on comment)
  
  (unless node
    (error "Need a node!"))

  (when *reflexive-checks-p*
    (when (on-tableau-p node concept)
      (error "~A : ~A already on tableau!" node concept)))
      
  (announce "Register as unexpanded ~A : ~A" node concept)

  (when depends-on 
    (announce "Depends on: ~A" depends-on))
  
  (register-action put-to-unexpanded 
                   depends-on
                   (list (list node concept))
                   
                   :det-action-p (when (and (=> new-choice-point (zerop new-choice-point))
                                            (every #'(lambda (x)
                                                       (zerop x))
                                                   (get-choice-points depends-on)))
                                   t)
                                          
                                     
                   :state-object node
                   :comment comment)

  (add-to-unexpanded node concept)

  (register-choice-points-for concept 
                              :node node 
                              :new-choice-point new-choice-point
                              :depends-on depends-on)

  (list concept) ; liefert die Lister der Konzept zurueck, die addiert wurden 
  )


(timed-defmethod register-as-unexpanded :around ((some-concept some-concept) &key node)
  
  (let ((res (call-next-method)))
  
    (when (role-domain (role some-concept))

      (let* ((role (role some-concept))
             (concept (role-domain role)))
        
        (unless (on-tableau-p node concept)
          
          (announce "Register role domain ~A of role ~A as unexpanded: ~A : ~A" 
                    concept role node some-concept)
          
          (setf res
                (nconc res
                       (register-as-unexpanded concept
                                               :node node
                                               :depends-on 
                                               (list (list node some-concept))))))))
    
    res))


(timed-defmethod register-as-unexpanded :around ((at-least-concept at-least-concept) &key node)
  (let ((res (call-next-method)))
  
    (when (role-domain (role at-least-concept))

      (let* ((role (role at-least-concept))
             (concept (role-domain role)))
        
        (unless (on-tableau-p node concept)
          
          (announce "Register role domain ~A of role ~A as unexpanded: ~A : ~A" 
                    concept role node at-least-concept)
          
          (setf res
                (nconc res 
                       (register-as-unexpanded concept
                                               :node node
                                               :depends-on 
                                               (list (list node at-least-concept))))))))
    
    res))

;;;
;;;
;;;

(timed-defmethod register-as-unexpanded ((edge abox-edge) &rest args 
                                   &key 
                                   (register-choice-points-p t)
                                   new-choice-point depends-on &allow-other-keys)

  (when register-choice-points-p
    (apply #'register-choice-points-for edge 
           :new-choice-point new-choice-point
           :depends-on depends-on 
           args)))

;;;
;;;
;;;

#|

(timed-defmethod register-as-unexpanded ((constraints list) &rest args)
  (dolist (constraint constraints)
    (apply #'register-as-unexpanded constraint args)))

|#

;;;
;;;
;;;


(timed-defmethod register-additional-dependencies ((concept concept) &key (error-p t) node new-choice-point depends-on comment)
  
    
  (when *reflexive-checks-p*
    (unless (on-tableau-p node concept)
      (error "~A : ~A is not on tableau!" node concept)))

  (let ((depends-on ;(cons (list node concept) 
         ;;; NEU, gloescht!
         depends-on))
      
    (announce "Register additional dependencies for ~A : ~A~%" node concept)

    (when depends-on 
      (announce "Depends on: ~A" depends-on))
  
    (register-action register-additional-dependencies
                     depends-on
                     (list (list node concept))
                     :state-object node
                     :comment comment)

    (register-choice-points-for concept 
                                :error-p error-p
                                :add-p t
                                :node node 
                                :new-choice-point new-choice-point
                                :depends-on depends-on)))



(timed-defmethod register-additional-dependencies ((edge abox-edge) &key (error-p t) new-choice-point depends-on comment)

  (let ((depends-on (cons edge depends-on)))

    (announce "Register additional dependencies for ~A" edge)
    (when depends-on 
      (announce "Depends on: ~A" depends-on))

    (register-action register-additional-dependencies
                     depends-on
                     (list edge)
                     :state-object edge
                     :comment comment)

    (register-choice-points-for edge 
                                :error-p error-p 
                                :add-p t
                                :new-choice-point new-choice-point
                                :depends-on depends-on)))


(defmethod register-additional-dependencies ((constraints list) &rest args)
  (dolist (constraint constraints)
    (apply #'register-additional-dependencies constraint args)))

;;;
;;;
;;;
;;;


(timed-defun register-choice-points-for (constraints 
                                   &key
                                   node 
                                   (error-p t)
                                   add-p
                                   new-choice-point
                                   depends-on 
                                   ;; Liste vom Typ ((knoten konzept) edge ...) 
                                   &allow-other-keys
                                   )

  
  (let ((constraints (ensure-list constraints)))
    
    (if depends-on
        (let ((choice-points nil))
          (loop as constraint in depends-on
                do (etypecase constraint
                     (cons 
                      (let ((node (first constraint))
                            (concept (second constraint)))
                        (dolist (point (get-choice-points concept :node node :error-p t))
                          (push point choice-points))))
                     (abox-edge
                      (dolist (point (get-choice-points constraint :error-p t))
                        (push point choice-points)))))
                               
          (let ((all-choice-points 
                 (delete-duplicates 
                  (if new-choice-point 
                      (cons new-choice-point choice-points)
                    choice-points))))
          
            (register-choice-points constraints 
                                    all-choice-points 
                                    :node node
                                    :error-p (and error-p (not add-p)))))

      ;;
      ;; ELSE: 
      ;; keine Constraint-Abhängigkeiten angegeben
      ;; 
      
      (if new-choice-point
          (register-choice-points constraints (list new-choice-point) 
                                  :error-p (and error-p (not add-p))
                                  :node node)
        (when error-p 
          (error "No new-choice-point given for ~A!" constraints))))))


