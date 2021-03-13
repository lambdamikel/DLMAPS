;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(timed-defmethod mark-deleted :after ((abox abox) (node abox-node))
  
  ;;: vorher wird deleted-p = T gesetzt, 
  ;;; und deactivate aufgerufen!
  ;;; hier ist der Knoten noch nicht "physisch"
  ;;; geloescht 

  (announce "Deleting node ~A" node))


(timed-defmethod mark-deleted :after ((abox abox1) (node abox-node))

  (with-slots (all-nodes old-nodes cache-sat-nodes)  abox

    (if (not *use-avl-trees-for-abox1-p*)
        (setf all-nodes (delete node all-nodes))
      (delete-from-avl-tree node all-nodes :key #'id))

    (when (and *maintain-old-nodes-p* (old-p node))
      ;;; werden momentan nicht benutzt (-> vernachlaessigt )
      (setf old-nodes (delete node old-nodes)))

    (when *maintain-cache-sat-nodes-p* 
      (setf cache-sat-nodes
            (delete node cache-sat-nodes)))))

;;;
;;;
;;;

(timed-defmethod delete-node ((abox abox) (node abox-node)  &rest args)
  (declare (ignorable args))
  (call-next-method))

(timed-defmethod delete-node ((abox abox1) (node abox-node)  &key (delete-edges-p t) &allow-other-keys)

  (if (old-p node)
      (call-next-method)

    (progn

      ;;; wird sonst von substrate7.lisp erledigt!

      (when delete-edges-p 
        (dolist (edge (outgoing node))  
          (delete-edge abox edge :error-p t))
        
        (dolist (edge (incoming node))  
          (delete-edge abox edge :error-p t))))))

;;;
;;;
;;;

(timed-defmethod delete-node :before ((abox abox) (node abox-node) &rest args)
  (declare (ignorable args))
  
  ;;; wichtig! :before! 
  ;;; mark-deleted muss die Kontextinformation haben

  (mark-deleted abox node))

;;;
;;;
;;;

(timed-defmethod activate-node :around ((abox abox) (node abox-node))
  (announce "Activating node ~A" node)
  (if (and *reflexive-checks-p* (active-p node))
      (error "Can't activate ~A, is already active!" node)
    (call-next-method)))


(defmethod activate-node :after ((abox abox) (node abox-node))
  (unregister-deterministically-expanded abox node))


(defmethod activate-node :after ((abox abox1) (node abox-node))

  (when *maintain-leaf-nodes-p*
    (unless (outgoing node)
      (pushnew node (slot-value abox 'leaf-nodes))))

  (when *maintain-deactivated-nodes-p*
    (setf (slot-value abox 'deactivated-nodes)
          (delete node (slot-value abox 'deactivated-nodes))))
  
  (when *maintain-active-nodes-p* 
    (when (and nil
               *reflexive-checks-p* 
               (heap-find-item  (slot-value abox 'active-nodes)
                                node))
      (error "already on heap!"))

    (heap-insert (slot-value abox 'active-nodes) node)))

;;;
;;;
;;;

(timed-defmethod deactivate-node :around ((abox abox) (node abox-node))
  (if (and *reflexive-checks-p* (not (active-p node)))
      (error "Can't deactivate ~A, is already deactivated!" node)
    (call-next-method)))
      

(defmethod deactivate-node :after ((abox abox) (node abox-node))  
  (announce "Deactivating node ~A" node))

(defmethod deactivate-node :after ((abox abox1) (node abox-node))

  (when *maintain-deactivated-nodes-p* 
    (push node (slot-value abox 'deactivated-nodes)))
  
  (when *maintain-leaf-nodes-p*
    (unless (outgoing node)
      (setf (slot-value abox 'leaf-nodes)
            (delete node (slot-value abox 'leaf-nodes)))
      
      (dolist (in (incoming node))
        (let ((pre (from in)))
          (unless (cdr (outgoing pre))
            (pushnew pre (slot-value abox 'leaf-nodes)))))))
    
  (delete-from-all-heaps abox node)
    
  (when *maintain-active-nodes-p*
    (heap-remove-item (slot-value abox 'active-nodes) node)
  
    (when (and nil *reflexive-checks-p* )
      (when (heap-find-item (slot-value abox 'active-nodes) node)
        (error "I've deactivated ~A, but ~A is still on active-nodes!" node)))))


;;;
;;;
;;;

(timed-defmethod unregister-deterministically-expanded ((abox abox) (node abox-node))
  (setf (slot-value node 'deterministically-expanded-p) nil))

(timed-defmethod register-deterministically-expanded ((abox abox) (node abox-node))
  (setf (slot-value node 'deterministically-expanded-p) t))



;;;
;;;
;;;

(defmethod get-state-vector ((node abox-node))
  (with-slots (initial-concept
               address
               rev-address
               succ-counter
               old-p 
               active-p
               checked-p
               deleted-p
               really-satisfiable-p
               cache-satisfiable-p
               deterministically-expanded-p
               realized-p
               stable-p 
               blocked-by
               blocking-for
               cluster-nodes) node

    (list ;old-p 
     initial-concept
     address
     rev-address
     succ-counter
     active-p
     checked-p
     deleted-p
     really-satisfiable-p
     cache-satisfiable-p
     deterministically-expanded-p
     realized-p
     stable-p 
     (copy-list blocked-by)
     (copy-list blocking-for)
     (copy-list cluster-nodes))))

(defmethod set-state-vector ((node abox-node) state &optional (maintain-index-structures-p t))
  (declare (ignorable maintain-index-structures-p))

  (let ((abox (in-graph node)))
    
    (with-slots (initial-concept               
                 address
                 rev-address
                 succ-counter
                 old-p 
              
                 active-p
                 checked-p
                 deleted-p
                 really-satisfiable-p
                 cache-satisfiable-p
                 deterministically-expanded-p
                 realized-p
                 stable-p 
                 blocked-by
                 blocking-for
               
                 cluster-nodes) node

      (let ((old-state (list active-p 
                             blocking-for
                             blocked-by
                             cache-satisfiable-p)))
      
        ;; fuer diese werden in ABox1 extra Indexstrukturen gepflegt!   


        (setf ;old-p (pop state)
         initial-concept (pop state)
         address (pop state)
         rev-address (pop state)
         succ-counter (pop state)
       
         active-p (pop state)
         checked-p (pop state)
         deleted-p (pop state)
         really-satisfiable-p (pop state)
         cache-satisfiable-p (pop state)
         deterministically-expanded-p (pop state)
         realized-p (pop state)
         stable-p  (pop state)
         blocked-by (pop state)
         blocking-for (pop state)
         cluster-nodes (pop state))


        (let ((new-state (list active-p 
                               blocking-for
                               blocked-by
                               cache-satisfiable-p)))
      

          (when maintain-index-structures-p 
            (adjust-index-structures abox node old-state new-state)
            (when *debug-p* (describe-object abox t))))))

    abox))

;;;
;;;
;;;

(timed-defmethod adjust-index-structures ((abox abox) (node abox-node) old-state new-state)
  (let ((old-active-p (pop old-state))
        (old-blocking-for (pop old-state))
        (old-blocked-by (pop old-state))
        (old-cache-satisfiable-p (pop old-state))

        (new-active-p (pop new-state))
        (new-blocking-for (pop new-state))
        (new-blocked-by (pop new-state))
        (new-cache-satisfiable-p (pop new-state))

        (*reflexive-checks-p* nil)
        (*check-if-still-blocked-p* nil))

    (unless *rollback-active-p* 
      (error "adjust index structures!"))

    (when (and old-active-p (not new-active-p))
      (deactivate-node abox node))

    (when (and (not old-active-p) new-active-p)
      (activate-node abox node))

    (let ((+blocking-for 
           (set-difference new-blocking-for old-blocking-for))
          (-blocking-for
           (set-difference old-blocking-for new-blocking-for))
          (+blocked-by
           (set-difference new-blocked-by old-blocked-by))
          (-blocked-by
           (set-difference old-blocked-by new-blocked-by)))
          

      (dolist (blocked +blocking-for)
        (register-blocking-blocked abox node blocked))

      (dolist (blocked -blocking-for)
        (unregister-blocking-blocked abox node blocked))

      (dolist (blocking +blocked-by)
        (register-blocking-blocked abox blocking node))

      (dolist (blocking -blocked-by)
        (unregister-blocking-blocked abox blocking node)))
           

    #| 
    (when (and old-blocked-by (not new-blocked-by))
      (princ "adjust index structures") (terpri)
      (unregister-blocking-blocked abox (first old-blocked-by) node))

    (when (and new-blocked-by (not old-blocked-by))
      (princ "adjust index structures") (terpri)
      (register-blocking-blocked abox (first new-blocked-by) node)) |# ))
    

;;;
;;;
;;;

(defmethod blocked-p ((node abox-node))
  (when (slot-value node 'blocked-by)
    t))

(defmethod blocking-p ((node abox-node))
  (when (slot-value node 'blocking-for)
    t))

;;;
;;;
;;;

(timed-defmethod register-blocking-blocked ((abox abox) (blocking abox-node) (blocked abox-node))
  (when (and *reflexive-checks-p* (blocked-p blocked))
    (error "Node ~A is already blocked!" blocked))

  (announce "Blocking ~A, blocked by ~A!" blocked blocking)
  
  (incf *blocked-nodes*)
  
  (deactivate-node abox blocked)

  (push blocked (slot-value blocking 'blocking-for))
  (push blocking (slot-value blocked 'blocked-by)))


(timed-defmethod register-blocking-blocked :after ((abox abox1) (blocking abox-node) (blocked abox-node))
  (when *maintain-blocked-nodes-p*
    (push blocked (slot-value (in-graph blocked) 'blocked-nodes))))

;;;
;;;
;;; 


(timed-defmethod unregister-blocking-blocked ((abox abox) (blocking abox-node) (blocked abox-node))   
  (with-slots (blocked-by) blocked
    
    (when (and *reflexive-checks-p* (not (blocked-p blocked)))
      (error "Node ~A is not blocked!" blocked))

    (if (and nil 
             ;;; hier kann man nicht einfach wieder die Blockierung herstellen, 
             ;;; dann kommt die Dependenzverwaltung durcheinander!!!!
             *check-if-still-blocked-p*

             (blocking-blocked-p (slot-value abox 'language) blocking blocked))

        (progn 
          ;;; immer noch blockiert?  
          ;;; Blockierung beibehalten 

          t)
        

      (progn 
        ;;; Blockierung aufheben 

        (announce "Unblocking ~A, blocked by ~A!" blocked blocking)
        
        (with-slots (blocking-for) blocking
          (setf blocked-by (delete blocking blocked-by))
          (setf blocking-for (delete blocked blocking-for)))

        (unless (active-p blocked)
          (activate-node abox blocked))))))


(timed-defmethod unregister-blocking-blocked :after ((abox abox1) (blocking abox-node) (blocked abox-node))   

  (when *maintain-blocked-nodes-p*
    (unless (blocked-p blocked)
      (setf (slot-value (in-graph blocked) 'blocked-nodes)
            (delete blocked (slot-value (in-graph blocked) 'blocked-nodes))))))


;;;
;;;
;;;

(timed-defmethod adjust-blocking-dependencies ((abox abox) (node abox-node))
  (when *adjust-blocking-dependencies-p* 
  
    (with-slots (blocked-by blocking-for) node
      
      (let ((*reflexive-checks-p* nil))
        ;;; Unregister-blocking-blocked meckert sonst, da
        ;;; ja blocking-blocked-Beziehungen inkrementell
        ;;; (und nicht sofort) aufgeloest werden

        (let ((blocked node))
          (dolist (blocking blocked-by)
            (unregister-blocking-blocked abox blocking blocked)))
      
        (let ((blocking node)) 
          (dolist (blocked blocking-for)
            (unregister-blocking-blocked abox blocking blocked)))))))


;;;
;;;
;;;

(timed-defmethod add-to-unexpanded ((node abox-node) items)
  (add-to-unexpanded (description node) items))

(timed-defmethod add-to-expanded ((node abox-node) items)
  (add-to-expanded (description node) items))
  
;;;
;;;
;;;

(timed-defmethod delete-from-unexpanded ((node abox-node) items)
  (delete-from-unexpanded (description node) items))

(timed-defmethod delete-from-expanded ((node abox-node) items)
  (delete-from-expanded (description node) items))

;;;
;;;
;;;


(timed-defmethod put-from-expanded-to-unexpanded ((concept concept) (node abox-node))
  (put-from-expanded-to-unexpanded concept (description node)))


(timed-defmethod put-from-unexpanded-to-expanded ((concept concept) (node abox-node))
  (put-from-unexpanded-to-expanded concept (description node)))

;;;
;;;
;;;

(timed-defmethod add-to-heaps ((abox abox) (node abox-node) (concept concept))
  t)

(timed-defmethod add-to-heaps ((abox abox1) (node abox-node) (concept concept))
  
  (with-slots (nodes-not-and-expanded
               nodes-not-atom-expanded
               nodes-not-or-expanded
               nodes-not-or-expanded1
               nodes-not-some-expanded
               nodes-not-some-expanded1
               nodes-not-feature-expanded
               nodes-not-feature-expanded1
               nodes-not-at-least-expanded
               nodes-not-at-least-expanded1) abox
     
    (typecase concept
      (atomic-concept

       (when *maintain-unexpanded-atomic-concepts-heap-p* 
         (unless (has-unexpanded-atomic-concepts-p node)
           (heap-insert nodes-not-atom-expanded node))))
        
      (and-concept

       (when *maintain-unexpanded-and-concepts-heap-p*
         (unless (has-unexpanded-and-concepts-p node)
           (heap-insert nodes-not-and-expanded node))))
        
      (or-concept

       (when *maintain-unexpanded-or-concepts1-heap-p*
         (unless (has-unexpanded-or-concepts-p node)
           ;; dann ist er schon im Heap!
           (heap-insert nodes-not-or-expanded1 node)))
           
       (when *maintain-unexpanded-or-concepts-heap-p*
         (unless (has-unexpanded-or-concepts-p node)
           ;; dann ist er schon im Heap!
           (heap-insert nodes-not-or-expanded node))))

      (at-least-concept

       (when *maintain-unexpanded-at-least-concepts1-heap-p*
         (unless (has-unexpanded-at-least-concepts-p node)
           ;; dann ist er schon im Heap!
           (heap-insert nodes-not-at-least-expanded1 node)))

       (when *maintain-unexpanded-at-least-concepts-heap-p* 
         (unless (has-unexpanded-at-least-concepts-p node)
           (heap-insert nodes-not-at-least-expanded node))))
        
      (attribute-exists-concept

       (when *maintain-unexpanded-attribute-exists-concepts1-heap-p* 
         (unless (has-unexpanded-attribute-exists-concepts-p node)
           (heap-insert nodes-not-feature-expanded1 node)))
        

       (when *maintain-unexpanded-attribute-exists-concepts-heap-p*
         (unless (has-unexpanded-attribute-exists-concepts-p node)
           (heap-insert nodes-not-feature-expanded node))))
        
      (some-concept
           
       (when *maintain-unexpanded-some-concepts1-heap-p*
         (unless (has-unexpanded-some-concepts-p node)
           (heap-insert nodes-not-some-expanded1 node)))

       (when *maintain-unexpanded-some-concepts-heap-p*
         (unless (has-unexpanded-some-concepts-p node)
           (heap-insert nodes-not-some-expanded node)))))))


(timed-defmethod delete-from-heaps ((abox abox) (node abox-node) (concept concept) &optional force-p)
  (declare (ignorable force-p))
  t)

(timed-defmethod delete-from-heaps ((abox abox1) (node abox-node) (concept concept) &optional force-p)

  (with-slots (nodes-not-and-expanded
               nodes-not-atom-expanded
               nodes-not-det-expanded 
               nodes-not-or-expanded
               nodes-not-or-expanded1
               nodes-not-some-expanded
               nodes-not-some-expanded1
               nodes-not-feature-expanded
               nodes-not-feature-expanded1
               nodes-not-at-least-expanded
               nodes-not-at-least-expanded1) abox

    (typecase concept
      (atomic-concept

       (when *maintain-unexpanded-atomic-concepts-heap-p*  
         (when (or force-p (not (has-unexpanded-atomic-concepts-p node)))
           (heap-remove-item nodes-not-atom-expanded node))))           
          
      (and-concept

       (when *maintain-unexpanded-and-concepts-heap-p*  
         (when (or force-p (not (has-unexpanded-and-concepts-p node)))
           (heap-remove-item nodes-not-and-expanded node))))

      (or-concept
           
       (when *maintain-unexpanded-or-concepts1-heap-p*  
         (when (or force-p (not (has-unexpanded-or-concepts-p node)))
           (heap-remove-item nodes-not-or-expanded1 node)))

       (when *maintain-unexpanded-or-concepts-heap-p*  
         (when (or force-p (not (has-unexpanded-or-concepts-p node)))
           (heap-remove-item nodes-not-or-expanded node))))

      (at-least-concept

       (when *maintain-unexpanded-at-least-concepts1-heap-p*  
         (when (or force-p (not (has-unexpanded-at-least-concepts-p node)))
           (heap-remove-item nodes-not-at-least-expanded1 node)))

       (when *maintain-unexpanded-at-least-concepts-heap-p*  
         (when (or force-p (not (has-unexpanded-at-least-concepts-p node)))
           (heap-remove-item nodes-not-at-least-expanded node))))

      (attribute-exists-concept

       (when *maintain-unexpanded-attribute-exists-concepts1-heap-p*
         (when (or force-p (not (has-unexpanded-attribute-exists-concepts-p node)))
           (heap-remove-item nodes-not-feature-expanded1 node)))

       (when *maintain-unexpanded-attribute-exists-concepts-heap-p*
         (when (or force-p (not (has-unexpanded-attribute-exists-concepts-p node)))
           (heap-remove-item nodes-not-feature-expanded node))))

      (some-concept

       (when *maintain-unexpanded-some-concepts1-heap-p* 
         (when (or force-p (not (has-unexpanded-some-concepts-p node)))
           (heap-remove-item nodes-not-some-expanded1 node)))
           
       (when *maintain-unexpanded-some-concepts-heap-p* 
         (when (or force-p (not (has-unexpanded-some-concepts-p node)))
           (heap-remove-item nodes-not-some-expanded node)))))))

;;;
;;;
;;;


(timed-defmethod delete-from-all-heaps ((abox abox1) (node abox-node))
  
  (with-slots (nodes-not-and-expanded
               nodes-not-atom-expanded
               nodes-not-det-expanded 
               nodes-not-or-expanded
               nodes-not-or-expanded1
               nodes-not-some-expanded
               nodes-not-some-expanded1
               nodes-not-feature-expanded
               nodes-not-feature-expanded1
               nodes-not-at-least-expanded
               nodes-not-at-least-expanded1) abox

    (when *maintain-unexpanded-atomic-concepts-heap-p* 
      (heap-remove-item nodes-not-atom-expanded node))

    (when *maintain-unexpanded-and-concepts-heap-p*
      (heap-remove-item nodes-not-and-expanded node))

    (when *maintain-unexpanded-or-concepts-heap-p*
      (heap-remove-item nodes-not-or-expanded node))

    (when *maintain-unexpanded-at-least-concepts-heap-p*
      (heap-remove-item nodes-not-at-least-expanded node))
    
    (when *maintain-unexpanded-attribute-exists-concepts-heap-p*
      (heap-remove-item nodes-not-feature-expanded node))
    
    (when *maintain-unexpanded-some-concepts-heap-p*
      (heap-remove-item nodes-not-some-expanded node))

    (when (old-p node)

      (when *maintain-unexpanded-or-concepts1-heap-p*
        (heap-remove-item nodes-not-or-expanded1 node))

      (when *maintain-unexpanded-at-least-concepts1-heap-p*
        (heap-remove-item nodes-not-at-least-expanded1 node))

      (when *maintain-unexpanded-attribute-exists-concepts1-heap-p*
        (heap-remove-item nodes-not-feature-expanded1 node))

      (when *maintain-unexpanded-some-concepts1-heap-p*
        (heap-remove-item nodes-not-some-expanded1 node)))))


;;;
;;;
;;;

(timed-defmethod register-already-known-to-be-satisfiable ((abox abox) (abox-node abox-node))
  (when (initial-concept abox-node)
    (register-concept-is-satisfiable (initial-concept abox-node))))

(timed-defmethod register-already-known-to-be-inconsistent ((abox abox) (abox-node abox-node))
  (when (and *reflexive-checks-p* (not (initial-concept abox-node)))
    (error "no initial concept!"))
  
  (register-concept-is-inconsistent (initial-concept abox-node)))

;;;
;;;
;;;

(timed-defmethod register-cache-satisfiable ((abox abox) (abox-node abox-node))
  (when (and *reflexive-checks-p* (cache-satisfiable-p abox-node))
    (error "Node ~A is already registered as cache satisfiable!" abox-node))

  (setf (slot-value abox-node 'cache-satisfiable-p) t))

(timed-defmethod register-cache-satisfiable :after ((abox abox1) (abox-node abox-node))

  (delete-from-all-heaps abox abox-node)

  (when *maintain-cache-sat-nodes-p*
    (push abox-node (slot-value (in-graph abox-node) 'cache-sat-nodes))))
  
;;;
;;;
;;;

(timed-defmethod register-label-is-stable ((abox abox) (node abox-node)) 

  (register-action change-state nil node)

  (setf (stable-p node) t)
  
  (when *reflexive-checks-p*
    (when (initial-concept node)
      (error "Node ~A alread has an initial concept!" node)))

  (when (or (and *cache-models-p*
                 (root-or-old-p node))

            *subtableau-caching-p*

            ;; beim Subtableau-caching werden ALLE
            ;; Knoten gecached  
            )

    ;; nur wenn das initial-concept gesetzt wird, 
    ;; wird fuer diese Konzept in (defrule cache-models )
    ;; das Modell gespeichert! die Existenz von initial-concept
    ;; bestimmt also, was gecached wird!
    
    (setf (slot-value node 'initial-concept)
          (get-node-concept abox node))))

;;;
;;;
;;;

(defmethod get-node-concept ((abox abox) (node abox-node))
  (let ((concepts nil))

    (loop-over-node-concepts (concept node)
      (push concept concepts))
    
    (with-enabled-concept-store
     (with-protected-concept-store
      (make-and-concept concepts)))))



;;;
;;;
;;;

(timed-defmethod is-successor-p ((succ abox-node) (of abox-node))
  
  (let ((nodes nil)) 

    (labels ((do-it (node)
               (unless (marked-p node)
                 (mark node)
                 (push node nodes)
                 
                 (or (eq node of)
                     (some #'(lambda (in) 
                               (do-it (from in)))
                           (incoming node))))))

      (with-new-marking-context 
       (prog1 
           (do-it succ)
         (unmark nodes))))))

;;;
;;;
;;;

(timed-defmethod activate-node ((abox abox) (item abox-item))
  (register-action change-state nil item)
  (setf (slot-value item 'active-p) t)
  (register-action change-state nil item))
  

(timed-defmethod deactivate-node ((abox abox) (item abox-item))
  (register-action change-state nil item)
  (setf (slot-value item 'active-p) nil)
  (register-action change-state nil item))


;;;
;;;

(timed-defmethod mark-deleted ((abox abox) (item abox-item))
  (register-action change-state nil item)
  (setf (slot-value item 'deleted-p) t)
  (register-action change-state nil item)
  (when (active-p item)
    (deactivate-node abox item)))

(timed-defmethod unmark-deleted ((abox abox) (item abox-item))
  (register-action change-state nil item)
  (setf (slot-value item 'deleted-p) nil)
  (register-action change-state nil item)
  (unless (active-p item)
    (activate-node abox item)))

