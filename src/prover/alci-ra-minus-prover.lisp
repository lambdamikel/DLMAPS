;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpersistentclass rolebox-abox (abox rolebox-substrate)))

;;;
;;;
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpersistentclass rolebox-abox-edge (rolebox-substrate-edge abox-edge)))

(defmethod inverse-edge ((edge rolebox-abox-edge))
  (slot-value edge 'inverse-edge))

;;;
;;;
;;;

(defun make-role-from-lookup-result (rbox res)
  (let ((*cur-rbox* rbox))
    (when res
      (if (cdr res)
          (parse-role `(or ,@res))
        (parse-role (first res))))))


(defmethod lookup ((substrate rolebox-abox) (r rolebox-abox-edge) (s rolebox-abox-edge))
  (lookup (substrate-rbox substrate)  
          (description r) (description s))) ; edge -> label

(defmethod lookup ((rbox rolebox) (r edge-label) (s edge-label))
  (lookup rbox
          (textual-description r) ; label -> role
          (textual-description s)))

(defmethod lookup ((rbox rolebox) (r simple-role) (s simple-role)) 
  (make-role-from-lookup-result rbox
                                (lookup rbox
                                        (textual-description r) 
                                        (textual-description s))))

(defmethod lookup ((rbox rolebox) (r or-role) (s or-role))
  (make-role-from-lookup-result rbox
                                (lookup rbox
                                        (rest (textual-description r)) ;; OR entfernen
                                        (rest (textual-description s)))))

(defmethod lookup ((rbox rolebox) (r or-role) (s simple-role))
  (make-role-from-lookup-result rbox
                                (lookup rbox 
                                        (rest (textual-description r))
                                        (textual-description s))))
     
(defmethod lookup ((rbox rolebox) (r simple-role) (s or-role))
  (make-role-from-lookup-result rbox
                                (lookup rbox 
                                        (textual-description r)
                                        (rest (textual-description s)))))
     

;;;
;;;
;;;

(defmethod make-inverse-description ((rbox rolebox) (description edge-label))
  (make-edge-description (type-of description)
                         (or (get-inverse-role (textual-description description))
                             (break "No inverse role for ~A?" (textual-description description))
                             (get-inverse-role (textual-description description)))))

;;;
;;;
;;;

(defmethod get-standard-node-class ((rolebox-abox rolebox-abox))
  'rolebox-abox-node)

(defmethod get-standard-edge-class ((rolebox-abox rolebox-abox))
  'rolebox-abox-edge)

;;;
;;;
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpersistentclass rolebox-abox-node (abox-node rolebox-substrate-node)))

;;;
;;; Überladen! Denn nun sind, dank der Role Box, alle inversen Kanten 
;;; explizit verzeichnet
;;; 

(defmethod add-a-choice-point ((edge rolebox-abox-edge) choice-point &key &allow-other-keys)
  (push choice-point (slot-value edge 'choice-points))
  (push choice-point (slot-value (description (inverse-edge edge)) 'choice-points)))
   

(defmethod set-choice-points ((edge rolebox-abox-edge) choice-points &key &allow-other-keys)  
  (let ((points (ensure-list choice-points)))
    (setf (slot-value edge 'choice-points) points)
    (setf (slot-value (description (inverse-edge edge)) 'choice-points) points)))
   
(defmethod manager-fn ((sel-edge rolebox-abox-edge) edges processed-edges &rest args)
  (declare (ignorable edges processed-edges args))
  (call-next-method))

(defmethod edge-setter ((edge rolebox-abox-edge) (role simple-role) &key remaining &allow-other-keys)
  (declare (ignore remaining))
  (change-textual-description (description edge) role)
  (change-textual-description (description (inverse-edge edge))
                              (get-inverse-role role))
  
  edge)

;;;
;;;
;;;  


(defmethod apply-rolebox-axioms ((substrate rolebox-abox) &rest args &key depends-on from-node &allow-other-keys)

  (apply #'call-next-method substrate 
         :edge-constructor 
         #'(lambda (from to comp &key support)
             (let ((edge 
                    (create-edge substrate 
                                 from to 
                                 (make-edge-description
                                  (get-standard-edge-description-class substrate)
                                  comp)
                                 :from-node from-node
                                 :depends-on (append depends-on support))))
               edge))
         args))


(defrule rolebox-application (alci-ra-minus rolebox-abox)

  (apply-rolebox-axioms abox)
         
  (when *debug-p* 
    (announce "Edges after Rolebox Application: ")
    (dolist (edge (get-edges abox))
      (describe-object edge t)
      (terpri))
    (terpri))

  +insert-body-code+)

;;;
;;;
;;;

(define-prover ((abox-sat alci-ra-minus rolebox-abox))
  (:init   
   (start-main))
  (:main 
   (perform (rolebox-application)
     (:body 
      (perform (abox-enumeration)
        (:body 
         (perform (deterministic-expansion)
           (:body 
            (if clashes
                (handle-clashes)                   
              (perform (or-expansion)
                (:positive 
                 (if clashes
                     (handle-clashes)
                   (restart-main)))
                (:negative 
                 (perform (some-expansion)
                   (:positive
                    (if clashes
                        (handle-clashes)
                      (restart-main)))
                   (:negative 
                    (success)))))))))
        (:after-successful-clash-repair 
         (restart-main))))))
  (:success 
   (completion-found)))




(defmethod reorder-ppi ((abox rolebox-abox))
  (let* ((copy (copy abox))
         (nodes (get-nodes copy))
         (order nil)
         (*cur-rbox* +rcc5-rolebox+))

    (with-abox* (abox) 
                                                            
      (loop while nodes do
            (let ((smallest
                   (remove-if #'(lambda (n)
                                  (get-role-successors n (parse-role 'pp)))
                              nodes)))
              (when (or (not smallest)
                        (cdr smallest))       
                (break "~A No linear structure present! Tree structure? DAG?" smallest))            
              (let ((smallest (first smallest)))
                (delete-node copy smallest)
                (push smallest order)
                (setf nodes (remove smallest nodes)))))

      (scramble abox 
                :new-order (mapcar #'(lambda (node)
                                       (find (id node)
                                             (get-nodes abox)
                                             :key #'id
                                             :test #'=))
                                   (reverse order))))))



(defmethod has-eq-edges-p ((abox rolebox-abox))
  (let ((*cur-rbox* +rcc5-rolebox+))                    
    (some #'(lambda (node)
              (cdr (get-role-successors node (parse-role 'eq))))
          (get-nodes abox))))


;;;
;;;
;;;

#|


(progn 

  (delete-all-tboxes)

  (with-rbox (test
              :delete-if-exists-p t
              :roles (r s t)
              :inverse-roles ((s r))
              :reflexive-roles (t)
              :axioms 
              ((r r (s r t))))

    (with-tbox (test :delete-if-exists-p t)
  
      (with-abox (test :delete-if-exists-p t)

        (instance a (some r (some r c)))

        (instance a d) 

        (instance a (all r (not c)))
        (instance a (all s (all (inv r) (all (inv r) (not d)))))

        (true! (abox-consistent?))

        (instance a (all t (not c)))

        (false! 
         (abox-consistent-p *cur-abox*))))))


(progn 

  (delete-all-tboxes)

  (with-rbox (test
              :delete-if-exists-p t
              :roles (r)
              :inverse-roles ((r r))
              :reflexive-roles (r)
              :axioms 
              ((r r (r))))

    (with-tbox (test :delete-if-exists-p t
                     :type 'rolebox-abox)


      (implies A B)
      
      (equivalent A (some r E))
      
      (equivalent C (all r B))

      (setf *print-pretty* t)

      ;;; MIDELORA-BUG wenn reuse-nodes-p verwendet wird!!!

      (princ (subsumes? c a :debug-p t :reuse-nodes-p nil :semantic-branching-p t))

      ;(visualize-taxonomy)
      )))



(with-rbox (rcc5-rolebox)
  
  (with-tbox (test :delete-if-exists-p t)
    
    (with-abox (test :delete-if-exists-p t
                     :type 'rolebox-abox)
      
      (ins a (and a
                  (all dr (not c))
                  (all po (not c))))
      
      (ins b b)
      (ins c c)    

      (rel a b dr)
      (rel b c pp)
    
      (true! (abox-consistent?))

      (ins c (or (all (inv pp) (not a))
                 (all ppi (not a))))

      (false! (abox-consistent?)))))


(progn 

  (delete-all-tboxes)

  (with-rbox (rcc5-rolebox)

    (with-abox (test :delete-if-exist-p t
                     :type 'rolebox-abox)
    
      (instance a a)

      (instance a (all eq b))

      (true! (abox-consistent-p *cur-abox*))

      (instance a (not b))

      (false! (abox-consistent-p *cur-abox* )))))
    
;;; 
;;; Härtetest! Funktioniert jetzt!!!
;;;


(progn 

  (delete-all-tboxes)
  
  (with-rbox (rcc5-rolebox)

    (setf *print-pretty* t)

    (with-tbox (meta :delete-if-exists-p t
                     :type 'rolebox-abox)

      (def* linear-time
            (and (all dr bottom)
                 (all ec bottom)
                 (all po bottom)
                 (all ppi linear-time)
                 (all pp linear-time)))
      
      (def* dense 
            (=> (some ppi top)
                (some ppi (some ppi top))))

      (def* right-bounded 
            (some ppi (and last-node
                           (all ppi bottom))))

      (def* left-bounded
            (some pp (and first-node 
                          (all pp bottom))))

      (def* left-unbounded
            (all pp (some pp top)))
    
      (def* right-unbounded
            (all ppi (some ppi top)))
      
      (with-abox (test :type 'rolebox-abox :delete-if-exists-p t)
    
        (ins a (and linear-time
                    (some ppi (some ppi (some ppi (some ppi (some ppi (some ppi dead))))))
                    left-bounded
                    right-bounded))
      
        (time (true! (abox-sat? test
                                :language +alci-ra-minus+ 
                                #+:clim
				:completion-found-hook 
                                #+:clim
				#'(lambda (x) (visualize (reorder-ppi x)))
                                :debug-p nil)))))))


(progn 
  (delete-all-tboxes)
  
  (with-rbox (rcc5-rolebox)
    (with-tbox (test)
      (with-abox (test)

        (ins a (or (and (some pp (and c (all eq (not c)))))
                   (and (some dr (and c (all eq (not d)))))))
      
        (true! (abox-sat? test))))))



(progn 
  (delete-all-tboxes)
  
  (with-rbox (rcc5-rolebox)
    (with-tbox (test)
      (with-abox (test)

        (ins a (and (some pp (some pp d))
                    (or (all pp (not d))
                        (some dr x))))

        (true! (abox-sat? test))))))


(progn 

  (delete-all-tboxes)

  (with-rbox (test
              :delete-if-exists-p t
              :roles (r s tt u w x)
              :inverse-roles ((tt w))
              :reflexive-roles (x)
              :axioms 
              ((r s (tt u))))

    (with-tbox (test :delete-if-exists-p t)
  
      (with-abox (test :delete-if-exists-p t)

        (instance c (all w (not a)))
        (instance a (all u (not c)))

        (instance c c)
        (instance a a)

        (related a b r)
        (related b c s)

        (false! 
         (abox-consistent-p *cur-abox*
                            :visualize-p t))))))



|#




