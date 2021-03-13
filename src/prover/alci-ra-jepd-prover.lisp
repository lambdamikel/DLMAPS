;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpersistentclass jepd-abox (rolebox-abox jepd-substrate)))

(defmethod get-language ((abox jepd-abox))
  +alci-ra-jepd+)

;;;
;;;
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpersistentclass jepd-abox-edge (jepd-substrate-edge rolebox-abox-edge)))

;;;
;;;
;;;

(defmethod get-standard-edge-description-class ((abox jepd-abox))
  'jepd-edge-label)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpersistentclass jepd-edge-label (edge-label)))

(defmethod initialize-description ((description jepd-edge-label))
  (labels ((check-role (role)
             (if (and (typep role 'role)
                      (not (or (typep role 'simple-role)
                               (typep role 'or-role))))
                 (error "Can't use role ~A for a JEPD abox!" (textual-description description))
               role)))
         
    (let ((role (textual-description description)))
      (check-role role)
      (unless (typep role 'role)
        (setf (textual-description description)
              (check-role
               (parse-role role)))))
    description))

;;;
;;;
;;;

(defmethod get-standard-node-class ((jepd-abox jepd-abox))
  'jepd-abox-node)

(defmethod get-standard-edge-class ((jepd-abox jepd-abox))
  'jepd-abox-edge)

;;;
;;;
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpersistentclass jepd-abox-node (rolebox-abox-node jepd-substrate-node)))

(defun jepd-abox-node-p (x)
  (typep x 'jepd-abox-node))

;;;
;;;
;;;

(defmethod role-consistent-p ((abox jepd-abox) &rest args)
  (declare (ignore args)) 
  (consistent-p abox
                :check-node-labels-p nil ; macht keinen Sinn!
                :check-edge-labels-p nil ; nur die Frame-Conditions checken!
                ))

(defmethod make-and-description ((descr role) &key descriptions)
  (let ((descr 
         (mapcar #'textual-description 
                 (reduce #'intersection 
                         (append  
                          (mapcar #'list (remove-if #'or-role-p (cons descr descriptions)))
                          (mapcar #'arguments (remove-if-not #'or-role-p (cons descr descriptions))))))))
    (when descr
      (parse-role `(or ,@descr)))))

(defmethod get-implied-label ((edge jepd-abox-edge) &key &allow-other-keys)
  (if (reflexive-edge-p edge)
      (description edge)
    (if (get-choice-points edge) 
        (description edge)
      ;; fresh edge, added by "add missing edges"!
      (with-slots (rbox) (in-graph edge)    
        (let* ((support 
                (remove-if #'(lambda (x)
                               (or (member edge x)
                                   (some #'reflexive-edge-p x)))
                           (get-support edge)))
               (comps (mapcar #'(lambda (supp)
                                  (list (lookup rbox (first supp) (second supp))
                                        supp))
                              support))

               (old-label (textual-description (description edge)))

               (relevant-comps-and-support
                (remove-if #'(lambda (x)
                               (let ((rx (first x)))
                                 (or (implies-p old-label rx)
                                     (some #'(lambda (y)     
                                               (let ((ry (first y)))
                                                 (and (implies-p ry rx)
                                                      (not (eq ry rx)))))
                                           comps))))
                           comps)))

          (if (not relevant-comps-and-support)

              (description edge)
             
            (let ((relevant-support
                   (apply #'append (mapcar #'second relevant-comps-and-support)))
                  
                  (new-label (make-and-description ;;; ineffizient!!!
                                                   old-label
                                                   :descriptions
                                                   (mapcar #'(lambda (comp)
                                                               (change-textual-description 
                                                                (copy (description edge)) comp))
                                                           (mapcar #'first relevant-comps-and-support)))))

              (unless new-label
                (return-from get-implied-label 
                  (values nil (remove-duplicates relevant-support))))
          
              (change-textual-description (description edge) new-label)
              
              (when *debug-p* 
                (format t "*** REGISTERING SUPPORT FOR ~A: ~A!~%" edge relevant-comps-and-support))
              
              (register-as-unexpanded edge
                                      :depends-on
                                      (apply #'append (mapcar #'second relevant-comps-and-support)))
              
              (description edge))))))))

;;;
;;;
;;;
    
(defrule abox-completion (alci-ra-jepd jepd-abox)
  (announce "Adding missing edges on level ~A" level)
  
  (add-missing-edges abox)

  (multiple-value-bind (abox clash-reasons)
      (compute-minimal-label abox)
    (unless abox
      (when *debug-p*
        (format t "*** COMPUTE-MINIMAL-LABEL ON LEVEL ~A RETURNED NIL! CLASH-REASONS: ~A!~%" level clash-reasons)
        (format t "    RETURNING CHOICE POINTS ~A!~%~%" (mapcar #'get-choice-points clash-reasons)))
      (return-from prover
        (values nil 
                (apply #'append (mapcar #'get-choice-points
                                        clash-reasons)))))
    (when *debug-p*   
      (format t "*** ABOX EDGES AFTER COMPUTE-MINIMAL-LABEL ON LEVEL ~A:~%" level)             
      (describe-object abox t))
    
    +insert-body-code+ ))

;;;
;;;
;;;

(defmethod add-missing-edges ((abox jepd-abox) &key &allow-other-keys)

  (apply #'call-next-method abox
         
         :edge-constructor 
         #'(lambda (from to)
             (create-edge abox 
                          from to
                          (make-edge-description 
                           (get-standard-edge-description-class abox)
                           (cons 'or (full-disjunctive-role 
                                      (substrate-rbox abox))))
                          :depends-on (append (created-by from)
                                              (created-by to))
                          :register-choice-points-p nil
                          :create-inverse-p t))
         nil))


;;;
;;; 
;;;

(define-prover ((abox-sat alci-ra-jepd jepd-abox)) 
  (:init   
   (start-main))

  (:main 
   (perform (abox-completion)
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


#|

(progn 

  (with-rbox (rcc5-rolebox)
    (with-tbox (test)
      (with-abox (test :delete-if-exists-p t)
    
        (ins a (and a
                    (all dr (not b))
                    (all po (not b))
                    (all ppi (not b))
                    (all pp (not b))
                    (all eq (not c))))
    
        (ins b b)
    
        (true! (abox-sat-p *cur-abox*))

        (ins b c)

        (false! (abox-sat-p *cur-abox*))))))


(with-rbox (rcc5-rolebox)
  (with-tbox (test)
    (with-abox (test :delete-if-exists-p t)

      (ins a a)
      (ins b b)

      (true! (abox-sat-p *cur-abox*)))))


(with-rbox (rcc5-rolebox)
  (with-tbox (test)
    (with-abox (test :delete-if-exists-p t)

      (ins a A)
      (ins b b)
      (ins c c)
      (rel a b pp)
      (rel b c pp)
      (rel a c (or dr ppi))

      (false! (abox-sat-p *cur-abox*)))))


(with-rbox (rcc5-rolebox)
  (with-tbox (test) 
    (with-abox (test :delete-if-exists-p t)

      (ins a a)
      (ins b b)
      (ins c c)
      (rel a b pp)
      (rel b c pp)
      (rel a c (or po ppi eq eq pp))

      (true! (abox-sat-p *cur-abox*)))))

(progn 

  (delete-all-tboxes) 

  (with-rbox (rcc5-rolebox)
    
    (with-tbox (meta :delete-if-exists-p t)

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
      
      (with-abox (test :delete-if-exists-p t)
        
        (ins a (and linear-time
                    (some ppi (some ppi (some ppi (some ppi  c))))
                    (some ppi (some ppi (some ppi
					      (some ppi
						    (some ppi
							  (some ppi 
								(some ppi
								      (some ppi
									    (some ppi
										  (some ppi d))))))))))
                    left-bounded
                    right-bounded))
      
        (time (true! (abox-sat? test 
                                #+:clim
				:completion-found-hook 
				#+:clim
                                #'(lambda (x) (visualize
                                               (reorder-ppi x)))
                                :debug-p nil)))))))


(with-rbox (rcc5-rolebox)
  
  (with-tbox (test :delete-if-exists-p t)
    
    (with-abox (test :delete-if-exists-p t)
      
      (ins a a)
      (ins b b)
      (ins c c)    
      
      (rel a b pp)
      (rel b c pp)

      (ins c (all (inv pp) (not a)))

      (false! (abox-Sat-p *cur-abox*)))))


|#


