;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(defmethod get-all-roles ((tbox tbox))
  (let ((roles nil))
    (maphash #'(lambda (name role)
                 (declare (ignorable name))
                 (push role roles))
             (slot-value (concept-store tbox) 'roles))
    (remove-duplicates (sort roles #'< :key #'id))))

(defmethod get-all-transitive-roles ((tbox tbox))
  (remove-if-not #'transitive-p (get-all-roles tbox)))

(defmethod get-all-features ((tbox tbox))
  (remove-if-not #'feature-p (get-all-roles tbox)))

;;;
;;;
;;;

(defmethod define-role ((role-name symbol) 
                        &key 

                        symmetric
                        
                        feature feature-p
                        inverse-feature inverse-feature-p

                        (inverse `(inv ,role-name)) 
                        
                        parent
                        parents 
                        
                        inverse-parent
                        inverse-parents

                        transitive 
                        transitive-p 

                        (domain 'top)
                        (range 'top))

  (with-concept-store (*cur-store*)

    (let* ((*create-inverse-role-p* nil)

           (role (or (find-role role-name)
                     (parse-role role-name)))

           (inverse-role-name inverse)

           (symmetric-p (or symmetric
                            (symmetric-role role)
                            (equal role-name inverse-role-name)))

           (inverse-role (if symmetric-p
                             role
                           (or (find-role `(inv ,role-name))
                               (parse-role inverse-role-name))))

           (symmetric-p (or symmetric-p 
                            (symmetric-role inverse-role)))

           (transitive-p (or transitive
                             transitive-p 
                             (transitive-p role)
                             (transitive-p inverse-role)))

           (domain (make-and-concept
                    (remove nil (list (when (role-domain role) 
                                        (role-domain role))
                                      (when (role-range inverse-role) 
                                        (role-range inverse-role))
                                      (parse-concept domain)))))
            
           (range (make-and-concept 
                   (remove nil (list (when (role-range role) 
                                       (role-range role))
                                     (when (role-domain inverse-role)
                                       (role-domain inverse-role))
                                     (parse-concept range)))))
           
           (feature-p (or feature-p 
                          feature
                          (feature-p role)))

           (inverse-feature-p (or inverse-feature-p 
                                  inverse-feature
                                  (feature-p inverse-role)))
            
           (parents (append (mapcar #'define-role 
                                    (append (ensure-list parent) (ensure-list parents)))
                            (superroles role)))

           (inverse-parents (append (mapcar #'define-role
                                            (append (ensure-list inverse-parent) (ensure-list inverse-parents)))
                                    (superroles inverse-role))))

      (register-inverses role inverse-role)
      
      (register-name-for-role inverse-role inverse-role-name)
      (register-name-for-role inverse-role `(inv ,role-name))

      (unless (consp inverse-role-name)
        (register-name-for-role role `(inv ,inverse-role-name)))

      (when parents
        (register-superroles role parents))

      (when inverse-parents
        (register-superroles inverse-role inverse-parents))
    
      (when transitive-p 
        (register-transitive role))
      
      (when symmetric-p
        (register-symmetric role))
      
      (when feature-p 
        (register-feature role))

      (when inverse-feature-p 
        (register-feature inverse-role))

      (when domain
        (register-domain role domain))
      
      (when range
        (register-range role range))

      (unless (parse-name role)
        (setf (slot-value role 'parse-name) role-name))

      (unless (parse-name inverse-role)
        (setf (slot-value inverse-role 'parse-name) inverse-role))

      role)))

(defmethod define-role (role-name &key &allow-other-keys)
  (error "*** BAD ROLE NAME ~A GIVEN!" role-name)) 


(defmethod register-superroles ((role role) superroles)
  (dolist (sr superroles)
    (pushnew sr (slot-value role 'superroles)))
  
  role)

(defmethod register-domain ((role role) (concept concept))
  (setf (role-domain role) concept)
  role)

(defmethod register-range ((role role) (concept concept))  
  (setf (role-range role) concept)
  role)

(defmethod register-transitive ((role simple-role))
  (setf (slot-value role 'transitive-p) t
        (slot-value (slot-value role 'inverse-role)
                    'transitive-p) t)
  role)

(defmethod register-symmetric ((role simple-role))
  (setf (slot-value role 'symmetric-role) t)
  role)

(defmethod register-feature ((role simple-role))
  (setf (slot-value role 'feature-p) t)
  role)

;;;
;;;
;;;

(defmethod prepare-roles ((tbox tbox))
  (labels ((cyclic-p (role current)             
             (or (member role (superroles current))
                 (some #'(lambda (x) 
                           (cyclic-p role x))
                       (superroles current))))

           (get-all-superroles (role)
             (remove-duplicates 
              (append (superroles role)
                      (reduce #'append
                              (mapcar #'get-all-superroles (superroles role)))))))
  
    (with-slots (concept-store) tbox
      (with-slots (roles all-store-roles) concept-store
        
        (dolist (role all-store-roles)
          (let* ((inv-role (get-inverse-role role))
                 
                 (superroles
                  (remove-duplicates
                   (append (superroles role)
                           (mapcar #'get-inverse-role (superroles inv-role)))))
                 
                 (inv-superroles 
                  (mapcar #'get-inverse-role superroles)))

            (setf (slot-value role 'superroles) superroles
                  (slot-value inv-role 'superroles) inv-superroles)
              
            (dolist (sr superroles)
              (pushnew role (slot-value sr 'subroles)))
            
            (dolist (sr inv-superroles)
              (pushnew inv-role
                       (slot-value sr 'subroles)))))
        
        (dolist (role all-store-roles)
          (when (is-simple-role-p role)
            (when (cyclic-p role role)
              (error "*** CYCLE IN ROLE HIERARCHY DETECTED FOR ~A!" role))
            (let ((all-srs (get-all-superroles role)))
              (setf (slot-value role 'all-superroles) all-srs)                  
              (dolist (sr all-srs)
                (pushnew role (slot-value sr 'all-subroles))))))
          
        (dolist (role all-store-roles)
          (when (is-simple-role-p role)
              
            (let ((inv-role (get-inverse-role role)))            
              
              (register-domain role 
                               (make-and-concept (remove nil
                                                         (remove-duplicates 
                                                          (append
                                                           (cons (role-domain role)
                                                                 (mapcar #'role-domain (all-superroles role)))
                                                           (when inv-role
                                                             (cons (role-range inv-role)
                                                                   (mapcar #'role-range (all-superroles inv-role)))))))))

              (register-range role
                              (make-and-concept (remove nil
                                                        (remove-duplicates 
                                                         (append
                                                          (cons (role-range role)
                                                                (mapcar #'role-range (all-superroles role)))
                                                          (when inv-role
                                                            (cons (role-domain inv-role)
                                                                  (mapcar #'role-domain (all-superroles inv-role))))))))))))

        (let ((changed t))

          ;;; Fixpunkt, alle Domain/Range-Konzepte f. die komplexen Rollen berechnen 
            
          (loop while changed do
                  
                (setf changed nil)
                  
                (dolist (role all-store-roles)

                  (when (is-and-role-p role) 
                    (when (and (not (role-domain role))
                               (every #'role-domain (arguments role)))
                      (setf changed t)
                      (register-domain role (make-and-concept (mapcar #'role-domain (arguments role)))))
                    (when (and (not (role-range role))
                               (every #'role-range (arguments role)))
                      (setf changed t)
                      (register-range  role (make-and-concept (mapcar #'role-range  (arguments role))))))
                    
                  (when (is-or-role-p role) 
                    (when (and (not (role-domain role))
                               (every #'role-domain (arguments role)))
                      (setf changed t)
                      (register-domain role (make-or-concept (mapcar #'role-domain (arguments role)))))
                    (when (and (not (role-range role))
                               (every #'role-range (arguments role)))
                      (setf changed t)
                      (register-range  role (make-or-concept (mapcar #'role-range  (arguments role))))))))))))
  tbox)
  

;;;
;;;
;;;

(defun has-common-parent-feature (a b)
  (and (feature-p a) 
       (feature-p b)
       (some #'(lambda (a)
                 (and (feature-p a)
                      (some #'(lambda (b) 
                                (and (feature-p b)
                                     (eq a b)))
                            (cons b (all-superroles b)))))
             (cons a (all-superroles a)))))

;;;
;;;
;;;

(defun create-and-role (roles &key feature-p) 
  (let ((roles (remove-duplicates roles)))
    (if (cdr roles)
        (make-instance 'and-role
                       :textual-description 
                       `(and ,@(mapcar #'textual-description roles))
                       :feature-p feature-p
                       :constructor-sym 'reparse-role
                       :feature-p t
                       :arguments roles

                       :superroles roles
                       :all-superroles (reduce #'append
                                               (mapcar #'all-superroles roles))
                       :concept-store *cur-store*
                       :tbox *cur-tbox*
                       :role-domain (make-and-concept (mapcar #'role-domain roles))
                       :role-range (make-and-concept (mapcar #'role-range roles)))
      (first roles))))

;;;
;;;
;;;

(defmacro defrole (role &rest args)
  `(define-role ',role 
                ,@(mapcar #'(lambda (x)
                              `(quote ,x))
                          args)))

(defmacro deffeature (role &rest args)
  `(define-role ',role :feature-p t 
                ,@(mapcar #'(lambda (x)
                              `(quote ,x))
                          args)))

(defmacro subrole (role &rest superroles)
  `(let ((role (parse-role ',role))
         (superroles (mapcar #'parse-role ',superroles)))
     (register-superroles role superroles)))

(defmacro domain (role concept)
  `(let ((role (parse-role ',role))
         (concept (parse-concept ',concept)))
     (register-domain role concept)))

(defmacro range (role concept)
  `(let ((role (parse-role ',role))
         (concept (parse-concept ',concept)))
     (register-range role concept)))

(defmacro transitive (role)
  `(register-transitive (parse-role ',role)))

