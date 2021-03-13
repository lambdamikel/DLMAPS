;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

(defvar *cur-rbox* nil)

(defvar *all-rboxes* nil)

;;;
;;;
;;;

(defgeneric inv-role (rolebox role))

(defgeneric lookup (rolebox r s))

;;;
;;; Role Box
;;;

(defpersistentclass rolebox ()
  ((name :reader name :initarg :name)
   (roles :reader roles :initarg :roles)
   (reflexive-roles :reader reflexive-roles :initarg :reflexive-roles)
   (n :reader n :initarg :n)
   (roles-to-inverses :reader roles-to-inverses :initarg :roles-to-inverses)
   (roles-to-ids :reader roles-to-ids :initarg :roles-to-ids)
   (ids-to-roles :reader ids-to-roles :initarg :ids-to-roles)
   (axioms :reader axioms :initform nil :initarg :axioms)
   (table :reader table :initarg :table)))


(defmethod print-object ((rbox rolebox) stream)
  (format stream "#<~A ~A>" (type-of rbox) (name rbox)))
           
(defun create-rbox (name roles
                         &key 
                         (error-p t)
                         (delete-if-exists-p t)
                         inverse-roles reflexive-roles 
                         axioms
                         (type 'rolebox))


  (labels ((to-keyword (x)
             (if (symbolp x)
                 (intern (format nil "~A" x) :keyword)
               (mapcar #'(lambda (x) 
                           (intern (format nil "~A" x) :keyword))
                       x))))

    (when delete-if-exists-p 
      (delete-rbox name :all-p t))

    (if (find-rbox name :error-p nil) 
        (if error-p
            (error "Rolebox named ~A already exists!" name)
          (return-from create-rbox nil))
      
      (if (not roles)
          (error "No roles!")

        (let* ((n (length roles))
               (inverse-roles (mapcar #'to-keyword
                                      (append inverse-roles
                                              (mapcar #'reverse inverse-roles)
                                              (mapcar #'(lambda (x) 
                                                          (list x x))
                                                      reflexive-roles))))
               (roles-to-ids (make-hash-table :size n))
               (ids-to-roles (make-array (list n)))
               (roles-to-inverses (make-hash-table :size n))
               (axiom-table (make-array (list n n)))
             
               (all-roles (mapcar #'to-keyword
                                  (tree-flatten
                                   (list inverse-roles
                                         axioms))))
               (roles (mapcar #'to-keyword roles))
               (reflexive-roles (mapcar #'to-keyword reflexive-roles)))
        
          (unless (subsetp all-roles roles)
            (error "Undeclared roles: ~A!" 
                   (set-difference all-roles roles)))
        
          (loop as role in roles 
                as i from 0 to n do
                (setf (gethash role roles-to-ids) i)
                (setf (aref ids-to-roles i) role))
        
          (loop as pair in inverse-roles
                do
                (let* ((role (first pair))
                       (inv (second pair))
                       (ret-inv (gethash role roles-to-inverses))
                       (ret-role (gethash inv roles-to-inverses)))
                  (if (or (and ret-inv 
                               (not (eq inv ret-inv)))
                          (and ret-role
                               (not (eq role ret-role))))
                      (error "Bad inverses roles!")
                    (progn
                      (setf (gethash role roles-to-inverses) inv)
                      (setf (gethash inv roles-to-inverses) role)))))

          (dolist (axiom axioms)
            (let ((from (to-keyword (first axiom)))
                  (to (to-keyword (second axiom)))
                  (res (mapcar #'to-keyword (ensure-list (third axiom)))))
            
              (when (member (list from to)
                            (mapcar #'butlast (remove axiom axioms))
                            :test #'equal)
                (error "Double axiom for ~A o ~A!" from to))
            
              (setf (aref axiom-table (gethash from roles-to-ids) (gethash to roles-to-ids))
                    (list res (mapcar #'(lambda (x) (gethash x roles-to-ids)) res)))))
        
          (let ((rbox 
                 (make-instance type 
                                :axioms axioms
                                :name name
                                :n n
                                :reflexive-roles reflexive-roles
                                :table axiom-table
                                :roles roles
                                :roles-to-inverses roles-to-inverses
                                :roles-to-ids roles-to-ids
                                :ids-to-roles ids-to-roles)))
            (push rbox *all-rboxes*)
            rbox))))))

(defmacro with-rbox ((name &key
                           (error-p t)
                           roles
                           inverse-roles
                           reflexive-roles
                           delete-if-exists-p
                           (type 'rolebox)
                           axioms)
                     &rest body)

  `(let ((*cur-rbox*
          (or (and ,(not delete-if-exists-p) (find-rbox ',name :error-p nil))
              (create-rbox ',name ',roles 
                           :axioms ',axioms
                           :type ',type
                           :error-p ,error-p 
                           :inverse-roles ',inverse-roles 
                           :reflexive-roles ',reflexive-roles
                           :delete-if-exists-p ,delete-if-exists-p))))
     ,@body))


(defmacro in-rbox (name &key
                        (error-p t)
                        axioms
                        roles
                        inverse-roles
                        reflexive-roles
                        delete-if-exists-p
                        (type 'rolebox))
  `(let ((x ni))
     (with-rbox (,name :roles ,roles
                       :axioms ,axioms
                       :type ,type
                       :error-p ,error-p 
                       :inverse-roles ',inverse-roles 
                       :reflexive-roles ',reflexive-roles
                       :delete-if-exists-p ,delete-if-exists-p)
       (setf x *cur-rbox*))
     (setf *cur-rbox* x)))

  
(defun find-rbox (name &key (error-p t))
  (if (typep name 'rolebox)
      name
    (or (find name *all-rboxes* :key #'name)
        (progn 
          (=> error-p (error "Can't find role box ~A!" name))
          nil))))
  

(defun delete-rbox (name &key all-p)
  (loop
   (let ((rbox (find-rbox name :error-p nil)))
     (unless rbox (return))    
     (setf *all-rboxes* 
           (delete rbox *all-rboxes*))
     (unless all-p (return)))))

;;;
;;;
;;;

(defmethod inv-role ((rbox rolebox) (role symbol))
  (with-slots (roles-to-inverses) rbox
    (or (gethash role roles-to-inverses)
        `(inv ,role))))

(defmethod inv-role ((rbox rolebox) (role list))
  (if (inv-role-p role)
      (if (symbolp (second role))
          (second role)
        (inv-role rbox (inv-role rbox (second role))))
    (mapcar #'(lambda (x) (inv-role rbox x)) role)))

(defun inv-role-p (x)
  (and (consp x)
       (eq (first x) 'inv)
       (second x)
       (not (cddr x))))

(defmethod full-disjunctive-role ((rbox rolebox))
  (roles rbox))

;;;
;;;
;;;

(defmethod get-role ((rbox rolebox) (role symbol))
  role)

(defmethod get-role ((rbox rolebox) (role list))
  (if (inv-role-p role)
      (inv-role rbox (second role))
    (let ((role (mapcar #'(lambda (x) (get-role rbox x)) role)))
      (if (member nil role)
          nil
        role))))
  
;;;
;;;
;;;

(defmethod encode ((box rolebox) (role symbol))
  (gethash (get-role box role) (roles-to-ids box)))

(defmethod encode ((box rolebox) (role list))
  (if (inv-role-p role)
      (encode box (get-role box role))
    (mapcar #'(lambda (r) 
                (encode box r))
            role)))


(defmethod decode ((box rolebox) (role integer))
  (when (and (not (minusp role))
             (< role (n box)))
    (aref (ids-to-roles box) role)))

(defmethod decode ((box rolebox) (role list))
  (mapcar #'(lambda (r) (decode box r)) role))

;;;
;;;
;;;

(defmethod lookup ((box rolebox) (from symbol) (to symbol))
  (with-slots (table) box   
    (let ((i (encode box (get-role box from)))
          (j (encode box (get-role box to))))
      (when (and i j)
        (first (aref table i j))))))

(defmethod lookup ((box rolebox) from to)
  (lookup box (ensure-list from) (ensure-list to)))

(defmethod lookup ((box rolebox) (from list) (to list)) ; aus Effizienzgruenden so gemacht
  (let ((rfrom (if (inv-role-p from)
                   (get-role box from)
                 from))
        (rto (if (inv-role-p to)
                 (get-role box to)
               to)))

    (if (or (inv-role-p from)
            (inv-role-p to))
        nil
      (with-slots (table) box 
        (let ((dis nil))
          (dolist (from rfrom)
            (dolist (to rto)
              (let ((i (get-role box from))
                    (j (get-role box to)))
                (if (or (inv-role-p i)
                        (inv-role-p j))
                    (return-from lookup nil)
                  (let* ((i (encode box i))
                         (j (encode box j)))
                    (if (and i j)              
                        (dolist (c (first (aref table i j)))
                          (push c dis))
                      (return-from lookup nil)))))))
          (remove-duplicates dis))))))

;;;
;;;
;;;

(defpersistentclass jepd-rolebox (rolebox) nil)

(defmethod initialize-instance :after ((rolebox jepd-rolebox) &rest initargs)
  (declare (ignorable initargs))
  (when (cdr (reflexive-roles rolebox))
    (error "More than one reflexive role found: ~A!" (reflexive-roles rolebox)))
  (dolist (role (roles rolebox))
    (unless (inv-role rolebox role)
      (error "Missing inverse role: ~A!" role)))
  (dolist (r (roles rolebox))
    (dolist (s (roles rolebox))
      (unless (lookup rolebox r s)
        (error "Missing role axiom: ~A o ~A!" r s)))))
