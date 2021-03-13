;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

(defun delete-all-dboxes ()
  (setf *all-dboxes* nil))

(defpersistentclass defined-query ()
  ((name :reader name :initarg :name)
   (head :reader head :initarg :head)
   (body :reader body :initarg :body)
   (query :reader query :initarg :query)))

(defpersistentclass dbox ()
  ((name-to-def-hash :accessor name-to-def-hash 
                     :initform (mht :size 30))
   (for-tbox :accessor for-tbox 
             :initarg :for-tbox)))

;;;
;;;
;;;

(defun create-dbox (&optional (tbox (current-tbox)))
  (let ((dbox (make-instance 'dbox :for-tbox tbox)))
    (push dbox *all-dboxes*)
    dbox))

(defun find-dbox (for-tbox &key (error-p t))
  (let ((for-tbox (or for-tbox 
                      *nrql-tbox*
                      (current-tbox))))
    (or (find for-tbox *all-dboxes* :key #'for-tbox)
        (when error-p 
          (nrql-error "Can't find DBOX for TBox ~A" for-tbox)))))

(defmethod register-defined-query ((def defined-query) (dbox dbox))
  (setf (gethash (name def)
                 (name-to-def-hash dbox))
        def))

(defmethod delete-defined-query ((def defined-query) (dbox dbox))
  (remhash (name def)
           (name-to-def-hash dbox)))

(defmethod all-names-of-defined-queries ((dbox dbox))
  (loop as name being the hash-key of (name-to-def-hash dbox)
        collect name))

;;;
;;;
;;;

(defmethod delete-all-definitions1 ((dbox dbox))
  (clrhash (name-to-def-hash dbox)))

;;;
;;;
;;; 

(defmethod get-definition1 ((substrate substrate) name &key (error-p t))
  (declare (ignorable name))
  (when error-p 
    (nrql-error "No definitions possible for substrate ~A!" substrate))
  nil)

(defmethod get-definition1 ((substrate racer-substrate) name &key (error-p t))
  (get-definition1 (dbox substrate) name :error-p error-p))

(defmethod get-definition1 ((dbox dbox) name &key (error-p t))
  (or
   (gethash name (name-to-def-hash dbox))
   (when error-p 
     (nrql-error "Can't find definition of query ~A in DBox for TBox ~A!" name (for-tbox dbox)))))
  
;;;
;;;
;;;

(defmethod describe-definition1 ((substrate racer-substrate) name) 
  (describe-definition1 (dbox substrate) name))

(defmethod describe-definition1 ((dbox dbox) name)
  (let ((def (get-definition1 dbox name)))
    `(defquery ,(name def) ,(head def) ,(body def))))

;;;
;;;
;;;

(defmethod check-for-name-clash  ((substrate racer-substrate) name args &key warn-p)
  (let ((def (get-definition1 substrate name)))
    (when def
      (when (and (not (cdr args)) ; (?x C) ?
                 (find name
                       (all-atomic-concepts (tbox substrate))
                       :key #'ensure-list
                       :test #'member))
        (when warn-p 
          (nrql-warning "Concept ~A exists in TBox ~A. Assuming you are referring to the concept ~A!" 
                        name (tbox substrate) name))
        (return-from check-for-name-clash t))

      (when (and (not (cddr args)) ; (?x ?y R) ?
                 (find name
                       (dl-prover-all-roles substrate)))
        (when warn-p 
          (nrql-warning "Role ~A exists in TBox ~A. Assuming you are referring to the role ~A!" 
                        name (tbox substrate) name))
        (return-from check-for-name-clash t)))))

;;;
;;;
;;;

(defmethod get-variable-substituted-query ((substrate racer-substrate) name args)
  (let ((def (get-definition1 substrate name)))
    (when def

      (check-for-name-clash substrate name args)

      (substitute-vois-in (body def)
                          (mapcar #'(lambda (old new) 
                                      (list old new))
                                  (head def)
                                  args)))))

;;;
;;;
;;;

(defmethod describe-all-definitions1 ((dbox dbox))
  (loop as name being the hash-key of (name-to-def-hash dbox)
        collect (describe-definition1 dbox name)))
  
;;;
;;;
;;;

(defun define-query1 (name head body &rest args 
                           &key keep-p (tbox (or *nrql-tbox* (current-tbox)))
                           &allow-other-keys)
  
  (let ((dbox (or (find-dbox tbox :error-p nil)
                  (create-dbox tbox))))

    (if (get-definition1 dbox name :error-p nil)
        (nrql-error "Query named ~A already exists!" name)
      
      (if keep-p 
          
          (let ((query
                 (apply #'racer-prepare-query head body args)))

            (register-defined-query (make-instance 'defined-query 
                                                   :name name :head head :body body
                                                   :query query)
                                    dbox)
            query)
        
        (let ((*use-repository-p* nil)
              (*put-into-repository-p* nil)
              (*report-tautological-queries-p* nil)
              (*report-inconsistent-queries-p* nil)
              (*optimize-p* nil)
              (*generate-code-p* nil)
              (*rewrite-semantically-p* nil)
              (*rewrite-to-dnf-p* nil))

          (let ((query
                 (apply #'racer-prepare-query head body args)))

            (register-defined-query (make-instance 'defined-query 
                                                   :name name :head head :body body
                                                   :query query)
                                    dbox)
            (delete-query (first query))
            
            name))))))

;;;
;;;
;;;

(defun undefine-query1 (name &key (tbox (or *nrql-tbox* (current-tbox))))
  (let ((dbox (find-dbox tbox :error-p nil)))
    (when dbox
      (delete-defined-query 
       (get-definition1 dbox name :error-p t) dbox)
      (all-names-of-defined-queries dbox))))

