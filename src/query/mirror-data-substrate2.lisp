;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;;
;;;

(defconstant +marker-for-abox-thing+ :abox-object)

(defconstant +marker-for-abox-relationship+ :abox-relationship)

;;;
;;;
;;;

(defconstant +marker-for-abox-individual+ :abox-individual)

(defconstant +marker-for-cd-object+ :abox-concrete-domain-object)

(defconstant +marker-for-cd-value+ :abox-concrete-domain-value)

(defconstant +marker-for-abox-attribute-relationship+ :abox-attribute-relationship)

(defconstant +marker-for-abox-role-relationship+ :abox-role-relationship)

(defconstant +marker-for-told-value-relationship+ :abox-told-value-relationship)

;;;
;;;
;;;

(defconstant +marker-for-annotation+ :owl-annotation)

(defconstant +marker-for-annotation-relationship+ :owl-annotation-relationship)

(defconstant +marker-for-annotation-object-relationship+ :owl-annotation-object-relationship)

(defconstant +marker-for-annotation-data-relationship+ :owl-annotation-datatype-relationship)

(defconstant +marker-for-annotation-concept-assertion+ :owl-annotation-concept-assertion)

(defconstant +marker-for-annotation-datatype-object+ :owl-annotation-datatype-object)

(defconstant +marker-for-annotation-value+ :owl-annotation-value)

(defconstant +marker-for-datatype-value+ :owl-datatype-value)

(defconstant +marker-for-datatype-role+ :owl-datatype-role)

;;;
;;;
;;;

(defpersistentclass mirror-data-substrate (data-substrate)
  ())

#+:sql-substrate
(defpersistentclass mirror-sql-data-substrate (sql-data-substrate mirror-data-substrate)
  ())

;;;
;;;
;;;

#+:sql-substrate
(defmethod compute-abox-mirror :before ((substrate mirror-sql-data-substrate) &key &allow-other-keys)
  (initialize-db substrate))

(defmethod compute-abox-mirror :after ((substrate mirror-data-substrate) &key &allow-other-keys)
  (with-critical-section

   (let ((*told-information-reasoning-p* t))
         
     (with-slots (abox tbox
                       abox-individuals-cache
                       told-values-cache
                       datatype-property-values-cache
                       role-assertions-in-domain-cache) substrate

       (let ((atoms (all-atomic-concepts tbox)))
         (dolist (ind abox-individuals-cache)
           (node-instance substrate ind :description 
                          (list +marker-for-abox-thing+
                                +marker-for-abox-individual+))
           (dolist (concepts atoms)
             (dolist (concept (ensure-list concepts))
               ;;; Known instance?
               (when (dl-prover-individual-instance-p substrate ind concept)
                 (node-instance substrate ind  :description concept))))))
       
       (dolist (attribute-assertion (all-attribute-assertions abox))
         (let ((abox-ind (second attribute-assertion))
               (cd-object (third attribute-assertion))
               (cd-attribute (fourth attribute-assertion)))

           (nodes-related substrate abox-ind cd-object
                          `(,+marker-for-abox-thing+
                            ,+marker-for-abox-relationship+
                            ,+marker-for-abox-attribute-relationship+ 
                            ,cd-attribute))

           (node-instance substrate abox-ind
                          :description  `(,+marker-for-abox-thing+ 
                                          ,+marker-for-abox-individual+))

           (node-instance substrate cd-object 
                          :description  `(,+marker-for-cd-object+
                                          ,+marker-for-abox-thing+))))

       (maphash #'(lambda (cd-object told-value)
                    (nodes-related substrate cd-object told-value
                                   `(,+marker-for-abox-relationship+
                                     ,+marker-for-told-value-relationship+))
                     
                    (node-instance substrate told-value
                                   :description `(;; ,+marker-for-abox-thing+ 
                                                  ,+marker-for-cd-value+ ,told-value)))
                told-values-cache)

       (let ((ancestors (mht :test #'equal :size 100)))
         (dolist (role (dl-prover-all-roles substrate))
           (setf (gethash role ancestors)
                 (atomic-role-ancestors role tbox)))

         (maphash #'(lambda (ind role-assertions)
                      (declare (ignorable ind))
                      (dolist (ra role-assertions)
                        (let ((from (first (first ra)))
                              (to (second (first ra)))
                              (role (second ra)))
                          (if (consp role) 
                              (let ((to from)
                                    (from to)
                                    (role (second role)))
                                (dolist (role (gethash role ancestors))
                                  (nodes-related substrate from to 
                                                 `(,+marker-for-abox-relationship+ 
                                                   ,+marker-for-abox-role-relationship+ 
                                                   ,role))
                                  (let ((inv (dl-prover-atomic-role-inverse substrate role)))
                                    (unless (consp inv)
                                      (nodes-related substrate to from
                                                     `(,+marker-for-abox-relationship+ 
                                                       ,+marker-for-abox-role-relationship+ 
                                                       ,inv))))))
                             
                            (dolist (role (gethash role ancestors))
                              (nodes-related substrate from to
                                             `(,+marker-for-abox-relationship+ 
                                               ,+marker-for-abox-role-relationship+ 
                                               ,role))
                              (let ((inv (dl-prover-atomic-role-inverse substrate role)))
                                (unless (consp inv)
                                  (nodes-related substrate to from
                                                 `(,+marker-for-abox-relationship+ 
                                                   ,+marker-for-abox-role-relationship+ 
                                                   ,inv)))))))))
                        
                  role-assertions-in-domain-cache)

         (dolist (ra (all-annotation-role-assertions abox))
           (let ((from (first (first ra)))
                 (to (second (first ra)))
                 (role (second ra)))
             (dolist (role (gethash role ancestors))
               (nodes-related substrate from to 
                              `(,+marker-for-annotation+
                                ,+marker-for-annotation-relationship+ 
                                ,+marker-for-annotation-object-relationship+ 
                                ,role)))))

         (dolist (ca (all-annotation-concept-assertions abox))
           (let ((ind (first ca))
                 (concept (second ca)))

             (node-instance substrate ind 
                            :description
                            `(,+marker-for-annotation+ 
                              ,+marker-for-annotation-concept-assertion+ 
                              ,concept))
                       
             (cond ((is-datatype-property-some-value-p substrate concept)
                    (let ((role (second concept))
                          (string (third (third concept))))

                      (nodes-related substrate ind string
                                     `(,+marker-for-annotation+ 
                                       ,+marker-for-annotation-relationship+ 
                                       ,+marker-for-annotation-data-relationship+ 
                                       ,role))

                      (node-instance substrate string
                                     :description 
                                     `(,+marker-for-annotation+
                                       ,+marker-for-annotation-value+ 
                                       ,string))))
                    
                   ((is-datatype-property-at-least-value-p substrate concept)
                    (let ((role (third concept))
                          (string (third (fourth concept))))
                       
                      (nodes-related substrate ind string
                                     `(,+marker-for-annotation+ 
                                       ,+marker-for-annotation-relationship+ 
                                       ,+marker-for-annotation-data-relationship+ 
                                       ,role))

                      (node-instance substrate string
                                     :description 
                                     `(,+marker-for-annotation+
                                       ,+marker-for-annotation-value+ 
                                       ,string)))))))

         (maphash #'(lambda (node dtps)
                      (dolist (dtp dtps)
                        (let* ((role (first dtp))
                               (value (second dtp))
                               (value-node ; (gensym (format nil "~A" node))
                                value))
                          
                          (node-instance substrate node :description 
                                         `(,+marker-for-abox-thing+
                                           ,+marker-for-abox-individual+))
                          
                          (node-instance substrate value-node :description 
                                         `(,+marker-for-datatype-value+ ,value))
                          
                          (nodes-related substrate node value-node 
                                         `(,+marker-for-abox-thing+
                                           ,+marker-for-abox-relationship+
                                           ,+marker-for-datatype-role+
                                           ,role)))))
                  
                  datatype-property-values-cache))))))

;;;
;;;
;;;

(defmethod reset-caches :after ((substrate mirror-data-substrate))
  (with-slots (data-nodes data-edges told-info) substrate
    
    (clrhash data-nodes)
    (clrhash data-edges)

    (dolist (entry (reverse told-info))
      (apply (symbol-function (first entry))
             (cons substrate (cdr entry))))))

;;;
;;;
;;;

(nrql-defun set-mirror-data-box (name)
  (setf *type-of-substrate* 'mirror-data-substrate)
  (enable-abox-mirroring)
  (set-data-box name))

(nrql-defmacro (in-mirror-data-box :nrql-function set-mirror-data-box))

;;;
;;;
;;;

#+:sql-substrate
(nrql-defun set-mirror-sql-data-box (name)
  (setf *type-of-substrate* 'mirror-sql-data-substrate)
  (enable-abox-mirroring)
  (set-sql-data-box name))

#+:sql-substrate
(nrql-defmacro (in-mirror-sql-data-box :nrql-function set-mirror-sql-data-box))


