;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;;
;;;

(defconstant +racer-internal-datatype-property-roles+  
  '(RACER-INTERNAL%HAS-STRING-VALUE
    RACER-INTERNAL%HAS-INTEGER-VALUE 
    RACER-INTERNAL%HAS-CARDINAL-VALUE 
    RACER-INTERNAL%HAS-REAL-VALUE 
    RACER-INTERNAL%HAS-BOOLEAN-VALUE))

(defconstant +racer-substrate-default-tbox+ 'cl-user::default)

;;;
;;;
;;;

(defconstant +cache-is-complete-marker+ :cache-complete-dvfkvf8974)

(defconstant +individual-exists-concept+ :individual-exists-7894jkjoif)

;;;
;;;
;;;

(defvar *racer-tbox* nil)

(defvar *racer-abox* nil)

;;;
;;;
;;;
;;;

(defconstant +default-hash-table-size+ 10000)

(defconstant +default-rehash-factor+ 1)

(defun mht (&key (size +default-hash-table-size+) (test #'eql))
  (make-hash-table :size size :rehash-size (* +default-rehash-factor+ size) :test test))

;;;
;;;
;;;

(defconstant +table-size-concept-assertions-cache+ 3000)

(defconstant +table-size-constraint-entailed-cache+ 3000)

(defconstant +table-size-concept-instances-cache+ 3000)

(defconstant +table-size-known-concept-instances-cache+ 3000)

(defconstant +table-size-individual-instance-cache+ 10000)

(defconstant +table-size-individual-attribute-fillers-cache+ 3000)

(defconstant +table-size-role-assertions-in-range-cache+ 10000)

(defconstant +table-size-role-assertions-in-domain-cache+ 10000)

(defconstant +table-size-role-descendants-cache+ 100)

(defconstant +table-size-atomic-role-inverse-cache+ 100)

(defconstant +table-size-role-successors-cache+ 100)

(defconstant +table-size-new-inds-hash+ 100)

(defconstant +table-size-datatype-property-values-cache+ 100)

(defconstant +table-size-told-values-cache+ 100)

;;;
;;;
;;;

(defpersistentclass racer-descriptions-substrate (substrate)

  ;;; Knoten und Kanten mit Racer-Descriptions

  ((tbox :initarg :tbox :reader tbox :initform nil)
   
   (racer-package :accessor racer-package :initarg :racer-package :initform :racer-user)))


(defpersistentclass dl-prover-substrate (substrate) 

  ;;; Knoten und Kanten mit Descriptions, optionale assoziierte ABox-Objekte!

  ((qbox :initarg :qbox :reader qbox :initform nil)
   (dbox :initarg :dbox :reader dbox :initform nil)

   (prepared-p :accessor prepared-p :initform nil)

   (saved-state-vector :accessor saved-state-vector :initform nil)
   
   (needs-filler-reasoning-p :reader needs-filler-reasoning-p :initform nil)

   (new-inds-hash :accessor new-inds-hash :initform (mht :size +table-size-new-inds-hash+ :test #'equal))

   (abox-individuals-cache :accessor abox-individuals-cache :initform :unknown)

   (concept-assertions-cache :accessor concept-assertions-cache 
                             :initform (mht :size +table-size-concept-assertions-cache+))

   ;;;
   ;;; 
   ;;; 

   (concept-instances-cache :accessor concept-instances-cache
                            :initform (mht :size +table-size-concept-instances-cache+ :test #'equal))

   (known-concept-instances-cache :accessor known-concept-instances-cache
                                  :initform (mht :size +table-size-known-concept-instances-cache+ :test #'equal))

   (individual-instance-cache :accessor individual-instance-cache
                              :initform (mht :test #'equal :size +table-size-individual-instance-cache+))


   (role-assertions-in-range-cache :accessor role-assertions-in-range-cache
                                   :initform (mht :size +table-size-role-assertions-in-range-cache+))

   (role-assertions-in-domain-cache :accessor role-assertions-in-domain-cache
                                    :initform (mht :size +table-size-role-assertions-in-domain-cache+))

   (role-descendants-cache :accessor role-descendants-cache
                           :initform (mht :size +table-size-role-descendants-cache+ :test #'equal))

   (role-successors-cache :accessor role-successors-cache
                          :initform (mht :size +table-size-role-successors-cache+ :test #'equal))

   (atomic-role-inverse-cache :accessor atomic-role-inverse-cache
                              :initform (mht :size +table-size-atomic-role-inverse-cache+ :test #'equal))

   (all-roles-cache :accessor all-roles-cache :initform :unknown)

   (transitive-roles-cache :accessor transitive-roles-cache :initform :unknown)

   ;;;
   ;;; die folgenden Slots / Hashtabellen dienen dazu, die Kommunikation mit
   ;;; dem RACER-Server zu beschleunigen (Cache fuer haeuftig gestellte Anfragen!) 
   ;;; 

   (constraints-cache :accessor constraints-cache :initform :unknown)

   (told-values-cache :accessor told-values-cache :initform (mht :size +table-size-told-values-cache+ :test #'eql))

   (datatype-property-values-cache :accessor datatype-property-values-cache
                                   :initform (mht :test #'equal :size +table-size-datatype-property-values-cache+))

   (constraint-entailed-cache :accessor constraint-entailed-cache
                              :initform (mht :test #'equal :size +table-size-constraint-entailed-cache+))

   (individual-attribute-fillers-cache :accessor individual-attribute-fillers-cache
                                       :initform (mht :test #'equal :size +table-size-individual-attribute-fillers-cache+))))


(defpersistentclass racer-substrate (dl-prover-substrate racer-descriptions-substrate)
  ;;; Knoten und Kanten mit Descriptions, optionale assoziierte ABox-Objekte!
  ((tbox :initarg :tbox :reader tbox :initform nil)
   (abox :initarg :abox :reader abox :initform nil)))


(defpersistentclass racer-dummy-substrate (racer-substrate) nil)
  ;;; nur fuer Ralf, keine Substrat-Objekte, nur ABox
  ;;; -> keine Substrate-Objekte-Unterklassen erforderlich, klar



#+:midelora
(defpersistentclass midelora-substrate (dl-prover-substrate prover::abox)
  ((abox :accessor abox)
   (tbox :accessor tbox)))

#+:midelora
(defpersistentclass midelora-substrate1 (midelora-substrate prover::abox1))

#+:midelora
(defmethod update-instance-for-different-class :after ((old prover::abox) (substrate midelora-substrate) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value substrate 'abox) substrate
        (slot-value substrate 'tbox) (prover::tbox substrate))
  (reset-substrate substrate))


(defpersistentclass tbox-mirror-substrate (dl-prover-substrate))

(defpersistentclass racer-tbox-mirror-substrate (tbox-mirror-substrate racer-dummy-substrate)
  ((mirror-of-tbox :reader mirror-of-tbox)
   (needs-filler-reasoning-p :initform nil)))

#+:midelora
(defpersistentclass midelora-tbox-mirror-substrate (tbox-mirror-substrate midelora-substrate)
  ((mirror-of-tbox :reader mirror-of-tbox)
   (needs-filler-reasoning-p :initform nil)))
  
;;;
;;;
;;;

(defmethod establish-context-for ((substrate racer-substrate) continuation)
  (let ((*racer-tbox* (tbox substrate))
        (*racer-abox* (abox substrate)))
    (if (next-method-p)
        (call-next-method)
      (funcall continuation))))

#-:dlmaps
(defmethod set-context-for ((substrate racer-substrate))
  (setf *racer-tbox* (tbox substrate)
        *racer-abox* (abox substrate)))

#+:dlmaps
(defmethod set-context-for progn ((substrate racer-substrate))
  (setf *racer-tbox* (tbox substrate)
        *racer-abox* (abox substrate)))

;;;
;;;
;;;

(defmethod reset-substrate :before ((substrate dl-prover-substrate) &key &allow-other-keys)
  (without-timeout
   (setf (saved-state-vector substrate) nil)
   (setf (prepared-p substrate) nil)))

(defmethod reset-substrate :after ((substrate dl-prover-substrate) &key &allow-other-keys)
  (without-timeout
   (setf (saved-state-vector substrate) 
         (get-state-vector substrate))
   
   (setf (prepared-p substrate) t)))
  
;;;
;;;
;;;

(defmethod reset-substrate ((substrate racer-substrate) &key &allow-other-keys)
  (clear-repository substrate)
  (reset-caches substrate)
  (if (find-tbox (tbox substrate) nil)
      (if (find-abox (abox substrate) nil)
          (compute-abox-mirror substrate)
        (nrql-error "Can't find ABox ~A!" (abox substrate)))
    (nrql-error "Can't find TBox ~A!" (tbox substrate))))

#+:midelora
(defmethod reset-substrate ((substrate midelora-substrate) &key &allow-other-keys)
  (clear-repository substrate)
  (reset-caches substrate)
  (compute-abox-mirror substrate))

(defmethod reset-substrate ((substrate racer-tbox-mirror-substrate) &key tbox)
  (reset-caches substrate)
  (setf (slot-value substrate 'tbox) 
        (or tbox
            (mirror-of-tbox substrate)))
  (if (find-tbox (tbox substrate) nil)
      (compute-tbox-mirror substrate)
    (nrql-error "Can't find TBox ~A!" (tbox substrate))))

#+:midelora
(defmethod reset-substrate ((substrate midelora-tbox-mirror-substrate) &key tbox)
  (declare (ignorable tbox))
  (reset-caches substrate)
  
  (setf (slot-value substrate 'tbox) 
        (or tbox
            (mirror-of-tbox substrate)))

  (if (prover::find-tbox (tbox substrate) :error-p nil)
      (compute-tbox-mirror substrate)
    (nrql-error "Can't find TBox ~A!" (tbox substrate))))


;;;
;;;
;;;


(defmethod reset-caches ((substrate dl-prover-substrate))
  (setf (constraints-cache substrate) :unknown)
  (setf (abox-individuals-cache substrate) :unknown)                        
  (setf (transitive-roles-cache substrate) :unknown)
  (setf (all-roles-cache substrate) :unknown)


  (dolist (slot '(concept-assertions-cache
                  atomic-role-inverse-cache
                  concept-instances-cache
                  known-concept-instances-cache
                  individual-instance-cache
                  role-assertions-in-range-cache
                  role-assertions-in-domain-cache
                  role-descendants-cache
                  role-successors-cache
                  told-values-cache
                  datatype-property-values-cache
                  constraint-entailed-cache
                  individual-attribute-fillers-cache))
    
    (clrhash (slot-value substrate slot))))
   
;;;
;;; DL-Prover-Ankopplung per Delegation (plus Caching etc.) 
;;;



(defmethod dl-all-individuals ((substrate racer-substrate) &rest args)
  (apply #'all-individuals args))

#+:midelora
(defmethod dl-all-individuals ((substrate midelora-substrate) &rest args)
  (apply #'prover::all-individuals args))



(defmethod dl-all-roles ((substrate racer-substrate) &rest args)
  (apply #'all-roles args))

#+:midelora
(defmethod dl-all-roles ((substrate midelora-substrate) &rest args)
  (apply #'prover::all-roles args))



(defmethod dl-all-features ((substrate racer-substrate) &rest args)
  (apply #'all-features args))

#+:midelora
(defmethod dl-all-features ((substrate midelora-substrate) &rest args)
  (apply #'prover::all-features args))



(defmethod dl-all-transitive-roles ((substrate racer-substrate) &rest args)
  (apply #'all-transitive-roles args))

#+:midelora
(defmethod dl-all-transitive-roles ((substrate midelora-substrate) &rest args)
  (apply #'prover::all-transitive-roles args))



(defmethod dl-all-constraints ((substrate racer-substrate) &rest args)
  (apply #'all-constraints args))

#+:midelora
(defmethod dl-all-constraints ((substrate midelora-substrate) &rest args)
  (declare (ignorable args))
  nil)


(defmethod dl-all-concept-assertions-for-individual ((substrate racer-substrate) &rest args)
  (apply #'all-concept-assertions-for-individual args))

#+:midelora
(defmethod dl-all-concept-assertions-for-individual ((substrate midelora-substrate) &rest args)
  (apply #'prover::all-concept-assertions-for-individual args))



(defmethod dl-retrieve-concept-instances ((substrate racer-substrate) concept abox 
                                          &key (candidates nil candidates-supplied-p) 
                                          &allow-other-keys)
  (if candidates-supplied-p 
      (retrieve-concept-instances concept abox candidates)
    (retrieve-concept-instances concept abox)))
    

#+:midelora
(defmethod dl-retrieve-concept-instances ((substrate midelora-substrate) concept abox 
                                          &rest args &key &allow-other-keys)
  (apply #'prover::get-concept-instances concept abox args))


(defmethod dl-individual-p ((substrate racer-substrate) &rest args)
  (apply #'individual-p args))

#+:midelora
(defmethod dl-individual-p ((substrate midelora-substrate) &rest args)
  (apply #'prover::individual-p args))


(defmethod dl-individual-instance-p ((substrate racer-substrate) &rest args)
  (apply #'individual-instance-p args))

#+:midelora
(defmethod dl-individual-instance-p ((substrate midelora-substrate) &rest args)
  (apply #'prover::individual-instance-p args))


(defmethod dl-constraint-entailed-p ((substrate racer-substrate) &rest args)
  (apply #'constraint-entailed-p args))

#+:midelora
(defmethod dl-constraint-entailed-p ((substrate midelora-substrate) &rest args)
  nil)


(defmethod dl-retrieve-individual-attribute-fillers ((substrate racer-substrate) &rest args)
  (apply #'retrieve-individual-attribute-fillers args))

#+:midelora
(defmethod dl-retrieve-individual-attribute-fillers ((substrate midelora-substrate) &rest args)
  nil)


(defmethod dl-all-role-assertions-for-individual-in-range ((substrate racer-substrate) &rest args)
  (apply #'all-role-assertions-for-individual-in-range args))

#+:midelora
(defmethod dl-all-role-assertions-for-individual-in-range ((substrate midelora-substrate) &rest args)
  (apply #'prover::all-role-assertions-for-individual-in-range args))


(defmethod dl-all-role-assertions-for-individual-in-domain ((substrate racer-substrate) &rest args)
  (apply #'all-role-assertions-for-individual-in-domain args))

#+:midelora
(defmethod dl-all-role-assertions-for-individual-in-domain ((substrate midelora-substrate) &rest args)
  (apply #'prover::all-role-assertions-for-individual-in-domain args))



(defmethod dl-atomic-role-descendants ((substrate racer-substrate) &rest args)
  (apply #'atomic-role-descendants args))

#+:midelora
(defmethod dl-atomic-role-descendants ((substrate midelora-substrate) &rest args)
  (apply #'prover::atomic-role-descendants args))


(defmethod dl-atomic-role-inverse ((substrate racer-substrate) &rest args)
  (apply #'atomic-role-inverse args))

#+:midelora
(defmethod dl-atomic-role-inverse ((substrate midelora-substrate) &rest args)
  (apply #'prover::atomic-role-inverse args))


(defmethod dl-all-role-assertions ((substrate racer-substrate) &rest args)
  (apply #'all-role-assertions args))

#+:midelora
(defmethod dl-all-role-assertions ((substrate midelora-substrate) &rest args)
  (apply #'prover::all-role-assertions args))



(defmethod dl-all-annotation-role-assertions  ((substrate racer-substrate) &rest args)
  (apply #'all-annotation-role-assertions args))
  
#+:midelora
(defmethod dl-all-annotation-role-assertions  ((substrate midelora-substrate) &rest args)
  nil)


(defmethod dl-all-attribute-assertions ((substrate racer-substrate) &rest args)
  (apply #'all-attribute-assertions args))

#+:midelora
(defmethod dl-all-attribute-assertions  ((substrate midelora-substrate) &rest args)
  nil)
  

(defmethod dl-tbox-classified-p ((substrate racer-descriptions-substrate) &rest args)
  (apply #'tbox-classified-p args))

#+:midelora
(defmethod dl-tbox-classified-p ((substrate midelora-substrate) &rest args)
  (apply #'prover::tbox-classified-p args))


(defmethod dl-tbox-coherent-p ((substrate racer-descriptions-substrate) &rest args)
  (apply #'tbox-coherent-p args))

#+:midelora
(defmethod dl-tbox-coheren-p ((substrate midelora-substrate) &rest args)
  (apply #'prover::tbox-coherent-p args))



(defmethod dl-classify-tbox ((substrate racer-descriptions-substrate) &rest args)
  (apply #'classify-tbox args))

#+:midelora
(defmethod dl-classify-tbox ((substrate midelora-substrate) &rest args)
  (apply #'prover:classify-tbox args))



(defmethod dl-all-annotation-concept-assertions ((substrate racer-substrate) &rest args)
  (apply #'all-annotation-concept-assertions args))

#+:midelora
(defmethod dl-all-annotation-concept-assertions ((substrate midelora-substrate) &rest args)
  nil)


(defmethod dl-atomic-concept-ancestors ((substrate racer-substrate) &rest args)
  (apply #'atomic-concept-ancestors args))

#+:midelora
(defmethod dl-atomic-concept-ancestors ((substrate midelora-substrate) &rest args)
  nil)


(defmethod dl-atomic-concept-synonyms ((substrate racer-substrate) &rest args)
  (apply #'atomic-concept-synonyms args))

#+:midelora
(defmethod dl-atomic-concept-synonyms ((substrate midelora-substrate) &rest args)
  (apply #'prover::atomic-concept-synonyms args))


(defmethod dl-all-concept-assertions ((substrate racer-substrate) &rest args)
  (apply #'all-concept-assertions args))

#+:midelora
(defmethod dl-all-concept-assertions ((substrate midelora-substrate) &rest args)
  (apply #'prover::all-concept-assertions args))

;;;
;;;
;;;

(defmethod retrieve-annotation-values ((substrate racer-substrate) ind property)
  (loop as concept in
        (gethash ind (datatype-property-values-cache substrate))
        when (eq (first concept) property)
        collect (second concept)))

#+:midelora
(defmethod retrieve-annotation-values ((substrate midelora-substrate) ind property)
  (declare (ignore ind property))
  nil)


;;;
;;; Abstraktionen; nur diese Funktionen sollen im Query-Evaluation-Code verwendet werden!
;;; 


(defmethod dl-prover-internal-individuals-related-p ((substrate racer-substrate) &rest args)
  (apply #'internal-individuals-related-p args))

#+:midelora
(defmethod dl-prover-internal-individuals-related-p ((substrate midelora-substrate) &rest args)
  (apply #'prover::individuals-related-p args))


(defmethod dl-prover-individual-synonyms ((substrate racer-substrate) ind)
  (with-critical-section 
    (retrieve-individual-synonyms ind nil (abox substrate))))

#+:midelora
(defmethod dl-prover-individual-synonyms ((substrate midelora-substrate) ind)
  (declare (ignorable ind))
  nil)


(defmethod dl-prover-abox-consistent-p ((substrate racer-substrate) &rest args)
  (apply #'abox-consistent-p args))

#+:midelora
(defmethod dl-prover-abox-consistent-p ((substrate midelora-substrate) &rest args)
  (apply #'prover::abox-consistent-p args))


(defmethod dl-prover-transitive-role-p ((substrate dl-prover-substrate) role)
  (with-critical-section 
   (when (eq (transitive-roles-cache substrate) :unknown)
     (setf (transitive-roles-cache substrate) 
       (dl-all-transitive-roles substrate (tbox substrate))))

   (member role (transitive-roles-cache substrate)
           :test #'equal)))

(defmethod dl-prover-all-roles ((substrate dl-prover-substrate))
  (with-critical-section 
   (when (eq (all-roles-cache substrate) :unknown)
     (setf (all-roles-cache substrate) 
           (dl-all-roles substrate (tbox substrate))))

   (all-roles-cache substrate)))

(defmethod dl-prover-role-successors-cache-complete-for-role-p ((substrate dl-prover-substrate) from role)
  (with-critical-section
   (dl-prover-is-known-role-successor-of-p substrate +cache-is-complete-marker+ from role)))

(defmethod dl-prover-register-role-successors-cache-is-complete-for-role ((substrate dl-prover-substrate) from role)
  (with-critical-section
   (dl-prover-register-role-successor substrate +cache-is-complete-marker+ from role)))

(defmethod dl-prover-all-role-successors ((substrate dl-prover-substrate) from role)
  (with-critical-section
   (if (dl-prover-role-successors-cache-complete-for-role-p substrate from role)
       (let ((hash (gethash role (role-successors-cache substrate))))
         (if hash 
             (let ((succs (gethash from hash))) ; +complete-marker+ entfernen!
               ;; (princ succs)
               ;; (terpri)
               (rest succs))
           (nrql-error "Runtime error: hashing problem")))
     :unknown)))

(defmethod dl-prover-is-known-role-successor-of-p ((substrate dl-prover-substrate) to from role)
  (with-critical-section

   ;;; auch wenn der Cache unvollstaendig ist! 
   ;;; er wird nur verwendet beim Suchen in der ABox "racer-retrieve-individual-fillers"
   ;;; fuer (transitive) Rollen, um Such-Endlosschleifen (Zyklen) aufzuloesen!
   ;;; 
   (let ((hash (gethash role (role-successors-cache substrate))))
     (if hash 
         (member to (gethash from hash))
       nil))))

(defmethod dl-prover-register-role-successor ((substrate dl-prover-substrate) to from role) 
  (with-critical-section

   (unless (gethash role (role-successors-cache substrate))      
     (setf (gethash role (role-successors-cache substrate))
           (mht :size 1000 :test #'equal)))

   (when (dl-prover-role-successors-cache-complete-for-role-p substrate from role)
     (nrql-error "Runtime error: cache is already complete!"))

   (multiple-value-bind (succs foundp)
       (gethash from (gethash role (role-successors-cache substrate)))
     (declare (ignorable succs))
     (if foundp 
         (push to (gethash from (gethash role (role-successors-cache substrate))))
       (setf (gethash from (gethash role (role-successors-cache substrate))) (list to))))))

(defmethod dl-prover-all-individuals ((substrate dl-prover-substrate))
  (with-critical-section 
   (if (eq (abox-individuals-cache substrate) :unknown)
       (unless *told-information-reasoning-p*              
         (values (setf (abox-individuals-cache substrate) 
                       (dl-all-individuals substrate (abox substrate)))
                 t))
     (values (abox-individuals-cache substrate) t))))

(defmethod dl-prover-individual-exists-p ((substrate dl-prover-substrate) ind)
  (with-critical-section 
      (dl-prover-individual-instance-p substrate ind +individual-exists-concept+)))

(defmethod dl-prover-all-constraints ((substrate dl-prover-substrate))
  (with-critical-section 
   (if (eq (constraints-cache substrate) :unknown)
       (unless *told-information-reasoning-p*
         (values (setf (constraints-cache substrate)
                       (dl-all-constraints substrate (abox substrate)))
                 t))
     (values (constraints-cache substrate) t))))

(defmethod dl-prover-all-concept-assertions-for-individual ((substrate dl-prover-substrate) ind)
  (with-critical-section
   (multiple-value-bind (val foundp)
       (gethash ind (concept-assertions-cache substrate))
     (if foundp 
         (values val t)
       (unless *told-information-reasoning-p*
         (let ((assertions 
                (dl-all-concept-assertions-for-individual substrate ind (abox substrate))))
            
           (setf (gethash ind (concept-assertions-cache substrate)) assertions)
          
           (dolist (assertion assertions)
             (let ((concept (second assertion)))
               (setf (gethash (list ind concept) (individual-instance-cache substrate)) t)

               (if (gethash concept (known-concept-instances-cache substrate))
                   (push ind (gethash concept (known-concept-instances-cache substrate)))
                 (setf (gethash concept (known-concept-instances-cache substrate)) (list ind)))))                
        
           (values assertions t)))))))

(defmethod dl-prover-retrieve-concept-instances ((substrate dl-prover-substrate) concept 
                                                 &key (candidates nil candidates-supplied-p)
                                                 continuation)
  (with-critical-section
   (multiple-value-bind (val foundp)
       (gethash concept (concept-instances-cache substrate))

     (if foundp

         ;; zweiter Wert wichtig, um bei NIL-Ergebnis
         ;; zu sehen, dass vollständig bzw. bereits berechnet!

         (if continuation

             (progn 
               (dolist (x val)
                 (funcall continuation x))
               (values val t))

           (values val t))
        
       (unless *told-information-reasoning-p*       
          
         (let* ((continuation
                 #'(lambda (ind) 
                     (setf (gethash (list ind concept) (individual-instance-cache substrate)) t)
                     (when continuation
                       (funcall continuation ind))))
                 
                (inds (if candidates-supplied-p 
                          (dl-retrieve-concept-instances substrate 
                                                         concept (abox substrate)
                                                         :candidates candidates
                                                         :continuation continuation)
                        (dl-retrieve-concept-instances substrate
                                                       concept (abox substrate)
                                                       :continuation continuation))))

           (setf (gethash concept (concept-instances-cache substrate)) inds)
              
           (values inds t)))))))


(defmethod dl-prover-retrieve-known-concept-instances ((substrate dl-prover-substrate) concept)
  (with-critical-section
   (gethash concept (known-concept-instances-cache substrate))))

(defmethod dl-prover-individual-instance-p ((substrate dl-prover-substrate) ind concept)
  (with-critical-section 
   (multiple-value-bind (val foundp)
       (gethash (list ind concept) (individual-instance-cache substrate))
     (if foundp
         (values val t)
       (unless *told-information-reasoning-p*
         (let ((res 
                (if (eq concept +individual-exists-concept+)
                    (dl-individual-p substrate ind (abox substrate))
                  (dl-individual-instance-p substrate ind concept (abox substrate)))))
           (setf (gethash (list ind concept) (individual-instance-cache substrate)) res)
           (values res t)))))))

(defmethod dl-prover-constraint-entailed-p ((substrate dl-prover-substrate) constraint)
  (with-critical-section 
   (multiple-value-bind (val foundp)
       (gethash constraint (constraint-entailed-cache substrate))
     (if foundp
         (values val t)
       (unless *told-information-reasoning-p*
         (let ((res 
                (dl-constraint-entailed-p substrate constraint (abox substrate))))
           (setf (gethash constraint (constraint-entailed-cache substrate)) res)
           (values res t)))))))

(defmethod dl-prover-retrieve-individual-attribute-fillers ((substrate dl-prover-substrate) ind attribute)
  (with-critical-section
   (multiple-value-bind (val foundp)
       (gethash (list ind attribute) (individual-attribute-fillers-cache substrate))
     (if foundp 
         (values val t)
       (unless *told-information-reasoning-p*
         (let ((fillers 
                (dl-retrieve-individual-attribute-fillers substrate ind attribute (abox substrate))))
           (setf (gethash (list ind attribute) (individual-attribute-fillers-cache substrate)) fillers)
           (values fillers t)))))))

(defmethod dl-prover-all-role-assertions-for-individual-in-range ((substrate dl-prover-substrate) ind)
  (with-critical-section
   (multiple-value-bind (val foundp)
       (gethash ind (role-assertions-in-range-cache substrate))
     (if foundp 
         (values val t)
       (unless *told-information-reasoning-p*
         (let ((assertions
                (dl-all-role-assertions-for-individual-in-range substrate ind (abox substrate))))
           (setf (gethash ind (role-assertions-in-range-cache substrate)) assertions)
           (values assertions t)))))))

(defmethod dl-prover-all-role-assertions-for-individual-in-domain ((substrate dl-prover-substrate) ind)
  (with-critical-section
   (multiple-value-bind (val foundp)
       (gethash ind (role-assertions-in-domain-cache substrate))
     (if foundp 
         (values val t)
       (unless *told-information-reasoning-p*
         (let ((assertions 
                (dl-all-role-assertions-for-individual-in-domain substrate ind (abox substrate))))
           (setf (gethash ind (role-assertions-in-domain-cache substrate)) assertions)
           (values assertions t)))))))

(defmethod dl-prover-atomic-role-descendants ((substrate dl-prover-substrate) role)
  (with-critical-section 
   (multiple-value-bind (val foundp)
       (gethash role (role-descendants-cache substrate))
     (if foundp 
         (values val t)
       (unless *told-information-reasoning-p*
         (let ((roles
                (dl-atomic-role-descendants substrate role (tbox substrate))))
           (setf (gethash role (role-descendants-cache substrate)) roles)
           (values roles t)))))))

(defmethod dl-prover-atomic-role-inverse ((substrate dl-prover-substrate) role)
  (with-critical-section 
   (multiple-value-bind (val foundp)
       (gethash role (atomic-role-inverse-cache substrate))
     (if foundp 
         (values val t)
       (unless *told-information-reasoning-p*
         (let ((inv-role
                (dl-atomic-role-inverse substrate role (tbox substrate))))
           (setf (gethash role (atomic-role-inverse-cache substrate)) inv-role)
           (values inv-role t)))))))

(defmethod dl-prover-told-value ((substrate racer-substrate) cd-object)
  (with-critical-section
   (with-slots (told-values-cache) substrate
     (gethash cd-object told-values-cache))))

;;;
;;;
;;;

(defmethod dl-concept-p ((substrate racer-substrate) &rest args)
  (apply #'concept-p args))

#+:midelora
(defmethod dl-concept-p ((substrate midelora-substrate) &rest args)
  t)

(defmethod dl-all-atomic-concepts ((substrate racer-substrate) &rest args)
  (apply #'all-atomic-concepts args))

#+:midelora
(defmethod dl-all-atomic-concepts ((substrate midelora-substrate) &rest args)
  (apply #'prover::all-atomic-concepts args))


(defmethod dl-atomic-concept-children ((substrate racer-substrate) &rest args)
  (apply #'atomic-concept-children args))

#+:midelora
(defmethod dl-atomic-concept-children ((substrate midelora-substrate) &rest args)
  (apply #'prover::atomic-concept-children args))


;;;
;;; fuer ABox Augmentation (Rules etc.) 
;;;

(defmethod dl-add-concept-assertion ((substrate racer-substrate) &rest args)
  (apply #'add-concept-assertion args))

#+:midelora
(defmethod dl-add-concept-assertion ((substrate midelora-substrate) &rest args)
  (apply #'prover::add-concept-assertion args))


(defmethod dl-add-role-assertion ((substrate racer-substrate) &rest args)
  (apply #'add-role-assertion args))

#+:midelora
(defmethod dl-add-role-assertion ((substrate midelora-substrate) &rest args)
  (apply #'prover::add-role-assertion args))


(defmethod dl-add-attribute-assertion ((substrate racer-substrate) &rest args)
  (apply #'add-attribute-assertion args))

(defmethod dl-add-constraint-assertion ((substrate racer-substrate) &rest args)
  (apply #'add-constraint-assertion args))

;;;
;;;
;;;

(defmethod dl-forget-concept-assertion ((substrate racer-substrate) &rest args)
  (apply #'forget-concept-assertion args))

#+:midelora
(defmethod dl-forget-concept-assertion ((substrate midelora-substrate) &rest args)
  (apply #'prover::forget-concept-assertion args))


(defmethod dl-forget-role-assertion ((substrate racer-substrate) &rest args)
  (apply #'forget-role-assertion args))

#+:midelora
(defmethod dl-forget-role-assertion ((substrate midelora-substrate) &rest args)
  (apply #'prover::forget-role-assertion args))


(defmethod dl-forget-constrained-assertion ((substrate racer-substrate) &rest args)
  (apply #'forget-constrained-assertion args))

(defmethod dl-forget-constraint ((substrate racer-substrate) &rest args)
  (apply #'forget-constraint args))

;;;
;;;
;;;

(defmethod dl-prover-forget-concept-assertion ((substrate dl-prover-substrate) &rest args)
  (apply #'dl-forget-concept-assertion substrate args)
  (substrate-needs-reset substrate))

(defmethod dl-prover-forget-role-assertion ((substrate dl-prover-substrate) &rest args)
  (apply #'dl-forget-role-assertion substrate args)
  (substrate-needs-reset substrate))

(defmethod dl-prover-forget-constrained-assertion ((substrate dl-prover-substrate) &rest args)
  (apply #'dl-forget-constrained-assertion substrate args)
  (substrate-needs-reset substrate))

(defmethod dl-prover-forget-constraint ((substrate dl-prover-substrate) &rest args)
  (apply #'dl-forget-constraint substrate args)
  (substrate-needs-reset substrate))


;;;
;;; 
;;;

(defmethod dl-prover-add-concept-assertion ((substrate dl-prover-substrate) ind concept &key (to-abox-p t))
  (with-critical-section
   (let ((assertion (list ind concept)))

     (when to-abox-p 
       (dl-add-concept-assertion substrate (abox substrate) ind concept)
       (substrate-needs-reset substrate)
       ;;; die naechste Query braucht ein reintialisiertes
       ;;; Substrate, klar... dennoch sinnvoll mit dem 
       ;;; veraenderten Substrate weiterzurechnen, s. 
       ;;; mode 6
       )

     (with-slots (concept-assertions-cache individual-instance-cache) substrate
       (setf (gethash (list ind concept) (individual-instance-cache substrate)) t)

       (setf (gethash ind concept-assertions-cache)
             (cons assertion (gethash ind concept-assertions-cache)))))))


(defmethod dl-prover-add-role-assertion ((substrate dl-prover-substrate) from to role &key (to-abox-p t))
  (with-critical-section 
   (let ((assertion (list (list from to) role)))

     (when to-abox-p 
       (dl-add-role-assertion substrate (abox substrate) from to role)
       (substrate-needs-reset substrate))

     (with-slots (role-assertions-in-domain-cache role-assertions-in-range-cache) substrate
       (setf (gethash from role-assertions-in-domain-cache)
             (cons assertion (gethash from role-assertions-in-domain-cache)))
       (setf (gethash to role-assertions-in-range-cache)
             (cons assertion (gethash to role-assertions-in-range-cache)))))))


#+:midelora
(defmethod dl-prover-add-attribute-assertion ((substrate midelora-substrate) ind object attribute  &key (to-abox-p t))
  (declare (ignorable ind object attribute to-abox-p))
  nil)

(defmethod dl-prover-add-attribute-assertion ((substrate racer-substrate) ind object attribute  &key (to-abox-p t))
  (with-critical-section 
   
   (when to-abox-p
     (dl-add-attribute-assertion substrate (abox substrate) ind object attribute)
     (substrate-needs-reset substrate))

   (with-slots (individual-attribute-fillers-cache) substrate
     (setf (gethash (list ind attribute) individual-attribute-fillers-cache)
           (cons object (gethash (list ind attribute) individual-attribute-fillers-cache))))))

#+:midelora
(defmethod dl-prover-add-constraint-assertion ((substrate midelora-substrate) constraint &key (to-abox-p t))
  (declare (ignorable constraint to-abox-p))
  nil)

(defmethod dl-prover-add-constraint-assertion ((substrate racer-substrate) constraint &key (to-abox-p t))
  (with-critical-section 
   
   (when to-abox-p 
     (dl-add-constraint-assertion substrate (abox substrate) constraint)
     (substrate-needs-reset substrate))
   
   (with-slots (constraints-cache)  substrate
     (push constraint constraints-cache))))

;;;
;;;
;;;

#-:dlmaps
(defmethod find-node ((substrate racer-substrate) ind &key &allow-other-keys)
  ind) 

#-:dlmaps
(defmethod get-associated-substrate-node ((substrate racer-substrate) abox-ind)
  (find-node substrate abox-ind :error-p nil))

;;;
;;;
;;;


(defun set-tbox (substrate
                 &key load-from-file error-p new-tbox-p tbox abox new-abox-p &allow-other-keys)
  (let ((tbox-name 
         (or (convert-to-racer-tbox-name tbox (racer-package substrate))
             ;;(convert-to-racer-tbox-name tbox :cl-user)
             (when (and abox new-abox-p) 
               ;;; RACER-TBOXEN sind immer in cl-user! 
               +racer-substrate-default-tbox+))))
    (with-slots (tbox) substrate
      (setf tbox
            (if load-from-file 
                (current-tbox)
              (when tbox-name
                (if new-tbox-p
                    (progn 
                      (when (find-tbox tbox-name nil)
                        (forget-tbox tbox-name))
                      (init-tbox tbox-name)
                      tbox-name)
                  (or (if (find-tbox tbox-name nil)
                          tbox-name
                        nil)
                      (when error-p 
                        (nrql-error "Can't find TBox ~A!" tbox-name))
                      tbox-name))))))))

(defun set-abox (substrate
                 &key load-from-file error-p new-abox-p abox &allow-other-keys)
  (let ((abox-name 
         (convert-to-racer-abox-name abox (racer-package substrate))
         ;;(convert-to-racer-abox-name abox :cl-user)
         ))
    (with-slots (abox tbox) substrate
      (setf abox
            (if load-from-file 
                (current-abox)
              (when abox-name              
                (if new-abox-p
                    (progn 
                      (when (find-abox abox-name nil)
                        (forget-abox abox-name))
                      (init-abox abox-name tbox)
                      abox-name)
                  (or (if (find-abox abox-name nil)
                          abox-name
                        nil)
                      (when error-p 
                        (nrql-error "Can't find ABox ~A!" abox-name))
                      abox-name))))))))

;;;
;;;
;;;

(defmethod initialize-instance :after ((substrate racer-substrate) &rest args
                                       &key load-from-file mirror-abox-p &allow-other-keys)
  (declare (ignorable mirror-abox-p))

  (with-critical-section
  
   (when load-from-file 
     (let ((*package* (racer-package substrate)))
       (load load-from-file)))

   (apply #'set-tbox substrate args)
   (apply #'set-abox substrate args)

   (setf (slot-value substrate 'dbox)
         (or (find-dbox (tbox substrate) :error-p nil)
             (create-dbox (tbox substrate))))))

#+:midelora
(defmethod initialize-instance :after ((substrate midelora-substrate) &rest args)

  (setf (slot-value substrate 'abox) substrate
        (slot-value substrate 'tbox) (prover::tbox substrate))

  (setf (slot-value substrate 'dbox)
        (or (find-dbox (tbox substrate) :error-p nil)
            (create-dbox (tbox substrate)))))

;;;
;;;
;;;

(defmethod delete-substrate :after ((substrate dl-prover-substrate) &key &allow-other-keys)
  (when (dbox substrate)
    (setf *all-dboxes* 
          (delete (dbox substrate) *all-dboxes*))))

;;;
;;;
;;;

(defmethod is-datatype-property-some-value-p ((substrate racer-substrate) concept)
  (and (consp concept)
       (eq (first concept) 'some)
       (let ((qual (third concept)))
         (and (consp qual)
              (third qual) ; Wert auch wirklich vorhanden? 
              (member (second qual)
                      +racer-internal-datatype-property-roles+)))))

#+:midelora
(defmethod is-datatype-property-some-value-p ((substrate midelora-substrate) concept)
  (declare (ignore concept))
  nil)



(defmethod is-datatype-property-at-least-value-p ((substrate racer-substrate) concept)
  (and (consp concept)
       (eq (first concept) 'at-least)
       (let ((qual (fourth concept)))
         (and (consp qual) 
              (third qual) ; Wert auch wirklich vorhanden? 
              (member (second qual)
                      +racer-internal-datatype-property-roles+)))))

#+:midelora
(defmethod is-datatype-property-at-least-value-p ((substrate midelora-substrate) concept)
  (declare (ignore concept))
  nil)


;;;
;;;
;;;

(defmethod compute-abox-mirror ((substrate dl-prover-substrate) &key &allow-other-keys)
  (with-critical-section
    
   (let ((ancestors (mht :test #'equal :size 1000)))

     (with-slots (abox tbox
                       told-values-cache
                       datatype-property-values-cache
                       individual-attribute-fillers-cache
                       role-assertions-in-domain-cache
                       role-assertions-in-range-cache
                       concept-assertions-cache
                       constraint-entailed-cache 
                       individual-instance-cache) substrate
      
       (labels ((register-is-instance-of (ind concept)
                  
                  (if (gethash concept (known-concept-instances-cache substrate))
                      (push ind (gethash concept (known-concept-instances-cache substrate)))
                    (setf (gethash concept (known-concept-instances-cache substrate))
                          (list ind)))

                  (setf (gethash (list ind concept) (individual-instance-cache substrate)) t))
                
                (rec-lookup (a &optional found) 
                  (let ((val (gethash a told-values-cache))
                        (found (cons a found)))

                    (when val
                      (if (consp val)
                          (dolist (b val)
                            (unless (member b found)
                              (let ((x (rec-lookup b found)))
                                (when x
                                  (return-from rec-lookup x)))))
                        val))))
                
                (register-told-value-for (a b)
                  (let ((av (rec-lookup a))
                        (bv (rec-lookup b)))
                    
                    (if (symbolp a) 
                        (if (symbolp b)

                            ;;; a = b
                            
                            (if av 
                                (setf (gethash b told-values-cache) av)
                              (if bv
                                  (setf (gethash a told-values-cache) bv)
                                (progn 
                                  (if (gethash a told-values-cache)
                                      (push b (gethash a told-values-cache))
                                    (setf (gethash a told-values-cache) (list b)))
                                  
                                  (if (gethash b told-values-cache)
                                      (push a (gethash b told-values-cache))
                                    (setf (gethash b told-values-cache) (list a))))))
                          
                          ;;; a = 3
                      
                          (setf (gethash a told-values-cache) b))

                      (if (symbolp b)

                          ;;; 3 = b 
                          
                          (setf (gethash b told-values-cache) a)

                        ;;; 3 = 4 

                        ))))

                (register-datatype-property-value (property ind property-value concept)

                  (if (gethash ind datatype-property-values-cache)
                      (push property-value (gethash ind datatype-property-values-cache))
                    (setf (gethash ind datatype-property-values-cache)
                          (list property-value)))
                  
                  ;;; also add to ABox!
                  
                  (when *add-role-assertions-for-datatype-properties-p*
                    
                    (let ((datatype-succs
                           (mapcar #'cadar
                                   (remove-if-not #'(lambda (x) 
                                                      (eq (second x) property))
                                                  (dl-prover-all-role-assertions-for-individual-in-domain 
                                                   substrate ind)))))

                      ;; is there a datatype successor which has the required concept assertion?
                      ;; -> dont need to add another one!

                      (unless (some #'(lambda (succ)
                                        (gethash (list succ concept)
                                                 (individual-instance-cache substrate)))
                                    datatype-succs)

                        (let ((new-ind
                               (intern (format nil "~A-~A"
                                               +secret-prefix+
                                               (incf *sym-count*))
                                       (racer-package substrate))))
                                 
                          (dl-prover-add-role-assertion substrate 
                                                        ind
                                                        new-ind
                                                        property)
                                 
                          (dl-prover-add-concept-assertion substrate
                                                           new-ind 
                                                           concept))))))

                (get-implied-concepts-for-cache (concept)
                  (let ((concepts
                         (or (gethash concept ancestors) 
                             (setf (gethash concept ancestors)
                                   (remove-duplicates
                                    (cons concept
                                          (cons 'top
                                                (cons '*top*
                                                      (if (consp concept)
                                                          (append
                                                    
                                                           (when (and *ensure-tbox-classification-p*
                                                                      (dl-tbox-classified-p substrate tbox)
                                                                      *classify-concepts-in-instance-assertions-p*)
                                                             (append 
                                                              (reduce #'append (dl-atomic-concept-ancestors substrate concept tbox))
                                                              (reduce #'append (mapcar #'ensure-list 
                                                                                       (dl-atomic-concept-synonyms substrate concept tbox)))))

                                                           (case (first concept)
                                                             (and (reduce #'append
                                                                          (mapcar #'get-implied-concepts-for-cache (rest concept))))
                                                      
                                                             (some
                                                              (when (is-datatype-property-some-value-p substrate concept)
                                                                (let ((role (second concept))
                                                                      (qual (third concept)))
                                                                  (mapcar #'(lambda (qc)
                                                                              `(some ,role ,qc))
                                                                          (get-implied-concepts-for-cache qual)))))
                                                      
                                                             (at-least 
                                                              (when (is-datatype-property-at-least-value-p substrate concept)
                                                                (let ((role (third concept))
                                                                      (qual (fourth concept))
                                                                      (n (second concept)))
                                                                  (mapcar #'(lambda (qc)
                                                                              `(at-least ,n ,role ,qc))
                                                                          (get-implied-concepts-for-cache qual)))))
                                                      
                                                             ((min max divisible not-divisible string= string<>
                                                                   > >= < <= <> = equal unequal)
                                                              (let ((attrib (second concept)))
                                                                `((an ,attrib)
                                                                  (a ,attrib))))

                                                             ((a an) 
                                                              (let ((attrib (second concept)))
                                                                `((an ,attrib)
                                                                  (a ,attrib))))))
                                                  
                                                        (when *ensure-tbox-classification-p*
                                                          (unless (dl-tbox-classified-p substrate tbox)
                                                            (dl-classify-tbox substrate tbox))
                                                          (append 
                                                           (reduce #'append (dl-atomic-concept-ancestors substrate concept tbox))
                                                           (reduce #'append
                                                                   (mapcar #'ensure-list 
                                                                           (dl-atomic-concept-synonyms substrate concept tbox)))))))))
                                    :test #'equal)))))
                    ;; (write concepts) 
                    concepts)))

         ;;;
         ;;; Start Mirroring
         ;;; 
                 
         (let ((*told-information-reasoning-p* nil)
               (datatype-assertions nil))

           ;; (princ 1)

           (dolist (c (dl-prover-all-constraints substrate))
             
             (setf (gethash c constraint-entailed-cache) t)
             (when (member (to-keyword (first c))
                           '(:= :equal :string= :boolean=))
               (apply #'register-told-value-for (rest c))))
           
           ;; (princ 2)
           
           (dolist (role (dl-prover-all-roles substrate))

             (when
                 #+:racer-server (role-p role tbox)
               #-:racer-server t
               ;; all-roles liefert mist!
               (dl-prover-atomic-role-descendants substrate role)
               (dl-prover-atomic-role-inverse substrate role)))

	   ;; (princ 3)
	   
           (dolist (ind (dl-prover-all-individuals substrate))
             
             (setf (gethash (list ind +individual-exists-concept+) 
                            (individual-instance-cache substrate)) t)

             (register-is-instance-of ind 'top)
             (register-is-instance-of ind '*top*))

           ;; (princ 4)

           (dolist (role-assertions (if *initial-abox-mirroring-p*
                                        (list (dl-all-annotation-role-assertions substrate abox)
                                              (dl-all-role-assertions substrate abox))
                                      (list (dl-all-annotation-role-assertions substrate))))

             (dolist (ra role-assertions)
               (let ((from (first (first ra)))
                     (to (second (first ra))))

                 (if (gethash to role-assertions-in-range-cache)
                     (push ra (gethash to role-assertions-in-range-cache))
                   (setf (gethash to role-assertions-in-range-cache)
                         (list ra)))
          
                 (if (gethash from role-assertions-in-domain-cache)
                     (push ra (gethash from role-assertions-in-domain-cache))
                   (setf (gethash from role-assertions-in-domain-cache)
                         (list ra))))))

           ;; (princ 5)

           (dolist (aa (dl-all-attribute-assertions substrate abox))
             (let* ((aa (rest aa))
                    (from (first aa))                     
                    (to (second aa))
                    (attribute (third aa))
                    (entry (list from attribute)))
              
               (if (gethash entry individual-attribute-fillers-cache)
                   (push to (gethash entry individual-attribute-fillers-cache))
                 (setf (gethash entry individual-attribute-fillers-cache)
                       (list to)))))
           
           ;; (princ 6)

           (dolist (concept-assertions (list (dl-all-annotation-concept-assertions substrate abox)
                                             (dl-all-concept-assertions substrate abox)))
                                        
             (dolist (ca concept-assertions)
               (let ((ind (first ca))
                     (concept (second ca)))
                
                 (when (gethash (list ind +individual-exists-concept+) 
                                (individual-instance-cache substrate))

                   ;;; hier sollen nur die ABox-Individuen bearbeitet werden;
                   ;;; Racer gibt unter all-annotation-concept-assertions auch
                   ;;; Klassen-Annotation etc. zurueck! 

                   (if (gethash ind concept-assertions-cache)
                       (push ca (gethash ind concept-assertions-cache))
                     (setf (gethash ind concept-assertions-cache) (list ca)))

                    
                   (let ((dtp-p
                          (or (is-datatype-property-some-value-p substrate concept)
                              (is-datatype-property-at-least-value-p substrate concept))))

                     (if (or *initial-abox-mirroring-p* dtp-p)
                          
                         (progn
                           (when dtp-p
                             (push ca datatype-assertions))
                           (let ((concepts (get-implied-concepts-for-cache concept)))
                             (dolist (concept concepts) ; orig. concept ist in (get-implied-...) enthalten! 
                               (register-is-instance-of ind concept))))

                       (register-is-instance-of ind concept)))))))
           
           ;; (princ 7)
            
           (dolist (da datatype-assertions)
             (let ((ind (first da))
                   (concept (second da)))

               ;;;
               ;;; Datatype-Property Encoding of Racer
               ;;; 
               
               (cond ((is-datatype-property-some-value-p substrate concept)
                      
                      (let* ((property (second concept))
                             (internal-attribute (second (third concept)))
                             (property-value (list property
                                                   (third (third concept)))))
                                 
                        (declare (ignorable internal-attribute))
                                 
                        ;;; e.g. 
                        ;;; (SOME |http://www.owl-ontologies.com/unnamed.owl#age| 
                        ;;;       (EQUAL RACER-INTERNAL%HAS-INTEGER-VALUE 35))
                                 
                        (register-datatype-property-value property ind property-value 
                                                          (third concept))))

                     ((is-datatype-property-at-least-value-p substrate concept)
                               
                      (let* ((property (third concept))
                             (internal-attribute (second (fourth concept)))
                             (property-value (list property
                                                   (third (fourth concept)))))

                        (declare (ignorable internal-attribute))
                                 
                        ;;; e.g. 
                        ;;; (AT-LEAST 3 |http://www.owl-ontologies.com/unnamed.owl#id| 
                        ;;;              (EQUAL RACER-INTERNAL%HAS-INTEGER-VALUE 35))
                                 
                        (register-datatype-property-value property ind property-value
                                                          (fourth concept)))))))))))))
        


(defmethod initialize-instance :after ((substrate tbox-mirror-substrate) &rest args
                                       &key &allow-other-keys)
  (declare (ignorable args))

  (setf (slot-value substrate 'mirror-of-tbox)
        (tbox substrate))
             
  ;; (setf (saved-state-vector substrate) 
  ;;      (get-state-vector substrate))

  )


(defmethod compute-tbox-mirror ((substrate tbox-mirror-substrate))
  (with-critical-section      
   (with-slots (abox tbox 
                     mirror-of-tbox
                     role-descendants-cache 
                     atomic-role-inverse-cache 
                     concept-instances-cache
                     ;; constraints-cache
                     abox-individuals-cache concept-assertions-cache
                     role-assertions-in-range-cache 
                     role-assertions-in-domain-cache                       
                     individual-instance-cache
                     individual-attribute-fillers-cache) substrate
    
     (let* ((package
             #-:midelora (racer-package substrate)
             #+:midelora (find-package :prover))
             
            (has-child-role (change-package-of-description 'has-child 
                                                           package))

            (has-parent-role (change-package-of-description 'has-parent 
                                                            package))

            (has-descendant-role (change-package-of-description 'has-descendant 
                                                                package))

            (has-ancestor-role (change-package-of-description 'has-ancestor 
                                                              package)))
    
       (let ((concepts 
              (cons #-:midelora 
                    '(racer::*top* racer::top)
                    #+:midelora 
                    '(prover::top)                     
                    (cons #-:midelora 
                          '(racer::*bottom* racer::bottom)
                          #+:midelora 
                          '(prover::bottom)
                           
                          (remove nil 
                                  (mapcar #'(lambda (x) 
                                              (let ((x (ensure-list x)))
                                                (remove-if #'(lambda (x)
                                                               (case (to-keyword x)
                                                                 ((:bottom :*bottom* :top :*top*) t)
                                                                 (otherwise 
                                                                  (not (dl-concept-p substrate x tbox)))))
                                                           x)))
                                          (dl-all-atomic-concepts substrate tbox)))))))

         (setf abox-individuals-cache (mapcar #'first concepts))
      
         (dolist (concept concepts)
           (let ((name (first concept)))

             (setf (gethash (list name +individual-exists-concept+) individual-instance-cache) t)

             (setf (gethash (list name name) individual-instance-cache) t)
             (setf (gethash name concept-assertions-cache) (list name))
                              
             (dolist (equi concept)
               (push equi (gethash name concept-assertions-cache))
               (setf (gethash (list name equi) individual-instance-cache) t)
                        
               (dolist (concept2 concepts)
                 (unless (equal concept concept2)
                   (dolist (equi concept2)
                     (setf (gethash (list name equi) individual-instance-cache) nil))))

               (setf (gethash equi concept-instances-cache)
                     (list name)))
              
             (let* ((children (remove-if-not #'(lambda (x) (dl-concept-p substrate x tbox))
                                             (mapcar #'first (dl-atomic-concept-children substrate name tbox))))

                    ;; Annahme: der erste Name in der Liste der Synonyme ist
                    ;; immer der Knotenname (Reihenfolge der Synonyme wie bei
                    ;; all-atomic-concepts angenommen!) 

                    (from-assertions (mapcar #'(lambda (child)
                                                 (list (list name child) has-child-role))
                                             children)))

               (setf (gethash name role-assertions-in-domain-cache) from-assertions)

               (dolist (assertion from-assertions)
                 (let ((child (second (first assertion))))
                   (if (gethash child role-assertions-in-range-cache)
                       (push assertion (gethash child role-assertions-in-range-cache))                  
                     (setf (gethash child role-assertions-in-range-cache) (list assertion)))))))))


          ;(set-tbox substrate :tbox 'default :new-tbox-p nil)
          ;(set-abox substrate :abox 'default :new-abox-p nil)

       (setf tbox nil
             abox nil)
          
       (setf (gethash has-parent-role role-descendants-cache) (list has-parent-role))
       (setf (gethash has-child-role role-descendants-cache) (list has-child-role))
                
       (setf (gethash has-descendant-role role-descendants-cache)
             (list has-descendant-role has-child-role))

       (setf (gethash has-ancestor-role role-descendants-cache)
             (list has-ancestor-role has-parent-role))
          
       (setf (transitive-roles-cache substrate) 
             (list has-descendant-role has-ancestor-role))
          
       (dolist (pair (list (list has-parent-role has-child-role)
                           (list has-descendant-role has-ancestor-role)))

         (setf (gethash (first pair) atomic-role-inverse-cache)
               (second pair))
         (setf (gethash (second pair) atomic-role-inverse-cache)
               (first pair)))))))

;;;
;;;
;;;

(defmethod convert-to-dl-prover-individual-name ((substrate racer-substrate) ind)
  (convert-to-racer-individual-name ind (racer-package substrate)))

#+:midelora
(defmethod convert-to-dl-prover-individual-name ((substrate midelora-substrate) ind)
  (change-package-of-description ind (find-package :prover)))

(defmethod convert-to-dl-prover-attribute-expression ((substrate racer-substrate) attrib)
  (convert-to-racer-attribute-expression attrib (racer-package substrate)))

