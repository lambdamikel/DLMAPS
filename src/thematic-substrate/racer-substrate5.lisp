;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;; die Basis (auch fuer nRQL!) ist in common.lisp! 
;;;

(defmethod info ((substrate racer-substrate) &key &allow-other-keys)
  (format nil "Name: ~A, Type: ~A Nodes: ~A, Edges: ~A"
          (name substrate)
          (type-of substrate)
          (no-of-nodes substrate)
          (no-of-edges substrate)))

;;;
;;;
;;;

(defpersistentclass racer-abox-mirror-substrate (racer-substrate))

;;;
;;;
;;;

(defpersistentclass racer-substrate-object (substrate-object) 
  ;;; stellt (optionale!) assoziation mit racer-objekten her
  ((racer-object :reader racer-object :initarg :racer-object :initform nil)
   (racer-description :reader racer-description :initarg :racer-description :initform nil)))

(defpersistentclass racer-substrate-node (racer-substrate-object substrate-node))

(defpersistentclass racer-substrate-edge (racer-substrate-object substrate-edge))

;;;
;;;
;;;

(defpersistentclass racer-descriptions-substrate-object (substrate-object))

(defpersistentclass racer-descriptions-substrate-node (racer-descriptions-substrate-object substrate-node))

(defpersistentclass racer-descriptions-substrate-edge (racer-descriptions-substrate-object substrate-edge))

;;;
;;; 
;;;

(defpersistentclass racer-abox-mirror-substrate-object (racer-substrate-object))

(defpersistentclass racer-abox-mirror-substrate-node (racer-abox-mirror-substrate-object racer-substrate-node))

(defpersistentclass racer-abox-mirror-substrate-edge (racer-abox-mirror-substrate-object racer-substrate-edge))

;;;
;;;
;;;

(defmethod get-standard-node-class ((substrate racer-substrate))
  'racer-substrate-node)

(defmethod get-standard-edge-class ((substrate racer-substrate))
  'racer-substrate-edge)

(defmethod get-standard-node-description-class ((substrate racer-substrate))
  (call-next-method))

(defmethod get-standard-edge-description-class ((substrate racer-substrate))
  (call-next-method))

;;; damit das "hybride racer-substrate" beliebig instantiert werden kann, 
;;; ist die description-class für die Substrate-Objekte nicht festgelegt!
;;; in der ABox werden natürlich Racer-Beschreibungen generiert; beim 
;;; Anlegen einer Knoten/Kanten-Beschreibung wird gesagt, ob die Beschreibung
;;; automatisch in eine Racer-Beschreibung konvertiert werden soll ("mirroring"), 
;;; oder alternativ kann die :racer-description "von Hand" gesetzt werden, und 
;;; dann ist die Substrate-Knoten/Kanten-Beschreibung unabhaengig von den Beschreibungen 
;;; in der Racer-Abox

;;;
;;; Beim Racer-Descriptions-Substrate wird *keine* assoziierte ABox verwendet; 
;;; daher werden, um Racer-Reasoning auszunutzen, einfach Racer-Konzepte/Rollen
;;; verwendet auf der Substrat-Ebene! Bestimmte Inkonsistenzen koennen natürlich
;;; nicht entdeckt werden, solange man nicht den Racer-Abox-Konsistenzcheck auf
;;; Racer-Descriptions-Substrate "reimplementiert". 
;;;

(defmethod get-standard-node-class ((substrate racer-descriptions-substrate))
  'racer-descriptions-substrate-node)

(defmethod get-standard-edge-class ((substrate racer-descriptions-substrate))
  'racer-descriptions-substrate-edge)

(defmethod get-standard-node-description-class ((substrate racer-descriptions-substrate))
  'racer-node-description)

(defmethod get-standard-edge-description-class ((substrate racer-descriptions-substrate))
  'racer-edge-description)

;;;
;;;
;;;

(defmethod get-standard-node-class ((substrate racer-abox-mirror-substrate))
  'racer-abox-mirror-substrate-node)

(defmethod get-standard-edge-class ((substrate racer-abox-mirror-substrate))
  'racer-abox-mirror-substrate-edge)

(defmethod get-standard-node-description-class ((substrate racer-abox-mirror-substrate))
  (call-next-method))

(defmethod get-standard-edge-description-class ((substrate racer-abox-mirror-substrate))
  (call-next-method))

;;;
;;;
;;;

(defun thematic-object-p (object)
  (and (typep object 'racer-substrate-object) 
       (racer-description object)))

(defun non-thematic-object-p (object)
  (and (typep object 'substrate-object)
       (not (thematic-object-p object))))

;;;
;;;
;;;

(defun non-thematic-node-p (node)
  (and (typep node 'substrate-node)
       (not (thematic-node-p node))))

(defun thematic-node-p (node)
  (and (typep node 'racer-substrate-node) 
       (racer-description node)))

;;;
;;;
;;;

(defun non-thematic-edge-p (edge)
  (and (typep edge 'substrate-edge)
       (not (thematic-edge-p edge))))

(defun thematic-edge-p (edge)
  (and (typep edge 'racer-substrate-edge) 
       (racer-description edge)))

;;;
;;;
;;;

(defmethod consistent-p ((obj racer-substrate) &key &allow-other-keys)
  (and ;(abox-consistent-p (abox obj))
   (call-next-method)))

;;;
;;; hier definiert, denn diese gibt es in nRQL nicht
;;;

(defmethod initialize-instance :after ((substrate racer-descriptions-substrate) &rest args
                                       &key load-from-file &allow-other-keys)
  (when load-from-file 
    (load load-from-file))
  (apply #'set-tbox substrate args))


(defmethod initialize-instance :after ((substrate racer-abox-mirror-substrate) &rest args
                                       &key &allow-other-keys)
  (let* ((abox (abox substrate))
         (nodes (all-individuals abox))
         (node-description-class (get-standard-node-description-class substrate))
         (edge-description-class (get-standard-edge-description-class substrate)))

   
    (let ((hash (make-hash-table :size 10000)))
      (dolist (assertion (all-concept-assertions abox))
        (let ((node (first assertion))
              (concept (second assertion)))
          (if (gethash node hash) 
              (push concept (gethash node hash))
            (setf (gethash node hash) (list concept)))))
    
      (dolist (node nodes)
        (let* ((concepts (gethash node hash))
               (concept (make-node-description node-description-class
                                               (or concepts :top))))
          (create-node substrate node concept))))
    
    (dolist (edge (all-role-assertions abox))
      (let ((from (caar edge))
            (to (cadar edge))
            (role (make-edge-description edge-description-class (second edge))))
        (unless (find-node substrate from)
          (create-node substrate from (make-node-description node-description-class 'top)))
        (unless (find-node substrate to)
          (create-node substrate to (make-node-description node-description-class 'top)))
        (create-edge substrate from to role)))))

;;;
;;;
;;;

(defmethod copy ((substrate racer-substrate) &rest args)
  (declare (ignorable args))
  (let ((copy (call-next-method)))
    (with-slots (tbox abox) copy 
      (setf tbox (tbox substrate)
            abox (abox substrate)))
    copy))

(defmethod copy ((substrate racer-descriptions-substrate) &rest args)
  (declare (ignorable args))
  (let ((copy (call-next-method)))
    (with-slots (tbox) copy 
      (setf tbox (tbox substrate)))
    copy))

;;;
;;;
;;;

(defmethod delete-graph :after ((substrate racer-substrate) &rest args &key delete-abox-p &allow-other-keys)
  (with-slots (abox) substrate
    (when (and delete-abox-p abox)
      (forget-abox abox))))

;;;
;;;
;;;

(defmethod initialize-instance :after ((node racer-substrate-node) &rest initargs 
                                       &key copy-p racer-description &allow-other-keys)
  (unless copy-p
    (with-slots (name in-graph racer-object) node
      (with-slots (abox racer-package) in-graph      
        (when abox
          (setf racer-object (convert-to-racer-individual-name name racer-package))
          (if racer-description
              (add-concept-assertion abox racer-object racer-description)
            (add-concept-assertion abox racer-object 'racer:top)))))))


(defmethod delete-node :after ((substrate racer-substrate) (node racer-substrate-node) &key &allow-other-keys)  
  (when (and (thematic-node-p node)
             (abox substrate))
    (forget-concept-assertion 
     (abox substrate) 
     (racer-object node) (racer-description node))))

;;;
;;;
;;;

(defmethod initialize-instance :after ((object racer-substrate-edge) &rest initargs 
                                       &key copy-p racer-description &allow-other-keys)
  (unless copy-p
    (with-slots (in-graph from to racer-object) object
      (with-slots (abox) in-graph
        (setf racer-object (list (racer-object from) (racer-object to) racer-description))
        (when racer-description
          (add-role-assertion abox (racer-object from) (racer-object to) racer-description))))))
  
(defmethod delete-edge :after ((substrate racer-substrate) (edge racer-substrate-edge) &key &allow-other-keys)
  (when (thematic-edge-p edge)
    (forget-role-assertion (abox substrate) 
                           (racer-object (from edge)) 
                           (racer-object (to edge))
                           (racer-description edge))))

;;;
;;; Wird bei der Objekterzeugung ":racer-description t" angeben, werden diese
;;; beiden Methoden verwendet, um die "description" in eine "gültige" (?) 
;;; Racer-Beschreibung umzuwandeln. Sollte für Unterklassen entsprechend
;;; angepasst werden - je nach "description"-Unterklasse sollten entsprechende
;;; RACER-Umschreibungen zurückgegeben werden (wenn überhaupt möglich...) 
;;; 

(defmethod convert-to-racer-description ((descr node-description) &optional package)
  (let ((descr (textual-description descr)))
    (convert-to-racer-concept-expression (if (consp descr)
                                             `(and ,@descr)
                                           descr)
                                         package)))

(defmethod convert-to-racer-description ((descr edge-description) &optional package)
  (convert-to-racer-role-expression (textual-description descr) package))

;;;
;;; Per Default werden Knoten gespiegelt, Kanten nicht!!! s. "convert-to-racer-description-p"!
;;;

(defmethod create-node ((substrate racer-substrate) name (description node-description)
                        &rest args)
  (apply #'make-node substrate :name name :description description args))


(defmethod make-node ((substrate racer-substrate) 
                      &rest args
                      &key description racer-description (convert-to-racer-description-p t) &allow-other-keys)
  (apply #'call-next-method substrate 
         :racer-description                 
         (if (and (not racer-description)
                  convert-to-racer-description-p)
             (convert-to-racer-description description (racer-package substrate))
           racer-description)
         args))

(defmethod create-edge ((substrate racer-substrate) (from substrate-node) (to substrate-node)
                        (description edge-description)
                        &rest args)
  (apply #'make-edge substrate from to :description description args))

(defmethod make-edge ((substrate racer-substrate) (from substrate-node) (to substrate-node)
                      &rest args
                      &key description racer-description (convert-to-racer-description-p nil) &allow-other-keys)
  (apply #'call-next-method substrate from to 
         :description description 
         :racer-description                      
         (if (and (not racer-description)
                  convert-to-racer-description-p)
             (convert-to-racer-description description (racer-package substrate))
           racer-description)
         args))
;;;
;;;
;;;

(defmethod copy-graph-item ((substrate racer-substrate) (object racer-substrate-object) &rest args)
  (apply #'call-next-method
         substrate object 
         :convert-to-racer-description-p nil
         :racer-object (racer-object object)
         :racer-description (racer-description object) 
         args))

;;;
;;;
;;;

(defmethod add-to-node ((substrate racer-substrate) (node substrate-node) (delta-description node-description)
                        &key &allow-other-keys)
  (break "To be implemented!"))

(defmethod add-to-node ((substrate racer-descriptions-substrate) (node substrate-node) (delta-description node-description)
                        &key &allow-other-keys)
  (break "To be implemented!"))

;;;
;;;
;;;

(defmethod get-associated-abox-individual ((substrate racer-substrate) (node substrate-node))
  nil)

(defmethod get-associated-abox-individual ((substrate racer-substrate)  (node racer-substrate-node))
  (racer-object node))

#+:midelora
(defmethod get-associated-abox-individual ((substrate midelora-substrate)  (node substrate-node))
  node)


;;;
;;;
;;;

(defmethod get-associated-substrate-node ((substrate substrate) abox-ind)
  ;;;(find-node substrate abox-ind :error-p nil))
  ;;; wegen des Package-Problemes!!!
  (loop as key being the hash-key of (node-table substrate) 
        as val being the hash-value of (node-table substrate) 
        when (and (symbolp key)
                  (string-equal (symbol-name key)
                                (symbol-name abox-ind)))
        return val))

#+:midelora
(defmethod get-associated-substrate-node ((substrate midelora-substrate) (abox-ind substrate-node))
  abox-ind)

;;;
;;;
;;;

(defmethod thematic-nodes ((substrate racer-substrate))
  (get-nodes substrate :satisfying #'thematic-node-p))


(defmethod thematic-edges ((substrate racer-substrate))
  (get-edges substrate :satisfying #'thematic-edge-p))

;;;
;;;
;;;

#|

(with-substrate (test :type 'racer-substrate 
                      :delete-if-exists-p t
                      :error-p t
                      :new-tbox-p t
                      :new-abox-p t 
                      :abox 'vdvgamma)
  
  (setf *x* *cur-substrate*)
  
  (node a (ncon (a b (all r (or (not d) (not c))))))
  
  (node b (ncon (c d)))
  
  (edge a b (econ r) :convert-to-racer-description-p t) ; wichtig, sonst wird keine ABox-Kante angelegt!

  
  (visualize *x*))



(with-substrate (test :type 'racer-descriptions-substrate 
                      :delete-if-exists-p t
                      :error-p t
                      :new-tbox-p t
                      :new-abox-p t 
                      :abox 'gamma)
  
  (setf *x* *cur-substrate*)
  
  (node a (ncon (and a (all r (not b)))))
  
  (node b (ncon (or b (all (inv r) (not a)))))
  
  (edge a b (econ r) :convert-to-racer-description-p t) ; wichtig, sonst wird keine ABox-Kante angelegt!
  
  (visualize *x*))



(with-substrate (family-mirror
                 :type 'racer-abox-mirror-substrate
                 :delete-if-exists-p t
                 ;:racer-package 'racer-user
                 :load-from-file "/home/mwessel/lispworks/work/dlmaps/query/family-1-no-signature.racer")
  (setf *x* *cur-substrate*))



(with-substrate (heflin
                 :type 'racer-abox-mirror-substrate
                 :delete-if-exists-p t
                 :racer-package 'thematic-substrate
                 :load-from-file "/home/mwessel/lispworks/work/dlmaps/query/heflin/heflin/university.lisp")
  (setf *x* *cur-substrate*))


(with-substrate (heflin
                 :type 'racer-abox-mirror-substrate
                 :delete-if-exists-p t
                 :racer-package 'thematic-substrate
                 :load-from-file "/home/mwessel/lispworks/work/dlmaps/query/heflin/heflin/university0.lisp")
  (setf *x* *cur-substrate*))


|# 

