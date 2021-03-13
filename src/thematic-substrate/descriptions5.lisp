;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;;
;;;

(defvar *node-description-class* nil)

(defvar *edge-description-class* nil)

;;;
;;;
;;;

(defpersistentclass semantic-entity () ; abstrakt
  ;;; 
  ;;; die textual-description is sozusagen die Basis-Beschreibung! 
  ;;; Bei einer edge-description z.B. die Liste der Rollen-Symbole, als Disjunktion interpretiert
  ;;; nur die description selbst weiß wie die "rohe" textual-description zu interpretieren ist
  ;;; (ob eine textual-descritpion vom Typ Liste z.B. eine Disjunktion oder Konjunktion repräsentiert)
  ;;;
  ((textual-description :accessor textual-description :initarg :textual-description)
   (original-description :accessor original-description :initarg :original-description)
   
   (constructor-sym :reader constructor-sym :initarg :constructor-sym :initform nil)))

(defmethod initialize-description ((descr semantic-entity))
  t)

(defmethod initialize-instance :after ((object semantic-entity) &rest initargs &key dont-initialize-p)
  (when (and (slot-boundp object 'textual-description)
             (not (slot-boundp object 'original-description)))
    (setf (original-description object) 
          (textual-description object)))
  (unless dont-initialize-p
    ;;; Idee: hier läuft der Parser an, um die ursprüngliche textual-description zu
    ;;; bearbeiten; original-description memoriert den originalen Wert der Descr. 
    (initialize-description object)))

(defmethod change-textual-description ((description semantic-entity) textual-description &key reinitialize-p)
  (setf (textual-description description) textual-description)
  (when reinitialize-p (initialize-description description))
  description)

(defmethod copy ((object semantic-entity) &rest args)
  (subclass-responsibility 'copy))

(defmethod print-object ((object semantic-entity) stream)
  (format stream "#<~A: ~A>" (type-of object) (textual-description object)))

;;;
;;; f. Graph-Visualizer
;;;

(defmethod get-textual-description ((object semantic-entity))
  (format nil "~A" (textual-description object)))

;;;
;;; Mixins - keine Descriptions!
;;;

(defpersistentclass racer-concept-mixin ()
  ((racer-concept :reader racer-concept)
   (racer-tbox :reader racer-tbox :initarg :racer-tbox :initform nil)))

(defpersistentclass racer-role-mixin ()
  ((racer-role :reader racer-role)
   (racer-tbox :reader racer-tbox :initarg :racer-tbox :initform nil)

   (allow-negated-roles-p :initarg :allow-negated-roles-p :initform nil)))

(defmethod dl-concept ((rcm racer-concept-mixin))
  (racer-concept rcm))


(defmethod dl-role ((rcm racer-role-mixin))
  (racer-role rcm))

;;;
;;; 
;;;

(defmethod initialize-description :after ((descr racer-concept-mixin))
  (with-slots (racer-package racer-concept racer-tbox textual-description) descr
    (setf racer-concept 
          (convert-to-racer-concept-expression textual-description racer-package)

          racer-tbox 
          (convert-to-racer-tbox-name racer-tbox racer-package))))

(defmethod initialize-description :after ((descr racer-role-mixin))
  (with-slots (racer-package racer-role racer-tbox textual-description) descr

    (setf racer-role
          (convert-to-racer-role-expression textual-description)

          racer-tbox
          (convert-to-racer-tbox-name racer-tbox racer-package))))

;;;
;;;
;;;


(defmethod check-same-tbox-and-package ((descr1 racer-concept-mixin) (descr2 racer-concept-mixin))
  (if (or (not (eq (racer-tbox descr1)
                   (racer-tbox descr2)))
          (not (eq (racer-package descr1)
                   (racer-package descr2))))
      (error "~A ~A must use the same TBox and/or package!" descr1 descr2)))


(defmethod check-same-tbox-and-package ((descr1 racer-role-mixin) (descr2 racer-role-mixin))
  (if (or (not (eq (racer-tbox descr1)
                   (racer-tbox descr2)))
          (not (eq (racer-package descr1)
                   (racer-package descr2))))
      (error "~A ~A must use the same TBox and/or package!" descr1 descr2)))

;;;
;;;
;;;

(defmethod get-negated-concept ((descr racer-concept-mixin))
  (let ((descr 
         (make-description 'racer-node-description
                           `(not ,(textual-description descr)))))
    (initialize-description descr)
    descr))

(defmethod concept-is-consistent-p ((descr racer-concept-mixin))
  (concept-satisfiable-p
   (convert-to-racer-concept-expression
    (racer-concept descr)
    (racer-package descr))
   (racer-tbox descr)))

(defmethod concept-is-tautological-p ((descr racer-concept-mixin))
  (not (concept-satisfiable-p
        (convert-to-racer-concept-expression
         `(not (and ,(racer-concept descr)))
         (racer-package descr))
        (racer-tbox descr))))

(defmethod concept-implies-p ((descr1 racer-concept-mixin) (descr2 racer-concept-mixin))
  (check-same-tbox-and-package descr1 descr2)
  (not (concept-satisfiable-p
        (convert-to-racer-concept-expression
         `(and ,(racer-concept descr1)
               (not ,(racer-concept descr2)))
         (racer-package descr1))
        (racer-tbox descr1))))

;;;
;;;
;;;

(defmethod consistent-p ((descr racer-concept-mixin) &rest args)
  (apply #'concept-is-consistent-p descr args))

(defmethod tautological-p ((descr racer-concept-mixin) &rest args)
  (apply #'concept-is-tautological-p descr args))

(defmethod implies-p ((descr1 racer-concept-mixin) (descr2 racer-concept-mixin) &rest args)
  (apply #'concept-implies-p descr1 descr2 args))

;;;
;;;
;;;

(defmethod role-is-consistent-p ((descr racer-role-mixin))
  t)

(defmethod role-is-tautological-p ((descr racer-role-mixin))
  nil)

(defmethod role-implies-p ((descr1 racer-role-mixin) (descr2 racer-role-mixin))
  (check-same-tbox-and-package descr1 descr2)
  (not (concept-satisfiable-p
        (convert-to-racer-concept-expression
         `(and (some ,(racer-role descr1) top)
               (all ,(racer-role descr2) bottom))
         (racer-package descr1))
        (racer-tbox descr1))))

;;;
;;;
;;;

(defmethod consistent-p ((descr racer-role-mixin) &rest args)
  (apply #'role-is-consistent-p descr args))

(defmethod tautological-p ((descr racer-role-mixin) &rest args)
  (apply #'role-is-tautological-p descr args))

(defmethod implies-p ((descr1 racer-role-mixin) (descr2 racer-role-mixin) &rest args)
  (apply #'role-implies-p descr1 descr2 args))

;;; 
;;; Abstrakte Descriptions
;;; Achtung! Die "racer"-Descriptions sind für die SUBSTRATE-EBENE, 
;;; *nicht* für die :racer-description-Ebene bei make-node etc.!!!! 
;;;

(defpersistentclass description (semantic-entity)
  ((satisfiable :reader satisfiable :initform :not-tested)
   (inconsistent :reader inconsistent :initform :not-tested)
   (tautological :reader tautological :initform :not-tested)
   (entails :reader entails :initform nil)
   (not-entails :reader not-entails :initform nil)
   (entailed-by :reader entailed-by :initform nil)
   (not-entailed-by :reader not-entailed-by :initform nil)))
   

(defmethod make-description ((class symbol) descr &rest args &key type &allow-other-keys)
  (apply #'make-instance (or type class) 
         :textual-description descr 
         :constructor-sym class 
         :allow-other-keys t 
         args))

(defmethod copy ((description description) &rest args)
  (apply #'make-description 
         (constructor-sym description) 
         (copy-tree (textual-description description))
         args))

;;;
;;;
;;;

(defpersistentclass node-description (description)) ;;; abstrakt

(defpersistentclass edge-description (description)) ;;; abstrakt

;;;
;;;
;;;

(defmethod get-inverse-description ((descr description))
  (subclass-responsibility 'get-inverse-description))

(defmethod get-negated-description ((descr description))
  (subclass-responsibility 'get-negated-description))

;;;
;;; Simple Descriptions: lediglich syntaktische Beschreibungen
;;; 

(defpersistentclass simple-description (description)) ;;; abstrakt

(defpersistentclass simple-disjunctive-description (simple-description)) ; Liste = Disjunktion

(defpersistentclass simple-conjunctive-description (simple-description)) ; Liste = Konjunktion

;;; (defpersistentclass boolean-description (simple-description)) ;;; to be implemented

;;;
;;;
;;;

(defmethod underspecified-p ((description simple-disjunctive-description) &rest args)
  (cdr (ensure-list (textual-description description))))

(defmethod get-disjuncts ((description simple-disjunctive-description) &rest args)
  (ensure-list (textual-description description)))

;;;
;;;
;;;

(defmethod underspecified-p ((description simple-conjunctive-description) &rest args)
  nil)

(defmethod get-conjuncts ((description simple-conjunctive-description) &rest args)
  (ensure-list (textual-description description)))

;;;
;;;
;;;

(defmethod initialize-description :after ((object simple-description))
  (with-slots (textual-description) object
    (when (consp textual-description) 
      (setf (slot-value object 'textual-description)
            (remove-duplicates textual-description
                               :test #'equal)))))

(defmethod print-object ((object simple-description) stream)
  (format stream "#<~A: ~A>" (type-of object)
          (textual-description object)))

;;;
;;; Achtung - bewusst redundant!!! Denke an unvollstaendige Algorithmen
;;;

(defmethod reset-sat-status ((description description))
  'ok)

(defmethod reset-sat-status :before ((description description))
  (with-slots (entails entailed-by 
                       not-entails not-entailed-by
                       satisfiable tautological inconsistent) description
    (setf satisfiable :not-tested
          tautological :not-tested
          inconsistent :not-tested
          entails nil
          entailed-by nil
          not-entails nil
          not-entailed-by nil)
    description))

;;;
;;;
;;;

(defmethod consistent-p :around ((description description) &rest args)
  (declare (ignorable args))
  (with-slots (satisfiable inconsistent tautological) description
    (when (eq satisfiable :not-tested)
      (setf satisfiable (call-next-method))
      (cond ((eq satisfiable t) ;;; auch :dont-know kann zurueckkommen!!! richtig so!
             (setf inconsistent nil))
            ((not satisfiable)
             (setf inconsistent t)
             (setf tautological nil))))
    satisfiable))

(defmethod inconsistent-p :around ((description description) &rest args)
  (declare (ignorable args))
  (with-slots (satisfiable inconsistent tautological) description
    (when (eq inconsistent :not-tested)
      (setf inconsistent (call-next-method))
      (cond ((eq inconsistent t)
             (setf satisfiable nil)
             (setf tautological nil))
            ((not inconsistent)
             (setf satisfiable t))))
    inconsistent))

(defmethod tautological-p :around ((description description) &rest args)
  (declare (ignorable args))
  (with-slots (satisfiable inconsistent tautological) description
    (when (eq tautological :not-tested)
      (setf tautological (call-next-method))
      (cond ((eq tautological t)
             (setf satisfiable t)
             (setf inconsistent nil))))
    tautological))

;;;
;;; Reasoning; consistent-p sollte NIL, T, :UNKNOWN liefern
;;;

(defmethod consistent-p ((description description) &rest args)
  (subclass-responsibility 'consistent-p))

(defmethod inconsistent-p ((description description) &rest args)
  ;;; Achtung! kann ja auch :dont-know zurueckkommen! 
  (eq (apply #'consistent-p description args) nil))

(defmethod tautological-p ((description description) &rest args)
  (subclass-responsibility 'tautological-p))

;;;
;;;
;;;
;;;

(defmethod implies-p :around ((descr1 description) (descr2 description) &rest args)
  (declare (ignorable args))
  (call-next-method))

#|
  ;;; funktioniert nicht! 
  ;;; denke an die Descriptions, die ihrer Beschreibungen wechseln, 
  ;;; dabei aber Objektidentität behalten! 

  (if (member descr2 (entails descr1))
      t
    (if (member descr2 (not-entails descr1))
        nil
      (let ((res (call-next-method)))
        (when (eq res t)
          (push descr2
                (slot-value descr1 'entails))
          (push descr1
                (slot-value descr2 'entailed-by)))
        (when (eq res nil)
          (push descr2
                (slot-value descr1 'not-entails))
          (push descr1
                (slot-value descr2 'not-entailed-by)))
        res))))
|#

;;;
;;;
;;;

(defmethod implies-p ((descr1 description) (descr2 description) &rest args)
  (subclass-responsibility 'implies-p))

;;;
;;;
;;;

(defmethod equivalent-p ((descr1 description) (descr2 description) &rest args)
  (and (apply #'implies-p descr1 descr2 args)
       (apply #'implies-p descr2 descr1 args)))

;;;
;;;
;;;

(defun is-atom-p (expr)
  (or (symbolp expr)
      (and (consp expr)
           (eq (first expr) :not)
           (symbolp (second expr))
           (not (cddr expr)))))

(defun negated-atoms-p (x y)
  (and (is-atom-p x)
       (is-atom-p y)
       (or (and (symbolp x)
                (consp y)
                (symbolp (second y))
                (eq (first y) :not)
                (eq x (cadr y)))
           (and (symbolp y)
                (consp x)
                (symbolp (second x))  
                (eq (first x) :not)
                (eq y (cadr x))))))

(defun get-negated-atom (x) 
  (if (eq x :bottom)
      :top
    (if (eq x :top)
        :bottom
      (if (and (consp x)
               (eq (first x) :not)
               (symbolp (second x)))
          (second x)
        (if (symbolp x)
            `(:not ,x)
          (error "Bad atom: ~A!" x))))))

;;;
;;;
;;;

(defmethod get-negated-description ((description simple-disjunctive-description)) 
  (if (consp (textual-description description))
      (make-description 'simple-conjunctive-description
                        (mapcar #'get-negated-atom (textual-description description)))
    (make-description 'simple-conjunctive-description
                      (get-negated-atom (textual-description description)))))

(defmethod get-negated-description ((description simple-conjunctive-description)) 
  (if (consp (textual-description description))
      (make-description 'simple-disjunctive-description
                        (mapcar #'get-negated-atom (textual-description description)))
    (make-description 'simple-disjunctive-description
                      (get-negated-atom (textual-description description)))))
    

;;;
;;;
;;;

(defmethod inconsistent-p ((description simple-disjunctive-description) &rest args)
  ;;; Verfeinerungsbeduerftig
  (every #'(lambda (x) (eq x :bottom))
         (ensure-list (textual-description description))))

(defmethod consistent-p ((description simple-disjunctive-description) &rest args)
  ;;; Entscheidbar -> nil, T, kein :UNKNOWN mehr
  (not (apply #'inconsistent-p description args)))

(defmethod tautological-p ((description simple-disjunctive-description) &rest args)
  (apply #'inconsistent-p (get-negated-description description) args))

;;;
;;;
;;;

(defmethod implies-p ((descr1 simple-disjunctive-description) (descr2 simple-disjunctive-description) &rest args)
  (every #'(lambda (x) 
             (apply #'inconsistent-p x args))
         (let ((descr2 (get-negated-description descr2)))
           (mapcar #'(lambda (disjunct)
                       (make-description 'simple-conjunctive-description
                                         (cons disjunct (textual-description descr2))))
                   (ensure-list (textual-description descr1))))))

(defmethod implies-p ((descr1 simple-disjunctive-description) (descr2 simple-conjunctive-description) &rest args)
  (every #'(lambda (x) 
             (apply #'inconsistent-p x args))
         (mapcar #'(lambda (prod)
                     (make-description 'simple-conjunctive-description
                                       prod))
                 (prod (ensure-list (textual-description descr2))
                       (mapcar #'get-negated-atom 
                               (ensure-list (textual-description descr1)))))))

;;;
;;;
;;;
      
(defmethod inconsistent-p ((description simple-conjunctive-description) &rest args)
  ;;; Verfeinerungsbeduerftig
  (or (some #'(lambda (x) (eq x :bottom)) (ensure-list (textual-description description)))
      (loop as l on (ensure-list (textual-description description))
            as x = (first l)
            thereis (some #'(lambda (y) 
                              (negated-atoms-p x y))
                          (rest l)))))
                          

(defmethod consistent-p ((description simple-conjunctive-description) &rest args)
  (not (apply #'inconsistent-p description args)))

(defmethod tautological-p ((description simple-conjunctive-description) &rest args)
  (apply #'inconsistent-p (get-negated-description description) args))

;;;
;;;
;;;

(defmethod implies-p ((descr1 simple-conjunctive-description) (descr2 simple-conjunctive-description) &rest args)
  (every #'(lambda (x) 
             (apply #'inconsistent-p x args))
         (mapcar #'(lambda (disjunct)
                     (make-description 'simple-conjunctive-description
                                       (cons (get-negated-atom disjunct)
                                             (ensure-list (textual-description descr1)))))
                 (ensure-list (textual-description descr2)))))


(defmethod implies-p ((descr1 simple-conjunctive-description) (descr2 simple-disjunctive-description) &rest args)
  (apply #'inconsistent-p 
         (make-description 'simple-conjunctive-description
                           (append (ensure-list (textual-description descr1))
                                   (mapcar #'get-negated-atom (ensure-list (textual-description descr2)))))
         args))


;;; 
;;; Simple Beschreibungen
;;; Nodes: KONJUNKTIV
;;; Edges: DISJUNKTIV!!!
;;; 

(defpersistentclass simple-node-description (node-description simple-conjunctive-description))

(defpersistentclass simple-edge-description (edge-description simple-disjunctive-description))

;;;
;;; Racer Descriptions: RACER-Beschreibungen inkl. TBox 
;;; RACER ABox-"Reimplementation"
;;;

(defpersistentclass racer-description (description)
  ((racer-package :reader racer-package :initarg :racer-package :initform nil)))

(defpersistentclass racer-node-description (node-description racer-description racer-concept-mixin))   

(defpersistentclass racer-edge-description (edge-description racer-description racer-role-mixin))

;;;
;;; Konstrukturen: Knoten
;;;

(defmethod make-node-description ((class symbol) descr &rest args)
  (apply #'make-description class descr args))

(defmacro ncon (descr &rest args)  
  `(make-standard-node-description ',descr ,@args))

(defun make-standard-node-description (descr &rest args)
  (apply #'make-node-description *node-description-class* descr args))


;;;
;;; Konstrukturen: Kanten 
;;;

(defmethod make-edge-description ((class symbol) descr &rest args)
  (apply #'make-description class descr args))

(defmacro econ (descr &rest args)
  `(make-standard-edge-description ',descr ,@args))

(defun make-standard-edge-description (descr &rest args)
  (apply #'make-edge-description *edge-description-class* descr args))


;;;
;;; AND/OR-Konstruktoren für Descriptions
;;;

(defmethod make-and-description :before ((descr description) &rest args &key descriptions &allow-other-keys)
  (unless (every #'(lambda (x) 
                     (eq (constructor-sym descr) (constructor-sym x)))
                 descriptions)
    (error "Descriptions not all of same type!")))

(defmethod make-or-description :before ((descr description) &rest args &key descriptions &allow-other-keys)
  (unless (every #'(lambda (x) 
                     (eq (constructor-sym descr) (constructor-sym x)))
                 descriptions)
    (error "Descriptions not all of same type!")))

;;;
;;; nur intern! 
;;;

(defmethod make-union-description ((descr simple-description) &rest args &key descriptions &allow-other-keys)
  (if descriptions
      (apply #'make-description 
             (constructor-sym descr)
             (reduce #'union 
                     (mapcar #'(lambda (x) (ensure-list (textual-description x)))
                             (cons descr descriptions)))
             args)
    descr))
                        
(defmethod make-intersection-description ((descr simple-description) &rest args &key descriptions &allow-other-keys)
  (if descriptions
      (apply #'make-description
             (constructor-sym descr)
             (reduce #'intersection 
                     (mapcar #'(lambda (x) (ensure-list (textual-description x)))
                             (cons descr descriptions)))
             args)
    descr))

;;;
;;; Logisch, extern: 
;;;

(defmethod make-and-description ((descr simple-conjunctive-description) &rest args)
  (apply #'make-union-description descr args))

#|

;;; falsch! -> man braucht echte BOOLEAN DESCRIPTIONS HIER! DNF bestimmen etc. 

(defmethod make-or-description ((descr simple-conjunctive-description) &rest descriptions)
  (make-union-description descr descriptions))
|#

(defmethod make-and-description ((descr simple-disjunctive-description) &rest args)  
  (apply #'make-intersection-description descr args))

(defmethod make-or-description ((descr simple-disjunctive-description) &rest args)
  (apply #'make-union-description descr args))

;;;
;;; Rasoning: Racer descriptions
;;;

(defmethod consistent-p ((description racer-node-description) &rest args)
  (concept-is-consistent-p description))

(defmethod consistent-p ((description racer-edge-description) &rest args)
  (role-is-consistent-p description))

(defmethod tautological-p ((description racer-node-description) &rest args)
  (concept-is-tautological-p description))

(defmethod tautological-p ((description racer-edge-description) &rest args)
  (role-is-tautological-p description))

;;;
;;;
;;;

(defmethod get-negated-description ((description racer-node-description))
  (get-negated-concept description))

;;;
;;;
;;;

(defmethod implies-p ((descr1 racer-node-description) (descr2 racer-node-description) &rest args)
  (apply #'concept-implies-p descr1 descr2 args))

(defmethod implies-p ((descr1 racer-edge-description) (descr2 racer-edge-description) &rest args)
  (apply #'role-implies-p descr1 descr2 args))

;;;
;;;
;;;

(defmethod make-and-description ((descr racer-node-description) &rest args &key descriptions &allow-other-keys)  
  (apply #'make-description 
         (type-of descr)
         `(and ,@(mapcar #'racer-concept (cons descr descriptions)))
         args))

(defmethod make-or-description ((descr racer-node-description) &rest args &key descriptions &allow-other-keys)  
  (apply #'make-description 
         (type-of descr)
         `(or ,@(mapcar #'racer-concept (cons descr descriptions)))
         args))

;;;
;;; Added in 2021:
;;;

#|
(defmethod implies-p ((descr1 #<MIDELORA-MAP-NODE-DESCRIPTION: description) (descr2 description) &rest args)
  (subclass-responsibility 'implies-p))
|#

