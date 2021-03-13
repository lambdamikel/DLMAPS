;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SPATIAL-SUBSTRATE; Base: 10 -*-

(in-package spatial-substrate)

;;;
;;;
;;;

(defpersistentclass rcc-query-mixin ()
  ((rcc-type :reader rcc-type :initarg :rcc-type)))

;;; evtl. weiter Typen von Map Queries; vorerst nur RCC! 

;;;
;;; 
;;;

(defpersistentclass map-query (query))

(defpersistentclass map-node-query (map-query unary-query))

(defpersistentclass map-edge-query (map-query binary-query))

;;;
;;;
;;;

(defpersistentclass map-top-query (map-node-query top-query))

(defpersistentclass map-bottom-query (map-node-query bottom-query))

;;;
;;;
;;; 

(defpersistentclass map-explicit-edge-query (map-edge-query))

(defpersistentclass map-implicit-edge-query (map-edge-query virtual-edge-query))

(defpersistentclass map-rcc-edge-query (map-edge-query rcc-query-mixin)
  ((inverse-query :reader inverse-query :initform nil :initarg :inverse-query)))

;;;
;;;
;;;

(defpersistentclass map-hybrid-query (map-query hybrid-query rcc-query-mixin))

(defpersistentclass map-hybrid-and-query (map-hybrid-query hybrid-and-query))

(defpersistentclass map-hybrid-or-query (map-hybrid-query hybrid-or-query))

;;;
;;; Knoten-Queries
;;;

(defpersistentclass map-simple-node-query (map-node-query substrate-simple-node-query))

(defpersistentclass map-simple-and-node-query (map-simple-node-query substrate-simple-and-node-query))

(defpersistentclass map-simple-or-node-query (map-simple-node-query substrate-simple-or-node-query))

(defpersistentclass map-predicate-node-query (map-node-query substrate-predicate-node-query))

(defpersistentclass map-racer-node-query (map-node-query substrate-racer-node-query))

;;;
;;;
;;;

(defpersistentclass map-instance-retrieval-query (map-node-query))

(defpersistentclass racer-map-instance-retrieval-query (map-instance-retrieval-query ts::racer-instance-retrieval-query))

#+:midelora
(defpersistentclass midelora-map-instance-retrieval-query (map-instance-retrieval-query ts::midelora-instance-retrieval-query))


;;;
;;; Kanten-Queries
;;;

(defpersistentclass map-simple-rcc-edge-query (map-explicit-edge-query map-rcc-edge-query substrate-simple-edge-query))

(defpersistentclass map-simple-rcc-or-edge-query (map-simple-rcc-edge-query substrate-simple-or-edge-query))

#|
(defpersistentclass map-simple-rcc-and-edge-query (map-simple-rcc-edge-query substrate-simple-and-edge-query))

|#

(defpersistentclass map-racer-rcc-edge-query (map-explicit-edge-query map-rcc-edge-query substrate-racer-edge-query))

(defpersistentclass map-predicate-edge-query (map-explicit-edge-query substrate-predicate-edge-query)) 

;;;
;;;
;;;

(defpersistentclass map-rcc-edge-retrieval-query (map-explicit-edge-query map-rcc-edge-query))

(defpersistentclass racer-map-rcc-edge-retrieval-query (map-rcc-edge-retrieval-query ts::racer-edge-retrieval-query))

#+:midelora
(defpersistentclass midelora-map-rcc-edge-retrieval-query (map-rcc-edge-retrieval-query ts::midelora-edge-retrieval-query))

;;;
;;;
;;;

(defpersistentclass map-virtual-simple-rcc-edge-query (map-implicit-edge-query map-rcc-edge-query virtual-simple-edge-query))

#|

(defpersistentclass map-virtual-simple-rcc-and-edge-query (map-virtual-simple-rcc-edge-query virtual-simple-and-edge-query))

|#


(defpersistentclass map-virtual-simple-rcc-or-edge-query (map-virtual-simple-rcc-edge-query virtual-simple-or-edge-query))

;;;
;;;
;;;

(defpersistentclass map-epsilon-query (map-implicit-edge-query)
  ((epsilon :reader epsilon :initarg :epsilon)))

(defpersistentclass map-inside-epsilon-query (map-epsilon-query))

(defpersistentclass map-outside-epsilon-query (map-epsilon-query))

(defpersistentclass map-ring-epsilon-query (map-epsilon-query)
  ((epsilon-max :reader epsilon-max :initarg :epsilon-max)))

;;;
;;;
;;;

(defpersistentclass map-distance-query (map-implicit-edge-query)
  ((epsilon :reader epsilon :initarg :epsilon)
   (inverse-query :reader inverse-query :initform nil :initarg :inverse-query)))

(defpersistentclass map-inside-distance-query (map-distance-query))

(defpersistentclass map-outside-distance-query (map-distance-query))

(defpersistentclass map-ring-distance-query (map-epsilon-query)
  ((epsilon-max :reader epsilon-max :initarg :epsilon-max)))

;;;
;;;
;;;

(defpersistentclass map-tuple-satisfies-query (map-implicit-edge-query virtual-predicate-edge-query))

(defpersistentclass map-part-of-query (map-implicit-edge-query))

;;;
;;;
;;;

(defun initialize-racer-atomic-rcc-query (query)
  (with-slots (inverse-p negated-p rcc-type                          
                         query-satisfiable query-tautological
                         textual-description) query

    (when negated-p
      (error "Negated query of type ~A found: ~A!" (type-of query) query))
    (when inverse-p
      (error "Inverse query of type ~A found: ~A!" (type-of query) query))

    (setf textual-description 
          (if (consp textual-description)
              (if (cdr textual-description)
                  (error "Bad RACER RCC role given: ~A!" textual-description)
                (first textual-description))
            textual-description))

    (let* ((rel textual-description)
           (type (cond ((member rel +rcc8-relationships+) :rcc8)
                       ((member rel +rcc5-relationships+) :rcc5)
                       ((member rel +rcc3-relationships+) :rcc3)
                       ((member rel +rcc2-relationships+) :rcc2)
                       ((member rel +rcc1-relationships+) :rcc1)
                       ((eq rel :top) :top)
                       (t :bottom))))
      
      (when (eq type :bottom)
        (setf query-satisfiable nil
              query-tautological nil))
      
      (when (eq type :top)
        (setf query-satisfiable t
              query-tautological t))
      
      (setf rcc-type 
            type
            
            textual-description
            (change-package-of-description rel (racer-package *cur-map*) t)))))


;;;
;;;
;;;

(defmethod initialize-description :before ((query map-simple-node-query))
  (setf (slot-value query 'textual-description)
        (ensure-list (slot-value query 'textual-description))))

(defmethod initialize-description :before ((query map-simple-rcc-edge-query))
  (setf (slot-value query 'textual-description)
        (ensure-list (slot-value query 'textual-description))))


;;;
;;;
;;;

(defmethod initialize-description :before ((query map-racer-node-query))
  (with-slots (textual-description racer-package) query
    ;;; '(:racer ...)
    (setf textual-description
          `(:racer
            ,@(change-package-of-description (rest textual-description) racer-package t)))))

;;;
;;;
;;;
            
(defmethod initialize-description ((query map-racer-rcc-edge-query))
  (initialize-racer-atomic-rcc-query query)
  (setf (slot-value query 'original-description)
        `(:racer ,(textual-description query))))

(defmethod initialize-description ((query racer-map-rcc-edge-retrieval-query))
  (initialize-racer-atomic-rcc-query query)
  (setf (slot-value query 'original-description)
        (textual-description query)))

#+:midelora
(defmethod initialize-description ((query midelora-map-rcc-edge-retrieval-query))
  (initialize-racer-atomic-rcc-query query)
  (setf (slot-value query 'original-description)
        (textual-description query)))

;;;
;;;
;;;

(defun initialize-simple-rcc-or-query (query) 
  (with-slots (inverse-p negated-p rcc-type 
                         original-description textual-description) query

    (setf textual-description 
          (if (consp textual-description)
              (case (first textual-description)
                (:not
                 (error "NOT?!"))
                (:or (rest textual-description))
                (otherwise textual-description))
            (ensure-list textual-description)))
    
    (setf textual-description 
          (if inverse-p
              (inverse-rcc-relation textual-description)
            textual-description))
                
    (setf inverse-p nil)

    (let* ((rel (ensure-list (textual-description query)))
           (type (cond ((null rel) :bottom)
                       ((subsetp rel +rcc8-relationships+) :rcc8)
                       ((subsetp rel +rcc5-relationships+) :rcc5)
                       ((subsetp rel +rcc3-relationships+) :rcc3)
                       ((subsetp rel +rcc2-relationships+) :rcc2)
                       ((subsetp rel +rcc1-relationships+) :rcc1)
                       (t :bottom))))
      
      (setf rcc-type 
            type
            
            textual-description
            (cond ((eq (first rel) :top) 
                   (if negated-p 
                       (list :bottom)
                     (list :top)))                  
                  ((or (eq (first rel) :bottom) 
                       (null rel))
                   (if negated-p 
                       (list :top)
                     (list :bottom)))
                  (t (if negated-p 
                         (negated-rcc-relation rel type)
                       rel)))
            
            original-description 
            `(:or ,@textual-description))

      (setf negated-p nil))))

;;;
;;;
;;;


(defmethod initialize-description ((query map-simple-rcc-or-edge-query))
  (initialize-simple-rcc-or-query query))

(defmethod initialize-description ((query map-virtual-simple-rcc-or-edge-query))
  (initialize-simple-rcc-or-query query))

;;;
;;;
;;;

(defmethod initialize-description ((query map-epsilon-query))
  (with-slots (epsilon textual-description) query    
    (setf epsilon
          (second textual-description))))

(defmethod initialize-description :after ((query map-ring-epsilon-query))
  (with-slots (epsilon epsilon-max textual-description) query    
    (setf epsilon-max
          (third textual-description))))

;;;
;;;
;;;

(defmethod initialize-description ((query map-distance-query))
  (with-slots (epsilon textual-description) query    
    (setf epsilon
          (second textual-description))))

(defmethod initialize-description :after ((query map-ring-distance-query))
  (with-slots (epsilon epsilon-max textual-description) query    
    (setf epsilon-max
          (third textual-description))))

;;;
;;;
;;;

(defmethod initialize-description :after ((query map-hybrid-query))
  (declare (ignore rem-conjuncts))
  (let ((rcc-edge-query-conjuncts
         (remove-if-not #'(lambda (x) (typep x 'map-rcc-edge-query))
                        (all-subqueries query))))
    
    (setf (slot-value query 'rcc-type)
          (loop as type in '(:rcc8 :rcc5 :rcc3 :rcc2 :rcc1) 
                when 
                (find type rcc-edge-query-conjuncts :key #'rcc-type)
                return type))))


