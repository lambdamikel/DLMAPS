;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SPATIAL-SUBSTRATE; Base: 10 -*-

(in-package spatial-substrate)

;;;
;;;
;;;

(defconstant +rcc-relationships+
  (remove-duplicates 
   (append '(:borders :bordered-by)
             +rcc8-relationships+
             +rcc5-relationships+
             +rcc3-relationships+
             +rcc2-relationships+
             +rcc1-relationships+)))

(defconstant +syntactic-sugar-tokens+
  '(:line line
    :chain chain
    :polygon polygon
    :line-or-chain line-or-chain
    :symbol symbol
    :area area))                              
                              
(defpersistentclass map-parser (simple-parser)
  ((reserved-tokens :initform 
                    (append '(:inside-epsilon inside-epsilon 
                              :outside-epsilon outside-epsilon 
                              :ring-epsilon ring-epsilon  
                              :inside-distance inside-distance 
                              :outside-distance outside-distance 
                              :ring-distance ring-distance  
                              :tuple-satisfies tuple-satisfies 
                              :part-of part-of
                              :has-part has-part)
                            +syntactic-sugar-tokens+
                            +reserved-tokens+))))

(defpersistentclass generic-map-parser (map-parser))

;;;
;;;
;;;

(defmethod get-parser-class-for-substrate ((map map*))
  ;;; gibt dann eventuell Laufzeitfehler - es werden u.U. 
  ;;; Queries erzeugt, die zur Laufzeit nicht ausgeführt werden 
  ;;; koennen, weil das falsche Substrate vorliegt; koennte man
  ;;; natuerlich schon zur Compilezeit ausmerzen, wenn der Dispatcher
  ;;; jeweils fuer verschiedene Parser-Klassen etnsp. angepasst wird
  'generic-map-parser)

;;;
;;;
;;;

(defun error-bad-query-for-substrate (sym substrate)
  (error "Bad query ~A for substrate of type ~A!" 
         sym (type-of substrate)))

;;;
;;;
;;;

(defmethod valid-role-description-p ((parser map-parser) expr)
  (or #-:midelora (call-next-method) 
      (valid-edge-or-description-p parser expr)))
 
(defmethod valid-node-and-description-p ((parser map-parser) expr)
  ;;; to be refined!
  (call-next-method))

(defmethod valid-node-or-description-p ((parser map-parser) expr)
  ;;; to be refined!
  (call-next-method))

;;;
;;;
;;;

(defmethod valid-edge-and-description-p ((parser map-parser) expr)
  ;;; nicht sinnvoll bei RCC!
  nil)

(defmethod valid-edge-or-description-p ((parser map-parser) expr)
  (let ((expr (change-package-of-description expr :keyword)))
    (or (and (symbolp expr)
             (or (member expr +rcc-relationships+)
                 (eq expr :bottom)
                 (eq expr :top)))
               
        (and (consp expr)       
             (or (eq (first expr) :or)
                 (eq (first expr) :not))
             (every #'(lambda (sym) 
                        (valid-edge-or-description-p parser sym))
                    (cdr expr))))))

;;;
;;;
;;;

(defmethod abox-thing-p ((parser map-parser) expr)
  (and (or (is-racer-map-p
            (substrate parser))
           #+:midelora 
           (is-midelora-map-p 
            (substrate parser)))
       (call-next-method)))

;;;
;;;
;;;


(defmethod substrate-thing-p ((parser map-parser) x)
  (let ((substrate (get-current-map)))
    (if (is-midelora-map-p substrate)
        (or (ts::substrate-var-p parser x)
            (ts::substrate-ind-p parser x)
            (ts::abox-var-p parser x)
            (ts::abox-ind-p parser x))
      (call-next-method))))

(defmethod abox-thing-p ((parser map-parser) x)
  (let ((substrate (get-current-map)))
    (if (is-midelora-map-p substrate)
        (or (ts::substrate-var-p parser x)
            (ts::substrate-ind-p parser x)
            (ts::abox-var-p parser x)
            (ts::abox-ind-p parser x))
      (call-next-method))))

(defmethod binary-substrate-query-p ((parser map-parser) expr) 
  (let ((substrate (get-current-map)))
    (if (is-midelora-map-p substrate)
        (and (binary-query-p parser expr)
             (and (<=> (abox-thing-p parser (first expr))
                       (abox-thing-p parser (second expr)))
                  (<=> (substrate-thing-p parser (first expr))
                       (substrate-thing-p parser (second expr)))))                  
      (call-next-method))))

(defmethod unary-substrate-query-p ((parser map-parser) expr) 
  (let ((substrate (get-current-map)))
    (if (is-midelora-map-p substrate)
        (unary-query-p parser expr)
      (call-next-method))))



(defmethod epsilon-query-p ((parser map-parser) expr)
  (and (binary-substrate-query-p parser expr)
       (consp (third expr))
       (let ((first (first (third expr)))
             (second (second (third expr))))
         (and (member first '(:inside-epsilon inside-epsilon 
                              :outside-epsilon outside-epsilon 
                              :ring-epsilon ring-epsilon))
              (or (and (numberp second)
                       (plusp second))
                  (and (consp second)
                       (every #'(lambda (x) 
                                  (and (numberp x)
                                       (plusp x)))
                              second)))))))

(defmethod inside-epsilon-query-p ((parser map-parser) expr)
  (and (epsilon-query-p parser expr)
       (member (first (third expr))
               '(:inside-epsilon inside-epsilon))))

(defmethod outside-epsilon-query-p ((parser map-parser) expr)
  (and (epsilon-query-p parser expr)
       (member (first (third expr))
               '(:outside-epsilon outside-epsilon))))


(defmethod ring-epsilon-query-p ((parser map-parser) expr)
  (and (epsilon-query-p parser expr)
       (member (first (third expr))
               '(:ring-epsilon ring-epsilon))
       (cddr (third expr))))

;;;
;;;
;;;

(defmethod distance-query-p ((parser map-parser) expr)
  (and (binary-substrate-query-p parser expr)
       (consp (third expr))
       (let ((first (first (third expr)))
             (second (second (third expr))))
         (and (member first '(:inside-distance inside-distance 
                              :outside-distance outside-distance 
                              :ring-distance ring-distance))
              (or (and (numberp second)
                       (plusp second))
                  (and (consp second)
                       (every #'(lambda (x) 
                                  (and (numberp x)
                                       (plusp x)))
                              second)))))))

(defmethod inside-distance-query-p ((parser map-parser) expr)
  (and (distance-query-p parser expr)
       (member (first (third expr))
               '(:inside-distance inside-distance))))

(defmethod outside-distance-query-p ((parser map-parser) expr)
  (and (distance-query-p parser expr)
       (member (first (third expr))
               '(:outside-distance outside-distance))))

(defmethod ring-distance-query-p ((parser map-parser) expr)
  (and (distance-query-p parser expr)
       (member (first (third expr))
               '(:ring-distance ring-distance))
       (cddr (third expr))))

;;;
;;;
;;;

(defmethod part-of-query-p ((parser map-parser) expr)
  (and (binary-substrate-query-p parser expr)
       (member (third expr)
               ;;; wird umgeschrieben! gibt nur map-part-of-query
               '(:part-of part-of :has-part has-part))))

(defmethod tuple-satisfies-query-p ((parser map-parser) expr)
  (and (or (and (binary-substrate-query-p parser expr)
                (consp (third expr))
                (member (first (third expr))
                        '(:tuple-satisfies tuple-satisfies))))))

(defmethod node-satisfies-query-p ((parser map-parser) expr)
  (and (or (and (unary-substrate-query-p parser expr)
                (consp (second expr))
                (member (first (second expr))
                        '(:satisfies satisfies))))))

;;;
;;;
;;;

(defmethod syntactic-sugar-query-p ((parser map-parser) expr)
  (and (unary-substrate-query-p parser expr)
       (member (first (ensure-list (second expr)))
               +syntactic-sugar-tokens+)))

;;;
;;;
;;;

(defmethod get-expression-type ((parser map-parser) expr)
  (when (consp expr)
    (cond ((inside-epsilon-query-p parser expr)
           'map-inside-epsilon-query)
          ((outside-epsilon-query-p parser expr)
           'map-outside-epsilon-query)
          ((ring-epsilon-query-p parser expr)
           'map-ring-epsilon-query)
          
          ((inside-distance-query-p parser expr)
           'map-inside-distance-query)
          ((outside-distance-query-p parser expr)
           'map-outside-distance-query)
          ((ring-distance-query-p parser expr)
           'map-ring-distance-query)

          ((tuple-satisfies-query-p parser expr)
           'map-tuple-satisfies-query)

          ((node-satisfies-query-p parser expr)
           'map-predicate-node-query)
                    
          ((part-of-query-p parser expr)
           'map-part-of-query)

          ((syntactic-sugar-query-p parser expr)
           'substrate-predicate-node-query)
          
          (t (call-next-method)))))

;;;
;;;
;;;

(defmethod make-dispatcher ((parser map-parser) sym)
  (make-instance 
   (let ((substrate (get-current-map)))
     (case sym

       (substrate-simple-and-node-query 

        (if (is-basic-map-p substrate)
            'map-simple-and-node-query
          
          (error-bad-query-for-substrate sym substrate)))

       
       (substrate-simple-or-node-query 
      
        (if (is-basic-map-p substrate)
            'map-simple-or-node-query
          (error-bad-query-for-substrate sym substrate)))


       
       ((substrate-simple-and-edge-query 
         substrate-simple-or-edge-query)

        (if (is-basic-explicit-relations-map-p substrate)
            'map-simple-rcc-or-edge-query
          'map-virtual-simple-rcc-or-edge-query))


       ;;;
       ;;;
       ;;;
                    
       (instance-retrieval-query
        (cond ((is-racer-map-p substrate)
               'racer-map-instance-retrieval-query)
              #+:midelora 
              ((is-midelora-map-p substrate)
               'midelora-map-instance-retrieval-query)
              (t (error-bad-query-for-substrate sym substrate))))
     
       (edge-retrieval-query 
        (cond ((is-racer-map-p substrate)
               'racer-map-rcc-edge-retrieval-query)
              #+:midelora 
              ((is-midelora-map-p substrate)
               'midelora-map-rcc-edge-retrieval-query)
              (t (error-bad-query-for-substrate sym substrate))))

       ;;;
       ;;;
       ;;;

       (substrate-racer-edge-query
        (if (and (is-racer-descriptions-map-p substrate)      
                 (is-explicit-relations-map-p substrate))
            'map-racer-rcc-edge-query
          (error-bad-query-for-substrate sym substrate)))
     
       (substrate-racer-node-query 
        (if (is-racer-descriptions-map-p substrate)
            'map-racer-node-query
          (error-bad-query-for-substrate sym substrate)))

       ;;;
       ;;;
       ;;;

       (substrate-predicate-edge-query 
        'map-predicate-edge-query)

       (substrate-predicate-node-query 
        'map-predicate-node-query)
     
       ;;;
       ;;;
       ;;;

       (and-query 'map-hybrid-and-query)
       (or-query 'map-hybrid-or-query)
       (top-query 'map-top-query)

       ;;;
       ;;;
       ;;; 

       (otherwise sym)))
   
   :dont-initialize-p t))



;;
;;
;;

#+:ignore
(defmethod valid-predicate-description-p ((parser simple-parser) expr)
  (and (consp expr)
       (member (to-keyword (first expr)) '(:satisfies))))
