;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;; 
;;;

(defgeneric parse-query (query parser &rest args))

(defgeneric unparse-query (query))

(defvar *var-counter* 0)

;;;
;;;
;;;

(defun parser-error (description)
  (nrql-error 
   (format nil "Parser Error: ~A" description)))

;;;
;;;
;;;

(defmethod parse-query (query (parser simple-parser) &rest args)
  (labels ((parse-it (expression &optional negated-p inverse-p)

             (let ((type (get-expression-type parser expression)))

               (format t "~A ~A~%" expression (type-of type))

               (typecase type
                 
                 (symbol 
                  
                  (case type
                 
                    (projection-operator
                     (let* ((query
                             (parse-it (third expression)
                                       nil
                                       nil)))

                       (setf (slot-value query 'projection-vois)
                             (if (not (eq (projection-vois query)
                                          :unspecified))
                                 (intersection (retrieve-vois query 
                                                              (second expression))
                                               (projection-vois query))
                               (retrieve-vois query 
                                              (second expression))))

                       query))
                    
                    (query-reference
                     (let ((referenced-query
                            (gethash (second expression)
                                     (query-hash parser)))
                                     
                           (vois 
                            (mapcar #'(lambda (voi) 
                                        (make-voi parser voi))
                                    (third expression))))

                       (unless referenced-query
                         (nrql-error
                          (format nil "Can't find anonymous subquery ~A" (third expression))))

                       (apply #'make-description 'query-reference
                              expression
                              :result-vois vois
                              :referenced-query referenced-query
                              :negated-p negated-p
                              :inverse-p inverse-p                                  
                              :parser parser
                              :allow-other-keys t                                   
                              args)))
                    
                    (not
                     (parse-it (second expression)
                               (not negated-p)
                               inverse-p))
                 
                    (inv
                     (parse-it (second expression)
                               negated-p
                               (not inverse-p)))
                    
                    (otherwise 
                     
                     (parser-error 
                      (format nil "Unrecognized expression ~A" expression)))))

                 (complex-query 
                  (let ((subexpressions 
                         (mapcar #'(lambda (x) 
                                     (parse-it x nil inverse-p))
                                 (get-subexpressions parser expression))))

                    (when (or negated-p inverse-p)
                      (nrql-error "Parser error: Query ~A is not in NNF" query))
                         
                    (apply #'make-description (type-of type)
                           expression
                           :allow-other-keys t                                   
                           :subqueries subexpressions
                           :parser parser
                           :negated-p negated-p
                           args)))
                 
                 (true-query
 
                  (apply #'make-description 
                         (type-of type)
                         nil
                         :parser parser
                         :allow-other-keys t                                   
                         args))

                 (false-query
                  
                  (apply #'make-description 
                         (type-of type)
                         nil
                         :parser parser
                         :allow-other-keys t                                   
                         args))
                 
                 (atomic-query    
               
                  (let ((vois (parse-vois parser expression)))

                    (when (and negated-p 
                               (is-same-as-query-p type))

                      #| ;;; Optimierung moeglich: Forward Checking beim Binden! 

                      (pushnew (first vois) 
                               (different-from (second vois)))
                      
                      (pushnew (second vois) 
                               (different-from (first vois)))

                      |# ) 

                    
                    (apply #'make-description (type-of type)
                           (if (cddr expression) 
                               (third expression)
                             (second expression))
                           :original-query expression
                           :allow-negated-roles-p 
                           *allow-negated-roles-p*
                           :allow-other-keys t
                           :vois vois 
                           :negated-p negated-p
                           :inverse-p inverse-p                                            
                           :parser parser
                           args)))

                 (otherwise
                  (parser-error 
                   (format nil "Unrecognized expression ~A" expression)))))))

    (parse-it query)))

;;;
;;;
;;;

(defmethod parse-vois ((parser simple-parser) expr)
  (mapcar #'(lambda (x)
              (make-voi parser x))
          (get-vois-from parser expr)))

;;;
;;;
;;;

(defmethod find-voi ((parser simple-parser) (voi-name symbol))
  (or (find voi-name
            (vois parser)
            :key #'(lambda (x) ;;; relaxed! correct? 
                     (symbol-name (textual-description x)))
            :test #'string-equal)

      #|
      
      (find voi-name
            (vois parser)
            :key #'aka-vois
            :test #'(lambda (x y) 
                      (find-if #'(lambda (y) 
                                   (string-equal (symbol-name y) x))
                               y)))
|# ))


(defmethod find-voi ((parser simple-parser) (voi voi))
  (find voi (vois parser)))

;;;
;;;
;;;

(defmethod corresponding-voi-p ((voi voi))
  (when (corresponding-voi voi)
    t))

;;;
;;; 
;;;

(defmethod retrieve-vois ((query query) vois) 
  (mapcar #'(lambda (x) (find-voi (parser query) x)) vois))

;;;
;;; 
;;;

(defmethod make-voi ((parser simple-parser) expr)
  (or (find-voi parser expr)
      (let* ((cor-voi (find-voi parser (get-corresponding-voi parser expr)))
             (voi (make-instance (cond ((abox-var-p parser expr)
                                        'abox-variable)
                                       ((abox-ind-p parser expr)
                                        'abox-individual)
                                       ((substrate-var-p parser expr)
                                        'substrate-variable)
                                       ((substrate-ind-p parser expr)
                                        'substrate-individual))
                                 :substrate (substrate parser)
                                 :una-voi-p (and (not (and (var-p parser expr)
                                                           (aux-var-p parser expr)))
                                                 ;;; neu - f. Individuen gilt die
                                                 ;;; UNA nicht mehr!
                                                 ;;; w.g. (same-as ?x betty) etc. 
                                                 (not (ind-p parser expr)))
                                 :keep-*-p (is-nrql-tbox-query-parser-p parser)
                                 :textual-description expr
                                 :corresponding-voi cor-voi)))

        (when cor-voi 
          (setf (slot-value cor-voi 'corresponding-voi) voi))

        (push voi (slot-value parser 'vois))

        voi)))



(defmethod delete-voi ((parser simple-parser) (voi voi))
  (setf (slot-value parser 'vois)
        (delete voi (slot-value parser 'vois))))
  
  
