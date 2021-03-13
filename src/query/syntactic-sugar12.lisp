 ;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

(defmethod transform-datatype-expression ((parser nrql-abox-query-parser) property expr)
  ;; (and (min 3) (max 5) (or ...))

  (let ((attribute (get-attribute-for-datatype-property parser property)))

    (labels ((get-simple-expressions (list)
               (remove-if-not #'(lambda (x)
                                  (and (consp x)
                                       (not (cddr x)) ; sonst wurde schon ersetzt!
                                       (member (to-keyword (first x))
                                               +racer-cd-predicates+)))
                              list)))
    
      (if (not (consp expr))
        
          (parser-error 
           (format nil "Unrecognized concept expression ~A" expr))
      
        (let ((op (to-keyword (first expr))))

          (if (member op +racer-cd-predicates+)

              (if (cddr expr)
                  
                  expr
                
                `(,(first expr) 
                  ,attribute
                  ,(second expr)))
            
            (case op
              ((:and :or)
               (let* ((simple-expressions
                       (get-simple-expressions (rest expr)))
                      (atomic-expressions
                       (remove-if-not #'symbolp (rest expr)))
                      (other-expressions
                       (remove-if #'(lambda (x) 
                                      (or (member x simple-expressions)
                                          (member x atomic-expressions)))
                                  (rest expr))))
                 `(,(first expr)
                   ,@(when atomic-expressions
                       (list atomic-expressions))
                   ,@(mapcar #'(lambda (simple-expr)
                                 `(,(first simple-expr) 
                                   ,attribute
                                   ,(second simple-expr)))
                             simple-expressions)
                   ,@(mapcar #'(lambda (x) 
                                 (transform-datatype-expression parser property x))
                             other-expressions))))

              ((:not)
               `(not ,(transform-datatype-expression parser property (second expr))))

              ((:an :a) 
               (if (second expr)
                   ;;; hier kann nur sowas wie INTEGER, STRING, REAL, CARDINAL STEHEN!
                   ;; TYPE CHECK:         
                   (if (eq (second expr) attribute)
                       `(racer:an ,attribute)
                     (if (member (to-keyword (second expr))
                                 '(:integer :string :real :cardinal :boolean))
                         (if (eq (second expr) (get-datatype-range parser property))
                             `(racer::an ,attribute)
                           `(racer:no ,attribute))
                       ;;(parser-error expr)
                       `(racer:no ,attribute)
                       ))
                 `(racer:an ,attribute)))
              
              ((:no)
               `(racer:no ,attribute))

              (otherwise 
               (parser-error 
                (format nil "Unrecognized concept expression ~A" expr))))))))))

#-:owl-datatype-support    
(defmethod replace-syntactic-concept-expression-sugar ((parser nrql-abox-query-parser) concept)  
  concept)

#+(and :owl-datatype-support :midelora)
(defmethod replace-syntactic-concept-expression-sugar ((parser midelora-abox-query-parser) concept)
  concept)

#+:owl-datatype-support    
(defmethod replace-syntactic-concept-expression-sugar ((parser nrql-abox-query-parser) concept)
  (when concept
    (cond ((symbolp concept) concept)
          
          ((consp concept)
           (let ((op (to-keyword (first concept))))

             (if (member op +racer-cd-predicates+)
                 
                 ;;; (> DTP 30) -> (some DTP-ROLE (> DTP-ATTRIBUTE 30))
                 
                 (let ((role (second concept)))
                  
                   (cond ((and (role-p role (tbox (substrate parser)))
                               (cd-attribute-p role (tbox (substrate parser))))
                         
                          concept)
                         
                         ((is-datatype-property-p parser role)
                          
                          `(some ,role 
                                 ,(transform-datatype-expression parser role
                                                                 `(,(first concept)
                                                                   ,(third concept)))))
                         
                         (t 
                          (parser-error
                           (format nil "Unrecognized concept expression ~A" concept)))))
               
               (case op 
               
                 ((:not)
                  `(not ,(replace-syntactic-concept-expression-sugar parser (second concept))))
               
                 ((:and :or)
                  `(,(first concept)
                    ,@(mapcar #'(lambda (x) 
                                  (replace-syntactic-concept-expression-sugar parser x))
                              (rest concept))))
               
                 ((:all :some)
                  (let ((role (second concept))
                        (qual (third concept)))
                  
                    (if (is-datatype-property-p parser role) 

                        `(,(first concept)
                          ,role
                          ,(transform-datatype-expression parser 
                                                          role 
                                                          (if (symbolp qual)
                                                              `(racer:an ,qual)
                                                            qual)))
                    
                      `(,(first concept)
                        ,role
                        ,(replace-syntactic-concept-expression-sugar parser qual)))))
               
                 ((:at-least :at-most :exactly)
                  (let* ((num (second concept))
                         (role (third concept))
                         (qual (fourth concept)))
                  
                    (if (is-datatype-property-p parser role) 
                      
                        `(,(first concept)
                          ,num
                          ,role                              
                          ,(transform-datatype-expression parser
                                                          role 
                                                          (if (symbolp qual)
                                                              `(racer:an ,qual)
                                                            qual)))
                    
                      `(,(first concept)
                        ,num ,role ,(replace-syntactic-concept-expression-sugar parser qual)))))
               
                 ;;; diese, eigentlich nue fuer Attribute gueltige Syntax wird 
                 ;;; nun einfach auf Datatype Properties ausgedehnt!
               
                 ((:a :an)
                
                  (let ((role (second concept)))
                  
                    (cond ((and (role-p role (tbox (substrate parser)))
                                (cd-attribute-p role (tbox (substrate parser))))
                         
                           concept)
                        
                          ((is-datatype-property-p parser role)

                           `(some ,role ,(transform-datatype-expression parser role `(an ,(third concept)))))
                         
                          (t 
                           (parser-error
                            (format nil "Unrecognized concept expression ~A" concept))))))

                 ((:no)
                
                  (let ((role (second concept)))
                    (cond ((and (role-p role (tbox (substrate parser)))
                                (cd-attribute-p role (tbox (substrate parser))))
                         
                           concept)
                        
                          ((is-datatype-property-p parser role)
                           `(all ,role ,(transform-datatype-expression parser role '(no))))

                          (t (parser-error
                              (format nil "Unrecognized concept expression ~A" concept))))))

                 (otherwise 

                  (if (member op +racer-cd-predicates+)
                    
                      (let ((role (second concept)))
                      
                        (cond ((and (role-p role (tbox (substrate parser)))
                                    (cd-attribute-p role (tbox (substrate parser))))
                             
                               concept)
                        
                              ((is-datatype-property-p parser role)
                             
                               `(some ,role ,(transform-datatype-expression parser 
                                                                            role (third concept))))
                              (t (parser-error 
                                  (format nil "Unrecognized concept expression ~A" concept)))))
               
                    (parser-error 
                     (format nil "Unrecognized concept expression ~A" concept))))))))
          
          (t (parser-error 
              (format nil "Unrecognized concept expression ~A" concept))))))

;;;
;;;
;;;

(defmethod is-datatype-property-p ((parser nrql-abox-query-parser) property)
  (and (symbolp property)
       (role-p property (tbox (substrate parser)))
       (role-used-as-datatype-property-p property (tbox (substrate parser)))))


(defmethod get-attribute-for-datatype-property ((parser nrql-abox-query-parser) property)
  (intern (format nil "RACER-INTERNAL%HAS-~A-VALUE"
                  (let ((range (datatype-role-range property (tbox (substrate parser)))))
                    ;;; HACK!
                    (if (eq range 'cardinal)
                        'integer
                      range)))
          (find-package :racer)))

(defmethod get-datatype-range ((parser nrql-abox-query-parser) property)
  (when (is-datatype-property-p parser property)
    (datatype-role-range property (tbox (substrate parser)))))

