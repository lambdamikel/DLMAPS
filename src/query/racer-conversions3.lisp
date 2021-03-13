;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

#+(and :midelora (not :dlmaps))
(defun nrql-error (&rest args)
  (apply #'error args))

(defconstant +racer-cd-predicates+ 
  '(:min :max :divisible :not-divisible :string= :string<>
    :> :>= :< :<= :<> := :equal :unequal
    :boolean= :boolean<>))

;;;
;;; Diese Funktionen bestimmen, was letztlich zum RACER-Server gesendet wird!!!
;;; 

(defun convert-to-racer-individual-name (expr &optional package)
  (normalize-name
   (if package
       (change-package-of-description expr 
                                      #-:dlmaps package
                                      #+:dlmaps (or package :cl-user))
     expr)))

(defun convert-to-racer-tbox-name (expr &optional package)
  (normalize-name
   (if package
       (change-package-of-description expr 
                                      #-:dlmaps package
                                      #+:dlmaps (or package :cl-user))
     expr)))

(defun convert-to-racer-abox-name (expr &optional package)
  (normalize-name
   (if package
       (change-package-of-description expr 
                                      #-:dlmaps package
                                      #+:dlmaps (or package :cl-user))
     expr)))

(defun convert-to-racer-role-expression (expr &optional package)
  (normalize-racer-role
   (if package
       (change-package-of-description expr package)
     expr)))

(defun convert-to-racer-attribute-expression (expr &optional package)
  (if package
      (change-package-of-description expr package)
    expr))

(defun convert-to-racer-concept-expression (expr &optional package)
  (normalize-racer-concept
   (if package
       (change-package-of-description expr package)
     expr)))

(defun convert-to-racer-constraint-expression (expr &optional package)
  (if package
      (change-package-of-description expr package)
    expr))

;;;
;;;
;;;

(defun normalize-racer-concept (concept)
  (labels ((do-it (concept)
             (if (consp concept)
                 (let ((op (intern (format nil "~A" (first concept)) :keyword))
                       (rel-op (first concept)))
                   (case op 
                     ((:not)
                      `(,rel-op ,(do-it (second concept))))
                     ((:or :and)
                      `(,rel-op ,@(mapcar #'do-it (rest concept))))
                     ((:some :all)
                      `(,rel-op ,(normalize-racer-role (second concept))
                                ,(do-it (third concept))))
                     ((:at-least :at-most :exactly)
                      `(,rel-op ,(second concept)
                                ,(normalize-racer-role (third concept))
                                ,@(when (fourth concept) 
                                    (list (do-it (fourth concept))))))
                     
                     ((:a :an :no)
                      `(,rel-op ,(second concept)))

                     ((:min :max :divisible :not-divisible
                       :string= :string<> 
                       :boolean= :boolean<>
                       :> :>= :< :<= :<> := :equal :unequal)
                      concept)

                     (otherwise 
                      #-:dlmaps (nrql-error "Parser error: what is ~A?" concept)
                      #+:dlmaps (error "Parser error: what is ~A?" concept))))
               concept)))

    (do-it concept)))

(defun is-valid-racer-concept-expression-p (concept &key (tbox nil tbox-supplied-p))
  (labels ((do-it (concept)
             (or (and concept
                      (symbolp concept))
                 (and (consp concept)
                      (let ((op (intern (format nil "~A" (first concept)) :keyword)))
                        (case op 
                          ((:or :and :not)
                           (do-it (second concept)))
                          ((:all :some)
                           (and (if tbox-supplied-p 
                                    (is-valid-racer-role-expression-p (second concept)
                                                                      :tbox tbox)
                                  (is-valid-racer-role-expression-p (second concept)))
                                (do-it (third concept))))
                          ((:at-least :at-most :exactly)
                           (and (if tbox-supplied-p 
                                    (is-valid-racer-role-expression-p (third concept)
                                                                      :tbox tbox)
                                  (is-valid-racer-role-expression-p (third concept)))
                                (=> (fourth concept) 
                                    (do-it (fourth concept)))))
                          ((:a :an :no)
                           (if tbox-supplied-p 
                               (is-valid-racer-attribute-expression-p (second concept) :tbox tbox)
                             (is-valid-racer-attribute-expression-p (second concept))))
                          ((:min :max :divisible :not-divisible)
                           (and (if tbox-supplied-p 
                                    (is-valid-racer-attribute-expression-p (second concept) :tbox tbox)
                                  (is-valid-racer-attribute-expression-p (second concept)))
                                (integerp (third concept))))
                          ((:string= :string<>)
                           (and (if tbox-supplied-p 
                                    (is-valid-racer-attribute-expression-p (second concept) :tbox tbox)
                                  (is-valid-racer-attribute-expression-p (second concept)))
                                (or (if tbox-supplied-p 
                                        (is-valid-racer-attribute-expression-p (third concept) :tbox tbox)
                                      (is-valid-racer-attribute-expression-p (third concept)))
                                    (stringp (third concept)))))
                          ((:boolean= :boolean<>)
                           (and (if tbox-supplied-p 
                                    (is-valid-racer-attribute-expression-p (second concept) :tbox tbox)
                                  (is-valid-racer-attribute-expression-p (second concept)))
                                (or (if tbox-supplied-p 
                                        (is-valid-racer-attribute-expression-p (third concept) :tbox tbox)
                                      (is-valid-racer-attribute-expression-p (third concept)))
                                    (member (third concept) '(#T #F)))))
                          ((:> :>= :< :<= :<> := :equal :unequal)
                           (and (is-valid-racer-aexpr-p (second concept))
                                (is-valid-racer-aexpr-p (third concept))))
                          (otherwise nil)))))))
    (do-it concept)))

(defun is-valid-racer-constraint-expression-p (expr)
  (declare (ignorable expr))
  #|
  (member (intern (format nil "~a" expr) :keyword)
          '(:> :>= :< :<= :<> := 
            :equal :unequal
            :string= :string<>
            :min :max :divisible :not-divisible))
|# t)

;;;
;;;
;;;

(defun normalize-racer-role (role)
  (labels ((do-it (role &key negated-p inverse-p)
             (if (symbolp role)      
                 (if (not negated-p)
                     (if (not inverse-p)
                         role
                       `(inv ,role))
                   (if (not inverse-p)
                       `(not ,role)
                     `(not (inv ,role))))
               (if (not (cdr role))
                   (do-it (car role) 
                          :negated-p negated-p 
                          :inverse-p inverse-p)
                 (ecase (first role)
                   (inv (do-it (second role)
                               :negated-p negated-p 
                               :inverse-p (not inverse-p)))
                   (not (do-it (second role)
                               :negated-p (not negated-p)
                               :inverse-p inverse-p)))))))

    (do-it role)))



(defun is-valid-racer-datatype-role-expression-p (role &key
                                                       (tbox nil tbox-supplied-p) 
                                                       check-p)
  
  (and (if tbox-supplied-p 
           (is-valid-racer-role-expression-p role
                                             :check-p check-p
                                             :tbox tbox)
         (is-valid-racer-role-expression-p role
                                           :check-p check-p))         
       
       (if tbox-supplied-p 
           (role-used-as-datatype-property-p role
                                             tbox)
         
         (role-used-as-datatype-property-p role (current-tbox)))))
                       
              
(defun is-valid-racer-role-expression-p (role &key 
                                              allow-negated-roles-p (tbox nil tbox-supplied-p) 
                                              check-p)
  (labels ((do-it (role) 
             (or (and role
                      (symbolp role)
                      ;; (=> check-p 
                      ;;    (role-p role))
                      (=> (and check-p 
                               (if tbox-supplied-p
                                   (role-p role tbox)
                                 (role-p role)))
                          (not 
                           (if tbox-supplied-p 
                               (cd-attribute-p role tbox)
                             (cd-attribute-p role)))))
      
                 (when (consp role)
                   (let ((first (intern (format nil "~A" (first role))
                                        :keyword)))
                     (case first
                       (:not
                        (and allow-negated-roles-p
                             (do-it (second role))))
                       (:inv 
                        (do-it (second role)))
                       (otherwise nil)))))))
    (do-it role)))

(defun is-valid-racer-attribute-expression-p (role &key check-p (tbox nil tbox-supplied-p))
  (and role (symbolp role)
       (=> check-p 
           (if tbox-supplied-p 
               (cd-attribute-p role tbox)
             (cd-attribute-p role)))))
             

(defun is-valid-racer-aexpr-p (aexpr)
  (declare (ignore aexpr))
  ; ist mir zu kompliziert das zu checken, ist eh eigentlich unnoetig... 
  t)

(defun negated-racer-role-p (role)
  ;;; Annahme: role ist valid-racer-role-expression-p 
  (and (consp role)
       (eq (intern (format nil "~A" (first role)) :keyword) :not)))

;;;
;;;
;;;
  
(defun normalize-name (name)
  (if (symbolp name) 
      name
    (if (not (cdr name))
        (car name)
      #-:dlmaps (nrql-error "Parser error: bad name ~A" name)
      #+:dlmaps (error "Parser error: bad name ~A" name))))

;;;
;;;
;;;

(defun racer-nnf (concept &optional negate-p)
  (if (consp concept)
      (let ((op (intern (format nil "~A" (first concept)) :keyword)))
        (case op 
          ((:not)
           (racer-nnf (second concept) (not negate-p)))
          (:and `(,(if negate-p 'or 'and)
                  ,@(mapcar #'(lambda (x) (racer-nnf x negate-p)) (rest concept))))
          (:or `(,(if negate-p 'and 'or) 
                 ,@(mapcar #'(lambda (x) (racer-nnf x negate-p)) (rest concept))))
          (:all 
           `(,(if negate-p 'some 'all) 
             ,(second concept) ,(racer-nnf (third concept) 
                                           negate-p)))
          (:some 
           `(,(if negate-p 'all 'some)
             ,(second concept) ,(racer-nnf (third concept) 						 
                                           negate-p)))
          (:at-least
           (let ((n (second concept)))
             (if (not negate-p)
                 (if (zerop (1- n))
                     `(some ,(third concept)
                            ,(or (racer-nnf (fourth concept) nil)
                                 'top))
                   `(at-least ,(second concept)
                              ,(third concept)
                              ,(or (racer-nnf (fourth concept) nil)
                                   'top)))
               (if (zerop (1- n))
                   `(all ,(third concept)
                         ,(or (racer-nnf (fourth concept) t) 'bottom))
                 `(at-most ,(1- n) 
                           ,(third concept)
                           ,(or (fourth concept) 'top))))))
              
          (:at-most
           (let ((n (second concept)))              
             (if (not negate-p)
                 (if (zerop n)
                     `(all ,(third concept)
                           ,(or (racer-nnf (fourth concept) t)
                                'bottom))
                   `(at-most ,(second concept)
                             ,(third concept)
                             ,(or (racer-nnf (fourth concept) nil)
                                  'top)))
               (if (zerop n)
                   `(some ,(third concept)
                          ,(or (racer-nnf (fourth concept) nil)
                               'bottom))
                 `(at-least ,(1+ n) 
                            ,(third concept)
                            ,(or (fourth concept) 'top))))))

          (:exactly 
           (racer-nnf `(and (at-least ,@(rest concept))
                            (at-most ,@(rest concept)))
                      negate-p))
          ((:a :an)
           (if negate-p
               `(no ,@(rest concept))
             concept))
          (:no
           (if negate-p
               `(a ,@(rest concept))
             concept))
          (:min
           (if negate-p
               `(max ,@(rest concept))
             concept))
          (:max 
           (if negate-p 
               `(min ,@(rest concept))
             concept))
          (:divisible 
           (if negate-p 
               `(not-divisible ,@(rest concept))
             concept))
          (:not-divisible 
           (if negate-p
               `(divisible ,@(rest concept))
             concept))              
          (:string= 
           (if negate-p
               `(string<> ,@(rest concept))
             concept))
          (:string<>
           (if negate-p
               `(string= ,@(rest concept))
             concept))
          (:boolean= 
           (if negate-p
               `(boolean<> ,@(rest concept))
             concept))
          (:boolean<>
           (if negate-p
               `(boolean= ,@(rest concept))
             concept))
          (:>
           (if negate-p
               `(<= ,@(rest concept))
             concept))
          (:>=
           (if negate-p
               `(< ,@(rest concept))
             concept))
          (:<
           (if negate-p
               `(>= ,@(rest concept))
             concept))
          (:<=
           (if negate-p
               `(> ,@(rest concept))
             concept))              
          (:equal
           (if negate-p
               `(unequal ,@(rest concept))
             concept))              
          (:unequal
           (if negate-p
               `(equal ,@(rest concept))
             concept))
          (otherwise 
           #-:dlmaps (nrql-error "Trying to compute NNF: what is ~A?" concept)
           #+:dlmaps (error "Trying to compute NNF: what is ~A?" concept))))
    (if negate-p 
        `(not ,concept)
      concept)))


