;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;; Hilfsfunktionen: Tuple-Konstruktion am Ende (evtl. mit Projektion auf
;;; Told Racer-Attribut-Werte!)
;;; 

(defun get-textual-head-entry (x) 
  (cond ((is-voi-p x)
         (textual-description x))
        ((consp x)
         (tree-map #'(lambda (x) 
                       (if (is-voi-p x)
                           (textual-description x)
                         x))
                   x))
        (t x)))

(defun get-binding-for (x)  
  (with-critical-section
    (cond ((and (consp x)
                (eq (first x) :told-value))
         
           ;;; Projektion durchfuehren!
           ;;; (told-value (has-age ?x))-Eintrag in 
           ;;; answer pattern!
         
           (let* ((property-or-attribute (caadr x))
                  (voi (cadadr x))
                  (ind (bound-to voi)))
           
             (if (is-datatype-property-p (parser *running-query*) property-or-attribute)
                 (retrieve-annotation-values *running-substrate* 
                                             ind property-or-attribute)
             
               (let ((x (dl-prover-retrieve-individual-attribute-fillers *running-substrate* ind property-or-attribute)))

                 (or
                  (mapcar #'(lambda (x) 
                              (or (dl-prover-told-value *running-substrate* x)
                                  :no-told-value))
                          x)
                  :no-known-cd-objects)))))
        
          ((consp x)
           
           (let ((x (dl-prover-retrieve-individual-attribute-fillers *running-substrate*
                                                                     (bound-to (second x))
                                                                     (first x))))
             (or x :no-known-cd-objects)))

          ((is-voi-p x)

           (bound-to x))

          (t :undefined))))

(defun get-tuple-for-pattern (pattern)
  (mapcar #'get-binding-for pattern))

(defun get-binding-list-for-pattern (pattern)
  (mapcar #'(lambda (x)
              (list (get-textual-head-entry x)
                    (get-binding-for x)))
          pattern))

;;;
;;;
;;;


(defmethod construct-result-tuple ((query query))
  (with-slots (parser substrate 
                      rule-con-pattern answer-pattern 
                      substrate) query
    (with-slots (new-inds-hash racer-package) substrate

      (let ((undefined-p nil))
      
        (labels ((process (var) 
                   (cond ((is-voi-p var)
                          (bound-to var))

                         ((and (consp var) (eq (first var) :new-ind))
                          (let* ((bindings (mapcar #'process (cddr var)))
                                 (key (cons (second var) bindings)))

                            (multiple-value-bind (entry foundp)
                                (gethash key new-inds-hash)
                              (if foundp
                                  entry
                                (setf (gethash key new-inds-hash)
                                      (intern (format nil "~A~{-~A~}"
                                                      (first key)
                                                      (rest key))
                                              (find-package racer-package)))))))
                       
                         ((or (stringp var)
                              (numberp var))
                        
                          var)
                         
                         ((and (consp var) 
                               (cddr var)
                               (not (cdddr var)))

                          ;; z.B. (+ (told-value (age ?x)) 30) 
                      
                          `(,(first var)
                            ,(process (second var))
                            ,(process (third var))))

                         (t 

                          ;; ?x, (age ?x), (told-value (age ?x)) 
                          
                          (let ((res 
                                 (first (ensure-list (get-binding-for var)))))
                              
                            (setf undefined-p
                                  (member res '(:no-known-cd-objects
                                                :no-told-value
                                                :undefined)))
                          
                            res))))

                 (process-concept (concept)
                   (cond ((symbolp concept) concept)

                         ((and (consp concept) (eq (first concept) :new-symbol))
                          (intern (format nil "~A~{-~A~}" 
                                          (second concept)
                                          (mapcar #'bound-to (cddr concept)))
                                  (find-package racer-package)))

                         ((consp concept)  
                          (cons (process-concept (car concept))
                                (process-concept (cdr concept))))
                       
                         (t concept))))

          (if (not (is-rule-p query))
              (get-binding-list-for-pattern answer-pattern)

            (mapcar #'(lambda (x)
                        (setf undefined-p nil)
                        (let* ((op (first x))
                               (res 
                                (ecase (to-keyword op)
                                  ((:instance :forget-concept-assertion)
                                   (let ((var (second x))
                                         (concept (third x)))
                                     `(,op ,(process var) 
                                           ,(process-concept concept))))
                          
                                  ((:related :constrained :forget-role-assertion)
                                   (let ((from (second x))
                                         (to (third x))
                                         (role (fourth x)))
                                     `(,op
                                       ,(process from)
                                       ,(process to)
                                       ,role)))

                                  ((:constraints :constraint)
                                   `(constraints
                                      ,@(mapcar #'(lambda (constraint) 
                                                    (list (first constraint)
                                                          (process (second constraint))
                                                          (process (third constraint))))
                                                (rest x)))))))
                                  
                          (if undefined-p 
                              :undefined
                            res)))
                             
                    rule-con-pattern)))))))
