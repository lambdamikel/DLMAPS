;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SPATIAL-SUBSTRATE; Base: 10 -*-

(in-package spatial-substrate)

;;;
;;;
;;;

(defmethod get-score ((query map-rcc-edge-query))  
  (let ((rel (ensure-list (textual-description query))))
    (if (thematic-substrate:bound-to (voi-from query))
        (if (thematic-substrate:bound-to (voi-to query))
            (values 98
                    :tester)
          (values
           (+ 80 
              (cond ((member :tppi rel) 10)
                    ((member :ntppi rel) 8)
                    ((member :po rel) 5)
                    ((member :ec rel) 3)
                    ((member :dc rel) 0)
                    (t 0)))
           :from-is-bound-enumerator))
      (if (thematic-substrate:bound-to (voi-to query))
          (values (+ 80
                     (cond ((member :tpp rel) 10)
                           ((member :ntpp rel) 8)
                           ((member :po rel) 5)
                           ((member :ec rel) 3)
                           ((member :dc rel) 0)
                           (t 0)))
                  :to-is-bound-enumerator)
        
        (values (cond ((member :tpp rel) 7)
                      ((member :ntpp rel) 7)
                      ((member :tppi rel) 10)
                      ((member :ntppi rel) 10)
                      (t 5))
                :enumerator)))))


(defmethod get-score ((query map-epsilon-query))
  (if (thematic-substrate:bound-to (voi-from query))
      (if (thematic-substrate:bound-to (voi-to query))
          (values 100 :tester)
        (values 20 :from-is-bound-enumerator))
    (if (thematic-substrate:bound-to (voi-to query))
        (values 70 :to-is-bound-enumerator)
      (values 10 :enumerator))))

(defmethod get-score ((query map-outside-epsilon-query))
  0)

;;;
;;;
;;;

(defmethod get-score ((query map-distance-query))
  ;;; die sind schweine schnell!!
  (if (thematic-substrate:bound-to (voi-from query))
      (if (thematic-substrate:bound-to (voi-to query))
          (values 100 :tester)
        (values 95 :from-is-bound-enumerator))
    (if (thematic-substrate:bound-to (voi-to query))
        (values 95 :to-is-bound-enumerator)
      (values 10 :enumerator))))
  
(defmethod get-score ((query map-outside-distance-query))
  0)

;;;
;;;
;;;

(defmethod get-score ((query map-part-of-query))
  (if (thematic-substrate:bound-to (voi-from query))
      (if (thematic-substrate:bound-to (voi-to query))
          (values 100 :tester)
        (values 100 :from-is-bound-enumerator))
    (if (thematic-substrate:bound-to (voi-to query))
        (values 100 :to-is-bound-enumerator)
      (values 30 :enumerator))))
  
;;;
;;;
;;;

(defmethod inverses-p ((c1 map-rcc-edge-query) (c2 map-rcc-edge-query))
  (and (eq (type-of c2) (type-of c1))
       (not (eq c1 c2))
       (eq (voi-from c1) (voi-to c2))
       (eq (voi-to c1) (voi-from c2))
       (set-equal (change-package-of-description 
                   (ensure-list (inverse-rcc-relation (textual-description c1)))
                   :keyword)
                  (change-package-of-description 
                   (ensure-list (textual-description c2))
                   :keyword))))

(defmethod inverses-p ((c1 map-distance-query) (c2 map-distance-query))
  (and (eq (type-of c2) (type-of c1))
       (not (eq c1 c2))
       (eq (voi-from c1) (voi-to c2))
       (eq (voi-to c1) (voi-from c2))
       (equal (change-package-of-description 
               (textual-description c1)
               :keyword)
              (change-package-of-description 
               (textual-description c2)
               :keyword))))

(defmethod inverses-p ((c1 map-edge-query) (c2 map-edge-query))
  nil)

                 
;;;
;;; Zueinander Inverse registrieren
;;;

(defmethod ts::mark-last-conjuncts-of-ands ((query map-hybrid-query))
  (register-inverses query))


;;;
;;;
;;;

(defmethod register-inverses ((query map-hybrid-or-query))
  (mapc #'register-inverses (subqueries query)))


(defmethod register-inverses ((query map-hybrid-and-query))

  (let ((edge-queries
         (remove-if-not #'is-map-edge-query-p
                        (subqueries query))))

    (dolist (c1 edge-queries)
      (let ((c2 (find-if #'(lambda (c2)
                             (inverses-p c1 c2))
                         edge-queries)))
        (when c2
          (setf (slot-value c1 'inverse-query) c2
                (slot-value c2 'inverse-query) c1))))

    (dolist (c edge-queries)
      (when (slot-value c 'active-p)
        (when (and (slot-exists-p c 'inverse-query)
                   (inverse-query c))
          (setf (slot-value (inverse-query c) 'active-p) nil))))))
