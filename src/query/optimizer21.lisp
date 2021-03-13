;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;; Achtung - der Optimizer funktioniert nur fuer Queries in DNF!
;;; Der Compiler funktioniert natuerlich fuer beliebige Plaene...
;;; Wie optimiert man allgemeine Boolean Queries, die nicht in einer (D)NF sind?
;;; 

(defun bind-vars-to (vars val)
  (dolist (var vars)
    (unless (is-query-individual-p var)
      (setf (bound-to var) val))
    (when (corresponding-voi var)
      (unless (is-query-individual-p (corresponding-voi var))
        (setf (bound-to (corresponding-voi var))
              val)))))

;;;
;;;
;;;

(defmethod compute-plan ((query atomic-query)) 
  (setf (slot-value query 'score)
        (get-score query)))

(defmethod compute-plan ((query true-query)) 
  (setf (slot-value query 'score)
        (get-score query)))

(defmethod compute-plan ((query false-query)) 
  (setf (slot-value query 'score)
        (get-score query)))

(defmethod compute-plan ((query complex-query))
  (labels ((do-it (plans no-of-plans-to-consider)
             
             
             (when plans
               
               (let ((plans (subseq (sort (copy-list plans) #'> :key #'third) 0 
                                    (min (length plans) no-of-plans-to-consider))))
                 
                 ;; (pprint plans) (terpri) (terpri)
                 
                 (if (not (second (first plans)))                     
                     (first plans)
                   
                   (do-it (mapcan #'(lambda (plan-rest-val)
                                      
                                      (mapcar #'(lambda (next)
                                                  (let* ((plan (first plan-rest-val))
                                                         (rem (second plan-rest-val))
                                                         (val (third plan-rest-val))
                                                         (n (length plan)))

                                                    (bind-vars-to (vois (parser next)) nil) ; reset all bindings
                                                             
                                                    (dolist (subquery plan) ; damit werden *alle* vois erfasst! 

                                                      (bind-vars-to
                                                       (if (is-query-reference-p subquery)
                                                           ;;; nur diese werden gebunden!
                                                           (result-vois subquery)
                                                         (all-vois subquery))
                                                       t))
                                                                                                          
                                                    (compute-plan next)
                                                    
                                                    (let ((score 
                                                           (float 
                                                            (+ val (/ (score next)
                                                                      (1+ n))))))
                                                      
                                                      (list (append plan (list next))
                                                            (remove next rem)
                                                            score))))
                                              
                                              (second plan-rest-val)))
                                  plans)
                          
                          no-of-plans-to-consider))))))

    (let* ((res (do-it (list (list nil (subqueries query) 0))
                       (if (is-and-query-p query)
                           (* 10 (length (all-subqueries query)))
                         1)))

           (plan (first res))
           (score (third res)))
      
      (setf (slot-value query 'subqueries) plan
            (slot-value query 'score) score)

      query)))

;;;
;;; Bewertung
;;;

(defmethod get-score :around ((query query))
  (if (or (exact-cache-reference query)
          (superset-cache-reference query)
          (subset-cache-reference query))
      
      ;;; bereits zur Compile-Zeit vorhandene Cache-Eintraege?
      ;;; Exploit "Database Statistics"! 
      
      ;;; (values (- 100000 (length (bindings (use-cached-bindings-of query)))) 'use-cache-entry)
      ;;; erzeugt fuerchterliche Kombinatorik! 
      
      (call-next-method)

    (call-next-method)))

;;;
;;;
;;;

(defmethod get-score ((query query))
  1)

(defmethod get-score ((query same-as-query))
  (if (negated-p query)       

      (if (bound-to (voi-from query))
          (if (bound-to (voi-to query))
              (values 950 :tester)
            (values 950 :from-is-bound-enumerator))
        (if (bound-to (voi-to query))
            (values 950 :to-is-bound-enumerator)
          (values 1 :enumerator)))

    (if (bound-to (voi-from query))
        (if (bound-to (voi-to query))
            (values 950 :tester)
          (values 950 :from-is-bound-enumerator))
      (if (bound-to (voi-to query))
          (values 950 :to-is-bound-enumerator)
        (values 1 :enumerator)))))



(defmethod get-score ((query top-query))
  1)

(defmethod get-score ((query bottom-query))
  1000)

(defmethod get-score ((query query-reference))
  ;; (score (referenced-query query))
  1)

(defmethod get-score ((query true-query))
  1000)

(defmethod get-score ((query false-query))
  1000)
 
;;;
;;; Achtung! muss angepasst werden, sobald "negated-p" vernuenftig funktioniert
;;;

#+:dlmaps
(defmethod get-score ((query substrate-simple-node-query))
  (if (bound-to (voi query))
      (values 100 :tester)
    (if (negated-p query) 
        (values 70 :enumerator)
      (values 80 :enumerator))))

#+:dlmaps
(defmethod get-score ((query substrate-simple-edge-query))
  (if (negated-p query)       
      (if (bound-to (voi-from query))
          (if (bound-to (voi-to query))
              (values 95 :tester)
            (values 60 :from-is-bound-enumerator))
        (if (bound-to (voi-to query))
            (values 60 :to-is-bound-enumerator)
          (values 20 :enumerator)))
    (if (bound-to (voi-from query))
        (if (bound-to (voi-to query))
            (values 95 :tester)
          (values 90 :from-is-bound-enumerator))
      (if (bound-to (voi-to query))
          (values 90 :to-is-bound-enumerator)
        (values 40 :enumerator)))))


#+:dlmaps
(defmethod get-score ((query substrate-predicate-node-query))
  (if (bound-to (voi query))
      (values 100 :tester)
    (if (negated-p query) 
        (values 70 :enumerator)
      (values 80 :enumerator))))

#+:dlmaps
(defmethod get-score ((query substrate-predicate-edge-query))
  (if (negated-p query)       
      (if (bound-to (voi-from query))
          (if (bound-to (voi-to query))
              (values 95 :tester)
            (values 60 :from-is-bound-enumerator))
        (if (bound-to (voi-to query))
            (values 60 :to-is-bound-enumerator)
          (values 20 :enumerator)))
    (if (bound-to (voi-from query))
        (if (bound-to (voi-to query))
            (values 95 :tester)
          (values 90 :from-is-bound-enumerator))
      (if (bound-to (voi-to query))
          (values 90 :to-is-bound-enumerator)
        (values 40 :enumerator)))))

;;;
;;;
;;;

(defmethod get-score ((query instance-retrieval-query))
  (if (bound-to (voi query))
      (values 99 :tester)
    
    (if (negated-p query) 
        (values 30 :enumerator)


      ;;; [51 ; 55[ 

      (values 

       (if *optimizer-use-cardinality-heuristics-p* 
           
           (float

            (loop as concept in (let ((concept (dl-concept query)))
                                  (if (and (consp concept) 
                                           (eq (first concept) 'and))
                                      (rest concept)
                                    (list concept)))
                  maximize 
            
                  (let* ((*told-information-reasoning-p* t)
                         (n (length
                             (dl-prover-retrieve-known-concept-instances (substrate (parser query))
                                                                         'top))))
              
                    (if (zerop n)
                  
                        51
                
                      (multiple-value-bind (all all-known)
                          (dl-prover-retrieve-concept-instances
                           (substrate (parser query))
                           concept)

                        (when nil 
                          (when all-known 
                            (princ concept)
                            (princ " all ") (princ (length all))
                            (terpri)))

                        (if all-known

                            ;; (+ 53 (- 2 (* 2 (/ (length all) n))))
                            (+ 51 (- 2 (* 2 (/ (length all) n))))

                          (multiple-value-bind (known known-known)
                              (dl-prover-retrieve-known-concept-instances
                               (substrate (parser query))
                               concept)

                            (when nil 
                              (when known-known 
                                (princ concept)
                                (princ " known ") (princ (length known)) (terpri)))

                            (if known-known

                                (+ 51 (- 2 (* 2 (/ (length known) n))))

                              51))))))))
         
         51)

       :enumerator))))


(defmethod get-score ((query has-known-successor-retrieval-query))
  (if (bound-to (voi query))
      (values 100 :tester)
    (if (negated-p query) 
        (values 40 :enumerator)
      (values 50 :enumerator))))


(defmethod get-score ((query edge-retrieval-query))
  (if (and (tbox (substrate query))
           (dl-prover-transitive-role-p (substrate query)
                                        (dl-role query)))
      
      (if (negated-p query) 
          (if (bound-to (voi-from query))
              (if (bound-to (voi-to query))
                  (values 94 :tester)
                (values 17 :from-is-bound-enumerator))
            (if (bound-to (voi-to query))
                (values 17 :to-is-bound-enumerator)
              (values 0 :enumerator)))

        (if (bound-to (voi-from query))
            (if (bound-to (voi-to query))
                (values 95 :tester)
              (values 55 :from-is-bound-enumerator))
          (if (bound-to (voi-to query))
              (values 55 :to-is-bound-enumerator)
            (values 5 :enumerator))))


    (if (negated-p query) 
        (if (bound-to (voi-from query))
            (if (bound-to (voi-to query))
                (values 98 :tester)
              (values 20 :from-is-bound-enumerator))
          (if (bound-to (voi-to query))
              (values 20 :to-is-bound-enumerator)
            (values 0 :enumerator)))
      (if (bound-to (voi-from query))
          (if (bound-to (voi-to query))
              (values 98 :tester)
            (values 60 :from-is-bound-enumerator))
        (if (bound-to (voi-to query))
            (values 60 :to-is-bound-enumerator)
          (values 10 :enumerator))))))


(defmethod get-score ((query racer-cd-edge-retrieval-query))
  (if (and (bound-to (voi-from query))
           (bound-to (voi-to query)))
      (values 95 :tester)
    (if (bound-to (voi-from query))
        (values 40
                :to-is-bound-enumerator)
      (if (bound-to (voi-to query))
          (values 40 
                  :from-is-bound-enumerator)
        (values 0 :enumerator)))))

;;;
;;;
;;;

(defmethod optimize-query ((query complex-query) &rest args &key show-plan-p &allow-other-keys)
  (declare (ignorable args))

  (bind-vars-to (vois (parser query)) nil)

  (compute-plan query)

  (bind-vars-to (vois (parser query)) nil)

  (dolist (sq (cons query (all-subqueries query)))
    (setf (slot-value sq 'is-optimized-p) t))

  (recompute-roles query)
 
  (when show-plan-p 
    (format t "~%~%Computed Plan: ~A~%~%" (subqueries query)))

  query)


(defmethod optimize-query ((query query) &rest args &key &allow-other-keys)
  (declare (ignorable args))

  (compute-plan query)

  (recompute-roles query)

  query)

(defmethod optimize-query ((query true-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))

  query)

(defmethod optimize-query ((query false-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))

  query)


;;;
;;;
;;;

(defmethod recompute-roles ((query and-query))
  (let ((plan (subqueries query)))
    (let ((first (first plan)))
      (bind-vars-to (vois (parser first)) nil)
                                                             
      (dolist (subquery plan)
        (multiple-value-bind (score role)
            (get-score subquery)
          (declare (ignorable score))
          (setf (slot-value subquery 'current-role) role))

        (bind-vars-to
         (if (is-query-reference-p subquery)
             ;;; nur diese werden gebunden!
             (result-vois subquery)
           (all-vois subquery))
         t)))))


(defmethod recompute-roles ((query or-query))
  (dolist (sq (subqueries query))
    (recompute-roles sq)))


(defmethod recompute-roles ((query atomic-query))
  (multiple-value-bind (score role)
      (get-score query)
    (declare (ignorable score))
    (setf (slot-value query 'current-role) role)))

