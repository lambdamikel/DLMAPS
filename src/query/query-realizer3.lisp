;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;; 
;;;

(defmethod semantically-rewrite-query ((query nrql-atomic-query) (parser simple-parser) 
                                       &key &allow-other-keys)
  query)

    
(defmethod semantically-rewrite-query ((query or-query) (parser simple-parser) 
                                       &rest args &key &allow-other-keys)

  (declare (ignorable args))

  (let* ((subqueries (mapcar #'(lambda (sq) 
                                 (apply #'semantically-rewrite-query sq parser args))
                             (subqueries query)))
         
         (subqueries 
          (remove-if #'query-inconsistent-p subqueries)))
    
    (cond ((not subqueries)
           
           query)

          ((not (cdr subqueries))
           
           (make-top-level-query (first subqueries))
           
           (first subqueries))

          (t  (make-description (type-of query) 
                                nil
                                :subqueries subqueries 
                                :parser parser)))))


(defmethod semantically-rewrite-query ((query and-query) (parser simple-parser)
                                       &rest args)
  (declare (ignorable args))
  
  (let* ((conjuncts (subqueries query))
         (old-conjuncts (remove-if-not #'(lambda (x) 
                                           (or (not (is-instance-retrieval-query-p x))
                                               (negated-p x)))
                                       conjuncts))
         (new-conjuncts
          (realize-abox-query-conjuncts conjuncts
                                        :tbox (tbox (substrate query))))
         (new-conjuncts
          (append old-conjuncts
                  (if (eq new-conjuncts 'inconsistent)
                      (list (make-description 'nrql-bottom-query 'bottom
                                              :parser parser
                                              :vois (list (first (vois parser)))))
                    (let ((cluster nil))
                      
                      (loop while new-conjuncts do 
                            (let* ((entry (first new-conjuncts))
                                   (voi (first entry))
                                   (concept (second entry))
                                   (others (loop as other in (rest new-conjuncts) 
                                                 when (eq (first other) voi)
                                                 collect other)))
                              (push (list voi (cons concept (mapcar #'second others))) 
                                    cluster)

                              (setf new-conjuncts (rest new-conjuncts))
                              (setf new-conjuncts 
                                    (set-difference new-conjuncts others))))

                      (dolist (entry cluster)
                        (unless (cdr (second entry))
                          (setf (cdr entry)  
                                (second entry))))

                      (mapcar #'(lambda (entry)
                                  (make-description 'nrql-instance-retrieval-query
                                                    (second entry)
                                                    :parser parser
                                                    :vois (list (first entry))))
                              cluster))))))

    (make-description (type-of query) 
                      nil
                      :subqueries new-conjuncts
                      :parser parser)))



