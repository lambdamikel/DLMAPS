;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SPATIAL-SUBSTRATE; Base: 10 -*-

(in-package spatial-substrate)

;;;
;;;
;;;

(defparameter *add-rcc-atoms-to-abox-for-reasoning* nil)
 
(defmethod syntactically-rewrite-query (query (parser generic-map-parser) &rest args)
  (call-next-method))

(defmethod syntactically-rewrite-atomic-query (query (parser generic-map-parser) &rest args 
                                                     &key negated-p &allow-other-keys)
  (cond ((unary-query-p parser query)
         
         (let* ((query (call-next-method))                (from (first query))
                (expr (second query))
                (op (first (ensure-list expr))))

           (cond  ((is-map-simple-node-query-p
                    (get-expression-type parser query))
                   `(,from
                     ,(change-package-of-description expr :keyword)))

                  ((syntactic-sugar-query-p parser query)
                   (case op 
                     ((area :area)
                      `(,from
                        (:satisfies 
                         (and (is-basic-map-polygon-p object)
                              (let ((area (calculate-area object)))
                                ,(second expr))))))

                     ((polygon :polygon)
                      `(,from
                        (:satisfies 
                         (is-basic-map-polygon-p object))))

                     ((line :line)
                      `(,from
                        (:satisfies 
                         (is-basic-map-line-p object))))

                     ((chain :chain)
                      `(,from
                        (:satisfies 
                         (is-basic-map-chain-p object))))
                    
                     ((line-or-chain :line-or-chain)
                      `(,from
                        (:satisfies 
                         (or (is-basic-map-chain-p object)
                             (is-basic-map-line-p object)))))
                    
                     ((symbol :symbol)
                      `(,from
                        (:satisfies 
                         (is-basic-map-symbol-p object))))))
                    
                  (t query))))

        ((binary-query-p parser query)
         
         (let* ((from (first query))
                (to (second query))
                (expr (ensure-list (third query))))
           
           (cond ((or (epsilon-query-p parser query)
                      (tuple-satisfies-query-p parser query))
                
                  query)

                 ((part-of-query-p parser query)
                  (if (subsetp expr '(:has-part has-part))
                      `(,to ,from :part-of)
                    `(,from ,to :part-of)))

                 ((distance-query-p parser query)
                  ;;; Symmetrisch, weil Centroid-Abstand!
                  `(and (,from ,to ,expr)
                        (,to ,from ,expr)))
                              
                 (t 
         
                  (let* ((expr (remove :and (remove :or (remove :racer expr))))
                         (expr0 (change-package-of-description expr :keyword))
                         (expr (translate-spatial-vocabulary expr0)))
                    
                    (cond ((valid-edge-or-description-p parser `(:or ,@expr)) ; RCC-Atom? 

                           (let* ((query (cond 
                                          ((typep (substrate parser) 'racer-descriptions-map)
                                           `(,from ,to (:racer ,@expr)))
                                          ((and (typep (substrate parser) 'racer-map)
                                                (binary-abox-query-p parser query))
                                           `(,from ,to (:or ,@expr)))
                                          ((and (typep (substrate parser) 'basic-map)
                                                (binary-substrate-query-p parser query))
                                           `(,from ,to (:or ,@expr)))
                                          (t `(,from ,to (:or ,@expr)))))
                        
                                  (type (get-expression-type parser query))
                        
                                  (relation (finer (finer (finer (finer expr)))))
                                  (relation
                                   (if negated-p 
                                       (negated-rcc-relation relation)
                                     relation))
                                  (inv-relation (inverse-rcc-relation relation)))

                             (typecase type
                               
                               ((or map-rcc-edge-retrieval-query
                                    map-racer-rcc-edge-query)

                                `(or ,@(mapcar #'(lambda (rel inv-rel)
                                                   `(and 
                                                     (,from ,to 
                                                            ,(if (is-map-racer-rcc-edge-query-p type)
                                                                 `(:racer ,rel)
                                                               rel)) 
                                                     (,to ,from ,(if (is-map-racer-rcc-edge-query-p type)
                                                                     `(:racer ,inv-rel)
                                                                   inv-rel))))
                                               relation inv-relation)))
                               
                               ((or map-simple-rcc-edge-query
                                    map-virtual-simple-rcc-edge-query)
                                
                                (cond (*add-rcc-atoms-to-abox-for-reasoning* 
                                       
                                       (let ((cor-from (ts::get-cor-voi parser from))
                                             (cor-to (ts::get-cor-voi parser to)))
                                         (declare (ignorable cor-from cor-to))
                                         
                                         `(and 
                                           
                                           ,@(when (member :crosses expr0)
                                               `((,from (:satisfies 
                                                         (or (is-basic-map-line-p object)
                                                             (is-basic-map-chain-p object))))))
                                           ,@(when (member :crossed-by expr0)
                                               `((,to (:satisfies 
                                                       (or (is-basic-map-line-p object)
                                                           (is-basic-map-chain-p object))))))
                                           
                                           (or ,@(mapcar #'(lambda (rel inv-rel)
                                                             `(and 
                                                               (,from ,to ,rel)
                                                               (,to ,from ,inv-rel)
                                                               ;; (,cor-from ,cor-to ,rel)
                                                               ;; (,cor-to ,cor-from ,inv-rel)
                                                               ))
                                                         relation inv-relation)))))

                                      (t 
                              
                                       `(and ,@(when (member :crosses expr0)
                                                 `((,from (:satisfies 
                                                           (or (is-basic-map-line-p object)
                                                               (is-basic-map-chain-p object))))))
                                             ,@(when (member :crossed-by expr0)
                                                 `((,to (:satisfies 
                                                         (or (is-basic-map-line-p object)
                                                             (is-basic-map-chain-p object))))))
                                   
                                             (,from ,to (:or ,@relation))
                                             (,to ,from (:or ,@inv-relation))))))
             
                               (otherwise (call-next-method)))))

                          (t (call-next-method))))))))
        
        (t (call-next-method))))

;;;
;;;
;;;

(defun translate-spatial-vocabulary (expr)
  (apply #'append
         (mapcar #'(lambda (sym)
                     (ensure-list
                      (case sym 
                        (:outside :dr)

                        (:contains :ppi)
                        (:inside-of :ppi)
                        
                        (:inside :pp)
                        (:contained-within :pp)                        
                        (:contained-in :pp)                        
                        
                        (:truly-contains :ntppi)
                        (:truly-inside-of :ntpp)
                        
                        (:truly-inside :ntpp)
                        (:truly-contained-within :ntpp)
                        (:truly-contained-in :ntpp)
                        
                        (:covers :tppi)
                        (:covered-by :tpp)
                        
                        (:touches-from-the-inside :tpp)
                        (:touched-from-the-inside :tppi)

                        
                        (:equal :eq)
                        (:congruent :eq)
                        (:same :eq)
                        
                        (:touches-from-the-outside :ec)
                        (:touched-from-the-outside :ec)
			(:touches :ec)
                        (:touched-by :ec) 
			(:adjacent :ec)
                        (:connects-to :ec)
                        (:connects :ec) 
			(:leads-to :ec)
                        (:leads-from :ec)
                        
                        (:overlaps :po)
                        (:overlapped-by :po)

                        (:intersects :po)
                        (:intersecting :po)                        
			(:flows-in '(:ec :po))
                        (:flows-out '(:ec :po))
                        
                        ;(:borders :ec) ; die Relation gibt es! u.a. zw. Linien und Polygonen 
                        ;(:bordered-by :ec)
                        
                        (:crosses :po)
                        (:crossed-by :po)
                        (otherwise sym))))

                 (ensure-list expr))))
