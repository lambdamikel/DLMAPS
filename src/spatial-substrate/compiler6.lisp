;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SPATIAL-SUBSTRATE; Base: 10 -*-

(in-package spatial-substrate)

;;;
;;; Code f. virtuelle RCC-Edge-Queries (raeumliche Selektion) 
;;; 

(defquery-code (map-virtual-simple-rcc-edge-query)
  (:tester
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-virtual-simple-rcc-edge-query))

   (:compiler (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    `(when (candidate-selected-p ,(or from `(thematic-substrate:bound-to ,(voi-from query)))
                                 ,(or to `(thematic-substrate:bound-to ,(voi-to query)))
                                 ',(textual-description query))
       ,body)))
  (:from-bound-enumerator
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-virtual-simple-rcc-edge-query))

   (:compiler (body &rest args &key to &allow-other-keys)
    (declare (ignorable args))
    `(with-selected-objects (,to (thematic-substrate:bound-to ,(voi-from query))
                                 (spatially-relevant-p ,to)
                                 ',(textual-description query))
       ,body)))
  (:to-bound-enumerator
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-virtual-simple-rcc-edge-query))

   (:compiler (body &rest args &key from &allow-other-keys)
    (declare (ignorable args))

    (unless (inverse-query query)
      (error "missing inverse query (map-virtual-simple-rcc-edge-query)"))

    `(with-selected-objects (,from (thematic-substrate:bound-to ,(voi-to query))
                                   (spatially-relevant-p ,from)
                                   ',(textual-description (inverse-query query)))
       ,body)))

  (:enumerator
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-virtual-simple-rcc-edge-query))

   (:compiler (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    `(abortable-dolist (,from (get-nodes *running-substrate*))
       (when (spatially-relevant-p ,from)
         (with-selected-objects (,to ,from
                                     (spatially-relevant-p ,to)
                                     ',(textual-description query))
           ,body))))))

;;;
;;; muss überladen werden, wegen "DC"! 
;;;

(defquery-code (map-simple-rcc-edge-query)
  (:tester 

   (:compiler (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    (if (not (member :dc (textual-description query)))
        (call-next-method)
      `(when (or (some #'(lambda (edge)
                           (matches-p ,query edge))
                       (get-edges-between *running-substrate*
                                          ,(or from `(thematic-substrate:bound-to ,(voi-from query)))
                                          ,(or to `(thematic-substrate:bound-to ,(voi-to query)))))
                 (not (get-edges-between *running-substrate*
                                         ,(or from `(thematic-substrate:bound-to ,(voi-from query)))
                                         ,(or to `(thematic-substrate:bound-to ,(voi-to query))))))
         ,body))))
  (:from-bound-enumerator
   
   (:compiler (body &rest args &key to &allow-other-keys)
    (declare (ignorable args))
    (if (not (member :dc (textual-description query)))
        (call-next-method)  
      `(abortable-dolist (,to (get-nodes *running-substrate*))
         (unless (eq ,to (thematic-substrate:bound-to ,(voi-from query)))
           (when (or (some #'(lambda (edge)
                               (matches-p ,query edge))
                           (get-edges-between *running-substrate*
                                              (thematic-substrate:bound-to ,(voi-from query))
                                              ,to))
                     (not (get-edges-between *running-substrate*
                                             (thematic-substrate:bound-to ,(voi-from query))
                                             ,to)))
             ,body))))))
  (:to-bound-enumerator 

   (:compiler (body &rest args &key from &allow-other-keys)
    (declare (ignorable args))
    (if (not (member :dc (textual-description query)))
        (call-next-method)  
      `(abortable-dolist (,from (get-nodes *running-substrate*))
         (when (or (some #'(lambda (edge)
                             (matches-p ,query edge))
                         (get-edges-between *running-substrate*                              
                                            ,from
                                            (thematic-substrate:bound-to ,(voi-to query))))
                   (not (get-edges-between *running-substrate*
                                           ,from
                                           (thematic-substrate:bound-to ,(voi-to query)))))
           ,body)))))
  (:enumerator

   (:compiler (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    (if (not (member :dc (textual-description query)))
        (call-next-method)
      `(abortable-dolist (,from (get-nodes *running-substrate*))
         (abortable-dolist (,to (get-nodes *running-substrate*))
           (when (and (not (eq ,from ,to))
                      (or (some #'(lambda (edge)
                                    (matches-p ,query edge))
                                (get-edges-between *running-substrate*
                                                   ,from ,to))
                          (not (get-edges-between *running-substrate*
                                                  ,to ,from))))
             ,body)))))))


;;;
;;; Code f. Epsilon-Querie 
;;;

(defquery-code (map-inside-epsilon-query)
  (:tester 
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-inside-epsilon-query))

   (:compiler (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    `(when (candidate-selected-p           
            ,(or from `(thematic-substrate:bound-to ,(voi-to query)))
            ,(or to `(thematic-substrate:bound-to ,(voi-from query)))
            :inside-epsilon
            :epsilon-r ,(epsilon query))
       ,body)))
  (:from-bound-enumerator 
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-inside-epsilon-query))

   (:compiler (body &rest args &key to &allow-other-keys)
    (declare (ignorable args))
    `(abortable-dolist (,to (get-nodes *running-substrate*))
       (when (and (spatially-relevant-p ,to)
                  (candidate-selected-p ,to
                                        (thematic-substrate:bound-to ,(voi-from query))
                                        :inside-epsilon
                                        :epsilon-r ,(epsilon query)))
         ,body))))
  (:to-bound-enumerator 
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-inside-epsilon-query))

   (:compiler (body &rest args &key from &allow-other-keys)
    (declare (ignorable args))
    `(with-selected-objects (,from (thematic-substrate:bound-to ,(voi-to query))
                                   (spatially-relevant-p ,from)
                                   :inside-epsilon 
                                   :epsilon-r ,(epsilon query))
       ,body)))
  (:enumerator
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-inside-epsilon-query))

   (:compiler (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    `(abortable-dolist (,from (get-nodes *running-substrate*))
       (when (spatially-relevant-p ,from)
         (with-selected-objects (,to ,from
                                     (spatially-relevant-p ,to)
                                     :inside-epsilon 
                                     :epsilon-r ,(epsilon query))
           ,body))))))
  
;;;
;;; 
;;;


(defquery-code (map-outside-epsilon-query)
  (:tester
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-outside-epsilon-query))

   (:compiler (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    `(when (candidate-selected-p           
            ,(or from `(thematic-substrate:bound-to ,(voi-to query)))
            ,(or to `(thematic-substrate:bound-to ,(voi-from query)))
            :outside-epsilon
            :epsilon-r ,(epsilon query))
       ,body)))
  (:from-bound-enumerator 
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-outside-epsilon-query))

   (:compiler (body &rest args &key to &allow-other-keys)
    (declare (ignorable args))
    `(abortable-dolist (,to (get-nodes *running-substrate*))
       (when (and (spatially-relevant-p ,to)
                  (candidate-selected-p ,to
                                        (thematic-substrate:bound-to ,(voi-from query))
                                        :outside-epsilon
                                        :epsilon-r ,(epsilon query)))
         ,body))))
  (:to-bound-enumerator
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-outside-epsilon-query))
   
   (:compiler (body &rest args &key from &allow-other-keys)
    (declare (ignorable args))
    `(with-selected-objects (,from (thematic-substrate:bound-to ,(voi-to query))
                                   (spatially-relevant-p ,from)
                                   :outside-epsilon 
                                   :epsilon-r ,(epsilon query))
       ,body)))
  (:enumerator
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-outside-epsilon-query))
   
   (:compiler (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    `(abortable-dolist (,from (get-nodes *running-substrate*))
       (when (spatially-relevant-p ,from)
         (with-selected-objects (,to ,from
                                     (spatially-relevant-p ,to)
                                     :outside-epsilon 
                                     :epsilon-r ,(epsilon query))
           ,body))))))

;;;
;;; 
;;;


(defquery-code (map-ring-epsilon-query)
  (:tester
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-ring-epsilon-query))
   
   (:compiler (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    `(when (candidate-selected-p           
            ,(or from `(thematic-substrate:bound-to ,(voi-to query)))
            ,(or to `(thematic-substrate:bound-to ,(voi-from query)))
            :ring-epsilon
            :epsilon-min ,(epsilon query)
            :epsilon-max ,(epsilon-max query))
       ,body)))
  (:from-bound-enumerator 
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-ring-epsilon-query))
   
   (:compiler (body &rest args &key to &allow-other-keys)
    (declare (ignorable args))  
    `(abortable-dolist (,to (get-nodes *running-substrate*))
       (when (and (spatially-relevant-p ,to)
                  (candidate-selected-p ,to
                                        (thematic-substrate:bound-to ,(voi-from query))
                                        :ring-epsilon
                                        :epsilon-min ,(epsilon query)
                                        :epsilon-max ,(epsilon-max query)))
         ,body))))
  (:to-bound-enumerator
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-ring-epsilon-query))
   
   (:compiler (body &rest args &key from &allow-other-keys)
    (declare (ignorable args))
    `(with-selected-objects (,from (thematic-substrate:bound-to ,(voi-to query))
                                   (spatially-relevant-p ,from)
                                   :ring-epsilon 
                                   :epsilon-min ,(epsilon query)
                                   :epsilon-max ,(epsilon-max query))
       ,body)))   
  (:enumerator
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-ring-epsilon-query))
   
   (:compiler (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    `(abortable-dolist (,from (get-nodes *running-substrate*))
       (when (spatially-relevant-p ,from)
         (with-selected-objects (,to ,from
                                     (spatially-relevant-p ,to)
                                     :ring-epsilon 
                                     :epsilon-min ,(epsilon query)
                                     :epsilon-max ,(epsilon-max query))
           ,body))))))

;;;
;;;  Code f. "Tuple Satisfies" Queries
;;;

(defquery-code (map-tuple-satisfies-query)
  (:tester 
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-tuple-satisfies-query))
   
   (:compiler (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    `(when (funcall ,(predicate query)
                    ,(or from `(thematic-substrate:bound-to ,(voi-to query)))
                    ,(or to `(thematic-substrate:bound-to ,(voi-from query))))
       ,body)))
  (:from-bound-enumerator 
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-tuple-satisfies-query))
   
   (:compiler (body &rest args &key to &allow-other-keys)
    (declare (ignorable args))
    `(abortable-dolist (,to (get-nodes *running-substrate*))
       (when (and (spatially-relevant-p ,to)
                  (funcall ,(predicate query)
                           (thematic-substrate:bound-to ,(voi-from query))
                           ,to))
         ,body))))
  (:to-bound-enumerator 
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-tuple-satisfies-query))

   (:compiler (body &rest args &key from &allow-other-keys)
    (declare (ignorable args))
    `(abortable-dolist (,from (get-nodes *running-substrate*))
       (when (and (spatially-relevant-p ,from)
                  (funcall ,(predicate query)
                           ,from 
                           (thematic-substrate:bound-to ,(voi-to query))))
         ,body))))
  (:enumerator
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-tuple-satisfies-query))

   (:compiler (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    `(abortable-dolist (,from (get-nodes *running-substrate*))
       (when (spatially-relevant-p ,from)
         (abortable-dolist (,to (get-nodes *running-substrate*))
           (when (and (spatially-relevant-p ,to)
                      (funcall ,(predicate query)
                               ,from ,to))
             ,body)))))))

;;;
;;; Code f. Distance Queries 
;;;


(defquery-code (map-inside-distance-query)
  (:tester
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-inside-distance-query))

   (:compiler (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    `(when (candidate-selected-p           
            ,(or from `(thematic-substrate:bound-to ,(voi-to query)))
            ,(or to `(thematic-substrate:bound-to ,(voi-from query)))
            :inside-distance
            :epsilon-r ,(epsilon query))
       ,body)))
  (:from-bound-enumerator
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-inside-distance-query))

   (:compiler (body &rest args &key to &allow-other-keys)
    (declare (ignorable args))
    `(abortable-dolist (,to (get-nodes *running-substrate*))
       (when (and (spatially-relevant-p ,to)
                  (candidate-selected-p ,to
                                        (thematic-substrate:bound-to ,(voi-from query))
                                        :inside-distance
                                        :epsilon-r ,(epsilon query)))
         ,body))))
  (:to-bound-enumerator 
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-inside-distance-query))
   
   (:compiler (body &rest args &key from &allow-other-keys)
    (declare (ignorable args))
    `(with-selected-objects (,from (thematic-substrate:bound-to ,(voi-to query))
                                   (spatially-relevant-p ,from)
                                   :inside-distance 
                                   :epsilon-r ,(epsilon query))
       ,body)))
  (:enumerator
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-inside-distance-query))

   (:compiler (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    `(abortable-dolist (,from (get-nodes *running-substrate*))
       (when (spatially-relevant-p ,from)
         (with-selected-objects (,to ,from
                                     (spatially-relevant-p ,to)
                                     :inside-distance 
                                     :epsilon-r ,(epsilon query))
           ,body))))))

;;;
;;; 
;;;


(defquery-code (map-outside-distance-query) 
  (:tester
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-outside-distance-query))

   (:compiler (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    `(when (candidate-selected-p           
            ,(or from `(thematic-substrate:bound-to ,(voi-to query)))
            ,(or to `(thematic-substrate:bound-to ,(voi-from query)))
            :outside-distance
            :epsilon-r ,(epsilon query))
       ,body)))
  (:from-bound-enumerator
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-outside-distance-query))
   
   (:compiler (body &rest args &key to &allow-other-keys)
    (declare (ignorable args))
    `(abortable-dolist (,to (get-nodes *running-substrate*))
       (when (and (spatially-relevant-p ,to)
                  (candidate-selected-p ,to
                                        (thematic-substrate:bound-to ,(voi-from query))
                                        :outside-distance
                                        :epsilon-r ,(epsilon query)))
         ,body))))
  (:to-bound-enumerator
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-outside-distance-query))

   (:compiler (body &rest args &key from &allow-other-keys)
    (declare (ignorable args))
    `(with-selected-objects (,from (thematic-substrate:bound-to ,(voi-to query))
                                   (spatially-relevant-p ,from)
                                   :outside-distance 
                                   :epsilon-r ,(epsilon query))
       ,body)))
  (:enumerator
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-outside-distance-query))

   (:compiler (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    `(abortable-dolist (,from (get-nodes *running-substrate*))
       (when (spatially-relevant-p ,from)
         (with-selected-objects (,to ,from
                                     (spatially-relevant-p ,to)
                                     :outside-distance 
                                     :epsilon-r ,(epsilon query))
           ,body))))))

;;;
;;; 
;;;

(defquery-code (map-ring-distance-query) 
  (:tester
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-ring-distance-query))

   (:compiler (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    `(when (candidate-selected-p           
            ,(or from `(thematic-substrate:bound-to ,(voi-to query)))
            ,(or to `(thematic-substrate:bound-to ,(voi-from query)))
            :ring-distance
            :epsilon-min ,(epsilon query)
            :epsilon-max ,(epsilon-max query))
       ,body)))
  (:from-bound-enumerator 
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-ring-distance-query))

   (:compiler (body &rest args &key to &allow-other-keys)
    (declare (ignorable args))
    `(abortable-dolist (,to (get-nodes *running-substrate*))
       (when (and (spatially-relevant-p ,to)
                  (candidate-selected-p ,to
                                        (thematic-substrate:bound-to ,(voi-from query))
                                        :ring-distance
                                        :epsilon-min ,(epsilon query)
                                        :epsilon-max ,(epsilon-max query)))
         ,body))))
  (:to-bound-enumerator
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-ring-distance-query))

   (:compiler (body &rest args &key from &allow-other-keys)
    (declare (ignorable args))
    `(with-selected-objects (,from (thematic-substrate:bound-to ,(voi-to query))
                                   (spatially-relevant-p ,from)
                                   :ring-distance 
                                   :epsilon-min ,(epsilon query)
                                   :epsilon-max ,(epsilon-max query))
       ,body)))
  (:enumerator
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-ring-distance-query))

   (:compiler (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    `(abortable-dolist (,from (get-nodes *running-substrate*))
       (when (spatially-relevant-p ,from)
         (with-selected-objects (,to ,from
                                     (spatially-relevant-p ,to)
                                     :ring-distance 
                                     :epsilon-min ,(epsilon query)
                                     :epsilon-max ,(epsilon-max query))
           ,body))))))

;;;
;;; 
;;;

(defquery-code (map-part-of-query)
  (:tester 
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-part-of-query))

   (:compiler (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    `(when (direct-component-p 
            ,(or from `(thematic-substrate:bound-to ,(voi-to query)))
            ,(or to `(thematic-substrate:bound-to ,(voi-from query))))
       ,body)))
  (:from-bound-enumerator
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-part-of-query))

   (:compiler (body &rest args &key to &allow-other-keys)
    (declare (ignorable args))
    `(abortable-dolist (,to (part-of (thematic-substrate:bound-to ,(voi-from query))))
       ,body)))
  (:to-bound-enumerator 
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-part-of-query))

   (:compiler (body &rest args &key from &allow-other-keys)
    (declare (ignorable args))
    `(abortable-dolist (,from (has-parts (thematic-substrate:bound-to ,(voi-to query))))
       ,body)))
  (:enumerator
   (:runtime (continuation &rest args)
    (declare (ignorable continuation args))
    (to-be-implemented 'map-part-of-query))

   (:compiler (body &rest args &key from to &allow-other-keys)
    (declare (ignorable args))
    `(abortable-dolist (,from (get-nodes *running-substrate*))
       (when (spatially-relevant-p ,from)
         (abortable-dolist (,to (get-nodes *running-substrate*))
           (when (and (spatially-relevant-p ,to)
                      (direct-component-p ,from ,to))
             ,body)))))))
