;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SPATIAL-SUBSTRATE; Base: 10 -*-

(in-package spatial-substrate)

;;;
;;; Zusammenfassungen von AND/OR-Konjunkten/Disjunkten anregen: 
;;;


(defmethod and-query-available-p ((a map-epsilon-query) (b map-epsilon-query))
  ; aus Faulheit, prinzipiell moeglich!
  nil)

(defmethod or-query-available-p  ((a map-epsilon-query) (b map-epsilon-query))
  ; aus Faulheit, prinzipiell moeglich!
  nil)

;;;
;;;
;;; 

(defmethod and-query-available-p ((a map-simple-rcc-edge-query) (b map-simple-rcc-edge-query))
  t)

(defmethod or-query-available-p  ((a map-simple-rcc-edge-query) (b map-simple-rcc-edge-query))
  t)

;;;
;;;
;;;

(defmethod and-query-available-p ((a map-virtual-simple-rcc-edge-query) (b map-virtual-simple-rcc-edge-query))
  t)

(defmethod or-query-available-p  ((a map-virtual-simple-rcc-edge-query) (b map-virtual-simple-rcc-edge-query))
  t)

;;;
;;;
;;;

(defmethod and-query-available-p ((a map-racer-rcc-edge-query) (b map-racer-rcc-edge-query))
  t)

(defmethod and-query-available-p ((a map-rcc-edge-retrieval-query) (b map-rcc-edge-retrieval-query))
  t)

;;;
;;;
;;;

(defmethod make-and-description ((descr map-racer-rcc-edge-query) &rest args &key descriptions &allow-other-keys)  
  (if (every #'(lambda (x)
                   (equal (textual-description descr)
                          (textual-description x)))
               descriptions)
    descr
    (apply #'make-description 
           (type-of descr)
           '(:racer :bottom)
           args)))


(defmethod make-and-description ((descr map-rcc-edge-retrieval-query) &rest args &key descriptions &allow-other-keys)  
  (if (every #'(lambda (x)
                   (equal (textual-description descr)
                          (textual-description x)))
             descriptions)
      descr
    (apply #'make-description 
           (type-of descr)
           :bottom
           args)))
