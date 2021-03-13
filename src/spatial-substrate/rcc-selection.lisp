;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SPATIAL-SUBSTRATE; Base: 10 -*-

(in-package spatial-substrate)

;;;
;;;
;;;

(defmethod get-range-for-object ((obj geom-point) 
				 (mode (eql :po)) &key)
  -1)

(defmethod get-range-for-object ((obj geom-point) 
				 (mode (eql :dc)) &key)
  (get-whole-range))
  
(defmethod get-range-for-object ((obj geom-point) 
				 (mode (eql :ec)) &key)
  -1)

(defmethod get-range-for-object ((obj geom-point) 
				 (mode (eql :eq)) &key)
  (get-range-for-object obj :intersects))

(defmethod get-range-for-object ((obj geom-point) 
				 (mode (eql :tppi)) &key)
  (get-range-for-object obj :intersects))
  
(defmethod get-range-for-object ((obj geom-point) 
				 (mode (eql :ntppi)) &key)
  (get-range-for-object obj :intersects))

(defmethod get-range-for-object ((obj geom-point) (mode (eql :ntpp)) &key)
  -1)

(defmethod get-range-for-object ((obj geom-point) 
				 (mode (eql :borders)) &key)
  (get-range-for-object obj :intersects))

(defmethod get-range-for-object ((obj geom-point) 
				 (mode (eql :bordered-by)) &key)
  (get-range-for-object obj :intersects))

;;;
;;;
;;;

(defmethod get-range-for-object ((obj bounding-box-mixin) 
				 (mode (eql :po)) &key)
  (get-range-for-object obj :intersects))

(defmethod get-range-for-object ((obj bounding-box-mixin) 
				 (mode (eql :dc)) &key)
  (get-whole-range))
  
(defmethod get-range-for-object ((obj bounding-box-mixin) 
				 (mode (eql :ec)) &key)
  (get-range-for-object obj :touches))

(defmethod get-range-for-object ((obj bounding-box-mixin) 
				 (mode (eql :eq)) &key)
  (get-range-for-bb-object obj))

(defmethod get-range-for-object ((obj bounding-box-mixin) 
				 (mode (eql :tppi)) &key)
  (get-range-for-object obj :covered-by))

(defmethod get-range-for-object ((obj bounding-box-mixin) 
				 (mode (eql :ntppi)) &key)
  (get-range-for-object obj :truly-inside))

(defmethod get-range-for-object ((obj bounding-box-mixin) 
				 (mode (eql :tpp)) &key)
  (get-range-for-object obj :inside))

(defmethod get-range-for-object ((obj bounding-box-mixin) 
				 (mode (eql :ntpp)) &key)
  (get-range-for-object obj :inside))

(defmethod get-range-for-object ((obj bounding-box-mixin) 
				 (mode (eql :borders)) &key)
  (get-range-for-object obj :touches))

(defmethod get-range-for-object ((obj bounding-box-mixin) 
				 (mode (eql :bordered-by)) &key)
  (get-range-for-object obj :touches))

(defmethod get-range-for-object ((obj geom-thing) 
				 (mode list) &key)
  (let (minx miny maxx maxy)
    (dolist (mode mode)
      (multiple-value-bind (xmin ymin xmax ymax)
          (get-range-for-object obj mode)
        (unless (= -1 xmin)
          (unless minx (setf minx xmin))
          (unless miny (setf miny ymin))
          (unless maxx (setf maxx xmax))
          (unless maxy (setf maxy ymax))
          (setf minx (min minx xmin)
                miny (min miny ymin)
                maxx (max maxx xmax)
                maxy (max maxy ymax)))))
    (if (and minx miny maxx maxy)
        (values minx miny maxx maxy)
      -1)))

;;;
;;; Grobtest: Bucket untersuchen ?
;;;

(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :po)) &key)
  (bucket-selected-p obj bucket :intersects))

(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :ec)) &key)
  (bucket-selected-p obj bucket :touches))
  
(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :dc)) &key)
  t)

(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :eq)) &key)
  (bucket-selected-p obj bucket :intersects))

(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :ntppi)) &key)
  (bucket-selected-p obj bucket :truly-inside))

(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :tppi)) &key)
  (bucket-selected-p obj bucket :covered-by))

(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :ntpp)) &key)
  (bucket-selected-p obj bucket :inside))

(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :tpp)) &key)
  (bucket-selected-p obj bucket :inside))

(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :borders)) &key)
  (bucket-selected-p obj bucket :touches))

(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :bordered-by)) &key)
  (bucket-selected-p obj bucket :touches))

(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode list) &key)
  (some #'(lambda (mode) 
            (bucket-selected-p obj bucket mode))
        mode))

;;;
;;;
;;;

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :po)) &key)
  nil)

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :dc)) &key)
  t)

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :ec)) &key)
  nil)

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :eq)) &key)
  nil)

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :ntppi)) &key)
  (bucket-selected-p obj bucket :truly-inside))

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :tppi)) &key)
  (bucket-selected-p obj bucket :covered-by))

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :ntpp)) &key)
  nil)

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :tpp)) &key)
  nil)

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :borders)) &key)
  nil)

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :bordered-by)) &key)
  (bucket-selected-p obj bucket :intersects))

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode list) &key)
  (some #'(lambda (mode) 
            (bucket-selected-p obj bucket mode))
        mode))

;;;
;;;
;;;

(defmethod bucket-selected-p ((obj geom-polygon) (bucket bucket) (mode (eql :dc)) &key)
  (bucket-selected-p obj bucket :outside))

(defmethod bucket-selected-p ((obj geom-polygon) (bucket bucket) (mode (eql :tpp)) &key)
  (bucket-selected-p obj bucket :inside))

(defmethod bucket-selected-p ((obj geom-polygon) (bucket bucket) (mode (eql :ntpp)) &key)
  (bucket-selected-p obj bucket :inside))

;;;
;;;
;;;

(defmethod candidate-selected-p ((reference-obj geom-thing) (candidate geom-thing) 
				 (mode (eql :po)) &key)
  (eq (calculate-rcc-relation reference-obj candidate) :po))

(defmethod candidate-selected-p ((reference-obj geom-thing) (candidate geom-thing) 
				 (mode (eql :ec)) &key)
  (eq (calculate-rcc-relation reference-obj candidate) :ec))

(defmethod candidate-selected-p ((reference-obj geom-thing) (candidate geom-thing) 
				 (mode (eql :dc)) &key)
  (eq (calculate-rcc-relation reference-obj candidate) :dc))

(defmethod candidate-selected-p ((reference-obj geom-thing) (candidate geom-thing) 
				 (mode (eql :eq)) &key)
  (eq (calculate-rcc-relation reference-obj candidate) :eq))

(defmethod candidate-selected-p ((reference-obj geom-thing) (candidate geom-thing) 
				 (mode (eql :ntppi)) &key)
  (eq (calculate-rcc-relation reference-obj candidate) :ntppi))

(defmethod candidate-selected-p ((reference-obj geom-thing) (candidate geom-thing) 
				 (mode (eql :tppi)) &key)
  (eq (calculate-rcc-relation reference-obj candidate) :tppi))


(defmethod candidate-selected-p ((reference-obj geom-thing) (candidate geom-thing) 
				 (mode (eql :ntpp)) &key)
  (eq (calculate-rcc-relation reference-obj candidate) :ntpp))

(defmethod candidate-selected-p ((reference-obj geom-thing) (candidate geom-thing) 
				 (mode (eql :tpp)) &key)
  (eq (calculate-rcc-relation reference-obj candidate) :tpp))

(defmethod candidate-selected-p ((reference-obj geom-thing) (candidate geom-thing) 
				 (mode list) &key)
  (member (calculate-rcc-relation reference-obj candidate) mode))

(defmethod candidate-selected-p ((reference-obj geom-thing) (candidate geom-thing) 
				 (mode (eql :borders)) &key)
  (eq (calculate-rcc-relation reference-obj candidate) :borders))

(defmethod candidate-selected-p ((reference-obj geom-thing) (candidate geom-thing) 
				 (mode (eql :bordered-by)) &key)
  (eq (calculate-rcc-relation reference-obj candidate) :bordered-by))

