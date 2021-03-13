;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GEOMETRY; Base: 10 -*-

(in-package geometry)

(defconstant +granularity+ 20)

(defmethod make-arc ((pfrom geom-point) 
		     (pto geom-point)
		     xc yc &key (granularity +granularity+))
  (multiple-value-bind (radius from)
      (distance-and-orientation* xc yc (x pfrom) (y pfrom))
    (multiple-value-bind (radius1 to)
        (distance-and-orientation* xc yc (x pto) (y pto))
      (if (=-eps radius radius1 0.001)
          (let* ((points nil)
                 (diff (normalize (- to from)))
                 (fac (/ diff granularity)))
            (dotimes (i (1- granularity) points)
              (let* ((angle (+ from (* (1+ i)  fac)))
                     (x (+ xc (* radius (cos angle))))
                     (y (+ yc (* radius (sin angle)))))
                (push (list x y) points))))
        (error "Bad points!")))))


(defun make-circle (x y radius &key (granularity +granularity+))
  (let* ((points nil)
	 (fac (/ (* 2 pi) granularity)))
    (dotimes (i granularity points)
      (let* ((angle (* i fac))
             (x (+ x (* radius (cos angle))))
             (y (+ y (* radius (sin angle)))))
        (push (list x y) points)))))
