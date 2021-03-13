;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GEOMETRY; Base: 10 -*-

(in-package geometry)

(defgeneric inside-epsilon-p (obj1 obj2 radius &key epsilon-a epsilon-b)
  (:documentation "Determine whether obj1 lies inside the epsilon-enclosure
of radius of object obj2."))

;;;
;;; Achtung! Diese Relation ist *nicht* symmetrisch!!!
;;;

(defmethod-memo inside-epsilon-p ((obj1 geom-point) (obj2 geom-thing) radius &key (epsilon-a 1) (epsilon-b 1))
    ((obj1 obj2 radius epsilon-a epsilon-b))
  (let ((radius (* radius radius)))
    (<= (distance-between obj1 obj2 :sqrt nil :sx epsilon-a :sy epsilon-b) radius)))

(defmethod-memo inside-epsilon-p* ((x number) (y number) (obj geom-thing) radius &key (epsilon-a 1) (epsilon-b 1))
    ((x y radius epsilon-a epsilon-b))
  (let ((radius (* radius radius)))
    (<= (distance-between-xy x y obj :sqrt nil :sx epsilon-a :sy epsilon-b) radius)))

;;;
;;;
;;;

(defmethod-memo inside-epsilon-p ((obj1 geom-line) (obj2 geom-point) radius &key (epsilon-a 1) (epsilon-b 1))
    ;;; zu lesen als: ist die Linie obj1 im Epsilon-Radius der Größe radius vom Punkt obj2 enthalten? 
    ;;; Referenz-Objekte (um das der Radius gezogen wird) ist immer obj2!!!!
    ;;; 
    ((obj1 obj2 radius epsilon-a epsilon-b))
  (let ((radius (* radius radius)))
    (and (<= (distance-between (p1 obj1) obj2 :sqrt nil :sx epsilon-a :sy epsilon-b) radius)
	 (<= (distance-between (p2 obj1) obj2 :sqrt nil :sx epsilon-a :sy epsilon-b) radius))))

(defmethod-memo inside-epsilon-p ((obj1 geom-line) (obj2 geom-line) radius &key (epsilon-a 1) (epsilon-b 1))
    ((obj1 obj2 radius epsilon-a epsilon-b))
  (let ((radius (* radius radius)))
    (and (<= (distance-between (p1 obj1) obj2 :sqrt nil :sx epsilon-a :sy epsilon-b) radius)
	 (<= (distance-between (p2 obj1) obj2 :sqrt nil :sx epsilon-a :sy epsilon-b) radius))))


(defmethod-memo inside-epsilon-p ((obj1 geom-line) (obj2 geom-chain-or-polygon) radius &key (epsilon-a 1) (epsilon-b 1))
    ((obj1 obj2 radius epsilon-a epsilon-b))  
  (let ((radius (* radius radius)))
    (labels ((do-it (x1 y1 x2 y2 mindist)
	       (if (< (distance-between* x1 y1 x2 y2 :sqrt nil :sx epsilon-a :sy epsilon-b) mindist)
		   t
		 (let ((mx (/ (+ x1 x2) 2))
		       (my (/ (+ y1 y2) 2)))
		   (and (some #'(lambda (segment)
				  (<= (distance-between-point-and-line mx my
								       (x (p1 segment))
								       (y (p1 segment))
								       (x (p2 segment))
								       (y (p2 segment))
								       :sqrt nil
								       :sx epsilon-a :sy epsilon-b)
				      radius))
			      (segments obj2))
			(do-it x1 y1 mx my mindist)
			(do-it mx my x2 y2 mindist))))))

      (and (<= (distance-between (p1 obj1) obj2 :sqrt nil :sx epsilon-a :sy epsilon-b) radius)
	   (<= (distance-between (p2 obj1) obj2 :sqrt nil :sx epsilon-a :sy epsilon-b) radius)
	   (do-it (x (p1 obj1)) (y (p1 obj1))
		  (x (p2 obj1)) (y (p2 obj1))
		  (/ (length-of-line obj1) +granularity+)))))) ; Teilstrecken untersuchen!

;;;
;;;
;;;

(defmethod-memo inside-epsilon-p ((obj1 geom-chain-or-polygon) (obj2 geom-point) radius &key (epsilon-a 1) (epsilon-b 1))
    ((obj1 obj2 radius epsilon-a epsilon-b))  
  (let ((radius (* radius radius)))
    (every #'(lambda (p)
	       (<= (distance-between p obj2 :sqrt nil :sx epsilon-a :sy epsilon-b) radius))
	   (point-list obj1))))

(defmethod-memo inside-epsilon-p ((obj1 geom-chain-or-polygon) (obj2 geom-line) radius &key (epsilon-a 1) (epsilon-b 1))
    ((obj1 obj2 radius epsilon-a epsilon-b))  
  (let ((radius (* radius radius)))
    (every #'(lambda (p)
	       (<= (distance-between p obj2 :sqrt nil :sx epsilon-a :sy epsilon-b) radius))
	   (point-list obj1))))


(defmethod-memo inside-epsilon-p ((obj1 geom-chain-or-polygon) (obj2 geom-chain-or-polygon) radius &key (epsilon-a 1) (epsilon-b 1))
    ((obj1 obj2 radius epsilon-a epsilon-b))  
  (every #'(lambda (s)
	     (inside-epsilon-p obj1 s radius :epsilon-a epsilon-a :epsilon-b epsilon-b))
	 (segments obj2)))

