;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SPATIAL-SUBSTRATE; Base: 10 -*-

(in-package spatial-substrate)

;;;
;;; Wichtig Konstante!!!!
;;; Bestimmt u.a., wie weit beim Einlesen unterschiedliche Punkte
;;; auseinander sein dürfen, damit sie nicht verschmolzen werden!
;;; Hält die Karte zusammen (topologische Graph-Integrität)
;;; 

(defconstant +spatial-index-epsilon+ 0.01)

(defconstant +grid-x-resolution+ 20)

(defconstant +grid-y-resolution+ 20)

;;;
;;;
;;;

(defgeneric install-as-current-index (obj)
  (:documentation "Let the spatial index obj be the current index."))

(defgeneric reset-spatial-index (obj)
  (:documentation "Resets the spatial index obj."))

(defgeneric insert-into-spatial-index (obj)
  (:documentation "Insert obj into current spatial index."))

(defgeneric remove-from-spatial-index (obj)
  (:documentation "Remove obj from current spatial index."))

(defgeneric candidate-selected-p (reference-obj candidate mode &key &allow-other-keys)
  (:documentation "Determine whether a relation << candidate MODE reference-obj >> holds."))

(defgeneric bucket-selected-p (reference-obj bucket mode &key &allow-other-keys)
  (:documentation "Determine whether the objects of BUCKET are relevant candidates to check 
whether a << candidate MODE reference-obj >> relation holds."))

(defgeneric get-range-for-object (reference-obj mode &key &allow-other-keys)
  (:documentation "Returns range of relevant buckets."))

;;;
;;;
;;;

(defparameter *mark-counter* 0)

(defparameter *cur-index* nil)

(defun get-new-mark-value ()
  (incf *mark-counter*))

;;;
;;;
;;;

(defpersistentclass spatial-index ()
  ((grid :accessor grid :initform (make-array (list +grid-x-resolution+ +grid-y-resolution+) :initial-element nil)
	 :initarg :grid)
   (elements :accessor elements :initform nil)
	     
   (xdiv :accessor xdiv :initarg :xdiv)
   (ydiv :accessor ydiv :initarg :ydiv)
   (xmin :accessor xmin :initarg :xmin)
   (ymin :accessor ymin :initarg :ymin)
   (xmax :accessor xmax :initarg :xmax)
   (ymax :accessor ymax :initarg :ymax)

   (xres :accessor xres :initarg :xres :initform 20)
   (yres :accessor yres :initarg :yres :initform 20)))

;;;
;;; ACHTUNG: alle si-Klassen sind abstrakt => keine Konstruktoren
;;;

(defpersistentclass si-geom-thing (geom-thing)
  ((level-array :accessor level-array
		:initform (make-array +no-of-levels+ :initial-element nil)
		:not-persistent)
   (in-buckets :accessor in-buckets :initform nil)

   (dont-display :accessor dont-display :initarg :dont-display :initform nil)
   (depth-value :accessor depth-value :initarg :depth-value :initform 0)))


(defmethod initialize-loaded-persistent-object ((obj si-geom-thing))
  (setf (level-array obj) (make-array +no-of-levels+ :initial-element nil)))

(defpersistentclass si-geom-point (si-geom-thing geom-point)
  ())

(defun si-p (x y &rest args)
  (or (let ((point (get-point-from-spatial-index* x y)))
        ;;; Rundungsfehler/Topologie-Korrektur!!!
        (when point
          (apply #'make-instance 'si-geom-point :x (x point) :y (y point) :allow-other-keys t args)))
      (apply #'make-instance 'si-geom-point :x x :y y :allow-other-keys t args)))

(defpersistentclass si-geom-line (si-geom-thing geom-line)
  ())

(defun si-l (p1 p2 &rest args)
  (apply #'make-instance 'si-geom-line :p1 p1 :p2 p2 :allow-other-keys t args))

(defpersistentclass si-geom-chain-or-polygon (si-geom-thing geom-chain-or-polygon)
  ())

(defpersistentclass si-geom-polygon (si-geom-chain-or-polygon geom-polygon)
  ((no-of-contained-polygons :accessor no-of-contained-polygons :initarg :no-of-contained-polygons :initform 0)))


(defpersistentclass si-geom-chain (si-geom-chain-or-polygon geom-chain)
  ())

;;;
;;;
;;;

(defun set-value-for (obj level value)
  (setf (svref (level-array obj) level) value))

(defun get-value-for (obj level)
  (svref (level-array obj) level))

;;;
;;;
;;;

(defmethod initialize-instance :after ((obj si-geom-thing) &rest initargs)
  (declare (ignore initargs))
  (insert-into-spatial-index obj))

(defmethod delete-object progn ((obj si-geom-thing) &key)
  (remove-from-spatial-index obj))
    
;;;
;;;
;;;

(defpersistentclass bucket ()
  ((bbox :accessor bbox :initarg :bbox)
   (elements :accessor elements :initarg :elements
	     :initform nil)))

;;;
;;;
;;;

(defun make-spatial-index (xmin1 ymin1 xmax1 ymax1)
  (let ((index
	 (make-instance 'spatial-index)))
    (with-slots (xdiv ydiv xmin ymin xmax ymax xres yres grid) index	
      (setf xmin xmin1
	    ymin ymin1
	    xmax xmax1
	    ymax ymax1)
      (setf xdiv (/ (- (1+ xmax1) xmin1) xres)
	    ydiv (/ (- (1+ ymax1) ymin1) yres))
      (dotimes (x xres)
	(dotimes (y yres)
	  (setf (aref grid x y)
	        (make-instance 'bucket
	                       :bbox 
	                       (make-bounding-box 
	                        (+ xmin (* x xdiv))
	                        (+ ymin (* y ydiv))
	                        (+ xmin (* (1+ x) xdiv))
	                        (+ ymin (* (1+ y) ydiv))))))))
    index))

(defun init-spatial-index (xmin ymin xmax ymax)
  (install-as-current-index
   (make-spatial-index xmin ymin xmax ymax)))


(defmethod install-as-current-index ((obj spatial-index))
  (setf *cur-index* obj))

;;;
;;;
;;;

(defmethod reset-spatial-index ((obj spatial-index))
  (with-slots (grid xres yres) obj
    (dotimes (x xres)
      (dotimes (y yres)
	(let ((bucket (aref grid x y)))
	  (when bucket
	    (setf
             (elements bucket) nil)))))))

(defmethod get-indizes-for-point* ((obj spatial-index) x y)
  (with-slots (xmin ymin xdiv ydiv) obj
    (values
     (truncate (- x xmin) xdiv)
     (truncate (- y ymin) ydiv))))

(defmethod get-bucket-for-point* ((obj spatial-index) x y)
  (multiple-value-call #'(lambda (x y)
			   (when (and (<= 0 x (1- (xres obj)))
				      (<= 0 y (1- (yres obj))))
			     (aref (grid obj) x y)))
    (get-indizes-for-point* obj x y)))

(defmethod get-elements-at* ((obj spatial-index) x y)
  (elements 
   (get-bucket-for-point* obj x y)))


(defun get-current-bucket (ix iy)
  (aref (grid *cur-index*) ix iy))


;;;
;;;
;;;

(defmethod insert-into-spatial-index ((obj si-geom-thing))    
  (push obj (elements *cur-index*))
  (with-selected-buckets (cur-bucket obj :intersects)    
    (push obj (elements cur-bucket))
    (push cur-bucket (in-buckets obj))))

;;;
;;;
;;;

(defmethod remove-from-spatial-index ((obj si-geom-thing))
  (with-selected-buckets (cur-bucket obj :intersects)    
    (setf (elements cur-bucket)
          (delete obj (elements cur-bucket))))
  (setf (elements *cur-index*)
        (delete obj (elements *cur-index*))))

;;;
;;; Grobeinstellung der zu untersuchenden Buckets:
;;;


(defun get-range-for-bb-object (obj &optional (offset 0))
  (if (bounding-box-p obj)
      (let* ((pmin (pmin obj))
	     (pmax (pmax obj))
	     (xmin (x pmin))
	     (ymin (y pmin))
	     (xmax (x pmax))
	     (ymax (y pmax)))
	(multiple-value-bind (ixmin iymin)
	    (get-indizes-for-point* *cur-index* 
				    (- xmin offset) 
				    (- ymin offset))
	  (multiple-value-bind (ixmax iymax)
	      (get-indizes-for-point* *cur-index* 
				      (+ xmax offset)
				      (+ ymax offset))
	    (with-slots (xres yres) *cur-index*
	      (values (max 0 ixmin)
		      (max 0 iymin)
		      (min ixmax (1- xres))
		      (min iymax (1- yres)))))))    
    (error "No bounding box!")))

(defun get-whole-range ()
  (with-slots (xres yres) *cur-index*
    (values 0 0 (1- xres) (1- yres))))  

;;;
;;;
;;;


(defmethod get-range-for-object ((obj t) (mode symbol) &key)

  ;;; diese Methode MUSS so allgemein sein, fuer 
  ;;; geom-thing wird bounding-box-mixin ausgehebelt 
  ;;; (geom-thing ist spezieller!) 

  (format t "*** WARNING: CANNOT GET RANGE FOR ~A ~A~%" obj mode)

  -1)

;;;

(defmethod get-range-for-object ((obj geom-point) (mode (eql :intersects)) &key)
  (multiple-value-bind (ix iy)
      (get-indizes-for-point* *cur-index*
			      (x obj) (y obj))
    (values ix iy ix iy)))

;;;

(defmethod get-range-for-object ((obj geom-point) (mode (eql :overlaps)) &key)
  (multiple-value-bind (ix iy)
      (get-indizes-for-point* *cur-index*
			      (x obj) (y obj))
    (values ix iy ix iy)))

;;;

(defmethod get-range-for-object ((obj geom-point) (mode (eql :inside-epsilon)) 
				 &key epsilon-r)
  (with-slots (xres yres) *cur-index*
    (multiple-value-bind (ixmin iymin)
        (get-indizes-for-point* *cur-index*
                                (- (x obj) epsilon-r)
                                (- (y obj) epsilon-r))
      (multiple-value-bind (ixmax iymax)
          (get-indizes-for-point* *cur-index*
                                  (+ (x obj) epsilon-r)
                                  (+ (y obj) epsilon-r))
        (values (max 0 ixmin)
                (max 0 iymin)
                (min ixmax (1- xres))
                (min iymax (1- yres)))))))

(defmethod get-range-for-object ((obj geom-point) (mode (eql :outside-epsilon)) 
				 &key)
  (get-whole-range))

(defmethod get-range-for-object ((obj geom-point) (mode (eql :ring-epsilon)) 
				 &key epsilon-min epsilon-max)
  (declare (ignorable epsilon-min))
  (get-range-for-object obj :inside-epsilon :epsilon-r epsilon-max))

;;;

(defmethod get-range-for-object ((obj geom-point) (mode (eql :inside-distance)) 
				 &key epsilon-r)
  (get-range-for-object obj :inside-epsilon :epsilon-r epsilon-r))


(defmethod get-range-for-object ((obj geom-point) (mode (eql :outside-distance)) 
				 &key epsilon-r)
  (get-range-for-object obj :outside-epsilon :epsilon-r epsilon-r))


(defmethod get-range-for-object ((obj geom-point) (mode (eql :ring-distance)) 
				 &key epsilon-min epsilon-max)
  (get-range-for-object obj :ring-epsilon :epsilon-min epsilon-min :epsilon-max epsilon-max))

;;;
;;;
;;;

(defmethod get-range-for-object ((obj bounding-box-mixin) (mode symbol) &key)
  (break))

(defmethod get-range-for-object ((obj bounding-box-mixin) 
				 (mode (eql :intersects)) &key)
  (get-range-for-bb-object obj))

(defmethod get-range-for-object ((obj bounding-box-mixin) 
				 (mode (eql :overlaps)) &key)
  (get-range-for-bb-object obj))

(defmethod get-range-for-object ((obj bounding-box-mixin) 
				 (mode (eql :inside)) &key)
  (get-range-for-bb-object obj))

(defmethod get-range-for-object ((obj bounding-box-mixin) 
				 (mode (eql :truly-inside)) &key)
  (get-range-for-bb-object obj))

(defmethod get-range-for-object ((obj bounding-box-mixin) 
				 (mode (eql :covered-by)) &key)
  (get-range-for-bb-object obj))

(defmethod get-range-for-object ((obj bounding-box-mixin) 
				 (mode (eql :outside)) &key)
  (get-range-for-bb-object obj 1))

(defmethod get-range-for-object ((obj bounding-box-mixin) 
				 (mode (eql :touches)) &key)
  ;;; wenn das Objekt berührt, ist es entweder im gleichen oder 
  ;;; (maximal "entfernt") im direkt angrenzenden Bucket verzeichnet 
  (with-slots (xres yres) *cur-index*
    (multiple-value-bind (xmin ymin xmax ymax)
        (get-range-for-bb-object obj) 
      (values (max 0 (1- xmin))
              (max 0 (1- ymin))
              (min (1+ xmax) (1- xres))
              (min (1+ ymax) (1- yres))))))

(defmethod get-range-for-object ((obj bounding-box-mixin) 
				 (mode (eql :lies-on)) &key)
  (get-range-for-bb-object obj))

(defmethod get-range-for-object ((obj bounding-box-mixin)
				 (mode (eql :inside-epsilon)) 
				 &key epsilon-r)
    (get-range-for-bb-object obj epsilon-r))

(defmethod get-range-for-object ((obj bounding-box-mixin)
				 (mode (eql :outside-epsilon)) 
				 &key)
  (get-whole-range))

(defmethod get-range-for-object ((obj bounding-box-mixin) 
                                 (mode (eql :ring-epsilon)) 
				 &key epsilon-min epsilon-max)
  (declare (ignorable epsilon-min))
  (get-range-for-object obj :inside-epsilon :epsilon-r epsilon-max))
 

(defmethod get-range-for-object ((obj bounding-box-mixin)
				 (mode (eql :inside-distance)) 
				 &key epsilon-r)
  (get-range-for-object (pcenter obj) :inside-epsilon :epsilon-r epsilon-r))


(defmethod get-range-for-object ((obj bounding-box-mixin)
				 (mode (eql :outside-distance)) 
                                 &key epsilon-r)
  (get-range-for-object (pcenter obj) :outside-epsilon :epsilon-r epsilon-r))


(defmethod get-range-for-object ((obj bounding-box-mixin) 
                                 (mode (eql :ring-distance)) 
				 &key epsilon-min epsilon-max)
  (get-range-for-object (pcenter obj) :ring-epsilon
                        :epsilon-min epsilon-min 
                        :epsilon-max epsilon-max))
 

;;;
;;; Grobtest: Bucket untersuchen ? 
;;; Es werden nur Buckets untersucht, die im zuvor berechneten "Range" liegen
;;; (s. get-range-for-object). 
;;; 
;;; Nur f.d. Map-Viewer (zum Malen): 
;;;

(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :intersects)) &key)
  (box-overlaps-box-p obj (bbox bucket)))

(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :overlaps)) &key)
  (box-overlaps-box-p obj (bbox bucket)))

(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :inside)) &key)
  (box-overlaps-box-p obj (bbox bucket)))

(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :truly-inside)) &key)
  (box-overlaps-box-p obj (bbox bucket)))

(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :covered-by)) &key)
  (and (box-overlaps-box-p obj (bbox bucket))
       (not (box-truly-inside-box-p (bbox bucket) obj))))

(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :touches)) &key)
  (and (box-overlaps-box-p obj (bbox bucket))
       (not (box-truly-inside-box-p (bbox bucket) obj))))

(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :lies-on)) &key)
  (bucket-selected-p obj bucket :touches))

(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :outside)) &key)
  (not (box-truly-inside-box-p (bbox bucket) obj)))

;;;
;;;
;;;

(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :inside-distance)) 
			      &key epsilon-r)
  (bucket-selected-p (pcenter obj) bucket :inside-epsilon :epsilon-r epsilon-r))

(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :outside-distance)) 
			      &key epsilon-r)
  (bucket-selected-p (pcenter obj) bucket :outside-epsilon :epsilon-r epsilon-r))


(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :ring-distance)) 
			      &key epsilon-min epsilon-max)
  (bucket-selected-p (pcenter obj) bucket :ring-epsilon
                     :epsilon-min epsilon-min
                     :epsilon-max epsilon-max))

;;;
;;; PUNKTE
;;;

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :intersects)) &key)
  (eq (get-bucket-for-point* *cur-index* (x obj) (y obj)) bucket))

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :overlaps)) &key)
  (eq (get-bucket-for-point* *cur-index* (x obj) (y obj)) bucket))

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :touches)) &key)
  (eq (get-bucket-for-point* *cur-index* (x obj) (y obj)) bucket))

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :lies-on)) &key)
  (eq (get-bucket-for-point* *cur-index* (x obj) (y obj)) bucket))

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :inside-epsilon)) 
			      &key epsilon-r)
  (or (bucket-selected-p obj bucket :intersects) 
      (let ((radius (radius (bbox bucket)))
	    (center (pcenter (bbox bucket))))
	(<= (+ (distance-between obj center) radius)
            epsilon-r))))

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :outside-epsilon)) 
			      &key epsilon-r)
  (not (bucket-selected-p obj bucket :inside-epsilon 
                          :epsilon-r epsilon-r)))

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :ring-epsilon)) 
			      &key epsilon-min epsilon-max)
  (and (bucket-selected-p obj bucket :inside-epsilon 
                          :epsilon-r epsilon-max)
       (bucket-selected-p obj bucket :outside-epsilon 
                          :epsilon-r epsilon-min)))

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :inside-distance)) 
			      &key epsilon-r)
  (bucket-selected-p obj bucket :inside-epsilon :epsilon-r epsilon-r))

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :outside-distance)) 
			      &key epsilon-r)
  (bucket-selected-p obj bucket :outside-epsilon :epsilon-r epsilon-r))


(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :ring-distance)) 
			      &key epsilon-min epsilon-max)
  (bucket-selected-p obj bucket :ring-epsilon
                     :epsilon-min epsilon-min
                     :epsilon-max epsilon-max))

;;;
;;; LINIEN
;;;

(defmethod bucket-selected-p ((obj geom-line) (bucket bucket) (mode (eql :intersects)) &key)
  (box-overlaps-box-p obj (bbox bucket)))

(defmethod bucket-selected-p ((obj geom-line) (bucket bucket) (mode (eql :overlaps)) &key)
  (box-overlaps-box-p obj (bbox bucket)))

(defmethod bucket-selected-p ((obj geom-line) (bucket bucket) (mode (eql :touches)) &key)
  (box-overlaps-box-p obj (bbox bucket)))

(defmethod bucket-selected-p ((obj geom-line) (bucket bucket) (mode (eql :lies-on)) &key)
  (box-overlaps-box-p obj (bbox bucket)))

(defmethod bucket-selected-p ((obj geom-line) (bucket bucket) (mode (eql :inside-epsilon)) 
			      &key epsilon-r)
  (enlarged-box-overlaps-box-p obj 
                               (bbox bucket)
                               epsilon-r))

(defmethod bucket-selected-p ((obj geom-line) (bucket bucket) (mode (eql :outside-epsilon)) 
			      &key)
  ;;; hier hab ich keine bessere Idee! alles zu teuer! 
  t)

(defmethod bucket-selected-p ((obj geom-line) (bucket bucket) (mode (eql :ring-epsilon)) 
			      &key epsilon-max epsilon-min)
  (declare (ignorable epsilon-min))
  (bucket-selected-p obj bucket :inside-epsilon :epsilon-r epsilon-max))


;;;
;;; KETTEN und POLYGONE
;;;
 
(defmethod bucket-selected-p ((obj geom-chain-or-polygon) (bucket bucket) (mode (eql :intersects)) &key)
  (some #'(lambda (segment)
	    (bucket-selected-p segment bucket :intersects))
	(segments obj)))

(defmethod bucket-selected-p ((obj geom-chain-or-polygon) (bucket bucket) (mode (eql :overlaps)) &key)
  (some #'(lambda (segment)
	    (bucket-selected-p segment bucket :intersects))
	(segments obj)))

(defmethod bucket-selected-p ((obj geom-chain-or-polygon) (bucket bucket) (mode (eql :touches)) &key)
  (some #'(lambda (segment)
	    (bucket-selected-p segment bucket :touches))
	(segments obj)))

(defmethod bucket-selected-p ((obj geom-chain-or-polygon) (bucket bucket) (mode (eql :lies-on)) &key)
  (some #'(lambda (segment)
	    (bucket-selected-p segment bucket :lies-on))
	(segments obj)))

(defmethod bucket-selected-p ((obj geom-chain-or-polygon) (bucket bucket) (mode (eql :inside-epsilon)) 
			      &key epsilon-r)
  (let ((box (bbox bucket)))
    (some #'(lambda (segment)
	      (enlarged-box-overlaps-box-p segment box epsilon-r))
	  (segments obj))))

(defmethod bucket-selected-p ((obj geom-chain-or-polygon) (bucket bucket) (mode (eql :outside-epsilon)) 
                              &key)
  ;;; hier hab ich keine bessere Idee! alles zu teuer! 
  t)

(defmethod bucket-selected-p ((obj geom-chain-or-polygon) (bucket bucket) (mode (eql :ring-epsilon)) 
			      &key epsilon-max epsilon-min)
  (declare (ignorable epsilon-min))
  (bucket-selected-p obj bucket :inside-epsilon :epsilon-r epsilon-max))

;;;
;;; POLYGONE
;;;

(defmethod bucket-selected-p ((obj geom-polygon) (bucket bucket) (mode (eql :inside)) &key)
  (box-overlaps-box-p obj (bbox bucket)))

(defmethod bucket-selected-p ((obj geom-polygon) (bucket bucket) (mode (eql :overlaps)) &key)
  (box-overlaps-box-p obj (bbox bucket)))

(defmethod bucket-selected-p ((obj geom-polygon) (bucket bucket) (mode (eql :covered-by)) &key)
  (some #'(lambda (segment)
	    (bucket-selected-p segment bucket :touches))
	(segments obj)))

(defmethod bucket-selected-p ((obj geom-polygon) (bucket bucket) (mode (eql :outside)) &key)
  (not (box-truly-inside-box-p (bbox bucket) obj)))

;;;
;;; Feintest: Objekt selektiert?
;;; INSIDE f. Bounding Boxes
;;; spez. fuer den Map-Viewer - zum Malen! (der brauch kein Epsilon)
;;; 

(defmethod candidate-selected-p ((reference-obj bounding-box) (candidate bounding-box-mixin) 
				 (mode (eql :intersects)) &key)
  (box-overlaps-box-p candidate reference-obj))

(defmethod candidate-selected-p ((reference-obj bounding-box) (candidate bounding-box-mixin) 
				 (mode (eql :overlaps)) &key)
  (box-overlaps-box-p candidate reference-obj))

(defmethod candidate-selected-p ((reference-obj bounding-box) (candidate bounding-box-mixin) 
				 (mode (eql :touches)) &key)
  (box-overlaps-box-p candidate reference-obj))

(defmethod candidate-selected-p ((reference-obj bounding-box) (candidate bounding-box-mixin) 
				 (mode (eql :lies-on)) &key)
  (box-overlaps-box-p candidate reference-obj))

(defmethod candidate-selected-p ((reference-obj bounding-box) (candidate bounding-box-mixin) 
				 (mode (eql :inside)) &key)
  (box-inside-box-p candidate reference-obj))

(defmethod candidate-selected-p ((reference-obj bounding-box) (candidate bounding-box-mixin) 
				 (mode (eql :truly-inside)) &key)
  (box-truly-inside-box-p candidate reference-obj))

(defmethod candidate-selected-p ((reference-obj bounding-box) (candidate bounding-box-mixin) 
				 (mode (eql :covered-by)) &key)
  (box-inside-box-p candidate reference-obj))

(defmethod candidate-selected-p ((reference-obj bounding-box) (candidate bounding-box-mixin) 
				 (mode (eql :outside)) &key)
  (not (box-inside-box-p candidate reference-obj)))


;;;
;;; 
;;;

(defmethod candidate-selected-p ((reference-obj bounding-box) (candidate geom-point) 
				 (mode (eql :intersects)) &key)
  (point-inside-box-p candidate reference-obj))

(defmethod candidate-selected-p ((reference-obj bounding-box) (candidate geom-point) 
				 (mode (eql :overlaps)) &key)
  (point-inside-box-p candidate reference-obj))

(defmethod candidate-selected-p ((reference-obj bounding-box) (candidate geom-point) 
				 (mode (eql :touches)) &key)
  (point-inside-box-p candidate reference-obj))

(defmethod candidate-selected-p ((reference-obj bounding-box) (candidate geom-point) 
				 (mode (eql :lies-on)) &key)
  (lies-on-p candidate reference-obj))

(defmethod candidate-selected-p ((reference-obj bounding-box) (candidate geom-point) 
				 (mode (eql :inside)) &key)
  (point-inside-box-p candidate reference-obj))

(defmethod candidate-selected-p ((reference-obj bounding-box) (candidate geom-point) 
				 (mode (eql :truly-inside)) &key)
  (point-truly-inside-box-p candidate reference-obj))

(defmethod candidate-selected-p ((reference-obj bounding-box) (candidate geom-point) 
				 (mode (eql :covered-by)) &key)
  (point-inside-box-p candidate reference-obj))

(defmethod candidate-selected-p ((reference-obj bounding-box) (candidate geom-point) 
				 (mode (eql :outside)) &key)
  (point-outside-box-p candidate reference-obj))

;;;
;;;
;;;

(defmethod candidate-selected-p ((reference-obj bounding-box-mixin) (candidate geom-point) 
				 (mode (eql :inside-distance)) &key epsilon-r)
  (candidate-selected-p (pcenter reference-obj) candidate
                        mode :epsilon-r epsilon-r))

(defmethod candidate-selected-p ((reference-obj bounding-box-mixin) (candidate geom-point) 
				 (mode (eql :outside-distance)) &key epsilon-r)
  (candidate-selected-p (pcenter reference-obj) candidate
                        mode :epsilon-r epsilon-r))

(defmethod candidate-selected-p ((reference-obj bounding-box-mixin) (candidate geom-point) 
				 (mode (eql :ring-distance)) &key epsilon-min epsilon-max)
  (candidate-selected-p (pcenter reference-obj) candidate
                        mode 
                        :epsilon-min epsilon-min
                        :epsilon-max epsilon-max))

;;;
;;; INTERSECTS
;;; 

(defmethod candidate-selected-p ((reference-obj geom-thing) (candidate geom-thing) 
				 (mode (eql :intersects)) &key)
  (intersects-p candidate reference-obj))

;;;
;;; TOUCHES
;;; 

(defmethod candidate-selected-p ((reference-obj geom-thing) (candidate geom-thing) 
				 (mode (eql :touches)) &key)
  (touches-p candidate reference-obj))

;;; 
;;; LIES ON
;;; 

(defmethod candidate-selected-p ((reference-obj geom-thing) (candidate geom-thing) 
				 (mode (eql :lies-on)) &key)
  (lies-on-p candidate reference-obj))

;;;
;;; INSIDE
;;; 

(defmethod candidate-selected-p ((reference-obj geom-polygon) (candidate geom-thing) 
				 (mode (eql :inside)) &key)
  (or (inside-p candidate reference-obj)
      (covered-by-p candidate reference-obj)))

#|
(defmethod candidate-selected-p ((reference-obj geom-polygon) (candidate geom-thing) 
				 (mode (eql :inside)) &key)
  (inside-p candidate reference-obj))

|#

;;;
;;; TRULY INSIDE = INSIDE
;;; 

(defmethod candidate-selected-p ((reference-obj geom-polygon) (candidate geom-thing) 
				 (mode (eql :truly-inside)) &key)
  (inside-p candidate reference-obj))

;;;
;;; COVERED-BY
;;; 

(defmethod candidate-selected-p ((reference-obj geom-polygon) (candidate geom-thing) 
				 (mode (eql :covered-by)) &key)
  (covered-by-p candidate reference-obj))

;;;
;;; OUTSIDE
;;; 

(defmethod candidate-selected-p ((reference-obj geom-polygon) (candidate geom-thing) 
				 (mode (eql :outside)) &key)
  (outside-p candidate reference-obj))

;;;
;;; OVERLAPS
;;; 

(defmethod candidate-selected-p ((reference-obj geom-polygon) (candidate geom-thing) 
				 (mode (eql :overlaps)) &key)
  (overlaps-p candidate reference-obj))

;;;
;;; EPSILON
;;; 

(defmethod candidate-selected-p ((reference-obj geom-thing) (candidate geom-thing)
				 (mode (eql :epsilon))
				 &key epsilon-r)
  (inside-epsilon-p candidate reference-obj epsilon-r))


(defmethod candidate-selected-p ((reference-obj geom-thing) (candidate geom-thing)
				 (mode (eql :inside-epsilon))
				 &key epsilon-r)
  (inside-epsilon-p candidate reference-obj epsilon-r))

(defmethod candidate-selected-p ((reference-obj geom-thing) (candidate geom-thing)
				 (mode (eql :outside-epsilon))
				 &key epsilon-r)
  (not (inside-epsilon-p candidate reference-obj epsilon-r)))


(defmethod candidate-selected-p ((reference-obj geom-thing) (candidate geom-thing)
				 (mode (eql :ring-epsilon))
				 &key epsilon-min epsilon-max)
  (and      (inside-epsilon-p candidate reference-obj epsilon-max)
            (not (inside-epsilon-p candidate reference-obj epsilon-min))))

;;;
;;; DISTANCE (ZW. PCENTEREN!) - WIRD FUER ALLE OBJEKTE HIERAUF ZURUECKGEFUEHRT!
;;; 

(defmethod candidate-selected-p ((reference-obj geom-point) (candidate geom-point)
				 (mode (eql :inside-distance))
				 &key epsilon-r)
  (<= (distance-between reference-obj candidate) epsilon-r))


(defmethod candidate-selected-p ((reference-obj geom-point) (candidate geom-point)
				 (mode (eql :outside-distance))
				 &key epsilon-r)
  (> (distance-between reference-obj candidate) epsilon-r))


(defmethod candidate-selected-p ((reference-obj geom-point) (candidate geom-point)
				 (mode (eql :ring-distance))
				 &key epsilon-min epsilon-max)
  (<= epsilon-min (distance-between reference-obj candidate) epsilon-max))

;;;
;;;
;;;


(defmethod candidate-selected-p ((reference-obj si-geom-point) (candidate bounding-box-mixin) 
				 (mode (eql :inside-distance)) &key epsilon-r)
  (candidate-selected-p reference-obj (pcenter candidate)
                        mode :epsilon-r epsilon-r))

(defmethod candidate-selected-p ((reference-obj si-geom-point) (candidate bounding-box-mixin) 
				 (mode (eql :outside-distance)) &key epsilon-r)
  (candidate-selected-p reference-obj (pcenter candidate)
                        mode :epsilon-r epsilon-r))

(defmethod candidate-selected-p ((reference-obj si-geom-point) (candidate bounding-box-mixin) 
				 (mode (eql :ring-distance)) &key epsilon-min epsilon-max)
  (candidate-selected-p reference-obj (pcenter candidate)
                        mode 
                        :epsilon-min epsilon-min
                        :epsilon-max epsilon-max))

;;;
;;;
;;;

(defmethod candidate-selected-p ((reference-obj bounding-box-mixin) (candidate bounding-box-mixin)
				 (mode (eql :inside-distance))
				 &key epsilon-r)
  (candidate-selected-p (pcenter reference-obj) (pcenter candidate) mode :epsilon-r epsilon-r))


(defmethod candidate-selected-p ((reference-obj bounding-box-mixin) (candidate bounding-box-mixin)
				 (mode (eql :outside-distance))
				 &key epsilon-r)
  (candidate-selected-p (pcenter reference-obj) (pcenter candidate) mode :epsilon-r epsilon-r))

(defmethod candidate-selected-p ((reference-obj bounding-box-mixin) (candidate bounding-box-mixin)
				 (mode (eql :ring-distance))
				 &key epsilon-min epsilon-max)
  (candidate-selected-p (pcenter reference-obj) (pcenter candidate) mode :epsilon-min epsilon-min :epsilon-max epsilon-max))


;;;
;;; Retrieval: 
;;;

(defun get-point-from-spatial-index* (x y &optional (epsilon +spatial-index-epsilon+))
  (let ((bucket (get-bucket-for-point* *cur-index* x y)))
    (when bucket
      (dolist (obj (elements bucket))
	(when (and (typep obj 'si-geom-point)
		   (=-eps x (x obj) epsilon)
		   (=-eps y (y obj) epsilon))
	  (return-from get-point-from-spatial-index* obj))))))

;;;
;;; Sortierung
;;; 

(defmethod sort-spatial-index ((obj spatial-index))                               
  (with-slots (grid xres yres elements) obj
    (let ((n (length elements)))
      (dolist (obj elements) 
        (setf (depth-value obj) 
              (typecase obj
                (geom-polygon
                 (+ 2
                    n 
                    (no-of-contained-polygons obj)))
                (geom-chain 
                 2)
                (geom-line
                 1)
                (geom-point 0)))))
      
    (dotimes (x xres)
      (dotimes (y yres)
        (let* ((bucket (aref grid x y))
               (elements (elements bucket)))
          (when bucket
            (setf (elements bucket) 
                  (sort elements #'>=
                        :key #'depth-value))))))
    (setf elements           
          (sort elements #'>=
                :key #'depth-value))))

(defun sort-current-spatial-index ()
  (sort-spatial-index *cur-index*))

