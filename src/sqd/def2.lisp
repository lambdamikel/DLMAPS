;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SQD; Base: 10 -*-

(in-package sqd)

(defvar *xmin* nil)
(defvar *ymin* nil)
(defvar *xmax* nil)
(defvar *ymax* nil)

(defun reset-sqd-bb ()
  (setf *xmin* nil
	*ymin* nil
	*xmax* nil
	*ymax* nil))

(defun calibrate-sqd-bb (x y)
  (when (or (not *xmin*)
	    (< x *xmin*))
    (setf *xmin* x))
  (when (or (not *xmax*)
	    (> x *xmax*))
    (setf *xmax* x))
  
  (when (or (not *ymin*)
	    (< y *ymin*))
    (setf *ymin* y))
  (when (or (not *ymax*)
	    (> y *ymax*))
    (setf *ymax* y)))

;;;
;;;
;;;

(defvar *?zsp*)
(defvar *?xmi*)
(defvar *?ymi*)
(defvar *?xma*)
(defvar *?yma*)
(defvar *?len*)
(defvar *?fre*)
(defvar *?wan*)
(defvar *?wen*)
(defvar *?f*)
(defvar *?x1*)
(defvar *?y1*)
(defvar *?x2*)
(defvar *?y2*)
(defvar *?sa*)
(defvar *?sar*)
(defvar *?swi*)
(defvar *?a*)
(defvar *?fla*)


;;;
;;;
;;;

(defclass sqd-block ()
  ((line-no :accessor line-no)
   (enum :accessor enum :initarg :enum)))

;;;
;;;
;;;

(defclass pg (sqd-block)
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (pkz :accessor pkz :initarg :pkz)
   (pnr :accessor pnr :initarg :pnr)            
   (os :accessor os :initarg :os)
   (os2 :accessor os2 :initarg :os2)))

(datenblock db-pg
	    (ds-kopf (if (not (or (string= *?etyp* "PG1")
				  (string= *?etyp* "PG2")))            
			 (return-from db-pg 'error)               
		       (setf *?os* nil *?os2* nil)))
	    (ds-x)
	    (ds-y)
	    (ds-pkz)
	    (ds-pnr)
	    (optional ds-os)
	    (ds-trennung)
	    ((return-from db-pg 
               (make-instance 'pg
		              :x  *?x*
		              :y  *?y*
		              :os *?os*
                              :os2 *?os2*
		              :pnr *?pnr*
		              :pkz *?pkz*))))


(defclass kr (sqd-block)
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (r :accessor r :initarg :r)
   (os :accessor os :initarg :os)
   (os2 :accessor os2 :initarg :os2)))


(datenblock db-kr
	    (ds-kopf (if (not (or (string= *?etyp* "KR1") 
				  (string= *?etyp* "KR2")))            
			 (return-from db-kr 'error)               
		       (setf *?os* nil *?os2* nil)))                                          
	    (ds-x)
	    (ds-y)
	    (ds-r)
	    (ds-os)
	    (ds-trennung)
	    ((return-from db-kr
               (make-instance 'kr
		              :x *?x*
		              :y *?y*
		              :r *?r*
		              :os *?os*
                              :os2 *?os2*))))


(defclass li (sqd-block)
  ((os :accessor os :initarg :os)
   (os2 :accessor os2 :initarg :os2)))

(datenblock db-li   
	    (ds-kopf 
	     (unless (or (string= *?etyp* "LI1") 
			 (string= *?etyp* "LI2") 
			 (string= *?etyp* "LI3"))
	       (return-from db-li 'error)))
	    (ds-os)
	    (ds-trennung)
	    ((return-from db-li
               (make-instance 'li
		              :os *?os*
                              :os2 *?os2*))))


(defclass bo (sqd-block)
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (r :accessor r :initarg :r)
   (w :accessor w :initarg :w)
   (os :accessor os :initarg :os)
   (os2 :accessor os2 :initarg :os2)))

(datenblock db-bo
	    (ds-kopf (unless (or (string= *?etyp* "BO1")
				 (string= *?etyp* "BO2"))
		       (return-from db-bo 'error)))
	    (ds-x)
	    (ds-y)
	    (ds-r)
	    (ds-w)
	    (ds-os)
	    (ds-trennung)
	    ((return-from db-bo
               (make-instance 'bo             
		              :x  *?x*
		              :y  *?y*
		              :r  *?r*
		              :w  *?w*
		              :os *?os*
                              :os2 *?os2*))))


(defclass sn (sqd-block)
  ((os :accessor os :initarg :os)
   (os2 :accessor os2 :initarg :os2)
   (points :accessor points :initarg :points)))  

(defparameter *db-points* nil)

(datenblock db-sn
	    (ds-kopf (if (not (or (string= *?etyp* "SN1")
				  (string= *?etyp* "SN2")
				  (string= *?etyp* "SN3")))			      
			 (return-from db-sn 'error)
		       (setf *db-points* nil)))
	    (ds-zsp)
	    (ds-xmi)
	    (ds-ymi)
	    (ds-xma)
	    (ds-yma)
	    (ds-len)
	    (ds-fre)
	    (ds-wan)
	    (ds-wen)
	    (optional ds-fld
		      (push *?wert* *db-points*))
					; Achtung: in Wirklichkeit muesste hier ein OR (fld fld-ohne-werte) stehen !
	    (optional ds-fld-ohne-werte)
	    (loop ds-fld-folge 
	          (push *?wert* *db-points*))
	    (ds-os)
	    (ds-trennung)
	    ((return-from db-sn
	       (if (not (zerop (mod (length *db-points*) 2)))
		   (break "Error in DB-SN: Uneven Pointlist!")
                 (make-instance 'sn
		                :os *?os*
                                :os2 *?os2*                     
		                :points (reverse *db-points*))))))


(defclass sy (sqd-block)
  ((nam :accessor nam :initarg :nam)
   (x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (os :accessor os :initarg :os)
   (os2 :accessor os2 :initarg :os2)))


(datenblock db-sy
	    (ds-kopf (unless (or (string= *?etyp* "SY1")
				 (string= *?etyp* "SY2"))
		       (return-from db-sy 'error)))
	    (ds-nam)
	    (ds-x)
	    (ds-y)
	    (ds-w)
	    (ds-f)
	    (ds-s)
	    (ds-x1)
	    (ds-y1)
	    (ds-x2)
	    (ds-y2)
	    (ds-os)
	    (ds-clp)
	    (ds-trennung)
	    ((return-from db-sy
               (make-instance 'sy
		              :nam *?nam*
		              :x  *?x*
		              :y   *?y*
		              :os *?os*
                              :os2 *?os2*))))


(defclass tx (sqd-block)         
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (w :accessor w :initarg :w)
   (h :accessor h :initarg :h)
   (m :accessor m :initarg :m)
   (txt :accessor txt :initarg :txt)
   (os :accessor os :initarg :os)
   (os2 :accessor os2 :initarg :os2)))

(datenblock db-tx
	    (ds-kopf (if (not (or (string= *?etyp* "TX1") 
				  (string= *?etyp* "TX2")))
			 (return-from db-tx 'error)
		       (progn (setf *?sar* nil *?swi* nil))))
	    (ds-x)
	    (ds-y)
	    (ds-h)
	    (ds-w)
	    (ds-a)
	    (ds-m)
	    (ds-txt)
	    
	    (optional ds-sar)		; mal kommen SAR und SWI vor OS, mal danach!
	    (optional ds-swi)
	    
	    (ds-os)
	    
	    (optional ds-sar)	    
	    (optional ds-swi)
	    
	    (ds-trennung)
	    ((return-from db-tx
               (make-instance 'tx
		              :x  *?x*
		              :y  *?y*
		              :h  *?h*
		              :w  *?w*
		              :m *?m*
		              :txt (recode-german-characters *?txt*)
		              :os *?os*
                              :os2 *?os2*))))


(defclass fl (sqd-block)
  ((nam :accessor name :initarg :nam)
   (x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (os :accessor os :initarg :os)
   (os2 :accessor os2 :initarg :os2)))

(datenblock db-fl
	    (ds-kopf (unless (or (string= *?etyp* "FL1")
				 (string= *?etyp* "FL2"))
		       (return-from db-fl 'error)))
	    (ds-nam)
	    (ds-x)
	    (ds-y)
	    (ds-w)
	    (ds-sa)
	    (ds-fla)
	    (ds-os)
	    (ds-trennung)
	    ((return-from db-fl
               (make-instance 'fl
		              :nam *?nam*
		              :x *?x*
		              :y *?y*
		              :os *?os*
                              :os2 *?os2*))))


#| kommen nicht vor in "va4"

(datenblock db-tp
	    (ds-kopf (unless (or (string= *?etyp* "TP1") 
				 (string= *?etyp* "TP2"))
		       (return-from tp 'error)))
	    (ds-x)
	    (ds-y)
	    (ds-nam)
	    (ds-os)
	    (ds-trennung))

(datenblock db-pa
	    (ds-kopf (unless (or (string= *?etyp* "PA1")
				 (string= *?etyp* "PA2"))
		       (return-from pa 'error)))
	    (ds-x)
	    (ds-y)
	    (ds-nam)
	    (ds-os)
	    (ds-trennung))
|#

;;;
;;;
;;;

(defmethod initialize-instance :after ((obj sqd-block) &rest initargs)
  (declare (ignore initargs))
  (setf (line-no obj) *line-counter*
	(enum obj) *?enum*))
;;;
;;;
;;;

(defmethod initialize-instance :after ((obj pg) &rest initargs)
  (declare (ignore initargs))
  (with-slots (x y) obj    
    (calibrate-sqd-bb x y)))

(defmethod initialize-instance :after ((obj sy) &rest initargs)
  (declare (ignore initargs))
  (with-slots (x y) obj
    (calibrate-sqd-bb x y)))

(defmethod initialize-instance :after ((obj bo) &rest initargs)
  (declare (ignore initargs))
  (with-slots (x y) obj
    (calibrate-sqd-bb x y)))

(defmethod initialize-instance :after ((obj sn) &rest initargs)
  (declare (ignore initargs))   
  (with-slots (points) obj
    (dolist (point (transform-xy-list points))      
      (calibrate-sqd-bb (first point)
			(second point)))))
