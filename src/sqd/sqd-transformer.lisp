;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SQD; Base: 10 -*-

(in-package sqd)

(defvar *line*)
(defvar *obj*)
(defvar *erg*)

;;;
;;; Vom benutzenden Modul (Client) muessen die Funktionen 
;;; transform-sqd-... 
;;; implementiert werden!
;;; Zudem: set-client-bb (xmin,ymin,xmax,ymax)
;;;

(defmethod construct ((master null) (rest list))
  nil)

(defmethod construct ((master pg) (rest list))
  (construct-object-for 
   :point
   (second (lookup-os (os master)))
   (second (lookup-os (os2 master)))
   
   :os-num (os master)
   :os2-num (os2 master)
   
   :enum (enum master)
   :line-no (line-no master)
   :x (x master)
   :y (y master)))

(defmethod construct ((master kr) (rest list))
  (construct-object-for 
   :circle
   (second (lookup-os (os master)))
   (second (lookup-os (os2 master)))

   :os-num (os master)
   :os2-num (os2 master)

   :enum (enum master)   
   :line-no (line-no master)
   :x (x master)
   :y (y master)
   :r (r master)))

(defmethod construct ((master li) (rest list))
  (let ((startpoint (first rest))
	(endpoint (second rest)))
    (construct-object-for 
     :line
     (second (lookup-os (os master)))
     (second (lookup-os (os2 master)))

     :os-num (os master)
     :os2-num (os2 master)
     
     :enum (enum master)   
     :line-no (line-no master)
     :p1 (construct startpoint nil)
     :p2 (construct endpoint nil))))


(defmethod construct ((master sn) (rest list))
  (let ((startpoint (first rest))
	(endpoint (second rest)))
    (construct-object-for
     :spline
     (second (lookup-os (os master)))
     (second (lookup-os (os2 master)))

     :os-num (os master)
     :os2-num (os2 master)

     :enum (enum master)   
     :line-no (line-no master)
     :p1 (construct startpoint nil)
     :p2 (construct endpoint nil)
     :points (points master))))

(defmethod construct ((master bo) (rest list))
  (let ((startpoint (first rest))
	(endpoint (second rest)))
    (construct-object-for
     :arc
     (second (lookup-os (os master)))
     (second (lookup-os (os2 master)))

     :os-num (os master)
     :os2-num (os2 master)

     :enum (enum master)   
     :line-no (line-no master)
     :x (x master)
     :y (y master)
     :r (r master)
     :w (w master)
     :p1 (construct startpoint nil)
     :p2 (construct endpoint nil))))


(defmethod construct ((master fl) (rest list))
  (construct-object-for 
   :area
   (second (lookup-os (os master)))
   (second (lookup-os (os2 master)))

   :os-num (os master)
   :os2-num (os2 master)

   :enum (enum master)   
   :line-no (line-no master)
   :components 
   (let ((components nil))
     (loop
      (let ((master (first rest))
            (params (rest rest)))               
        (cond ((null rest) (return components))
              (t
               (unless (listp master)                                 
                 (push 
                  (construct master (first params))
                  components))
               (setf rest (rest rest)))))))))

(defmethod construct ((master sy) (rest list))
  (construct-object-for
   :symbol
   (second (lookup-os (os master)))
   (second (lookup-os (os2 master)))

   :os-num (os master)
   :os2-num (os2 master)

   :enum (enum master)   
   :line-no (line-no master)
   :x (x master)
   :y (y master)
   :name (nam master)))

(defmethod construct ((master tx) (rest list))
  (construct-object-for 
   :text
   (second (lookup-os (os master)))
   (second (lookup-os (os2 master)))

   :os-num (os master)
   :os2-num (os2 master)

   :enum (enum master)   
   :line-no (line-no master)
   :x (x master)
   :y (y master)
   :h (h master)
   :w (w master)
   :text (txt master)))

;;;
;;;
;;;

(defun transform (liste)
  (mapcar #'(lambda (obj)
	      (let ((master (first obj))
		    (rest (rest obj)))             
		(setf *obj* obj)
		(construct master (first rest)))) ; ( (master (.....) = rest ) )
	  liste))


(defun read-sqd-file (fn)
  (setf *line-counter* 0)
  (reset-sqd-bb)
  (transform
   (let ((objects nil)
	 (line))
     (with-open-file (stream fn :direction :input)
       (setf line (get-line stream))
       (loop 
        (multiple-value-bind (erg line1)
            (read-rec line 1 stream)               
          (push erg objects)
          (setf *erg* erg)                                              
          (if (eq line1 'eof) 
              (return *line-counter*)
            (setf line line1)))))
     (set-client-bb *xmin* *ymin* 
		    *xmax* *ymax*)
     (tree-reverse objects))))

(defun read-rec (line letzte-stufe stream)	   
  (let ((erg nil))            
    (loop   
      
     (loop
      (when (or (eq line 'eof)
                (not (or (ds-trennung line) (blank-line-p line))))
        (return))
      (setf line (get-line stream))) 

     (when (eq line 'eof)         
       (return-from read-rec (values  erg 'eof)))
      
     (if (not (ds-kopf line))
         (break "Error! Out of Sync!")
	
       (let ((stufe *?stu*))
         (setf *line* line)   
         (cond ((and (= stufe 1) (= letzte-stufe 1) (not (null erg)))
                (return-from read-rec (values  erg line)))
               ((= stufe letzte-stufe)                          
                (let* ((name (intern *?etyp* "SQD"))
                       (fn
                        (case name ; db = DATENBLOCK! 
                          ((pg1 pg2) #'db-pg)
                          ((kr1 kr2) #'db-kr)			   
                          ((li1 li2 li3) #'db-li)
                          ((bo1 bo2) #'db-bo)
                          ((sn1 sn2 sn3) #'db-sn)
                          ((sy1 sy2) #'db-sy)
                          ((tx1 tx2) #'db-tx)
                          ((fl1 fl2) #'db-fl)
                          ((tp1 tp2) #'db-tp)
                          ((pa1 pa2) #'db-pa)
                          (otherwise (break "Error! Unknown Block!")))))
                  (let ((obj (funcall fn stream line)))
                    (if (not (eq obj 'error))
                        (progn
                          (push obj erg)                                    
                          (setf line (get-line stream)))
                      (break "Error! Wrong Blockformat!")))))
		
               ((> stufe letzte-stufe)
                (multiple-value-bind (erg1 line1)
                    (read-rec line stufe stream)                           
                  (push  erg1 erg)
                  (setf line line1)
                  (when (eq line1 'eof)
                    (return-from read-rec (values erg 'eof)))))
		
               ((< stufe  letzte-stufe)                                                       
                (return-from read-rec (values erg line)))))))))



