;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SQD; Base: 10 -*-

(in-package sqd)

#|
( { optional } { "STRING" } {   (   variable?   {   ( test-fn )   } )  \/  ( ( test-fn ) ) } ) +

Faelle:
(o s (v? (test)))
(o (v? (test)))
(s (v? (test)))
(s (v?))
(s)
((v?))

|#

(defvar flag)
(defvar *?st*)
(defvar *?etyp*)
(defvar *?stu*)
(defvar *?enum*)
(defvar *?wert*)
(defvar *?h*)
(defvar *?m*)
(defvar *?nam*)
(defvar *?os*)
(defvar *?os2* nil)
(defvar *?pkz*)
(defvar *?pnr*)
(defvar *?p*)
(defvar *?r*)
(defvar *?txt*)
(defvar *?w*)
(defvar *?x*)
(defvar *?y*)

;;;
;;;
;;;

(datensatz ds-trennung
	   ("*"))


(let (?st)
  (datensatz ds-kopf
	     ("ETYP=" (?etyp (progn  
			       (setf flag nil)                                
			       (member ?etyp '("LI1" "LI2" "LI3" "PG1" "PG2" "SN1" "SN2" "SN3"
					             "BO1" "BO2"
					             "TX1" "TX2" "FL1" "FL2" "SY1" "SY2" "PA1" "PA2" "TP1" "TP2"
					             "KR1" "KR2")
				       :test #'string=))))
	     ("STU=" (?stu (progn
			     (setf ?stu (read-from-string ?stu))
			     (integerp ?stu))))
	     ( "ENUM="  (?enum ) )
	     ("EB=" (?eb (progn
			   (setf ?eb (read-from-string ?eb))
			   (integerp ?eb))))
	     (optional "ST=" (?st (progn
				    (setf flag t)
				    (setf ?st (read-from-string ?st))
				    (integerp ?st))))
	     ((  (progn
		   (when flag 
		     (setf *?st* ?st))
		   (setf *?etyp* ?etyp
			 *?stu* ?stu
			 *?enum* ?enum))))))

(datensatz ds-a
	   ("A"))

(datensatz ds-f
	   ("F"))

(datensatz ds-fld-ohne-werte
	   ("FLD"))

(datensatz ds-fld
	   ("FLD")
	   ((?wert (setf ?wert (read-hex ?wert))))
	   (( (setf *?wert* ?wert))))

(datensatz ds-fre
	   ("FRE"))


(datensatz ds-h
	   ("H")
	   ((?h (setf ?h (read-hex ?h))))
	   (((setf *?h* ?h))))

(datensatz ds-len
	   ("LEN"))

(datensatz ds-m
	   ("M")
	   ((?m (setf ?m (read-hex ?m))))
	   (((setf *?m* ?m))))


(datensatz ds-nam
	   ("NAM")
	   ((?nam))
	   (((setf *?nam* ?nam))))

(let (?os2)
  (datensatz ds-os
	     ("OS" ((setf flag nil)))
	     ((?os (progn
		     (setf ?os (read-from-string ?os))
		     (integerp ?os))))
	     (optional (?os2
			(progn
			  (setf flag t)
			  (setf ?os2 (read-from-string ?os2))
			  (integerp ?os2))))
	     (((progn
		 (when flag
		   (setf *?os2* ?os2))
		 (setf *?os* ?os))))))

(datensatz ds-pkz
	   ("PKZ")  
	   ((?pkz))
	   (((setf *?pkz* ?pkz))))

(datensatz ds-pnr
	   ("PNR")
	   ((?pnr))
	   (((setf *?pnr* ?pnr))))

(datensatz ds-r
	   ("R")
	   ((?r (setf ?r (read-hex ?r))))
	   (((setf *?r* ?r))))

(datensatz ds-sar
	   ("SAR"))

(datensatz ds-swi
	   ("SWI"))

(datensatz ds-txt
	   ("TXT")
	   ((?txt))
	   (((setf *?txt* ?txt))))

(datensatz ds-w
	   ("W")
	   ((?w (setf ?w (read-hex ?w))))
	   (((setf *?w* ?w))))

(datensatz ds-wan
	   ("WAN"))

(datensatz ds-wen
	   ("WEN"))

(datensatz ds-x
	   ("X")	   
	   ((?x (setf ?x (read-hex ?x))))
	   (((setf *?x* ?x))))


(datensatz ds-xma
	   ("XMA"))

(datensatz ds-xmi
	   ("XMI"))

(datensatz ds-y
	   ("Y")	   
	   ((?y (setf ?y (read-hex ?y))))
	   (((setf *?y* ?y))))

(datensatz ds-yma
	   ("YMA"))

(datensatz ds-ymi
	   ("YMI"))

(datensatz ds-fld-folge	   
	   ((?wert (setf ?wert (read-hex ?wert))))
	   (((setf *?wert* ?wert))))


(datensatz ds-s
	   ("S")
	   ("0"))

(datensatz ds-x1
	   ("X1"))

(datensatz ds-y1
	   ("Y1"))

(datensatz ds-x2
	   ("X2"))

(datensatz ds-y2
	   ("Y2"))


(datensatz ds-zsp
	   ("ZSP"))

(datensatz ds-clp
	   ("CLP"))

(datensatz ds-fla
	   ("FLA"))

(datensatz ds-sa
	   ("SA"))

