;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SPATIAL-SUBSTRATE; Base: 10 -*-

(in-package spatial-substrate)

;;;
;;;
;;;

(defconstant +red1+ (make-rgb-color 0.5 0 0))
(defconstant +red2+ (make-rgb-color 0.7 0 0))
(defconstant +red3+ (make-rgb-color 1 0 0))

(defconstant +green1+ (make-rgb-color 0 0.5 0))
(defconstant +green2+ (make-rgb-color 0 0.7 0))
(defconstant +green3+ (make-rgb-color 0 0.85 0 ))
(defconstant +green4+ (make-rgb-color 0 1 0 ))


(defconstant +yellow1+ (make-rgb-color 0.5 0.5 0))
(defconstant +yellow2+ (make-rgb-color 0.7 0.7 0))
(defconstant +yellow3+ (make-rgb-color 0.8 0.8 0))
(defconstant +yellow4+ (make-rgb-color 0.9 0.9 0))
(defconstant +yellow5+ (make-rgb-color 1.0 1.0 0))



(defconstant +blue1+ (make-rgb-color 0 0 0.5))
(defconstant +blue2+ (make-rgb-color 0 0 0.7))
(defconstant +blue3+ (make-rgb-color 0 0 1))

(defconstant +gray1+ (make-rgb-color 0.4 0.4 0.4))
(defconstant +gray2+ (make-rgb-color 0.6 0.6 0.6))
(defconstant +gray3+ (make-rgb-color 0.8 0.8 0.8))
(defconstant +gray4+ (make-rgb-color 0.9 0.9 0.9))
(defconstant +gray5+ (make-rgb-color 0.3 0.3 0.3))

(defconstant +brown1+ (make-rgb-color 0.8 0.5 0.5))
(defconstant +brown2+ (make-rgb-color 0.9 0.7 0.7))
(defconstant +brown3+ (make-rgb-color 0.6 0.5 0.4))

;;;
;;;
;;;


(defparameter *no-color-for* nil)

(defmethod get-mapcolor-for ((os null) (type symbol))
  +black+)

(defmethod get-mapcolor-for ((os symbol) (type symbol))
  (unless (member os *no-color-for*)
    (format t "Warning! No map color defined for ~A!~%" os)
    (push os *no-color-for*) ) 
  +black+)


(defmethod get-mapcolor-for ((os number) (type symbol))
  (get-mapcolor-for (second (lookup-os os)) type))

(defmethod get-mapcolor-for ((os (eql :NADELBAUM)) (type symbol))
  +green1+)

(defmethod get-mapcolor-for ((os (eql :SEE-NICHT-SCHIFFBAR)) (type symbol))
  +blue1+)

(defmethod get-mapcolor-for ((os (eql :OBSTANBAU)) (type symbol))
  +green2+)

(defmethod get-mapcolor-for ((os (eql :FRIEDHOF-FUER-NICHTCHRISTEN)) (type symbol))
  +yellow1+)

(defmethod get-mapcolor-for ((os (eql :S-BAHN-STATION)) (type symbol))
  +red1+)

(defmethod get-mapcolor-for ((os (eql :TOPOGRAPHISCHE-GRENZE)) (type symbol))
  +brown1+)

(defmethod get-mapcolor-for ((os (eql :SPORTPLATZBEGRENZUNG)) (type symbol))
  +brown2+)

(defmethod get-mapcolor-for ((os (eql :PLATZ-Z-B-MARKTPLATZ-RASTPLATZ-FESTPLATZ)) (type symbol))
  +gray1+)

(defmethod get-mapcolor-for ((os (eql :FRIEDHOF)) (type symbol))
  +brown2+)

(defmethod get-mapcolor-for ((os (eql :ACKER)) (type symbol))
  +brown3+)

(defmethod get-mapcolor-for ((os (eql :NATURSCHUTZGEBIETSGRENZE)) (type symbol))
  +black+)

(defmethod get-mapcolor-for ((os (eql :BAB)) (type symbol))
  +gray1+) 

(defmethod get-mapcolor-for ((os (eql :BAB-MITTELLINIE)) (type symbol))
  +gray2+)

(defmethod get-mapcolor-for ((os (eql :KILOMETRIERUNG-BAB-BAB-AEHNLICH)) (type symbol))
  +black+)

(defmethod get-mapcolor-for ((os (eql :BAHNBETRIEBSGELAENDE)) (type symbol))
  +gray2+)

(defmethod get-mapcolor-for ((os (eql :P+R-SYMBOL)) (type symbol))
  +yellow+)

(defmethod get-mapcolor-for ((os (eql :U-BAHN-STATION)) (type symbol))
  +red2+)

(defmethod get-mapcolor-for ((os (eql :U-BAHN-UNTERIRDISCH)) (type symbol))
  +yellow2+)

(defmethod get-mapcolor-for ((os (eql :U-BAHN-OBERIRDISCH)) (type symbol))
  +yellow2+)

(defmethod get-mapcolor-for ((os (eql :FUSSGAENGERZONE)) (type symbol))
  +gray3+)

(defmethod get-mapcolor-for ((os (eql :LINIENH-BACH-NICHT-SCHIFFBAR)) (type symbol))
  +blue1+)

(defmethod get-mapcolor-for ((os (eql :ABRENZUNG-DER-BAB-USW-GEG-AUSFAHRT-UNSICHTBAR)) (type symbol))
  +gray3+)

(defmethod get-mapcolor-for ((os (eql :NUTZUNGSARTENGRENZE-UNSICHTBAR)) (type symbol))
  +black+)

(defmethod get-mapcolor-for ((os (eql :PARK-MIT-EINZELSYMBOLEN)) (type symbol))
  +green1+)

(defmethod get-mapcolor-for ((os (eql :LAUBHOLZ)) (type symbol))
  +brown3+)

(defmethod get-mapcolor-for ((os (eql :VERKEHRSBEGLEITGRUEN)) (type symbol))
  +green2+)

(defmethod get-mapcolor-for ((os (eql :BUNDESSTRASSE)) (type symbol))
  +gray3+)

(defmethod get-mapcolor-for ((os (eql :AUFFAHRT-AUF-BAB-BAB-AEHNLICH-B)) (type symbol))
  +gray1+)

(defmethod get-mapcolor-for ((os (eql :ABGRENZUNG-FLAECHENHAFTER-GEWAESSER-UNSICHTBAR)) (type symbol))
  +blue2+)

(defmethod get-mapcolor-for ((os (eql :FLIESSRICHTUNGSPFEIL)) (type symbol))
  +yellow1+)

(defmethod get-mapcolor-for ((os (eql :WEG-HINTEGRUND-IN-FLAECHENFARBE)) (type symbol))
  +gray4+)

(defmethod get-mapcolor-for ((os (eql :PARKPLATZ)) (type symbol))
  +gray2+)

(defmethod get-mapcolor-for ((os (eql  :SPORTPLATZINNENFLAECHE)) (type symbol))
  +gray5+)

(defmethod get-mapcolor-for ((os (eql :WOHNEN)) (type symbol))
  +gray2+)

(defmethod get-mapcolor-for ((os (eql :TEICH-NICHT-SCHIFFBAR)) (type symbol))
  +blue2+)

(defmethod get-mapcolor-for ((os (eql :LAUBWALD)) (type symbol))
  +green3+)

(defmethod get-mapcolor-for ((os (eql :FLUSS-SCHIFFBAR)) (type symbol))
  +blue2+)

(defmethod get-mapcolor-for ((os (eql :BAB-RANDBEGRENZUNG)) (type symbol))
  +gray1+)

(defmethod get-mapcolor-for ((os (eql :MOOR-SUMPF)) (type symbol))
  +brown3+)

(defmethod get-mapcolor-for ((os (eql :LINIENH-KANAL-GRABEN-NICHT-SCHIFFBAR)) (type symbol))
  +blue3+)

(defmethod get-mapcolor-for ((os (eql :FLAECHENH-GEWAESSERBEGR-NICHT-SCHIFFBAR)) (type symbol))
  +blue3+)

(defmethod get-mapcolor-for ((os (eql :GEHOELZ)) (type symbol))
  +brown1+)

(defmethod get-mapcolor-for ((os (eql :LAUBBAUM)) (type symbol))
  +yellow1+)

(defmethod get-mapcolor-for ((os (eql :BRUECKE)) (type symbol))
  +gray3+)

(defmethod get-mapcolor-for ((os (eql :WIESE-WEIDE)) (type (eql :AREA)))
  +green1+)

(defmethod get-mapcolor-for ((os (eql :WIESE-WEIDE)) (type (eql :SYMBOL)))
  +green3+)

(defmethod get-mapcolor-for ((os (eql :GK-GITTERLINIE-2-KM)) (type symbol))
  +black+)

(defmethod get-mapcolor-for ((os (eql :KANAL-SCHIFFBAR)) (type symbol))
  +blue3+)

(defmethod get-mapcolor-for ((os (eql :INDUSTRIE-GEWERBE)) (type symbol))
  +red1+)

(defmethod get-mapcolor-for ((os (eql :NEBENBAHN-HAFENB-RANGIERANL-USW-OBERIRDISCH)) (type symbol))
  +gray2+)

(defmethod get-mapcolor-for ((os (eql :SONSTIGE-STRASSE)) (type symbol))
  +black+)

(defmethod get-mapcolor-for ((os (eql :S-BAHN-OBERIRDISCH)) (type symbol))
  +red3+)

(defmethod get-mapcolor-for ((os (eql :ZUSAETZLICHE-TOPOGRAPHIE-KEINE-NUTZUNGSGRENZE)) (type symbol))
  +black+)

(defmethod get-mapcolor-for ((os (eql :KLEINGARTEN)) (type symbol))
  +yellow2+)

(defmethod get-mapcolor-for ((os (eql :BRACHFLAECHE)) (type symbol))
  +white+)

(defmethod get-mapcolor-for ((os (eql :FLAECHENH-GEWAESSERBEGR-SCHIFFBAR)) (type symbol))
  +green2+)

(defmethod get-mapcolor-for ((os (eql :NUTZUNGSARTENGRENZE)) (type symbol))
  +black+)

(defmethod get-mapcolor-for ((os (eql :HAUPTVERKEHRSSTRASSE)) (type symbol))
  +black+)

(defmethod get-mapcolor-for ((os (eql :GK-GITTERLINIE-1-KM)) (type symbol))
  +yellow3+)

(defmethod get-mapcolor-for ((os (eql :GITTERPUNKTE-FUER-DAS-EINPASSEN-VON-VORLAGEN)) (type symbol))
  +yellow4+)

(defmethod get-mapcolor-for ((os (eql :BLATTSCHNITT-BEARBEITUNGSGRENZE-FUER-SCHRIFT)) (type symbol))
  +black+)

(defmethod get-mapcolor-for ((os (eql :POSTAMT)) (type symbol))
  +red2+)

(defmethod get-mapcolor-for ((os (eql :KIRCHE)) (type symbol))
  +red3+)

(defmethod get-mapcolor-for ((os (eql :KAPELLE)) (type symbol))
  +red3+)

(defmethod get-mapcolor-for ((os (eql :OEFFENTLICHES-GEBAEUDE)) (type symbol))
  +yellow3+)

(defmethod get-mapcolor-for ((os (eql :KONTUR-FUER-OEFFENTLICHES-GEBAEUDE)) (type symbol))
  +yellow3+)

