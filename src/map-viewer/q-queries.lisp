;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SPATIAL-SUBSTRATE; Base: 10 -*-

(in-package spatial-substrate)

(defun q1 ()
  (let ((q '(?*x wohngebiet))
        (v '(?*x)))
    (inspector-answer-query q v
                            "Zeige mir alle Wohngebiete.")))

(defun q2 ()
  (let ((q '(?*x park))
        (v '(?*x)))
    (inspector-answer-query q v
                            "Zeige mir alle Parks.")))

(defun q3 ()
  (let ((q '(and (?*x wohngebiet) (?*y gruenflaeche) (?*x ?*y ec)))
        (v '(?*x ?*y)))
    (inspector-answer-query q v "Zeige mir alle Wohngebiete, 
die an einer Grünfläche liegen.")))

(defun q4 ()
  (let ((q '(and (?*x park) (?*x ?*y ppi) (?*y teich)))
        (v '(?*x ?*y)))
    (inspector-answer-query q v "Zeige mir alle Parks, 
die einen Teich enthalten.")))
  
(defun q5 ()
  (let ((q '(and (?*x wohngebiet) (?*x ?*y ec) (?*y gruenflaeche) (?*y ?*z ppi) (?*z (:or teich see))))
        (v '(?*x ?*y ?*z)))
    (inspector-answer-query q v "Zeige mir alle Wohngebiete, 
die an einer Grünfläche liegen, 
welche einen Teich oder See enthält.")))

(defun q6 ()
  (let ((q '(and (?*x wohngebiet) 
                 (?*x ?*y ec) 
                 (?*y gruenflaeche) 
                 (?*y ?*z ppi) 
                 (?*z (:or teich see))
                 (?*x ?*u ec) 
                 (?*u gewerbe)))
        (v '(?*x ?*y ?*z ?*u)))
    (inspector-answer-query q v
                            "Zeige mir alle Wohngebiete, 
die an einer Gruenfläche liegen, 
welche einen Teich oder See enthält; 
das Wohngebiet soll zudem an einem 
Gewerbegebiet liegen (zum Einkaufen).")))


(defun q7 ()
  (let ((q '(and (?*x (:or teich see)) 
                 (?*x ?*y ec)
                 (?*y (:or fluss bach))
                 (?*y ?*z (:or ec po)) 
                 (?*z wohngebiet) 
                 (?*x ?*w pp)
                 (?*w park)))

        (v '(?*x ?*y ?*z ?*w)))
    (inspector-answer-query q v
                            "Gibt es einen Fluss oder Bach, 
der ein Wohngebiet kreutzt, 
und dann in einen Teich oder See fließt, 
der in einem Park liegt?")))

(defun q8 ()
  (let ((q '(and (?*x wohngebiet)
                 (?*x ?*y (:or ec po))
                 (?*y autobahn)))
        (v '(?*x ?*y)))
    (inspector-answer-query q v
                            "Zeige mir alle Wohngebiete, 
die an einer Autobahn liegen.")))

(defun q9 ()
  (let ((q '(and (?*x park)
                 (?*x ?*y (:or ec po))
                 (?*y autobahn)
                 (?*x ?*z ppi)
                 (?*z (and wasser flaeche))))
        (v '(?*x ?*y)))
    (inspector-answer-query q v
                            "Zeige mir alle Parks, 
die an der Autobahn liegen, 
und die eine Wasserfläche enthalten.")))

(defun q10 ()
  (let ((q '(and (?*x park)
                 (?*x ?*y ppi)
                 (?*y laubbaum)))
        (v '(?*x ?*y)))
    (inspector-answer-query q v
                            "Zeige mir alle Parks mit Laubbäumen")))

(defun q11 ()
  (let ((q '(and (?*x wald)))
        (v '(?*x)))
    (inspector-answer-query q v
                            "Zeige mir alle Wälder.")))
(defun q12 ()
  (let ((q
         `(?*x (and wohnen
                    (some ec
                          (and wiese-weide
                               (:or (some tppi teich-nicht-schiffbar)
                                   (some ntppi teich-nicht-schiffbar))))
                    (:or (some tppi oeffentliches-gebaeude)

                        (some ntppi oeffentliches-gebaeude))
                    (all ec (:or wiese-weide
                                parkplatz)))))
        (v '(?*x)))
    (inspector-answer-query q v
                            "Zeige mir Wohngebiete,
die an eine Wiese/Weide angrenzen, 
die einen Teich enthält,
sodass alle angrenzenden Flächen 
ebenfalls Wiesen/Weiden oder
Parkplätze sind.")))

(defun q13 ()
  (let ((q '(and (?*wohngebiet wohngebiet)
                 (?*u-bahn-station u-bahn-station)
                 (?u-bahn-station ?wohngebiet (:inside-epsilon 100))
                 (?*wohngebiet ?*teich :contains)
                 (?*teich (:or :teich :see))))
        (v '(?*wohngebiet ?*u-bahn-station ?*teich)))
    (inspector-answer-query q v 
                            "Zeige mir Wohngebiete,
die Teiche oder Seen enthalten, 
und in deren Epsilon-Umgebung von 
100 Metern eine U-Bahn-Station liegt.")))

(defun q14 ()
  (let ((q '(and 
             ;; note: Q operator not implemented in MIDELORA: 
             ;; (?*x (and wohngebiet (at-least 8 ntppi gebaeude)))
             ;; hence: 
             (?*x (and wohngebiet (at-least 8 ntppi top)))
             (?x (:satisfies 
                  (and (geometry::is-geom-polygon-p object)
                       (> (geometry:calculate-area object) 50000))))))
        (v '(?*x)))
    (inspector-answer-query q v
                            "Zeige mir Wohngebiete, 
die mindestens 8 Gebäude enthalten, 
und deren Fläche größer als 50000 ist.")))

(defun q15 ()
  (let ((q '(and (?*x (and wohngebiet (at-least 8 ntppi top)))
                 (?x (:area (< area 50000)))))
        (v '(?*x)))
    (inspector-answer-query q v
                            "Welche Wohngebiete mit mindestens 8 Gebäuden
haben eine Fläche, die kleiner als 5000 ist?")))

(defun q16 ()
  (let ((q '(and (?*x :bach)  
                 (?*x ?*y :touches) 
                 (?*y :teich)
                 (?*x ?*z :crosses)
                 (?*z :park)
                 (?*z ?*y :contains) 
                 (?*x ?*w :touches) 
                 (?*w :wohngebiet)))
        (v '(?*x ?*y ?*z ?*w)))

    (inspector-answer-query q v
                            "Welcher Bach fließt von einem Wohngebiet 
in einen Teich, der in einem Park liegt?")))

(defun q17 ()
  (let ((q '(and (?*wohngebiet wohngebiet)
                 (?*wohngebiet (ALL EC (not (:or industrie gewerbe))))
                 (?*wohngebiet ?*gebaeude :contains)
                 (?*gebaeude gebaeude)
                 (?kirche ?wohngebiet (:inside-epsilon 200))
                 (?*u-bahn-station u-bahn-station)
                 (?u-bahn-station ?wohngebiet (:inside-epsilon 100))
                 (?*wohngebiet ?*teich :contains)
                 (?*teich (:or :teich :see))
                 (?*kirche kirche)))
        (v
         '(?*wohngebiet 
           ?*gebaeude
           ?*kirche 
           ?*u-bahn-station
           ?*teich)))
    (inspector-answer-query q v
                            "Zeige mir Wohngebiete,
die nicht an Industrie- oder Gewerbegebiete angrenzen,
die Gebaeude enthalten, und 
die eine Kirche in der Epsilon-Umgebung von 200 Metern, 
eine U-Bahn-Station im Epsilon-Umkreis von 100 Metern, 
sowie einen Teich (oder See) enthalten.")))

(defun q18 ()
  (let ((q '(?*x wohngebiet))
        (v '(?*x)))
    (inspector-answer-query q v
                            "Zeige mir alle Wohngebiete.")))


(defun q19 ()
  (let ((q '(and (?*wohngebiet wohngebiet)
                 (?u-bahn-station ?wohngebiet (:inside-epsilon 100))))
        (v '(?*wohngebiet ?u-bahn-station)))
    (inspector-answer-query q v 
                            "Zeige mir Wohngebiete,
in deren Epsilon-Umgebung von 
100 Metern eine U-Bahn-Station liegt.")))


(defun q20 ()
  )
