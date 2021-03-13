;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GEOMETRY; Base: 10 -*-

(in-package geometry)

;;;
;;; Achtung - das sind Axproximationen! 
;;; F�r konvexe Polygone lassen sich einfacherere und
;;; korrekte Algorithmen angeben, die ohne die wichtigsten
;;; Funktionen "one-part-is-inside/outside-p*" auskommen
;;; (z.B. kann touches-p LINE x POLY gucken, ob alle Punkte
;;; des Polygons in nur einer der durch LINE geschaffenen
;;; Halbebene (Seite) liegen, etc.) Dann m�sste man aber 
;;; vorher auf Konvexit�t pr�fen (ist das teuer?)
;;;


;;;
;;; Achtung: ein Objekt ber�hrt sich eventuell selbst (reflexiv)!
;;; LINE X POLYGON, CHAIN x POLYGON, POLYGON x POLYGON
;;; (auch LINE X LINE im "intersects.lisp"-Modul)
;;;

(defmethod touches-p ((obj1 geom-thing) (obj2 geom-thing))
  ;;;(error "TOUCHES-P ~A ~A: Not well-defined!" obj1 obj2))
  )
  
(defmethod touches-p ((line geom-line) (poly geom-polygon))
  (and (intersects-p line poly)
       (or (lies-on-p line poly)
           (not (one-part-is-inside-p line poly)))))

(defmethod touches-p ((poly geom-polygon) (line geom-line))
  (touches-p line poly))

#|
(defmethod touches-p ((chain1 geom-chain) (chain2 geom-chain))
  ;;; (error "TOUCHES-P ~A ~A: Not well-defined!" chain1 chain2))
  )
|#

(defmethod touches-p ((chain1 geom-chain) (chain2 geom-chain))
  (and (intersects-p chain1 chain2)
       (every #'(lambda (segment)
                  (=> (intersects-p segment chain2)
                      (touches-p segment chain2)))
              (segments chain1))))

(defmethod touches-p ((obj geom-chain) (poly geom-polygon))
  (and (intersects-p obj poly)
       (every #'(lambda (segment)
                  (=> (intersects-p segment poly)
                      (touches-p segment poly)))
              (segments obj))))

(defmethod touches-p ((poly geom-polygon) (obj geom-chain))
  (touches-p obj poly))

(defmethod touches-p ((poly1 geom-polygon) (poly2 geom-polygon))
  (and (not (eq poly1 poly2))
       (intersects-p poly1 poly2)
       (every #'(lambda (segment)
                  (=> (intersects-p segment poly2)
                      (touches-p segment poly2)))
              (segments poly1))))

;;;
;;; Achtung: ein Objekt "bedeckt" sich stets selbst (reflexiv)!
;;; Covered-by: LINE X POLYGON, CHAIN x POLYGON, POLYGON x POLYGON
;;; Covers: Inverse dazu
;;;

(defmethod covered-by-p ((chain1 geom-chain) (chain2 geom-chain))
  ;;; (error "COVERED-BY-P ~A ~A: Not well-defined!" chain1 chain2))
  )

(defmethod covered-by-p ((obj1 geom-thing) (obj2 geom-thing))
  ;;; (error "COVERED-BY-P ~A ~A: Not well-defined!" obj1 obj2))
  )

(defmethod covers-p ((chain1 geom-chain) (chain2 geom-chain))
  ;;; (error "COVERS-P ~A ~A: Not well-defined!" chain1 chain2))
  )

(defmethod covers-p ((obj1 geom-thing) (obj2 geom-thing))
  ;;; (error "COVERS-P ~A ~A: Not well-defined!" obj1 obj2))
  )

(defmethod covered-by-p ((line geom-line) (poly geom-polygon))
  (and (intersects-p line poly)
       (or (lies-on-p line poly)
           (not (one-part-is-outside-p line poly)))))

(defmethod covers-p ((poly geom-polygon) (line geom-line))
  (covered-by-p line poly))

(defmethod covered-by-p ((chain-or-poly geom-chain-or-polygon) (poly geom-polygon))
  (and (not (eq chain-or-poly poly))
       (intersects-p chain-or-poly poly)
       (every #'(lambda (segment)
                  (=> (intersects-p segment poly)
                      (covered-by-p segment poly)))
              (segments chain-or-poly))))

(defmethod covers-p ((poly geom-polygon) (chain-or-poly geom-chain-or-polygon))
  (covered-by-p chain-or-poly poly))

;;;
;;; CROSSES / CROSSED-BY (OD- bzw. 1D-Schnitte)
;;; LINE X LINE, LINE X POLYGON, CHAIN X POLYGON
;;; (auch LINE X LINE im "intersects.lisp"-Modul)
;;; 

(defmethod crosses-p ((obj1 geom-thing) (obj2 geom-thing))
  ;;; (error "CROSSES-P ~A ~A: Not well-defined!" obj1 obj2))
  )

(defmethod crossed-by-p ((obj1 geom-thing) (obj2 geom-thing))
  ;;; (error "CROSSED-BY-P ~A ~A: Not well-defined!" obj1 obj2))
  )

(defmethod crosses-p ((line geom-line) (chain geom-chain))
  (some #'(lambda (segment) 
            (crosses-p line segment))
        (segments chain)))

(defmethod crossed-by-p ((chain geom-chain) (line geom-line))
  (crosses-p line chain))

(defmethod crosses-p ((line geom-line) (poly geom-polygon))
  (and (intersects-p line poly)
       (one-part-is-inside-p line poly)))

(defmethod crossed-by-p ((poly geom-polygon) (line geom-line))
  (crosses-p line poly))

(defmethod crosses-p ((chain geom-chain) (poly geom-polygon))
  (some #'(lambda (segment) 
            (crosses-p segment poly))
        (segments chain)))

(defmethod crossed-by-p ((poly geom-polygon) (chain geom-chain))
  (crosses-p chain poly))

;;;
;;; Achtung: ein Objekt �berlappt sich nicht selbst! 
;;; POLY X POLY
;;; 

(defmethod overlaps-p ((obj1 geom-thing) (obj2 geom-thing))
  ;;; (error "OVERLAPS-P ~A ~A: Not well-defined!" obj1 obj2))
  )

(defmethod overlaps-p ((poly1 geom-polygon) (poly2 geom-polygon))
  (and (not (eq poly1 poly2))
       (intersects-p poly1 poly2)       
       (some #'(lambda (segment)
                 (one-part-is-inside-p segment poly2))
             (segments poly1))))

;;;
;;; Kongruenz
;;; TOTAL
;;;

(defmethod congruent-p ((obj1 geom-thing) (obj2 geom-thing))
  nil)

(defmethod congruent-p ((p1 geom-point) (p2 geom-point))
  (point-=-p p1 p2))

(defmethod congruent-p ((l1 geom-line) (l2 geom-line))
  (line-=-p l1 l2))

(defmethod congruent-p ((obj1 geom-chain-or-polygon) (obj2 geom-chain-or-polygon))
  (or (equal-p obj1 obj2)
      (and (every #'(lambda (segment) 
                      (lies-on-p segment obj2))
                  (segments obj1))
           (every #'(lambda (segment) 
                      (lies-on-p segment obj1))
                  (segments obj2)))))
