;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)
                   
;;;
;;;
;;;             

(defun qe-p (qa qb &optional (res nil res-supplied-p))
  (if res-supplied-p
      (unless (eq (query-entails-p
                   (prep-query qa *cur-substrate* nil)
                   (prep-query qb *cur-substrate* nil)
                   :enforce-same-arity-p nil)
                  res)
        (error "Wrong answer for ~A |= ~A - should be ~A!" qa qb res))
    (query-entails-p
     (prep-query qa *cur-substrate* nil)
     (prep-query qb *cur-substrate* nil)
     :enforce-same-arity-p nil)))

;;;
;;;
;;; 


(defun run-entailment-tests ()
  (qe-p '(and (top ?x))
        '(and (?b ?a sister-of))
        nil)

  (qe-p '(and (?a woman) (?b man) (?c human))
        '(top ?x)
        t)

  (qe-p '(and (?a woman) (?b man) (?c human))
        '(bottom ?x)
        nil)

  (qe-p '(and (?a woman) (?a man))
        '(bottom ?x)
        t)

  (qe-p '(bottom ?x)
        '(and (?a woman) (?b man) (?c human))
        t)

  (qe-p '(and (?b human) (?a human))
        '(and (top ?x) (top ?y))
        t)

  (qe-p '(and (?a human))
        '(bind-individual betty) 
        nil)

  (qe-p '(and (?a human))
        '(or (bind-individual betty) 
             (not (bind-individual betty)))
        t)

  (qe-p '(and (?x ?y has-sister))
        '(and (?b ?a sister-of))
        t)

  (qe-p '(and (?x ?y has-sister))
        '(and (?a ?b sister-of))
        nil)

  (qe-p '(and (?x father))
        '(and (?a man))
        t)

  (qe-p '(and (?x father))
        '(and (?a man) (?b top))
        t)

  (qe-p '(and (?x father) (?y top))
        '(and (?a man))
        t)

  (qe-p '(and (?x man))
        '(and (?a ?b has-brother))
        nil)

  (qe-p '(and (?a ?b brother-of))
        '(and (?x man))
        t)

  (qe-p '(and (?x father) 
              (?x ?y has-sister))
        '(and (?b ?a sister-of)
              (?a man))
        t)

  (qe-p '(and (?x father) 
              (?x ?y has-sister))      
        '(and (?a ?b sister-of)
              (?b man))
        nil)

  (qe-p '(and (?y father) 
              (?y ?x has-sister))
        '(and (?a ?b sister-of)
              (?b man))
        t)

  (qe-p '(?zz mother)
        '(?x woman)
        t)

  (qe-p '(?x (some has-child top))
        '(?a ?b has-child)
        nil)

  (qe-p '(?a ?b has-child)
        '(?x (some has-child top))
        t)
                        
  (qe-p '(and (?a woman)
              (and (?b woman)
                   (?c woman))
              (?d man))
        '(and (?x woman)
              (?y human)
              (?z human))
        t)

  (qe-p '(and (betty human))
        '(and (betty woman))
        nil)

  (qe-p '(and (betty woman))
        '(and (betty human))
        t)

  ;;;
  ;;;
  ;;;

  (qe-p '(and (betty woman))
        '(and (?x top))
        t)

  (qe-p '(and (?x top))
        '(and (betty woman))
        nil)


  (qe-p '(or (?x c) (not (?x c)))
        '(and (?x top))
        t)

  (qe-p '(and (?x top))
        '(or (?x c) (not (?x c)))
        t)


  (qe-p '(or (?x c) (not (?x c)))
        '(betty woman)
        nil)

  ;;;
  ;;; 
  ;;;

  (qe-p '(betty woman)
        '(?x top)
        t)

  (qe-p '(?x top)
        '(or (?x c) (not (?x c)))
        t)

  (qe-p '(betty woman)
        '(or (?x c) (not (?x c)))
        t)
      
  ;;;
  ;;;
  ;;;

  (qe-p '(betty woman)
        '(or (alice woman) (betty human))
        t)


  ;;;
  ;;; Demo: lokale lexiografisch Ordnung der Vars entscheidet! 
  ;;;

  (qe-p '(or (?x man) (?y woman))
        '(?a man)
        nil)

  (qe-p '(?a man)
        '(or (?x man) (?y woman))
        t)

  (qe-p '(?a man)
        '(or (?y man) (?x woman))
        nil)

  (qe-p '(?b woman)
        '(or (?x man) (?y woman))
        nil)

  (qe-p '(?b woman)
        '(or (?y man) (?x woman))
        t)

  ;;;
  ;;;
  ;;;

  (qe-p '(or (?x c) (?x (not c)))
        '(?x top)
        t)

  (qe-p '(?x top)
        '(or (?x c) (?x (not c)))
        t)
      
  ;;;
  ;;;
  ;;;

  (qe-p '(or (?x (not c)) (?x c))
        '(top ?x)
        t)

  (qe-p '(top ?x)
        '(or (?x c) (?x (not c)))
        t)

  ;;;
  ;;;
  ;;;


  (qe-p '(not (betty woman))
        '(top ?x)
        t)

  (qe-p '(top ?x)
        '(or (?x c) (?x (not c)))
        t)

  (qe-p '(not (betty woman))
        '(or (?x c) (?x (not c)))
        t)

  ;;;
  ;;;
  ;;;

  (qe-p '(not (bind-individual betty))
        '(or (?x c) (?x (not c)))
        t)

  (qe-p '(not (bind-individual betty))
        '(top ?x)
        t)

  ;;;
  ;;;
  ;;;

  (qe-p '(bind-individual betty)
        '(or (?x c) (?x (not c)))
        t)

  (qe-p '(not (?y d))
        '(or (?x c) (?x (not c)))
        t)

  ;;;
  ;;;
  ;;;

  (qe-p '(not (bind-individual betty))
        '(or (?x c) (?x (not c)))
        t)

  (qe-p '(not (bind-individual betty))
        '(top ?x)
        t)

  ;;;
  ;;;
  ;;;

  (qe-p '(bind-individual betty)
        '(or (?x c) (?x (not c)))
        t)

  (qe-p '(not (?y d))
        '(or (?x c) (?x (not c)))
        t)

  ;;;
  ;;;
  ;;;

  (qe-p '(bind-individual betty)
        '(?x top)
        t)

  (qe-p '(bind-individual betty)
        '(top ?x)
        t)

  ;;;
  ;;;
  ;;;

  (qe-p '(not (bind-individual betty))
        '(top ?x)
        t)


  (qe-p '(not (bind-individual betty))
        '(?x top)
        t)


  ;;;
  ;;;
  ;;; 

  (qe-p '(not (bind-individual betty))
        '(betty woman)
        nil)

  (qe-p '(not (betty woman))
        '(?x top)
        t)

  (qe-p '(not (betty woman))
        '(top ?x)
        t))



;;;
;;; Unverstandene Faelle: 
;;;

#|

(princ (qe-p '(not (?x c)) '(?x (not c))))

(princ (qe-p '(?x (not c)) '(not (?x c))))

|#

;;; 
;;; aber das wuerde heissen, dass die Aequivalent sind... falsch
;;; 

