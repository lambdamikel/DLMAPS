
;*****************************************************************************
;*                                                                           *
;*  conc(n)         ::= (and {conc_cl(n)}*)            [*and_br*]            *
;*  conc_cl(n)      ::= (or {conc_lit(n)}*)            [*and_br*]            *
;*  conc_lit(n)     ::= conc_var(n) |          \       [*neg_prob*]          *
;*                      (not conc_var(n))      /                             *
;*  conc_var(0)     ::= Ci                                                   *
;*  conc_var(n+1)   ::= Ci |                   \  [*prim_conc_prob*]         *
;*                      (ALL Rj conc_cl(n))    /                             *
;*                                                                           *
;*****************************************************************************


;*****************************************************************************
;*   returns conc(n) ::= (AND <conc_cl(n)> ... <conc_cl(n)>)                 *
;*****************************************************************************
(defun rand-conc-mak ()
 (if *sort-clauses*
  (con-sort (c-mkand (rand-listof_conc_cl-mak *and_br* *mod_degree*)))
  (c-mkand (rand-listof_conc_cl-mak *and_br* *mod_degree*))
))


;*****************************************************************************
;*   returns (<conc_cl(n)> ... <conc_cl(n)>)                                 *
;*****************************************************************************
(defun rand-listof_conc_cl-mak (and_br mod_degree)
 (if (zerop and_br) 
  nil
  (cons (rand-conc_cl-mak mod_degree) 
        (rand-listof_conc_cl-mak (1- and_br)  mod_degree))))


;*****************************************************************************
;*  returns conc_cl(n) ::= (OR <conc_lit(n)> ... <conc_lit(n)>)              *
;*****************************************************************************
(defun rand-conc_cl-mak (mod_degree)
 (c-mkor (rand-listof_conc_lit-mak *or_br* mod_degree)))


;*****************************************************************************
;*  returns (<conc_lit(n)> ... <conc_lit(n)>)                                *
;*****************************************************************************
(defun rand-listof_conc_lit-mak (or_br mod_degree)
 (if (zerop or_br) 
  nil
  (cons (rand-conc_lit-mak mod_degree) 
        (rand-listof_conc_lit-mak (1- or_br) mod_degree))))


;*****************************************************************************
;*  returns conc_lit(n) ::= <conc_var(n)> |                                  *
;*                          (not <conc_var(n)>)                              *
;*****************************************************************************
(defun rand-conc_lit-mak (mod_degree)
 (if (rand-event *neg_prob*)
  (c-mknot (rand-conc_var-mak mod_degree))
  (rand-conc_var-mak mod_degree)))


;*****************************************************************************
;*  conc_var(0)     ::= Ci                                                   *
;*  conc_var(n+1)   ::= Ci |                   \  [*prim_conc_prob*]         *
;*                      (ALL Rj conc_cl(n))    /                             *
;*****************************************************************************
(defun rand-conc_var-mak (mod_degree) 
 (if (zerop mod_degree)
  (rand-conc_atom-mak)
  (if (rand-event *prim_conc_prob*)
   (rand-conc_atom-mak)
   (rand-conc_ruled_var-mak mod_degree))))


;*****************************************************************************
;*  conc_atom ::= Ci                                                         *
;*****************************************************************************
(defun rand-conc_atom-mak () 
 (c-mkatom (random *var_num*)))


;*****************************************************************************
;*   conc_ruled_var ::= (ALL Rj >conc_cl(n)>)                                *
;*****************************************************************************
(defun rand-conc_ruled_var-mak (mod_degree)
 (c-mkall (rand-conc_rule-mak) (rand-conc_cl-mak (1- mod_degree))))


;*****************************************************************************
;*  conc_rule ::= Ri                                                         *
;*****************************************************************************
(defun rand-conc_rule-mak () 
 (if (equal *logic* "MK")
  (c-mkrule (1+ (random *rule_num*)))
  (c-mkrule 0)))




;******************* costructors ****************************************

;(defun c-isvar (c) (atom c))
;(defun c-isneg (c) (and (not (c-isvar c)) (equal (car c) 'not)))
;(defun c-isall (c) (and (not (c-isvar c)) (equal (car c) 'all))) 
;(defun c-issome (c) (and (not (c-isvar c)) (equal (car c) 'some))) 

;(defun c-mknot (c) (list 'not c))
;(defun c-mkand (c_list) (cons 'and c_list))
;(defun c-mkor  (c_list) (cons 'or  c_list))
;(defun c-mksome (r c) (list 'some r c))
;(defun c-mkall  (r c) (list 'all  r c))

;(defun c-mkrule (num) (intern (concatenate 'string "R" (write-to-string num))))
;(defun c-mkatom (num) (intern (concatenate 'string "C" (write-to-string num))))

;returns T with probability prob, NIL with prbability 1-prob;
(defvar *MAX_NUM*)
(setq *MAX_NUM* 1000000)
(defun rand-event (prob) (> (* prob *MAX_NUM*) (random *MAX_NUM*) ))

   





