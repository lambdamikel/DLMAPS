;******************* destructors ****************************************

;(defun c-neg-get-body (c) (cadr c))
;(defun c-and-get-args (c) (cdr c))
;(defun c-or-get-args (c) (cdr c))
;(defun c-all-get-rule (c) (cadr c))
;(defun c-all-get-body (c) (caddr c))
;(defun c-some-get-rule (c) (cadr c))
;(defun c-some-get-body (c) (caddr c))

(defmacro c-neg-get-body (c)  `(cadr ,c))
(defmacro c-and-get-args (c)  `(cdr ,c))
(defmacro c-or-get-args (c)   `(cdr ,c))
(defmacro c-all-get-rule (c)  `(cadr ,c))
(defmacro c-all-get-body (c)  `(caddr ,c))
(defmacro c-some-get-rule (c) `(cadr ,c))
(defmacro c-some-get-body (c) `(caddr ,c))


;******************* costructors ****************************************
;(defun c-istop (c) (equal c '*top*))
;(defun c-isbot (c) (equal c '*bot*))
;(defun c-isvar (c) (atom c))
;(defun c-isneg (c) (and (not (c-isvar c)) (equal (car c) 'not)))
;(defun c-isall (c) (and (not (c-isvar c)) (equal (car c) 'all))) 
;(defun c-issome (c) (and (not (c-isvar c)) (equal (car c) 'some))) 
;(defun c-isand (c) (and (not (c-isvar c)) (equal (car c) 'and)))
;(defun c-isor (c) (and (not (c-isvar c)) (equal (car c) 'or)))

;(defun c-mknot (c) (list 'not c))
;(defun c-mkand (c_list) (cons 'and c_list))
;(defun c-mkor  (c_list) (cons 'or  c_list))
;(defun c-mksome (r c) (list 'some r c))
;(defun c-mkall  (r c) (list 'all  r c))


(defmacro c-istop (c)  `(equal ,c '*top*))
(defmacro c-isbot (c)  `(equal ,c '*bot*))
(defmacro c-isvar (c)  `(atom ,c))
(defmacro c-isneg (c)  `(and (not (c-isvar ,c)) (equal (car ,c) 'not)))
(defmacro c-isall (c)  `(and (not (c-isvar ,c)) (equal (car ,c) 'all))) 
(defmacro c-issome (c) `(and (not (c-isvar ,c)) (equal (car ,c) 'some))) 
(defmacro c-isand (c)  `(and (not (c-isvar ,c)) (equal (car ,c) 'and)))
(defmacro c-isor (c)   `(and (not (c-isvar ,c)) (equal (car ,c) 'or)))

(defmacro c-mknot (c)      `(list 'not ,c))
(defmacro c-mkand (c_list) `(cons 'and ,c_list))
(defmacro c-mkor  (c_list) `(cons 'or  ,c_list))

(defmacro c-mksome (r c) `(list 'some ,r ,c))
(defmacro c-mkall  (r c) `(list 'all  ,r ,c))

;(defun c-mkrule (num) (intern (concatenate 'string "R" (write-to-string num))))
;(defun c-mkatom (num) (intern (concatenate 'string "C" (write-to-string num))))
 
(defmacro c-mkrule (num) `(intern (concatenate 'string "R" (write-to-string ,num))))
(defmacro c-mkatom (num) `(intern (concatenate 'string "C" (write-to-string ,num))))
 



