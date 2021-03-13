;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%% Routine di controllo per generatore random                              %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

; Modified by I. Horrocks    1997

(defvar *stream_results* T)
(defvar *timeout* 1000)
(defvar *skip-tests* nil)
(defvar *timed-out-tests* 0)

(defun execute_tab (logic n_iter min max step nvar boxes depth &key (fname "results.text") seed timeout skip-tests)
 (setq *logic* logic)
 (setq *var_num* nvar)
 (setq *rule_num* boxes)        
 (setq *mod_degree* depth)
 (when seed (setf *random-state* seed))
 (when timeout (setf *timeout* timeout))
 (setq *skip-tests* skip-tests)
 (setf *stream_results*
       (if *print_results*
	   (open (if fname fname "results.text") :direction :output :if-exists :supersede)
	 T))
 (if *print_prefwff* 
  (setq *stream_prefwff* (OPEN "out_prefwff" :DIRECTION :OUTPUT)))
 (if *print_con*
  (setq *stream_con* (OPEN "out_con" :DIRECTION :OUTPUT)))
 (setq *print-pretty* nil)
 (execute_tab1 n_iter min max step)
 (when *print_results*
       (close *stream_results*))
 (if *print_prefwff* 
  (close *stream_prefwff*))
 (if *print_con*
  (close *stream_con*))
)

(defun generate_con (logic n_iter min max step nvar boxes depth xy-list)
 (setq *logic* logic)
 (setq *var_num* nvar)
 (setq *rule_num* boxes)        
 (setq *mod_degree* depth)
 (setf *stream_results*
       (if *print_results*
	   (open "results.text" :direction :output :if-exists :error)
	 T))
 (if *print_prefwff* 
  (setq *stream_prefwff* (OPEN "out_prefwff" :DIRECTION :OUTPUT)))
 (if *print_con*
  (setq *stream_con* (OPEN "out_con" :DIRECTION :OUTPUT)))
 (setq *print-pretty* nil)
 (generate_con1 n_iter min max step xy-list)
 (when *print_results*
       (close *stream_results*))
 (if *print_prefwff* 
  (close *stream_prefwff*))
 (if *print_con*
  (close *stream_con*))
)

(defparameter *minor-test-number* 1)


(defun execute_tab1 (n_iter min max step)
    (tab_print-probl n_iter min max step)
    (tab_print-grail-info)
    (tab_printseed)
    (do ((n_cl min (+ n_cl step))) 
        ((= n_cl (+ step max)))
         (tab_print_varnum n_cl)
         (setq *and_br* n_cl) 
	 (setq *timed-out-tests* 0)
         (do ((j 0 (+ j 1))) 
         ((= j n_iter))
          (tab_generate_and_test
	   (and *skip-tests*
		(or (and (>= j 5) (> (/ *timed-out-tests* j) 0.9))
		    (and (>= j 10) (> (/ *timed-out-tests* j) 0.75))
		    (and (>= j 20) (> (/ *timed-out-tests* j) 0.575))))))))


(defun generate_con1 (n_iter min max step xy-list)
    (tab_print-probl n_iter min max step)
    (tab_printseed)
    (format t "Generate test clauses ~A~%" xy-list)
;    (decf y)
    (do ((n_cl min (+ n_cl step))) 
        ((= n_cl (+ step max)))
;         (tab_print_varnum n_cl)
         (setq *and_br* n_cl) 
         (do ((j 0 (+ j 1))) 
         ((= j n_iter))
	  (if (and (eql (first xy-list) n_cl) (eql (second xy-list) (1+ j)))
	      (let* ((x (first xy-list)) (y (second xy-list))
		     (of (open (format nil "clause~D.~D" x y)
			      :direction :output))
		    (tempf *stream_results*))
		(setf *stream_results* of)
		(princ ";" of) (tab_print-probl n_iter min max step)
		(princ ";" of) (tab_printseed)
		(format of "; Clause~D.~D~%(setf con~%'" x y)
		(prin1 (tab_generate) of)
		(format of ")~%")
		(close of)
		(setf *stream_results* tempf)
		(setf xy-list (cddr xy-list))
		(when (endp xy-list) (return-from generate_con1 nil)))
	    (progn
	      (tab_generate)
	      (values))))))

#|
(defun execute_tab1 (n_iter min max step)
    (tab_print-probl n_iter min max step)
    (tab_printseed)

    (let ((*results-file* (open "results.text" :direction :output :if-exists :supersede)))

    (do ((n_cl min (+ n_cl step))) 
        ((= n_cl (+ step max)))
         (tab_print_varnum n_cl)
         (setq *and_br* n_cl)
	 (let (rlist)
         (do ((j 0 (+ j 1))) 
         ((= j n_iter))
          (setf *minor-test-number* (1+ j))
          (setf rlist (cons (tab_generate_and_test) rlist)))
	 (when *execute_test*
	       (format *results-file* "~S ~9,2F~%"
		       (floor *and_br* *var_num*)
		       (nth (floor (length rlist) 2) (sort rlist #'>))))))
    (close *results-file*)))
|#

(defun tab_generate_and_test (skip)
 (let ((con (tab_generate)))
  (progn
   (tab_save_wffs con)
   (if *execute_test*
    (cond 
     (skip
      (incf *timed-out-tests*)
      (format *stream_results* "~%B ~10,3F  100000  1000000 ~10,3F" *timeout* *timeout*))
     ((equal *logic* "MK") 
      (if (null *timeout*)
        (tab_test_MK con)
        (bounded_test_tab con)))
     (t (princ "ERROR: ALC works only with MK!!!!")))
    (values)
))))


(defun tab_generate () (rand-conc-mak))


(defun tab_test_MK (con) 
  (progn
   (terpri *stream_results*)
;   (gbc t)                    ; GARBAGE COLLECTING
   (let ((t1 (get-internal-run-time)))
    (let ((ris  (not (alc-concept-coherent con))))
     (let ((deltat (float (/ (- (get-internal-run-time) t1) 
                             internal-time-units-per-second))))
      (tab_print-sat ris)
      (princ (format NIL " ~9,2F"  deltat ) *stream_results*)
      (princ (format NIL "~8D ~8D ~9,2F" *variable-assignments* *total-model-size* *run-time*) *stream_results*)
; return deltat
      deltat
)))))


#+ALLEGRO 
(defmacro time-bound (seconds expression)
  `(mp:with-timeout 
     (,seconds . ((incf *timed-out-tests*)
		  (format *stream_results* "B ~10,3F  100000  1000000 ~10,3F",
			  *timeout*,*timeout*))) ,expression))

#+ALLEGRO 
(defmacro sat-result (flag) `(if (null ,flag) "U" "S"))

#+ALLEGRO 
(defun bounded_test_tab (con) 
 (progn
  (terpri *stream_results*)
;  (gbc t)                    ; GARBAGE COLLECTING
  (let ((t1 (get-internal-run-time)))
   (let ((ris  (time-bound *timeout* 
                 (sat-result (alc-concept-coherent con)))))
    (let ((deltat (float (/ (- (get-internal-run-time) t1) 
                            internal-time-units-per-second))))
     (if (null ris)
      nil
      (progn 
       (princ ris *stream_results*)
       (princ (format NIL " ~10,3F"  deltat ) *stream_results*)
      (princ (format NIL "~8D ~8D ~10,3F" *variable-assignments* *total-model-size* *run-time*) *stream_results*)
(force-output *stream_results*)))
; return deltat
     deltat
)))))



;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; ausiliarie di stampa
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun tab_print-probl (n_iter min max step)
 (princ
  (format NIL 
   "%%% #CL: ~D ~D ~D  #IT: ~D  #var: ~D LOGIC: ~S HOST: ~S PROPS: ~5,2F DEG: ~D TABLEAUX"
   min max step n_iter  *var_num* *logic* (system::getenv "HOST") *prim_conc_prob*  *mod_degree* 0) *stream_results*)
 (terpri *stream_results*)
)

(defun tab_print-sat (flag)
 (if flag
  (princ "U " *stream_results*)
  (princ "S " *stream_results*))
)

(defun tab_print-results (type val)
 (princ type *stream_results*)
 (princ ": " *stream_results*)
 (princ val *stream_results*)
 (princ "  " *stream_results*))

(defun tab_print_varnum (i) 
         (terpri *stream_results*)
         (princ i *stream_results*)
         (princ " CLAUSES: " *stream_results*)
	 (princ " [SEED " *stream_results*)
	 (princ *random-state* *stream_results*)
	 (princ "]" *stream_results*)
) 

(defun tab_save_wffs (conc)
 (if  *print_con*
  (progn
    (format *stream_con* "; ~S Clauses, test ~S~%" *and_br* *minor-test-number*)
   (princ "(princ (not (time (alc-concept-coherent " *stream_con*)
   (terpri *stream_con*)
   (princ "'" *stream_con*)
   (princ conc *stream_con*)
   (terpri *stream_con*)
   (princ ")))) " *stream_con*)
   (terpri *stream_con*)
 ))
 (if *print_prefwff* 
  (progn
   (cond 
    ((equal *logic* "S5") (princ "(princ (time (s5-taut " *stream_prefwff*))
    ((equal *logic* "K")  (princ "(princ (time (K-taut " *stream_prefwff*))
    (t (princ "ERROR: unspecified logic")))
   (terpri *stream_prefwff*)
   (princ "'(NOT " *stream_prefwff*)
   (princ (con2prefwff conc) *stream_prefwff*)
   (terpri *stream_prefwff*)
   (princ ")))) " *stream_prefwff*)
   (terpri *stream_prefwff*)
 ))
)

(defun tab_printseed ()
 (princ "%%% SEED " *stream_results*)
 (princ *random-state* *stream_results*)
 (terpri *stream_results*))

(defun tab_print-grail-info ()
  (if (boundp '*version-number*)
      (format *stream_results* "%%% FaCT Version: ~A~%" *version-number*)
  (format *stream_results* "%%% KSAT~%")))
