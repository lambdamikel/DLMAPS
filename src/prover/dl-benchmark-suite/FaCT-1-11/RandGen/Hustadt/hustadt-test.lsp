(defvar *timeout* 1000)

#+ALLEGRO
(defmacro time-bound (seconds expression)
  `(mp:with-timeout 
     (,seconds . nil) ,expression))

(defmacro sat-result (flag) `(if (null ,flag) "U" "S"))

(defun mpi-test (N n-tests start end step &key (fname "results.text") (seed nil) (timeout 1000)
		   (skip-tests nil) (t-file nil))
  (let ((*r-f* (open fname :direction :output :if-exists :supersede))
	(*t-f* (when t-file (open t-file)))
	(timed-out-tests 0)
	(skip nil))
    (when t-file (dotimes (i 4) (read-line *t-f* nil nil)))
    (format *r-f* "%%% #CL: ~S ~S ~S  #IT: ~S  #var: ~S  MPI TEST~%"
	start end step n-tests N)
    (when seed (setf *random-state* seed))
    (if (boundp '*version-number*)
	(format *r-f* "%%% FaCT Version: ~A~%" *version-number*)
      (format *r-f* "%%% KSAT~%"))
    (format *r-f* "%%% SEED ~S~2%" *random-state*)
    (setf *var_num* N)
    (do ((l start (+ l step)))
	((> l end) (values))
	(format *r-f* "~S CLAUSES: [SEED ~S]~%" l *random-state*)
	(setf *and_br* l)
	(setf *timeout* timeout)
	(setf timed-out-tests 0)
	(setf skip nil)
	(when t-file (read-line *t-f* nil nil))
	(dotimes (i n-tests)
		 (let ((con (generate-rand-formula))
		       (l (when t-file (read-line *t-f* nil nil))))
		   (if (and l (or (not (eq (elt l 0) #\B))
				  (>= (read-from-string (subseq l 1)) *timeout*)))
		       (format *r-f* "~A~%" l)
		     (let* ((start-time (get-internal-run-time))
			    (res
			     (if skip
				 nil
			       (progn
				 #+ALLEGRO (time-bound *timeout*
						       (sat-result (alc-concept-coherent con)))
				 #-ALLEGRO (sat-result (alc-concept-coherent con)))))
			    (deltat (/ (- (get-internal-run-time) start-time)
				       internal-time-units-per-second)))
		       (if (null res)
			   (progn
			     (setf deltat *timeout*)
			     (incf timed-out-tests)
			     (format *r-f* "~A ~10,3F ~8D ~8D ~10,3F~%"
				     "B" deltat 100000 1000000 deltat))
			 (format *r-f* "~A ~10,3F ~8D ~8D ~10,3F~%"
				 res deltat *variable-assignments* *total-model-size* *run-time*))
		       (when 
			(and skip-tests
			     (or (and (>= i 5) (> (/ timed-out-tests i) 0.9))
				 (and (>= i 10) (> (/ timed-out-tests i) 0.75))
				 (and (>= i 20) (> (/ timed-out-tests i) 0.575))))
			(setf skip T)))))))
    (when t-file (close *t-f*))
    (close *r-f*)))
