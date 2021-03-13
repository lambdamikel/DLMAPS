(defun get-fname (fname &key (exists nil) (prompt "file name: "))
  (do ()
      ((and fname (stringp fname) (or (not exists) (probe-file fname))) (pathname fname))
      (when fname
	    (format t "file not found - ~A~%" fname))
      (princ prompt)
      (setf fname (read *terminal-io*))
      (when (symbolp fname)
	    (setf fname (string-downcase (symbol-name fname))))))

(defun process-data (&key ifname ofname)
  (setf ifname (get-fname ifname  :exists T :prompt "input file: "))
  (setf ofname (get-fname ofname :prompt "output file: "))
  (unless (probe-file ifname)
	  (return-from process-data (values)))
  (when (probe-file ofname)
	(do ((ow nil))
	    ((eq ow 'y))
	    (format t "file exists ~S - overwrite [y/n]: " (namestring ofname))
	    (setf ow (read *terminal-io*))
	    (when (eq ow 'n)
		  (return-from process-data (values)))))
  (let* ((if (open ifname :direction :input))
	 (of (open ofname :direction :output :if-exists :supersede))
	 (s (read-line if))
	 (test-params (read-from-string
		       (concatenate
			'string "(" (subseq s (+ (search "#CL:" s) 4)
					    (search "#IT:" s)) ")")))
	 (start (first test-params)) (end (second test-params)) (step (third test-params))
	 (tests (read-from-string (subseq s (+ (search "#IT:" s) 4)))))
    (format of "#~A~%" s)
    (let ((comment "%%%"))
      (do ()
	  ((or (< (length comment) 3) (not (equal (subseq comment 0 3) "%%%"))))
	  (format of "#~A~%" (setf comment (read-line if)))))
    (multiple-value-bind (sec min hour date month year day daylight-saving time-zone)
			 (decode-universal-time (file-write-date if))
			 (format T "~%File: ~A; Date: ~D/~D/~D"
				 (namestring ifname) date month year)
			 (format of "# File: ~A; Date: ~D/~D/~D~%"
				 (namestring ifname) date month year))
    (format T "~%Number of clauses: start ~D, end ~D, step ~D; ~D tests per data point.~2%"
	    start end step tests)
    (format of "# Number of clauses: start ~D, end ~D, step ~D; ~D tests per data point.~%#~%"
	    start end step tests)
    (format of "#~9@A ~10@A ~10@A ~10@A ~10@A ~10@A ~10@A ~10@A ~10@A ~10@A ~10@A ~%"
	    "Clauses" "Bounded" "Prob-sat" "Median-t" "Mean-t" "Median-s" "Mean-s"
	    "Median-c" "Mean-c" "Median-r" "Mean-r")
    (do ((i start (+ i step)))
	((> i end) nil)
	(let ((c (read-from-string (read-line if)))
	      times sat-calls clashes sat-times (sat-count 0) (bound-count 0))
	  (dotimes (j tests)
		   (let ((data (read-from-string
				(concatenate 'string "(" (read-line if) ")"))))
		     (when (eq (first data) 's) (incf sat-count))
		     (when (eq (first data) 'b) (incf bound-count))
		     (setf times (cons (second data) times))
		     (setf sat-calls (cons (third data) sat-calls))
		     (setf clashes (cons (fourth data) clashes))
		     (setf sat-times (cons (fifth data) sat-times))))
	  (setf times (sort times #'<))
	  (setf sat-calls (sort sat-calls #'<))
	  (setf clashes (sort clashes #'<))
	  (setf sat-times (sort sat-times #'<))
	  (format of "~10D ~10D ~10,2F ~10,3F ~10,3F ~10D ~10,3F ~10D ~10,3F ~10,3F ~10,3F ~%"
		  c
		  bound-count
		  (/ sat-count (- tests bound-count))
		  (nth (floor tests 2) times)
		  (/ (reduce #'+ times) (- tests bound-count))
		  (nth (floor tests 2) sat-calls)
		  (/ (reduce #'+ sat-calls) (- tests bound-count))
		  (nth (floor tests 2) clashes)
		  (/ (reduce #'+ clashes) (- tests bound-count))
		  (nth (floor tests 2) sat-times)
		  (/ (reduce #'+ sat-times) (- tests bound-count)))))
    (close if)
    (close of)
    (values)))

(defun scatter-data (&key ifname ofname)
  (setf ifname (get-fname ifname  :exists T :prompt "input file: "))
  (setf ofname (get-fname ofname :prompt "output file: "))
  (unless (probe-file ifname)
	  (return-from scatter-data (values)))
  (when (probe-file ofname)
	(do ((ow nil))
	    ((eq ow 'y))
	    (format t "file exists ~S - overwrite [y/n]: " (namestring ofname))
	    (setf ow (read *terminal-io*))
	    (when (eq ow 'n)
		  (return-from scatter-data (values)))))
  (let* ((if (open ifname :direction :input))
	 (of (open ofname :direction :output :if-exists :supersede))
	 (s (read-line if))
	 (test-params (read-from-string
		       (concatenate
			'string "(" (subseq s (+ (search "#CL:" s) 4)
					    (search "#IT:" s)) ")")))
	 (start (first test-params)) (end (second test-params)) (step (third test-params))
	 (tests (read-from-string (subseq s (+ (search "#IT:" s) 4)))))
    (format of "#~A~%" s)
    (let ((comment "%%%"))
      (do ()
	  ((or (< (length comment) 3) (not (equal (subseq comment 0 3) "%%%"))))
	  (format of "#~A~%" (setf comment (read-line if)))))
    (multiple-value-bind (sec min hour date month year day daylight-saving time-zone)
			 (decode-universal-time (file-write-date if))
			 (format T "~%File: ~A; Date: ~D/~D/~D"
				 (namestring ifname) date month year)
			 (format of "# File: ~A; Date: ~D/~D/~D~%"
				 (namestring ifname) date month year))
    (format T "~%Number of clauses: start ~D, end ~D, step ~D; ~D tests per data point.~2%"
	    start end step tests)
    (format of "# Number of clauses: start ~D, end ~D, step ~D; ~D tests per data point.~%#~%"
	    start end step tests)
    (format of "#~9@A ~10@A ~10@A ~10@A ~10@A ~10@A ~10@A ~10@A ~10@A ~10@A ~10@A ~%"
	    "Clauses" "Bounded" "Prob-sat" "Median-t" "Mean-t" "Median-s" "Mean-s"
	    "Median-c" "Mean-c" "Median-r" "Mean-r")
    (do ((i start (+ i step)))
	((> i end) nil)
	(let ((c (read-from-string (read-line if)))
	      times sat-calls clashes sat-times (sat-count 0) (bound-count 0))
	  (dotimes (j tests)
		   (let ((data (read-from-string
				(concatenate 'string "(" (read-line if) ")"))))
		     (when (eq (first data) 's) (incf sat-count))
		     (when (eq (first data) 'b) (incf bound-count))
		     (format of "~10D ~10,2F ~10D ~10D ~%"
			     c (second data) (third data) (fourth data))))))
    (close if)
    (close of)
    (values)))

(defun q-percentiles (&key ifname ofname (overwrite nil) (3-d t))
  (setf ifname (get-fname ifname  :exists T :prompt "input file: "))
  (setf ofname (get-fname ofname :prompt "output file: "))
  (unless (probe-file ifname)
	  (return-from q-percentiles (values)))
  (when (and (probe-file ofname) (not overwrite))
	(do ((ow nil))
	    ((eq ow 'y))
	    (format t "file exists ~S - overwrite [y/n]: " (namestring ofname))
	    (setf ow (read *terminal-io*))
	    (when (eq ow 'n)
		  (return-from q-percentiles (values)))))
  (let* ((if (open ifname :direction :input))
	 (of (open ofname :direction :output :if-exists :supersede))
	 (s (read-line if))
	 (test-params (read-from-string
		       (concatenate
			'string "(" (subseq s (+ (search "#CL:" s) 4)
					    (search "#IT:" s)) ")")))
	 (start (first test-params)) (end (second test-params)) (step (third test-params))
	 (tests (read-from-string (subseq s (+ (search "#IT:" s) 4))))
	 (n-vars (read-from-string (subseq s (+ (search "#var:" s) 5)))))
    (format of "#~A~%" s)
    (let ((comment "%%%"))
      (do ()
	  ((or (< (length comment) 3) (not (equal (subseq comment 0 3) "%%%"))))
	  (format of "#~A~%" (setf comment (read-line if)))))
    (multiple-value-bind (sec min hour date month year day daylight-saving time-zone)
			 (decode-universal-time (file-write-date if))
			 (format T "~%File: ~A; Date: ~D/~D/~D"
				 (namestring ifname) date month year)
			 (format of "# File: ~A; Date: ~D/~D/~D~%"
				 (namestring ifname) date month year))
    (format T "~%Number of clauses: start ~D, end ~D, step ~D; ~D tests per data point.~2%"
	    start end step tests)
    (format of "# Number of clauses: start ~D, end ~D, step ~D; ~D tests per data point.~%#~%"
	    start end step tests)
    (if 3-d
	(format of "#~9@A ~10@A ~10@A~%" "L/N" "Percent" "Time")
      (format of "#~69@A~60@A~%#~9@A~10@A~10@A~10@A~10@A~10@A~10@A~10@A~10@A~10@A~10@A~10@A~10@A~10@A~%"
	      "Sat Times" "Backtrack Search Space"
	      "L/N" "Prob-Sat" "50%" "60%" "70%" "80%" "90%" "100%"
	      "50%" "60%" "70%" "80%" "90%" "100%"))
    (do ((i start (+ i step)))
	((> i end) nil)
	(let ((c (read-from-string (read-line if)))
	      times backtracks clashes sat-times (sat-count 0) (bound-count 0))
	  (dotimes (j tests)
		   (let ((data (read-from-string
				(concatenate 'string "(" (read-line if) ")"))))
		     (when (eq (first data) 's) (incf sat-count))
		     (when (eq (first data) 'b) (incf bound-count))
		     (setf times (cons (second data) times))
		     (setf backtracks (cons (third data) backtracks))
;		     (setf clashes (cons (fourth data) clashes))
;		     (setf sat-times (cons (fifth data) sat-times))
))
	  (setf times (sort times #'<))
	  (setf backtracks (sort backtracks #'<))
;	  (setf clashes (sort clashes #'<))
;	  (setf sat-times (sort sat-times #'<))
	  (if 3-d
	      (dolist (p '(50 60 70 80 90 100))
		      (format of "~10,1F ~10D ~10,3F~%"
			      (/ c n-vars) p (nth (1- (floor (* tests p) 100)) times)))
	    (progn
	      (format of "~10,1F~10,3F" (/ c n-vars) (/ sat-count tests))
	      (dolist (p '(50 60 70 80 90 100))
		      (format of "~10,3F"
			      (nth (1- (floor (* tests p) 100)) times)))
	      (dolist (p '(50 60 70 80 90 100))
		      (format of "~10D"
			      (nth (1- (floor (* tests p) 100)) backtracks)))))
	  (format of "~%")))
    (close if)
    (close of)
    (values)))

;(process-data)
;(bye)
