;;;-*- Mode: Lisp; Package: CL-USER -*-

(in-package "CL-USER")

(defun normalize (pathname)
  "ALC5-windows needs this #@!?. It cannot deal with underlines in logical pathnames."
  (string-downcase (namestring (translate-logical-pathname pathname))))

;;; Directory for T98 KB data files:
(defparameter *kb-data-dir*
    (normalize "dl-test:Data;T98-kb;"))

;;; Directory for T98 satisfiability test data files:
(defparameter *sat-data-dir*
  (normalize "dl-test:Data;T98-sat;"))

;;; Directory for application KB data files:
(defparameter *appn-data-dir*
  (normalize "dl-test:Data;Appn-kb;"))

;;; Directory for synthetic KB data files:
(defparameter *synth-data-dir*
  (normalize "dl-test:Data;DFKI-synthetic;"))

;;; Directory for random KB data files:
(defparameter *random-data-dir*
  (normalize "dl-test:Data;DFKI-random;"))

;;; Directory for T98 AKB data files:
(defparameter *abox-data-dir*
  (normalize "dl-test:Data;T98-abox;"))

;;; Directory for results files:
(defparameter *results-dir*
  (normalize "dl-test:Results;"))

;;; Directory for new DL AKB data files:
(defparameter *new-abox-data-dir*
  (normalize "dl-test:Data;DL-abox;"))


(defvar *if*)


;;;
;;;
;;;

(defmacro myopen (name &rest args)
  `(open (normalize ,name) ,@args))

(defmacro myeval (expr)
  `(let ((expr 
          (tools:change-package-of-description ,expr :prover nil nil)))
     (eval expr)))

;;;
;;;
;;;

(defparameter *krss-syntax* '(top bottom and or not some all at-least at-most exactly))

(defparameter *syntax-map* (pairlis *krss-syntax* *concept-syntax*))

(defparameter *t98-kb-files*
  '( "k_branch" 
     "k_d4" 
     "k_dum" 
     "k_grz" 
     "k_lin"
     "k_path"
     "k_ph"
     "k_poly"
     "k_t4p"))

(defparameter *t98-con-files*
  '("k_branch" "k_d4" "k_dum" "k_grz" "k_lin" "k_path" "k_ph" "k_poly" "k_t4p"))

(defparameter *appn-kb-files*
  '("galen2"
    "galen1" 
    "galen"))

(defparameter *synth-kb-files*
  '( "hc14"  "hc18" "hc112" "hc24" "hc28" "hc212" "hc34" "hc36" "hc38"
     "hc44" "hc48" "hc412"
))

(defparameter *random-kb-files*
  '( "kris151" "kris301" "kris451" "kris601" "kris751" "kris901"
               "kris1051" "kris1201" "kris1351" 
               "kris1501" "kris2001" 
               "kris4001"
               "kris6001" "kris8001"
               "kris10001" "kris12001" "kris14001" "kris16001"
               "kris18001" "kris20001" "kris25001" "kris30001" "kris35001" 
               "kris40001"
               "kris45001" 
               "kris50001"))

(defparameter *t98-abox-files*
  '("k_branch_n"
    "k_d4_n" 
    "k_dum_n"
    "k_grz_n" 
    "k_lin_n"
    "k_path_n"
    "k_ph_n"
    "k_poly_n"
    "k_t4p_n"
    ))

(defparameter *hard-appn-kb-files*
  '("galen2" "galen1" "galen"
             "espr-gcis" "wisber-gcis" "ckb-gcis" "fss-gcis"
             "bike1" "bike2" "bike3"
             ))

(defparameter *new-appn-kb-files*
  (append '("ckb-roles" "datamont-roles" "espr-roles" "fss-roles"
            ;"stereo-roles" 
                        "wisber-roles" "wines")
          *hard-appn-kb-files*))

(defmacro time-bound (seconds expression)
  `(with-timeout 
       ((* 1.0 ,seconds) . ('*timed-out*)) ,expression))

(defun translate-concept-syntax (c)
  (cond
   ((listp c)
    (case (car c)
      ((and or)
       (cons (cdr (assoc (car c) *syntax-map*))
	     (mapcar #'translate-concept-syntax (cdr c))))
      ((some all)
       (cons (cdr (assoc (car c) *syntax-map*))
	     (cons (second c)
		   (mapcar #'translate-concept-syntax (cddr c)))))
      (not
       (list (cdr (assoc (car c) *syntax-map*))
	     (translate-concept-syntax (second c))))
      (at-least
       (let ((trans (cdr (assoc (car c) *syntax-map*))))
         (if (null trans)
             `(some ,(third c) *top*)
           (list trans (second c) (third c)))))
      (at-most
       (let ((trans (cdr (assoc (car c) *syntax-map*))))
         (if (null trans)
             (if (zerop (second c))
                 `(all ,(third c) *bottom*)
               '*top*)
           (list trans (second c) (third c)))))
      (exactly
       (let ((trans (cdr (assoc (car c) *syntax-map*))))
         (if (null trans)
             `(some ,(third c) *top*)
           (list trans (second c) (third c)))))
      (T (error "Concept ~S doesn't seem to be KRSS syntax" c))))
   ((or (eq c 'top) (eq c 'bottom)) (cdr (assoc c *syntax-map*)))
   (T c)))
      
(defun test-kb (kb &key (test t) (timeout *kb-timeout*) (filename nil))
  (*initialise-kb* filename)
  (let ((c-count 0))
    (dolist (s kb)
      (ecase (car s)
	((DEFINE-PRIMITIVE-ROLE DEFINE-PRIMITIVE-ATTRIBUTE))
	(DEFINE-PRIMITIVE-CONCEPT
         (incf c-count)
         (if (third s) (rplaca (cddr s) (translate-concept-syntax (third s)))))
	(DEFINE-CONCEPT
         (incf c-count)
         (rplaca (cddr s) (translate-concept-syntax (third s))))
        (DEFINE-DISJOINT-PRIMITIVE-CONCEPT
         (incf c-count)
         (rplaca (cdddr s) (translate-concept-syntax (fourth s))))
        (IMPLIES
         (rplaca (cdr s) (translate-concept-syntax (second s)))
         (rplaca (cddr s) (translate-concept-syntax (third s))))))
    (let* ((start-time (get-internal-run-time))
	   (res (time-bound timeout 
			    (progn
			      (dolist (s kb) (myeval s))
			      (*classify-kb*))))
	   (deltat (/ (- (get-internal-run-time) start-time)
		      internal-time-units-per-second)))
      (if (eq res '*timed-out*)
          (values '*timed-out* c-count nil)
	(values deltat c-count (if test (*test-coherent* 'prover::test)))))))

(defun run-kb-test (testfile &key (timeout *kb-timeout*) (prov nil) (start 1))
  (let ((*if* (myopen testfile :direction :input))
	(max-n 0) (test-n 0) (max-time 0) (correct T) (max-c 0))
    (unwind-protect
        (progn
          (loop repeat (1- start) do
                (incf test-n)
                (read *if* nil nil))
          (loop
           (let ((kb (read *if* nil nil)))
             (when (null kb) (return))
             (incf test-n)
             (when (or (not *max-kb*) 
                       (< test-n *max-kb*))
               (format t "Testing ~A KB ~D..." testfile test-n)
               (multiple-value-bind
                   (deltat c-count coherent)
                   (test-kb kb :timeout timeout)
                 (when (and (not (eq deltat '*timed-out*))
                            (or (and prov coherent) (and (not prov) (not coherent))))
                   (format t "PROBLEMI!! - concept TEST should be ~A~%"
                           (if prov "incoherent" "coherent"))
                   (setf correct nil)
                (break)
                   (return))
                 (if (eq deltat '*timed-out*)
                     (progn
                       (format t "TIMED OUT~%")
                       (return))
                   (progn
                     (format t "OK - test took ~,2Fs~%" deltat)
                     (if (<= deltat *kb-timeout*)
                         (progn
                           (setf max-n test-n)
                           (setf max-c c-count)
                           (setf max-time deltat))
                       (return)))))))
          ;(break)
           ))
      (close *if*))
    (values max-n max-c correct max-time)))
      
;;;**************************************************************************

(defun test-con (con &key (timeout *sat-timeout*))
  (setf con (translate-concept-syntax con))
  (let* ((start-time (get-internal-run-time))
	 (res (time-bound timeout (*test-concept* con)))
	 (deltat (/ (- (get-internal-run-time) start-time)
                    internal-time-units-per-second)))
    (if (eq res '*timed-out*)
	(values '*timed-out* nil)
      (values deltat res))))

(defun run-sat-test (testfile &key (timeout *sat-timeout*) (prov nil) (start 1))
  (declare (ignore timeout))
  (let ((*if* (myopen testfile :direction :input))
	(max-n 0) (test-n 0) (max-time 0) (correct T))
    (unwind-protect
        (progn
          (loop repeat (1- start) do
                (incf test-n)
                (read *if* nil nil))
          (loop
           (let ((con (read *if* nil nil)))
             (when (null con) (return))
             (incf test-n)
             (format t "Testing ~A concept ~D..." testfile test-n)
             (multiple-value-bind (deltat coherent) (test-con con)
               (when (and (not (eq deltat '*timed-out*))
                          (or (and prov coherent) (and (not prov) (not coherent))))
                 (format t "PROBLEMI!! - concept ~S should be ~A~%"
                         con (if prov "incoherent" "coherent"))
                 (break)
                 (setf correct nil)
                 (return))
               (if (eq deltat '*timed-out*)
                   (progn
                     (format t "TIMED OUT~%")
                     (return))
                 (progn
                   (format t "OK - test took ~,2Fs~%" deltat)
                   (if (<= deltat *sat-timeout*)
                       (progn
                         (setf max-n test-n)
                         (setf max-time deltat))
                     (return))))))))
      (close *if*))
    (values max-n correct max-time)))

;;;**************************************************************************

(let (#+:mcl (ccl:*warn-if-redefine* nil)
             #+:allegro (excl:*redefinition-warnings* nil)
             )
  (defun safe-set-xor-test (set-1 set-2)
    (cond ((symbolp set-1)
           (if (listp set-2)
               (member set-1 set-2)
             (eql set-1 set-2)))
          ((symbolp set-2)
           (if (listp set-1)
               (member set-2 set-1)
             (eql set-1 set-2)))
          (t (null (set-exclusive-or set-1 set-2)))))
  )

(defun check-classified-kb (testfile)
  (let ((*if* (myopen (concatenate 'string testfile ".tree") :direction :input)))
    (unwind-protect
        (loop
         (let ((s (read *if* nil nil)))
           (if (null s) (return (values T nil)))
           (let* ((first-s (if (consp (first s))
                               (first (first s))
                             (first s)))
                  (supers (*direct-supers* first-s))
                  (subs (*direct-subs* first-s)))
             (if (or (set-exclusive-or supers (second s) :test #'safe-set-xor-test)
                     (and (third s)
                          (set-exclusive-or subs (third s)
                                            :test #'safe-set-xor-test)))
                 (progn
                   #+:MCL (break)
                   (format t "~%*** Mismatch: 
Concept: ~S
Supers:  ~S
Subs:    ~S
File:    ~S~%" first-s supers subs s)
                   (break)

                   (return (values nil (first s))))))))
      (close *if*))))

(defun run-appn-test (testfile &key (timeout *appn-timeout*) (check T))
  (let ((*if* (myopen (concatenate 'string testfile ".tkb") :direction :input))
	(kb nil))
    (unwind-protect
        (progn
          (format t "Testing ~A..." testfile)
          (loop
           (let ((s (read *if* nil nil)))
             (when (null s) (return))
             (setf kb (cons s kb)))))
      (close *if*))
    (setf kb (nreverse kb))
    (multiple-value-bind
        (deltat c-count coherent)
        (test-kb kb :test nil :timeout timeout :filename testfile)
      (if (not (eq deltat '*timed-out*))
          (progn
            (format t "OK - test took ~,2Fs~%" deltat)
          ;(break)
            (if check
                (progn
                  (format t "  Testing concept hierarchy...")
                  (multiple-value-bind (correct c-prob)
                      (check-classified-kb testfile)
                    (setf coherent correct)
                    (if coherent
                        (format t "OK~%")
                      (progn (format t "PROBLEMI!! - classification errors for ~A~%"
                                     c-prob)
                    (break)
                        ))
                    (values (format nil "~9,2F" deltat) c-count (if correct "Y" "N"))))
              (values (format nil "~9,2F" deltat) c-count "?")))
        (progn
          (format t "TIMED OUT~%")
          (values (format nil ">~9,2F" timeout) c-count "?"))))))


;;;**************************************************************************
      
(defun test-akb-correct (akb)
  (do ((q akb (cdr q))
       (x nil (car q))
       (correct T (myeval (car q))))
      ((or (endp q) (not correct))
       (if (not correct) (cdr x)))))

#|
(defun test-akb (akb retrieval &key (test t) (timeout *abox-timeout*))
  (*initialise-abox*)
  (let ((i-count 0))
    (dolist (s (first akb))
      (case (car s)
	(INSTANCE
         (incf i-count)
         (rplaca (cddr s) (translate-concept-syntax (third s))))
	(RELATED
	 (rplaca (cddr s) (translate-concept-syntax (third s))))))
    (let* ((start-time (get-internal-run-time))
	   (res (time-bound timeout (progn (dolist (s (first akb)) (eval s))
                                      (*classify-akb*))))
	   (deltat (/ (- (get-internal-run-time) start-time)
		      internal-time-units-per-second)))
      (if (eq res '*timed-out*)
          (values '*timed-out* i-count nil)
	(values deltat i-count (if test (test-akb-correct retrieval)))))))

(defun test-akb (akb retrieval &key (test t) (timeout *abox-timeout*))
  (*initialise-abox*)
  (let ((i-count 0))
    (dolist (s akb)
      (case (car s)
	(INSTANCE
         (incf i-count)
         (rplaca (cddr s) (translate-concept-syntax (third s))))
	(RELATED
	 (rplaca (cddr s) (translate-concept-syntax (third s))))))
    (let* ((start-time (get-internal-run-time))
	   (res (time-bound timeout (progn (dolist (s akb) (eval s))
                                      (*classify-akb*))))
	   (deltat (/ (- (get-internal-run-time) start-time)
		      internal-time-units-per-second)))
      (if (eq res '*timed-out*)
          (values '*timed-out* i-count nil)
	(values deltat i-count (if test (test-akb-correct retrieval)))))))
|#

(defun test-akb (akb retrieval &key (test t) (timeout *abox-timeout*))
  (*initialise-abox*)
  (let ((i-count 0)
        (inds nil))
    (dolist (s akb)
      (case (car s)
	(INSTANCE
         (unless (member (second s) inds)
           (incf i-count)
           (push (second s) inds))
         (rplaca (cddr s) (translate-concept-syntax (third s))))
	(RELATED
         (unless (member (third s) inds)
           (incf i-count)
           (push (third s) inds))
	 (rplaca (cdddr s) (translate-concept-syntax (fourth s))))))
    (let* ((start-time (get-internal-run-time))
           correct
	   (res (time-bound timeout (progn (dolist (s akb) (myeval s))
                                      (prog1
                                          (*classify-akb*)
                                        (setf correct
                                              (test-akb-correct retrieval))))))
	   (deltat (/ (- (get-internal-run-time) start-time)
		      internal-time-units-per-second)))
      (if (eq res '*timed-out*)
          (values '*timed-out* i-count nil)
	(values deltat i-count (if test correct))))))

#|
(defun run-abox-test (testfile &key (timeout *abox-timeout*) (prov nil))
  (let ((*if* (myopen testfile :direction :input))
	(max-n 0) (test-n 0) (correct T) (max-c 0) (time 0))
    (unwind-protect
        (loop
         (let ((kb (read *if* nil nil)))
           (when (null kb) (return))
           (incf test-n)
           (format t "Testing ~A A-KB ~D..." testfile test-n)
           (multiple-value-bind
               (deltat c-count coherent)
               (test-kb (first kb) :timeout timeout)
             (when (and (not (eq deltat '*timed-out*))
                        (or (and prov coherent) (and (not prov) (not coherent))))
               (format t "PROBLEMI!! - concept TEST should be ~A~%"
                       (if prov "incoherent" "coherent"))
               (setf correct nil)
               (return))
             (if (not (eq deltat '*timed-out*))
                 (progn
                   (format t "~%Classified KB OK - took ~,2Fs~%" deltat)
                   (multiple-value-bind
                       (abox-deltat i-count i-error)
                       (test-akb (cdr kb) :timeout timeout)
                     (declare (ignore i-count))
                     (when (and (not (eq abox-deltat '*timed-out*)) i-error)
                       (format t "PROBLEMI!! - ~A should be instance of ~A~%"
                               (first i-error) (second i-error))
                       (break)
                       (setf correct nil)
                       (return))
                     (if (eq abox-deltat '*timed-out*)
                         (progn
                           (format t "TIMED OUT~%")
                           (return))
                       (progn
                         (format t "Abox OK - test took ~,2Fs~%" abox-deltat)
                         (if (<= abox-deltat *abox-timeout*)
                             (progn
                               (setf max-n test-n)
                               (setf max-c c-count)
                               (setf time abox-deltat))
                           (return))))))
               (return)))))
      (close *if*))
    (values max-n max-c correct time)))
|#

(defun run-abox-test (testfile &key (timeout *abox-timeout*) (prov nil) (start 1))
  (let ((*if* (myopen testfile :direction :input))
	(max-n 0) (test-n 0) (correct T) (max-c 0) (time 0) (max-n-i-count 0))
    (unwind-protect
        (loop for (tbox abox retrieval) in (read *if* nil nil)
              until (null tbox)
              do (incf test-n)
              when (and (>= test-n start) 
                        (or (not *max-kb*)
                            (< test-n *max-kb*)))
              do
              (format t "Testing ~A A-KB ~D..." testfile test-n)
              (multiple-value-bind
                  (deltat c-count coherent)
                  (test-kb tbox :timeout timeout)
                (when (and (not (eq deltat '*timed-out*))
                           (or (and prov coherent) (and (not prov) (not coherent))))
                  (format t "PROBLEMI!! - concept TEST should be ~A~%"
                          (if prov "incoherent" "coherent"))
                  (break)
                  (setf correct nil)
                  (return))
                (if (not (eq deltat '*timed-out*))
                    (progn
                      (format t "~%Classified KB OK - took ~,2Fs~%" deltat)
                      (multiple-value-bind
                          (abox-deltat i-count i-error)
                          (test-akb abox retrieval :timeout timeout)
                        (when (and (not (eq abox-deltat '*timed-out*)) i-error)
                          (format t "PROBLEMI!! - ~A should be instance of ~A~%"
                                  (first i-error) (second i-error))
                          (break)
                          (setf correct nil)
                          (return))
                        (if (eq abox-deltat '*timed-out*)
                            (progn
                              (format t "TIMED OUT~%")
                              (return))
                          (progn
                            (format t "Abox OK - test took ~,2Fs~%" abox-deltat)
                            (if (<= abox-deltat *abox-timeout*)
                                (progn
                                  (setf max-n test-n)
                                  (setf max-n-i-count i-count)
                                  (setf max-c c-count)
                                  (setf time abox-deltat))
                              (return))))))
                  (return))))
      (close *if*))
    (values max-n max-c max-n-i-count correct time)))

;;;**************************************************************************

(defconstant tex-special-chars '(#\_))

(defun tex-string (s)
  (if (= (length s) 0) s
    (if (member (elt s 0) tex-special-chars)
	(concatenate 'string (list #\\ (elt s 0)) (tex-string (subseq s 1)))
      (concatenate 'string (list (elt s 0)) (tex-string (subseq s 1))))))


(defun kb-tests (test-files &key (system *system-name*) (timeout *kb-timeout*)
                            (start-p 1) (start-n start-p))
  (let* ((tex-filename (concatenate 'string *results-dir* system "_t98_kb_res.tex"))
         (txt-filename (concatenate 'string *results-dir* system "_t98_kb_res.txt"))
         (mode (if (or (not (probe-file txt-filename)) (eql start-p 1))
                   ':supersede
                 ':append))
         (res-tex (myopen tex-filename :direction :output :if-exists mode))
	 (res-txt (myopen txt-filename :direction :output :if-exists mode)))
    (unwind-protect
        (dolist (f test-files)
          (multiple-value-bind 
              (r-p c-c-p c-p time-p) 
              (run-kb-test (concatenate 'string *kb-data-dir* f "_p.tkb") 
                           :timeout timeout :prov T :start start-p)
            (multiple-value-bind
                (r-n c-c-n c-n time-n) 
                (run-kb-test (concatenate 'string *kb-data-dir* f "_n.tkb")
                             :timeout timeout :prov nil :start start-n)
              (format res-tex "~A & " (tex-string f))
              (format res-tex "~A & ~:D & ~A & ~,2Fs & "
                      r-p c-c-p (if c-p "Y" "N") time-p)
              (format res-tex "~A & ~:D & ~A & ~,2Fs \\\\~%"
                      r-n c-c-n (if c-n "Y" "N") time-n)
              (format res-txt "~20A ~5D ~5D ~A ~6,2Fs ~5D ~5D ~A ~6,2Fs~%"
                      f r-p c-c-p (if c-p "Y" "N") time-p 
                      r-n c-c-n (if c-n "Y" "N") time-n))))
      (close res-tex)
      (close res-txt))))

(defun sat-tests (test-files &key (system *system-name*) (timeout *sat-timeout*)
                             (start-p 1) (start-n start-p))
  (let* ((tex-filename (concatenate 'string *results-dir* system "_t98_sat_res.tex"))
         (txt-filename (concatenate 'string *results-dir* system "_t98_sat_res.txt"))
         (mode (if (or (not (probe-file txt-filename)) (eql start-p 1))
                   ':supersede
                 ':append))
         (res-tex (myopen tex-filename :direction :output :if-exists mode))
	 (res-txt (myopen txt-filename :direction :output :if-exists mode)))
    (unwind-protect
        (dolist (f test-files)
          (multiple-value-bind 
              (r-p c-p time-p) 
              (run-sat-test (concatenate 'string *sat-data-dir* f "_p.alc") 
                            :timeout timeout :prov T :start start-p)
            (multiple-value-bind
                (r-n c-n time-n) 
                (run-sat-test (concatenate 'string *sat-data-dir* f "_n.alc")
                              :timeout timeout :prov nil :start start-n)
              (format res-tex "~A & " (tex-string f))
              (format res-tex "~A & ~A & ~,2Fs & " r-p (if c-p "Y" "N") time-p)
              (format res-tex "~A & ~A & ~,2Fs \\\\~%" r-n (if c-n "Y" "N") time-n)
              (format res-txt "~20A ~5D ~A ~6,2Fs ~5D ~A ~6,2Fs~%"
                      f r-p (if c-p "Y" "N") time-p r-n (if c-n "Y" "N") time-n))))
      (close res-tex)
      (close res-txt))))

(defun appn-tests (test-files &key (system *system-name*) (timeout *appn-timeout*))
  (let ((res-tex (myopen (concatenate 'string *results-dir* system "_appn_kb_res.tex")
                         :direction :output :if-exists :supersede))
	(res-txt (myopen (concatenate 'string *results-dir* system "_appn_kb_res.txt")
                         :direction :output :if-exists :supersede)))
    (unwind-protect
        (dolist (f test-files)
          (multiple-value-bind 
              (r-p c-c c-p) 
              (run-appn-test (concatenate 'string *appn-data-dir* f) 
                             :timeout timeout)
            (format res-tex "~A & " (tex-string f))
            (format res-tex "~:D & ~A & ~A \\\\~%" c-c r-p c-p)
            (format res-txt "~20A ~5D ~A ~A~%" f c-c r-p c-p)))
      (close res-tex)
      (close res-txt))))

(defun synth-kb-tests (test-files &key (system *system-name*) (timeout *appn-timeout*))
  (let ((res-tex (myopen (concatenate 'string *results-dir* system "_synth_kb_res.tex")
                         :direction :output :if-exists :supersede))
	(res-txt (myopen (concatenate 'string *results-dir* system "_synth_kb_res.txt")
                         :direction :output :if-exists :supersede)))
    (unwind-protect
        (dolist (f test-files)
          (multiple-value-bind 
              (r-p c-c c-p) 
              (run-appn-test (concatenate 'string *synth-data-dir* f) 
                             :timeout timeout)
            (format res-tex "~A & " (tex-string f))
            (format res-tex "~:D & ~A & ~A \\\\~%" c-c r-p c-p)
            (format res-txt "~8A ~5D ~A ~A~%" f c-c r-p c-p)))
      (close res-tex)
      (close res-txt))))

(defun rand-kb-tests (test-files &key (system *system-name*) (timeout *appn-timeout*))
  (let ((res-tex (myopen (concatenate 'string *results-dir* system "_rand_kb_res.tex")
                         :direction :output :if-exists :supersede))
	(res-txt (myopen (concatenate 'string *results-dir* system "_rand_kb_res.txt")
                         :direction :output :if-exists :supersede)))
    (unwind-protect
        (dolist (f test-files)
          (multiple-value-bind 
              (r-p c-c c-p) 
              (run-appn-test (concatenate 'string *random-data-dir* f) 
                             :timeout timeout)
            (format res-tex "~A & " (tex-string f))
            (format res-tex "~:D & ~A & ~A \\\\~%" c-c r-p c-p)
            (format res-txt "~12A ~5D ~A ~A~%" f c-c r-p c-p)))
      (close res-tex)
      (close res-txt))))

(defun abox-tests (test-files &key (system *system-name*) (timeout *abox-timeout*)
                              (abox-data-dir *abox-data-dir*)
                              (start 1))
  (let ((res-tex (myopen (concatenate 'string *results-dir* system "_t98_abox_res.tex")
                         :direction :output :if-exists :supersede))
	(res-txt (myopen (concatenate 'string *results-dir* system "_t98_abox_res.txt")
                         :direction :output :if-exists :supersede)))

    (unwind-protect
        (dolist (f test-files)
          (multiple-value-bind 
              (r-p c-c inds c-p time-p) 
              (run-abox-test (concatenate 'string abox-data-dir f ".akb") 
                             :timeout timeout :start start)
            (format res-tex "~A & " (tex-string f))
            (format res-tex "~:D & ~A & ~A & ~,2Fs \\\\~%"
                    c-c r-p (if c-p "Y" "N") time-p)
            (format res-txt "~20A ~5D ~5D ~3D ~A ~10,2Fs~%"
                    f c-c inds r-p (if c-p "Y" "N") time-p)))
      (close res-tex)
      (close res-txt))))
