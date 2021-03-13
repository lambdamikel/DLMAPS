;;; -*- Mode: LISP; Package: CL-USER; Base: 10; Syntax: Ansi-common-lisp -*-


(in-package cl-user)

(eval-when (:compile-toplevel :laod-toplevel :execute)
  (defparameter *prover*
    (cond ((boundp '*reasoner*)
           (pushnew :fact *features*)
           *reasoner*)
          ((boundp '*dl-prover-name*)
           (pushnew :race *features*)
           *dl-prover-name*)
          (t (error "Cannot determine prover.")))))
    
#+:fact
(defparameter *fact-logic* 'k)

#+:fact
(defun *test-concept* (concept)
  (alc-concept-coherent concept :logic *fact-logic*))

#+:race
(defun *test-concept* (concept)
  (satisfiable concept 'tancs))



;;; ======================================================================

(when *load-pathname*
  (setf (logical-pathname-translations "tancs")
        (list (list "tancs;**;*.*" 
	            (concatenate 'string 
		                 (directory-namestring *load-pathname*)
		                 #+:ALLEGRO "**/*.*"
                                 #+:MCL "**:*.*")))))

(defparameter *default-timeout* 600) ; in seconds

;;; ======================================================================

;;; The parser has been developed based on the infix reader
;;; provided by Mark Kantrowitz.


(defparameter *infix-readtable* (copy-readtable nil))
(defparameter *normal-readtable* (copy-readtable nil))

(defun infix-reader (stream subchar arg)
  (declare (ignore arg subchar))
  (let ((*readtable* *infix-readtable*)
        (*normal-readtable* *readtable*))
    (read-char stream) ; get rid of opening left parenthesis
    (read-infix stream)))

(set-dispatch-macro-character #\# #\I #'infix-reader *readtable*) ; was #\# #\$
(set-dispatch-macro-character #\# #\I #'infix-reader *infix-readtable*)

(defmacro infix-error (format-string &rest args)
  `(error ,format-string
	  ,@args))

(defun read-infix (stream)
  (let* ((result (gather-superiors '\) stream)) ; %infix-end-token%
	 (next-token (read-token stream)))
    (unless (same-token-p next-token '\)) ; %infix-end-token%
      (infix-error "Infix expression ends with ~S." next-token))
    result))

(defun read-regular (stream)
  (let ((*readtable* *normal-readtable*))
    (read stream t nil t)))

;;; ********************************
;;; Reader Code ********************
;;; ********************************

(defun same-operator-p (x y)
  (same-token-p x y))

(defun same-token-p (x y)
  (and (symbolp x)
       (symbolp y) 
       (string-equal (symbol-name x) (symbol-name y))))

;;; Peeking Token Reader

(defvar *peeked-token* nil)
(defun read-token (stream)
  (let ((result (if *peeked-token*
		    (pop *peeked-token*)
		  (read stream nil nil))))
    (if (and (characterp result)
             (or 
              (char= result #\Newline)
              (char= result #\Return)))
	(read-token stream)
      result)))

(defun peek-token (stream)
  (unless *peeked-token*
    (push (read stream nil nil) *peeked-token*))
  (car *peeked-token*))

;;; Hack to work around + and - being terminating macro characters,
;;; so 1e-3 doesn't normally work correctly.

(defun fancy-number-format-p (left operator stream)
  (when (and (symbolp left)
	     (find operator '(+ -) :test #'same-operator-p))
    (let* ((name (symbol-name left))
	   (length (length name)))
      (when (and (valid-numberp (subseq name 0 (1- length)))
		 ;; Exponent, Single, Double, Float, or Long
		 (find (subseq name (1- length))
		       '("e" "s" "d" "f" "l")
		       :test #'string-equal))
	(read-token stream)
	(let ((right (peek-token stream)))
	  (cond ((integerp right)
		 ;; it is one of the fancy numbers, so return it
		 (read-token stream)
		 (let ((*readtable* *normal-readtable*))
		   (read-from-string (format nil "~A~A~A" 
					     left operator right))))
		(t
		 ;; it isn't one of the fancy numbers, so unread the token
		 (push operator *peeked-token*)
		 ;; and return nil
		 nil)))))))

(defun valid-numberp (string)
  (let ((saw-dot nil))
    (dolist (char (coerce string 'list) t)
      (cond ((char= char #\.)
	     (if saw-dot
		 (return nil)
		 (setq saw-dot t)))
	    ((not (find char "01234567890" :test #'char=))
	     (return nil))))))

;;; Gobbles an expression from the stream.

(defun gather-superiors (previous-operator stream)
  "Gathers an expression whose operators all exceed the precedence of
   the operator to the left."
  (let ((left (get-first-token stream)))
    (loop
      (setq left (post-process-expression left))
      (let ((peeked-token (peek-token stream)))
	(let ((fancy-p (fancy-number-format-p left peeked-token stream)))
	  (when fancy-p
	    ;; i.e., we've got a number like 1e-3 or 1e+3 or 1f-1
	    (setq left fancy-p
		  peeked-token (peek-token stream))))
	(unless (or (operator-lessp previous-operator peeked-token)
		    (and (same-operator-p peeked-token previous-operator)
			 (operator-right-associative-p previous-operator)))
	  ;; The loop should continue when the peeked operator is
	  ;; either superior in precedence to the previous operator,
	  ;; or the same operator and right-associative.
	  (return left)))
      (setq left (get-next-token stream left)))))

(defun get-first-token (stream)
  (let ((token (read-token stream)))
    (if (token-operator-p token)
	;; It's an operator in a prefix context.
	(apply-token-prefix-operator token stream)
	;; It's a regular token
	token)))

(defun apply-token-prefix-operator (token stream)
  (let ((operator (get-token-prefix-operator token)))
    (if operator
	(funcall operator stream)
	(infix-error "Not a prefix operator: ~S" token))))

(defun get-next-token (stream left)
  (let ((token (read-token stream)))
    (apply-token-infix-operator token left stream)))

(defun apply-token-infix-operator (token left stream)
  (let ((operator (get-token-infix-operator token)))
    (if operator
	(funcall operator stream left)
	(infix-error "Not an infix operator: ~S" token))))

;;; Fix to read-delimited-list so that it works with tokens, not
;;; characters.

(defun infix-read-delimited-list (end-token delimiter-token stream)
  (do ((next-token (peek-token stream) (peek-token stream))
       (list nil))
      ((same-token-p next-token end-token)
       ;; We've hit the end. Remove the end-token from the stream.
       (read-token stream)
       ;; and return the list of tokens.
       ;; Note that this does the right thing with [] and ().
       (nreverse list))
    ;; Ignore the delimiters.
    (when (same-token-p next-token delimiter-token)
      (read-token stream))
    ;; Gather the expression until the next delimiter.
    (push (gather-superiors delimiter-token stream) list)))


;;; ********************************
;;; Precedence *********************
;;; ********************************


(defparameter *operator-ordering* 
    '(( \( \: )
      ( box pos )
      ( ~ )
      ( / )
      ( & )
      ( |\|| )
      ( => <= )
      ( <=> <~> )
      ( \, \.)			; do not delete! Needed for reading!
      ( \) )
      ( %infix-end-token% ))		; end of infix expression
  "Ordered list of operators of equal precedence.")



(defun operator-lessp (op1 op2)
  (dolist (ops *operator-ordering* nil)
    (cond ((find op1 ops :test #'same-token-p)
	   (return nil))
	  ((find op2 ops :test #'same-token-p)
	   (return t)))))

(defparameter *right-associative-operators* '(\.))
(defun operator-right-associative-p (operator)
  (find operator *right-associative-operators*))


;;; ********************************
;;; Define Operators ***************
;;; ********************************

(defvar *token-operators* nil)
(defvar *token-prefix-operator-table* (make-hash-table))
(defvar *token-infix-operator-table* (make-hash-table))
(defun token-operator-p (token)
  (find token *token-operators*))
(defun get-token-prefix-operator (token)
  (gethash token *token-prefix-operator-table*))
(defun get-token-infix-operator (token)
  (gethash token *token-infix-operator-table*))

(eval-when (compile load eval)
  (defmacro define-token-operator (operator-name &key
						 (prefix nil prefix-p)
						 (infix nil infix-p))
    `(progn
       (pushnew ',operator-name *token-operators*)
       ,(when prefix-p
	  `(setf (gethash ',operator-name *token-prefix-operator-table*)
		 #'(lambda (stream)
		     ,@(cond ((and (consp prefix)
				   (eq (car prefix) 'infix-error))
			      ;; To avoid ugly compiler warnings.
			      `((declare (ignore stream))
				,prefix))
			     (t
			      (list prefix))))))
       ,(when infix-p
	  `(setf (gethash ',operator-name *token-infix-operator-table*)
		 #'(lambda (stream left)
		     ,@(cond ((and (consp infix)
				   (eq (car infix) 'infix-error))
			      ;; To avoid ugly compiler warnings.
			      `((declare (ignore stream left))
				,infix))
			     (t
			      (list infix)))))))))

;;; Readtable definitions for characters, so that the right token is returned.
(eval-when (compile load eval)
  (defmacro define-character-tokenization (char function)
    `(set-macro-character ,char ,function nil *infix-readtable*)))


;;; ********************************
;;; Operator Definitions ***********
;;; ********************************

(define-character-tokenization #\&
    #'(lambda (stream char)
	(declare (ignore char stream))
	'&))
(define-token-operator &
    :infix  `(and ,left ,(gather-superiors '& stream)))


(define-character-tokenization #\|
    #'(lambda (stream char)
	(declare (ignore char stream))
	'\|))
(define-token-operator \|
    :infix  `(or ,left ,(gather-superiors '\| stream)))

(define-character-tokenization #\:
    #'(lambda (stream char)
	(declare (ignore char stream))
	'\:))

(defun convert-role (role-number)
  (intern (format nil "R~D" role-number)))

(define-token-operator box
    :prefix (let ((role (gather-superiors '\: stream))
		  (concept 
		    (cond ((same-token-p (peek-token stream) '\:)
			   (read-token stream)
			   (gather-superiors 'box stream))
			  (t
			   (infix-error "Missing formula in box term.")))))
	      `(all ,(convert-role role) ,concept)))

(define-token-operator pos
    :prefix (let ((role (gather-superiors '\: stream))
		  (concept 
		    (cond ((same-token-p (peek-token stream) '\:)
			   (read-token stream)
			   (gather-superiors 'pos stream))
			  (t
			   (infix-error "Missing formula in pos term.")))))
	      `(some ,(convert-role role) ,concept)))

(define-character-tokenization #\~
    #'(lambda (stream char)
	(declare (ignore stream char))
	'\~))
(define-token-operator \~
    :prefix `(not ,(gather-superiors '\~ stream)))

(define-character-tokenization #\(
    #'(lambda (stream char)
	(declare (ignore stream char))
	'\())
(define-token-operator \(
    :infix `(,left ,@(infix-read-delimited-list '\) '\, stream))
    :prefix (let ((list (infix-read-delimited-list '\) '\, stream)))
	      (if (null (rest list))
		  ;; only one element in list. works correctly if list is NIL
		  (first list)
		  ;; several elements in list
		  `(progn ,@list))))

(define-character-tokenization #\)
    #'(lambda (stream char)
	(declare (ignore stream char))
	'\)))
(define-token-operator \)
    :prefix (infix-error "Incomplete expression.")
    :infix (infix-error "Extra close parenthesis."))


(define-character-tokenization #\,
    #'(lambda (stream char)
	(declare (ignore stream char))
	'\,))
(define-token-operator \,
    :infix `(,left ,(gather-superiors '\, stream)))

(define-character-tokenization #\%
    #'(lambda (stream char)
	(declare (ignore char))
	(do ((char (peek-char nil stream t nil t)
		   (peek-char nil stream t nil t)))
	    ((or (char= char #\newline) (char= char #\return)
		 ;; was #\$
;		 (char= char #\))
		 )
	     ;; Gobble characters until the end of the line or the
	     ;; end of the input.
	     (cond ((or (char= char #\newline) (char= char #\return))
		    (read-char stream)
		    (read stream t nil t))
		   (t
		    ;; i.e., return %infix-end-token%
		    (read stream t nil t))))
	  (read-char stream))))

(define-character-tokenization #\<
    #'(lambda (stream char)
	(declare (ignore char))
	(cond ((char= (peek-char nil stream t nil t) #\=)
	       (read-char stream t nil t)
               (if (char= (peek-char nil stream t nil t) #\>)
	         (progn (read-char stream t nil t)
                        '<=>)
                 '<=))
	      ((char= (peek-char nil stream t nil t) #\~)
	       (read-char stream t nil t)
               (read-char stream t nil t)
	       '<~>))))

(define-token-operator =>
    :infix `(implication ,left ,(gather-superiors '=> stream)))

(define-token-operator <=
    :infix `(reverse-implication ,left ,(gather-superiors '=> stream)))

(define-token-operator <=>
    :infix `(equivalence ,left ,(gather-superiors '=> stream)))

(define-token-operator <~>
    :infix `(tilde-equivalence ,left ,(gather-superiors '=> stream)))

(define-token-operator input_formula
    :prefix (cond ((same-token-p (peek-token stream) '\()
                   (read-token stream)
                   `(input-formula ,@(infix-read-delimited-list '\) '\, stream)))
                  (t (error "not expected."))))

(define-token-operator inputformula
    :prefix (cond ((same-token-p (peek-token stream) '\()
                   (read-token stream)
                   `(input-formula ,@(infix-read-delimited-list '\) '\, stream)))
                  (t (error "not expected."))))

(define-token-operator true
    :prefix (progn stream '*top*))
(define-token-operator false
    :prefix (progn stream '*bottom*))

(define-character-tokenization #\.
    #'(lambda (stream char)
	(declare (ignore stream char))
	'\.))
(define-token-operator \.
    :infix `(,left .,(gather-superiors '\. stream)))



;;; ********************************
;;; Syntactic Modifications ********
;;; ********************************

;;; Post processes the expression to remove some unsightliness caused
;;; by the way infix processes the input. Note that it is also required
;;; for correctness in the a<b<=c case. 

(defun post-process-expression (expression)
  (if (and (consp expression)
	   (= (length expression) 3))
      (destructuring-bind (operator left right) expression
	(cond ((and (consp left)
		    (same-operator-p (first left) operator)
		    (find operator '(\| \& begin)
			  :test #'same-operator-p))
	       ;; Flatten the expression if possible
	       (cond #|((and (eq operator '-)
			   (= (length left) 2))
		      ;; -a-b --> (+ (- a) (- b)). 
		      `(+ ,left (- ,right)))
		     ((and (eq operator '/)
			   (= (length left) 2))
		      ;; ditto with /
		      `(/ (* ,(second left) ,right)))|#
		     (t 
		      ;; merges a+b+c as (+ a b c).	     
		      (append left (list right)))))
	      (t
	       expression)))
      expression))

(defun parse (string &optional (verbose nil))
  (let* (;(*package* (find-package 'pdl))
         (*readtable* *infix-readtable*)
         (result (read-from-string (format nil "#I(~A
)" string))))
    (when verbose
      (print result))
    result))


(defun parse-file (filename)
  (let ((*readtable* *infix-readtable*)
        (*peeked-token* nil))
    (with-open-file (stream filename :direction :input)
      (gather-superiors '\. stream))))

;;; ======================================================================

(defun get-parameters (x)
  (values
   (read-from-string 
    (subseq x (+ 2 (search "-K" x)) (search "-C" x)))
   (read-from-string 
    (subseq x (+ 2 (search "-C" x)) (search "-V" x)))
   (read-from-string 
    (subseq x (+ 2 (search "-V" x)) (search "-D" x)))
   (read-from-string 
    (subseq x (+ 2 (search "-D" x)) (search "." x :from-end t)))
   (read-from-string 
    (subseq x (+ 1 (search "." x :from-end t)) (length x)))))

(defvar *clauses*)
(defvar *vars*)
(defvar *depth*)
(defvar *ln-runtime*)

(defun term-satisfiable (term stream timeout-secs)
  (multiple-value-bind (result elapsed-run-time elapsed-real-time
                               elapsed-mf-time
                               elapsed-gc-time bytes-consed)
                       (with-timeout (timeout-secs
                                      (timed-out-handler))  ;; returns '(?)  !!!??
                         (with-timed-form #'(lambda () (*test-concept* term))))
    (declare (ignore elapsed-real-time elapsed-mf-time elapsed-gc-time))
    (setf elapsed-run-time (max 0.0001 elapsed-run-time))
    (incf *ln-runtime* (log elapsed-run-time))
    (format stream "~&i -clauses ~D -vars ~D -depth ~D ~0,2F ~A"
            *clauses* *vars* *depth*
            elapsed-run-time
            (if (eq result 't)
              "S"
              (if (eq result 'nil)
                "U"
                "?")))
    (values result elapsed-run-time bytes-consed)))

(defun less-p (a b)
  (labels ((encode (k c v d i)
	     (+ i (* 100 d) (* 1000 v) (* 10000 c) (* 100000 k))))
    (< (multiple-value-call #'encode (get-parameters (namestring a)))
       (multiple-value-call #'encode (get-parameters (namestring b))))))

(defun bench (p dir stream reference-file)
  (print p *trace-output*)
  (format stream "% Prover ~A" *prover*)
  (format stream "~%% Settings")
  (format stream "~%% Timeout ~D" *default-timeout*)
  (format stream "~%p ~A"
          p)
  (let* ((files 	  
	  (sort 
	   (remove-if-not #'(lambda (file)
			      (search "p-"
				      (namestring file)))
			  (directory dir))
	   #'less-p))
	 (ln-runtime 0)
	 (n-instances nil))
    (unless files
      (warn "No files found."))
    (loop for file in files do
	  (print file *trace-output*)
          (let* ((input-formulas (parse-file file))
                 (last-formula (first (last input-formulas)))
                 (hypotheses nil)
                 (axioms nil)
                 #+:race (tbox (init-tbox 'tancs))
                 )
            (loop for input-formula in input-formulas
		  unless (eq input-formula last-formula) do
                  (destructuring-bind (input-formula name type formula)
		                      input-formula
                    (declare (ignore input-formula name))
                    (ecase type
                      (hypothesis (push formula hypotheses))
                      (axiom (push formula axioms)))))
            (setf axioms (nreverse axioms))
            #+:fact (unless (null axioms)
                      (error "Axioms are not supported in this version."))
            #+:race (loop for axiom in axioms do
                          (case (first axiom)
                            (implication (add-concept-axiom tbox
                                                            (second axiom)
                                                            (third axiom)
                                                            :inclusion-p t))
                            (equivalence (add-concept-axiom tbox
                                                            (second axiom)
                                                            (third axiom)
                                                            :inclusion-p t)
                                         (add-concept-axiom tbox
                                                            (third axiom)
                                                            (second axiom)
                                                            :inclusion-p t))
                            (otherwise (add-concept-axiom tbox
                                                          '*top*
                                                          axiom
                                                          :inclusion-p t))))
            (multiple-value-bind (k c v d i)
		                 (get-parameters (namestring file))
	      (when (= i 1)
		(cond ((not (null n-instances))
		       (format reference-file 
                               "~0,2F"
                               (exp (/ ln-runtime n-instances)))
		       (setf n-instances 0)
		       (setf ln-runtime 0))
		      (t (setf n-instances 0)))
		(format reference-file "~&r p-~A-K~D-C~D-V~D-D~D "
			p
			k
			c
			v 
			d))
	      (incf n-instances)
	      (let ((*depth* d)
		    (*clauses* c)
		    (*vars* v)
		    (*ln-runtime* 0)
                    #+:race (*auto-classify* :lazy)
                    (encode-axioms-start (get-internal-run-time)))
                #+:race (ensure-knowledge-base-state ':tbox-prepared tbox)
                (let ((preencode-time (- (get-internal-run-time) encode-axioms-start)))
                  (when (> preencode-time 0.0)
                    (incf ln-runtime (log preencode-time))))
                (case (and (consp (fourth last-formula))
                           (first (fourth last-formula)))
                  (equivalence (term-satisfiable 
                                `(and (or (not ,(second (fourth last-formula)))
                                          ,(third (fourth last-formula)))
                                      (or (not ,(third (fourth last-formula)))
                                          ,(second (fourth last-formula))))
                                stream
                                *default-timeout*))
                  (otherwise (term-satisfiable
                              `(and (not ,(fourth last-formula))
                                    .,hypotheses)
                              stream
                              *default-timeout*)))
               (incf ln-runtime *ln-runtime*)))))
    (when files 
      (format reference-file 
              "~0,2F"
              (exp (/ ln-runtime n-instances))))))

;;; ======================================================================

(defun tancs-bound-cnf (filename reference-file)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (bench "bound-cnf" "tancs:tancs;bound-cnf;*.*" stream reference-file)))

(defun tancs-bound-cnf-modk (filename reference-file)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (bench "bound-modK" "tancs:tancs;bound-cnf-modK;*.*" stream reference-file)))

(defun tancs-unbound-qbf (filename reference-file)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (bench "unbound-qbf" "tancs:tancs;unbound-qbf;*.*" stream reference-file)))

(defun tancs-unbound-qbf-modk (filename reference-file)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (bench "unbound-qbf-modK" "tancs:tancs;unbound-qbf-modK;*.*" stream reference-file)))

(defun tancs-bound-cnf-modS4 (filename reference-file)
  (let (#+:race (*encode-roles-as-transitive* t)
        #+:race (*encode-roles-as-reflexive* t)
	#+:fact (*fact-logic* 's4))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (bench "bound-cnf-modS4" "tancs:tancs;bound-cnf-modS4;*.*" stream reference-file))))

(defun tancs-unbound-qbf-modS4 (filename reference-file)
  (let (#+:race (*encode-roles-as-transitive* t)
        #+:race (*encode-roles-as-reflexive* t)
	#+:fact (*fact-logic* 's4))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (bench "unbound-qbf-modS4" "tancs:tancs;unbound-qbf-modS4;*.*" stream reference-file))))

(defun tancs-persat (filename reference-file)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (bench "persat" "tancs:tancs;persat;*.*" stream reference-file)))

(defun tancs-persat-modK (filename reference-file)
  (let (#+:race (*encode-roles-as-transitive* t)
        #+:race (*encode-roles-as-reflexive* t)
	#+:fact (*fact-logic* 's4))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (bench "persat-modK" "tancs:tancs;persat-modK;*.*" stream reference-file))))

(defun tancs-persat-modS4 (filename reference-file)
  (let (#+:race (*encode-roles-as-transitive* t)
        #+:race (*encode-roles-as-reflexive* t)
	#+:fact (*fact-logic* 's4))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (bench "persat-modS4" "tancs:tancs;persat-modS4;*.*" stream reference-file))))

(defun run-tancs-tests ()
  (with-open-file (reference-file "tancs:tancs;tancs.result" 
		   :direction :output :if-exists :supersede)
    (format reference-file "%Prover ~A" *prover*)
    (format reference-file "~%%Settings")
    (format reference-file "~%%Timeout ~A" *default-timeout*)
    (tancs-bound-cnf "tancs:tancs;bound-cnf.result" reference-file)))
    (tancs-bound-cnf-modk "tancs:tancs;bound-cnf-modk.result" reference-file)
    (tancs-unbound-qbf "tancs:tancs;unbound-qbf.result" reference-file)
    (tancs-unbound-qbf-modk "tancs:tancs;unbound-qbf-modk.result" 
                            reference-file)
    (tancs-unbound-qbf-modS4 "tancs:tancs;unbound-qbf-modS4.result"
			     reference-file)
    (tancs-bound-cnf-modS4 "tancs:tancs;bound-cnf-modS4.result" 
			   reference-file)
    #+:race (tancs-persat "tancs:tancs;persat.result" reference-file)
    #+:race (tancs-persat-modK "tancs:tancs;persat-modK.result" reference-file)
    #+:race (tancs-persat-modS4 "tancs:tancs;persat-modS4.result" reference-file)
    'done))

    
