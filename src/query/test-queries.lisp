;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: CL-USER; Base: 10 -*-

(in-package :CL-USER)

;;;
;;;
;;;

(defvar *raise-nrql-error* t)

(defvar *skip-p* nil)

(defvar *r*)

(defvar *er*)

(defvar *m*)

(defvar *s*)

(defvar *logfile-directory* "nrql:test-cases;results;")
       
#+:lracer
(defvar *tbox-verbose* t)
           
;;;
;;;
;;;

(defmacro with-skipping-if-necessary (&body body)
  #-:dlmaps
  `(progn
     ,@body)
  #+:dlmaps 
  `(let ((*skip-p* t))
     ,@body))

(defmacro requires-multiprocessing ((&optional 
                                     (skip
                                      #-:multiprocess-queries t
                                      #+:multiprocess-queries nil))
                                    &body body)
  `(let ((*skip-p* ,skip))
     
     ,@(if skip 
           (mapcar #'(lambda (x) 
                       (when (member (first x) '(tc query query1 tbox-query)) x))
                   body)
         body)))


;;;
;;;
;;;

(defmacro => (a b)
  `(or (not ,a) ,b))

(defmacro <=> (a b)
  `(and (=> ,a ,b)
	(=> ,b ,a)))


(defun set-equal (a b &rest args)
  (and (apply #'subsetp a b args)
       (apply #'subsetp b a args)))

(defun ensure-list (arg)
  (if (listp arg)
      arg
    (list arg)))

(defun test-error (error &rest args)
  (if *raise-nrql-error*
      (apply #'cerror "Continue nRQL Test" error args)
    (progn
      (format t "*** THIS IS JUST A WARNING (ERROR-P SET TO NIL): ")
      (apply #'format t error args))))
    

;;;
;;;
;;;

(defun res-equal-p (a b) 
  (cond ((and (consp a) 
              (consp b))
         (set-equal a b :test #'equalp))
        ((and (symbolp a)
              (symbolp b))
         (eq a b))
        (t nil))) 

(defun query-and-answer-equal-p (a b) 
  (and (eq (first a)
	   (first b))
       (res-equal-p (second a) (second b))))


(defun get-all-answers-equal-p (a b) 
  (set-equal a b :test #'query-and-answer-equal-p))


;;;
;;;
;;;

(defvar *queries-and-results* nil)

(defmacro with-logging ((file) &body body)
  `(let ((*queries-and-results* nil))
     (format t "Logging ~A~%" ,file)
     ,@body

     (with-open-file (stream ,file
                             :direction :output :if-exists :supersede)
       (format stream "~S~%" *queries-and-results*))
     
     'done))


(defmacro with-verification ((file) &body body)
  `(let ((*queries-and-results* nil))
     ,@body
     (with-open-file (stream ,file :direction :input)

       (let ((string 
              (format nil "Verifying ~A..." ,file)))
         (when (or #-:lracer *tbox-verbose*
                   #+:lracer t)
           (dotimes (i (length string))
             (princ "-"))
           (terpri))
         (format t string))
       
       (labels ((okay ()
                  (format t "Okay!~%")))

         (let ((input (read stream)))         
           (mapc #'(lambda (result e-result)
                     (let ((q (first result))
                           (r (second result))
                           (pred (third result))

                           (eq (first e-result))
                           (er (second e-result))
                           (pred1 (third e-result)))
		       
		       
                       (when
                           #-:lracer racer:*tbox-verbose*
                         #+:lracer t
                         (format t "~%~%Query:~%")
                         (pprint q) (terpri)
                         (format t "~%>>>>>>> Racer Result:~%")
                         (pprint result)
                         (format t "~%~%>>>>>>> Logfile Expected Result:~%")
                         (pprint e-result)
                         (terpri)
                         (terpri))

                       (when (or (not (equalp q eq))
                                 (not (equalp pred pred1)))
			 (break "Bad Log File ~A: ~A ~A" ,file q eq))

                       (cond ((or (eq r :timeout)
                                  (eq er :timout))

                              (format t "~%*** WARNING: TIMEOUT ENCOUNTERED, CONTINUING~%"))
                             
                             ((or (eq r :acquire-process-failed-pool-size-exceeded)
                                  (eq er :acquire-process-failed-pool-size-exceeded))
                              
                              (format t "~%*** WARNING: ACQUIRE PROCESS FAILED, MAX. POOL SIZE REACHED, CONTINUING~%"))
			     
                             ((and (consp r) (consp er))

			      (let* ((*raise-nrql-error*
                                      (and *raise-nrql-error*
                                           (not 
                                            (or (ts::tree-find r :timeout)
                                                (ts::tree-find er :timeout)
                                                (ts::tree-find r :acquire-process-failed-pool-size-exceeded)
                                                (ts::tree-find er :acquire-process-failed-pool-size-exceeded)))))

                                     (pred2 (if (eq pred 'get-all-answers-equal-p)
                                                #'query-and-answer-equal-p
                                              #'equalp)))
                                
				(if (not (funcall pred r er))

				    (let ((m (set-difference er r :test pred2))
					  (s (set-difference r er :test pred2)))
                                
                                      (setf *er* er
                                            *r* r
                                            *m* m
                                            *s* s)
                                      
				      (format t "~%~%*** MISSING TUPLES:~%")
                                      (pprint m)
                                      (format t "~%~%*** UNEXPECTED TUPLES:~%")
				      (pprint s)
				      (terpri)
                                      (terpri)

				      (test-error "DIFFERENCE DETECTED FOUND: ~A EXPECTED: ~A~%"
						  (length r) (length er) m s))
				  (okay))))
                             
                             (t (let ((*raise-nrql-error*
                                       (and *raise-nrql-error*
                                            (not 
                                             (or (and (consp r) 
                                                      (ts::tree-find r :timeout))
                                                 (and (consp er) 
                                                      (ts::tree-find er :timeout))
                                                 (and (consp r)
                                                      (ts::tree-find r :acquire-process-failed-pool-size-exceeded))
                                                 (and (consp er) 
                                                      (ts::tree-find er :acquire-process-failed-pool-size-exceeded)))))))
                                  
                                  (if (not (funcall pred r er))

                                      (test-error "DIFFERENCE DETECTED FOUND: ~A EXPECTED: ~A~%"
                                                  r er)
                                    
                                    (okay)))))))

                 (reverse *queries-and-results*)
                 (reverse input)))))
     'done))

(defmacro rr (body &key (test 'res-equal-p))
  `(let ((res
          ,body))

     (push (list ',body res ',test) *queries-and-results*)

     (when #-:lracer nil ; racer:*tbox-verbose*
       #+:lracer t 
       (pprint ',body)
       (pprint res)
       (terpri) (terpri))
     
     res))

(defun racer-load-kb (fn)
  (load (format nil "nrql:test-cases;~A.racer" fn)))


(defmacro without-nrql-timeout (&body body) 
  `(let ((*timeout* nil))
     ,@body))

;;;
;;;
;;;

(defvar *counter* 0)

(defvar *create-log-file-p* nil)

(defvar *mode* nil)

(defvar *timeout* nil)

;;;
;;;
;;;

(defun reset1 ()
  (setf *counter* 0
        *queries-and-results* nil))

(defmacro tc (body &key (test 'res-equal-p) (error-p t) name)
  `(let ((name 
          (format nil
                  "~A~A.ntc" 
                  *logfile-directory*
                  (or ',name
                      (incf *counter*)))))
     (unless *skip-p*
       (case *mode*
         (:logging 
          (with-logging (name)  
                        (rr ,body :test ,test)))
         (:verification 
          (let ((*raise-nrql-error* ,error-p))
            (with-verification (name)
                               (rr ,body :test ,test))))
         (otherwise 
          (rr ,body))))))

;;;
;;;
;;;

(defvar *abox* nil)

(defvar *tbox* nil)

(defmacro query0 (query res-args)
  `(tc (racer-answer-query ',res-args ',query 
                           :timeout *timeout* :abox (or *abox* (current-abox)))))

(defmacro query1 (query res-args)
  `(tc (racer-prepare-query ',res-args ',query
                            :timeout *timeout* :abox (or *abox* (current-abox)))))

(defmacro tbox-query (query res-args)
  `(tc (racer-answer-tbox-query ',res-args ',query
                                :timeout *timeout* :tbox (or *tbox* (current-tbox)))))

;;;
;;;
;;;

(defun load-kbs ()
  (full-reset)
  (mapc #'racer-load-kb 
        '("family-1"
          "told-values"
          "negated-roles"
          "gemein"))
  (owl-read-file (format nil "nrql:test-cases;annotation-test.owl"))
  (owl-read-file (format nil "nrql:test-cases;constraint-query-dtp.owl")))
  

;;;
;;;
;;;

(defconstant +test-cases+
  '((progn 

      (full-reset)

      (enable-data-substrate-mirroring)

      (owl-read-file "nrql:test-cases;owl-mirror.owl")

      (query0 (AND (?X |http://www.owl-ontologies.com/unnamed.owl#person|)
                   (?*X ?*NAME |http://www.owl-ontologies.com/unnamed.owl#name|)
                   (?*NAME ((:PREDICATE (SEARCH "Wessel")) 
                            ((:PREDICATE (SEARCH "Michael"))
                             (:PREDICATE (SEARCH "Achim")))))
                   (?*X ?*AGE |http://www.owl-ontologies.com/unnamed.owl#age|)
                   (?*AGE ((:PREDICATE (< 40)))))
              (?X ?*NAME ?*AGE)))


    (progn 
      
      (full-reset)

      (owl-read-file "nrql:test-cases;owl-datatype-properties3.owl")

      (tc
       (FIRERULE
        (?X (A |http://www.owl-ontologies.com/unnamed.owl#name|))
        ((RELATED
          ?X
          (NEW-IND NAME-OF ?X)
          |http://www.owl-ontologies.com/unnamed.owl#name|)
         (CONSTRAINED
          (NEW-IND NAME-OF ?X)
          (NEW-IND STRING-VALUE-OF-NAME-OF ?X)
          RACER-INTERNAL%HAS-STRING-VALUE)
         (:CONSTRAINTS
          (STRING= (NEW-IND STRING-VALUE-OF-NAME-OF ?X)
                   (told-value (|http://www.owl-ontologies.com/unnamed.owl#name|
                                ?X)))))))

      (tc 
       (retrieve (?x ?Y)
                 (?x ?y (:constraint 
                         |http://www.owl-ontologies.com/unnamed.owl#name|
                         |http://www.owl-ontologies.com/unnamed.owl#name| 
                         string=))))

      ;;; 

      (full-reset)

      (enable-data-substrate-mirroring)
      (owl-read-file "nrql:test-cases;owl-datatype-properties3.owl")

      (tc 
       (retrieve (?*x $?*x-age ?*y $?*y-age) 
                 (and (?*x $?*x-age |http://www.owl-ontologies.com/unnamed.owl#age|)
                      (?*y $?*y-age |http://www.owl-ontologies.com/unnamed.owl#age|)
                      ($?*x-age $?*y-age (:satisfies (:predicate =))))))

      (tc 
       (retrieve (?*x $?*x-name ?*y $?*y-name) 
                 (and (?*x $?*x-name |http://www.owl-ontologies.com/unnamed.owl#name|)
                      (?*y $?*y-name |http://www.owl-ontologies.com/unnamed.owl#name|)
                      ($?*x-name $?*y-name (:satisfies (:predicate string=))))))

      ;;;

      (full-reset)

      (add-role-assertions-for-datatype-properties)
      (owl-read-file "nrql:test-cases;owl-datatype-properties3.owl")

      (tc
       (retrieve (?x ?y) (?x ?y (:constraint 
                                 |http://www.owl-ontologies.com/unnamed.owl#name|
                                 |http://www.owl-ontologies.com/unnamed.owl#name|
                                 string=)))))

    (progn 

      (full-reset)

      (racer-load-kb "keno2")

      (query0 (AND (?Z CUP) (?Y ?Z ON) (?X ?Z NEAR) (?Z ?C NEAR)) (?Z))

      (query0 (AND (?Y ?Z ON) (?X ?Z NEAR) (?Z ?C NEAR)) (?Z ?Y))
      
      (query0 (AND (?Y ?Z ON) (?X ?Z NEAR) (?Z ?C NEAR)) (?z))

      (query0 (AND (?Y ?Z ON) (?X ?Z NEAR) (?Z ?C NEAR)) (?Z))

      (query0 (AND (?Z ?Y ON) (?Z ?X NEAR) (?Z ?C NEAR)) (?z)))

    (progn 
      (full-reset)

      (racer-load-kb "family-1-no-signature")

      (query0 (and (?x man) (not (:same-as ?x charles))) (?x))

      (query0 (and (?x man) (:same-as ?x charles)) (?x))
	
      (query0 (betty top) (betty))

      (query0 (and (?x top)) (?x))
	
      (query0 (and (?x woman)) (?x))
	
      (query0 (and (?x woman) (?y woman)) (?x))

      (query0 (and (?x woman) (?y woman)
                   (not (?x ?y has-descendant))
                   (not (?y ?x has-descendant)))

              (?x ?y))

      (query0 (or (betty (and woman))                       
                  (and (?x woman)
                       (betty top)
                       (or (?x ?y has-sibling)
                           (?x ?y has-child))
                       (?y man)))
         
              (?x ?y betty))


      (query0 (or                       
               (betty (and woman))                       
               (and (?x woman)
                    (betty top)
                    (or (?x ?y has-sibling)
                        (?x ?y has-child))
                    (?y man)))
         
              (betty ?x ?y))

      (query0 (or                       
               (and (?x woman)
                    (?y human)
                    (?z man)                   
                    (?x ?y has-child)
                    (?y ?z has-sibling)))                                                   
              (?x ?y ?z))

      (query0 (and (charles top)
                   (?x man))                    
              (?x charles))

      (query0 (betty man)
              nil)

      (query0 (AND (BETTY MAN)
                   (NOT (?Y BETTY (INV HAS-CHILD))))
              (betty ?y))
	
      (query0 (betty top)
              ())
	
      (query0 (betty top)
              (betty))

      (query0 (and (not (?x ?y has-child))
                   (not (?y ?x has-child)))                     
              (?x ?y))
	
	
      (query0 (and (inv (?x ?y has-child)))                    
              (?x ?y))
	
      (query0 (and
               (betty woman)
               (and (?x woman)
                    (?x man)))                       
              (betty))

      (query0 (and
               (or (?x woman) (?x man)))
              (?x))

      (query0 (or (?x woman) (?y man))
              (?x))

      (query0 (?x man)
              (?x))

      (query0 (not (?x man))
              (?x))

      (query0 (?x (not c))
              (?x))

      (query0 (not (?x c))
              (?x))

      (query0 (not (?x ?y has-sibling))
              (?x ?y))

      (query0 (?x ?y has-sibling)
              (?x ?y))

      (query0 (?x top)
              (?x))

      (query0 (and (betty woman) (?x woman))
              (?x))
	
      (query0 (not (and (betty woman)))
              (betty))

      (query0 (or (top ?x) (top ?y))
              (?x ?y))
	
      (query0 (not (?x (not c)))
              (?x))

      (query0 (or (?x c) (?x (not c)) )                         
              (?x))

      (query0 (or (?x c) (?x (not c)))                         
              (?x))

      (query0 (or (?x c) (not (?x c)))                         
              (?x))

      (query0 (inv (and (?x ?y has-child) (?x woman) (?y man)))                         
              (?x ?y))
	
      (query0 (and (?x ?y has-child) (?x woman) (?y man))                         
              (?x ?y))
	
      (query0 (not (and (betty woman) (?x woman)))
              (?x))
	
      (query0 (and (?x woman) (?y man))
              (?y))
	
      (query0 (and (?x woman) (?x man))
              (?x))
	
      (query0 (and (?x (not woman))) 
              (?x))

      (query0 (and (?x (not woman))) 
              (?x))
	
      (query0 (and (?x woman) (?y woman) (not (?x ?y has-descendant))) 
              (?x ?y))

      (query0 (and (?x top)) (?x))
	
      (query0 (and (?a woman)) (?a))
	
      (query0 (and (?c mother)) (?c))
	
      (query0 (and (?x man)) (?x))

      (query0 (and (?z brother)) (?z))

      (query0 (and (?t uncle)) (?t))

      (query0 (and (?x father)) (?x))

	
      (query0 (not (and (betty woman)))
              (betty))
	
      (query0 (or (?x c) (?x (not c)))                         
              (?x))
	
      (query0 (and (?x woman) (?y man))
              (?x ?y))

      (query0 (and (?x (and c d)))
              (?x))

      (query0 (not (and (betty woman)))
              (betty))

      (query0 (or (?x human) (?x (not human)))                         
              (?x))

      (query0 (and (?x woman) (?y man))
              (?x ?y))

      (query0 (or (?x woman) (?y man))
              (?x ?y))

      (query0 (and (?x top)) (?x))
	
      (query0 (and (?a woman)) (?a))
	
      (query0 (and (?b woman)) (?b))

      (query0 (and (?c mother)) (?c))
	
      (query0 (and (?x woman) (?x ?y has-child)) (?x ?y))

      (query0 (and (?x man)) (?x))

      (query0 (and (?z brother)) (?z))

      (query0 (and (?t uncle)) (?t))

      (query0 (and (?x father)) (?x))
	
      (query0 (and (?a man) (?a ?b has-child)) (?a ?b))

      (query0 (and (?c man) (?a ?b has-child) (?b ?c has-child) ) (?a ?b ?c))

      (query0 (and (?x uncle)) (?x))

      (query0 (?x (an age)) (?x))
	
      (query0 (?x (a name)) (?x))

      (query0 (and (?x woman) (?x (> age 50))) (?x))

      (query0 (and (?x man) (?x (> age 35))) (?x))

      (query0 (and (?x human) (?x (> age 18))) (?x))

      (query0 (and (?x human) (?x (< age 80))) (?x))

      (query0 (and (?x woman) (?x (and (> age 70) (< age 100)))) (?x))

      (query0 (and (?x (string= name "Alice Smith"))) (?x))
	
      (query0 (and (alice (string= name "Alice Smith"))) (alice))
	
      (query0 (and (alice (string= name "Alice Doe"))) (alice))

      (query0 (and (?x (a name)) (?x (an age))
                   (and (?x (or (string= name "Alice Smith")
                                (string= name "Betty Doe")))
                        (?x (or (> age 50)
                                (< age 100)))))         
              (?x))
	
      (query0 (and (?x (a name)) (?x (an age))
                   (or (?x (or (string= name "Alice Smith")
                               (string= name "Betty Doe")))
                       (?x (or (> age 50)
                               (< age 100)))))
	 
              (?x))

      (query0 (?x ?y (:constraint age age <)) (?x ?y))

      (query0 (?x ?x (:constraint (has-mother age) (has-father age) <)) (?x ?x))
	
      (query0 (and (?x ?mother has-mother) 
                   (?x ?father has-father)
                   (?mother ?father (:constraint age age <))) (?x ?mother ?father))

      (query0 (and (?x ?x loves))
              (?x))
	
      (query0 (and (?x ?y loves))
              (?x ?y))

      (query0 (and (?x ?x (:constraint age age <)))
              (?x))

      (query0 (not (and (?x ?x (:constraint age age <))))
              (?x))
	
      (query0 (?x ?x (:constraint 
                      (has-father has-mother age) 
                      (has-mother has-mother age) =))
              (?x))

      (query0 (and (?x ?mother has-mother) (?x ?father has-father)
                   (?x ?y has-sibling))
              (?x ?mother ?father))

      (query0 (and (?x ?mother has-mother) (?x ?father has-father)
                   (or (?mother ?father has-brother)
                       (?father ?mother has-sister)))
              (?x ?mother ?father))

      (query0 (alice betty (:constraint (age) (age) >)) ())

      (query0 (?x ?y (:constraint (age) (age) <)) (?x ?y))
	
      (query0 (alice ?y (:constraint (age) (age) <)) (alice ?y) )
	
      (query0 (inv (alice ?y (:constraint (age) (age) <))) (alice ?y))
	
      (query0 (inv (alice ?y (:constraint (age) (age) <))) ())

      (query0 (alice ?y (:constraint (age) (age) >)) ())

      (query0 (alice ?y (:constraint (age) (age) >)) (alice ?y))

      (query0 (not (alice ?y (:constraint (age) (age) <))) (alice ?y))
	
      (query0 (and (?X ?m HAS-MOTHER)  (?x ?f has-father) (?m ?f (:constraint age age >))) (?x ?m ?f))
	
      (query0 (and (?X ?m HAS-MOTHER)  (?x ?f has-father) (?m ?f (:constraint age age <))) (?x ?m ?f))
	
      (query0 (and (?x ?m has-mother) (?x ?f has-father)) (?x ?m ?f))
	
      (query0 (not (and (?x ?m has-mother) (?x ?f has-father))) (?x ?m ?f))

      (query0 (or (not (?x ?m has-mother)) (not (?x ?f has-father))) (?x ?m ?f))
	
      (query0 (and (?x ?m has-mother) (?x ?f has-father)) (?x))

      (query0 (not (and (?x ?m has-mother) (?x ?f has-father))) (?x))

      (query0 (or (not (?x ?m has-mother)) (not (?x ?f has-father))) (?x))

      (query0 (and (?X ?m HAS-MOTHER)  
                   (?x ?f has-father) 
                   (?m ?f (:constraint age age <))) (?x ?m ?f))
	
      (query0 (not (and (?X ?m HAS-MOTHER)  
                        (?x ?f has-father) 
                        (?m ?f (:constraint age age <)))) (?x ?m ?f))

      (query0 (or (not (?X ?m HAS-MOTHER))
                  (not (?x ?f has-father))
                  (not (?m ?f (:constraint age age <)))) (?x ?m ?f))

      (query0 (?x ?x (:constraint (has-mother age) (has-father age) <)) (?x))

      (query0 (?x ?x (:constraint (has-mother age) (has-father age) <)) (?x))

      (query0 (not (?x ?x (:constraint (has-mother age) (has-father age) <))) (?x))
	
      (query0 (?x ?x (:constraint
                      (has-mother age)
                      (has-father age) <)) (?x))

      (query0 (?x ?x (:constraint 
                      (has-father has-mother age)
                      (has-mother has-mother age) =))
              (?x))
	
      (query0 (not (?x ?x (:constraint 
                           (has-father has-mother age)
                           (has-mother has-mother age) =)))
              (?x))
	
      (query0 (and (?x top) (?y top))
              (?x ?y))

      (query0 (?x ?y (:constraint 
                      (has-father has-mother age)
                      (has-mother has-mother age) =))
              (?x ?y))
	
      (query0 (not (?x ?y (:constraint 
                           (has-father has-mother age)
                           (has-mother has-mother age) =)))
              (?x ?y))
	
      (query0 (?x ?y (:constraint 
                      (has-father has-mother age)
                      (has-mother has-father age) >=))
              (?x ?y))
	
      (query0 (not (?x ?y (:constraint 
                           (has-father has-mother age)
                           (has-mother has-father age) >=)))
              (?x ?y))

      (query0 (?x ?y (:constraint 
                      (has-father has-mother age)
                      (has-mother has-mother age) >=))
              (?x ?y))
	
      (query0 (not (?x ?y (:constraint 
                           (has-father has-mother age)
                           (has-mother has-mother age) >=)))
              (?x ?y))
	
      (query0 (?x eve (:constraint 
                       (has-father has-mother age)
                       (has-mother has-mother age) >=))
              (?x eve))

      (query0 (not (?x eve (:constraint 
                            (has-father has-mother age)
                            (has-mother has-mother age) >=)))
              (?x eve))

      (query0 (alice ?y (:constraint (age) (age) >)) ())

      (query0 (alice ?y (:constraint (age) (age) >)) (alice ?y))

      (query0 (alice ?y (:constraint (age) (age) >)) ())

      (query0 (alice ?y (:constraint (age) (age) >)) (alice ?y))
	

      (query0 (?x ?y (:constraint (age) (age) <)) (?x ?y))

      (query0 (?x ?y (:constraint (age) (age)
                      (= age-1 (+ age-2 30))))
              (?x ?y))

      (query0 (?x ?y (:constraint (age) (age)
                      (<= (+ age-1 30) age-2 )))
              (?x ?y))

      (query0 (?x ?y (:constraint (age) (age) =)) (?x ?y))
	
      (query0 ($?x $?y (:constraint (age) (age) =)) ($?x $?y))

      (query0 (and (?x woman) (?y top) (?x (:has-known-successor has-child))) (?x))
	
      (query0 (not (and (?x woman) (?y top) (?x (:has-known-successor has-child)))) (?x))
	
      (query0 (and (?x mother) (?x ?y has-child)) (?x)))

    (progn 

      (full-reset)
      
      (racer-load-kb "owl-datatype-properties")

      ;;; p1 - integer p2 - string p3 - boolean - wird in Racer als
      ;;; string behandelt; die interne Rolle heisst
      ;;; has-string-value; aber als datatype-role-range kommt
      ;;; "boolean" zurueck; typing TRUE FALSE funktioniert ebenfalls
      ;;; noch nicht
      ;;; 

      ;;;
      ;;; Basic AN
      ;;; 

      (query0 (?x (an |http://a.com/ontology#p1|)) (?X))
	
      (query0 (?X (an |http://a.com/ontology#p2|)) (?x))

      (query0 (?X (an |http://a.com/ontology#p3|)) (?x))

      ;;;
      ;;; Typing 
      ;;;
	
      (query0 (?X (some |http://a.com/ontology#p1| top)) (?x))

      (query0 (?X (some |http://a.com/ontology#p1| integer)) (?x))

      (query0 (?X (some |http://a.com/ontology#p1| string)) (?x))

      (query0 (?X (some |http://a.com/ontology#p1| boolean)) (?x))

      ;;;
      ;;;
      ;;;

      (query0 (?X (some |http://a.com/ontology#p2| top)) (?x))

      (query0 (?X (some |http://a.com/ontology#p2| integer)) (?x))

      (query0 (?X (some |http://a.com/ontology#p2| string)) (?x))

      (query0 (?X (some |http://a.com/ontology#p2| boolean)) (?x))


      ;;;
      ;;;
      ;;;

      (query0 (?X (some |http://a.com/ontology#p3| top)) (?x))

      (query0 (?X (some |http://a.com/ontology#p3| integer)) (?x))

      (query0 (?X (some |http://a.com/ontology#p3| string)) (?x))

      (query0 (?X (some |http://a.com/ontology#p3| boolean)) (?x))
	
	
      ;;;
      ;;; Basic NO
      ;;;
	
      (query0 (?X (no |http://a.com/ontology#p1|)) (?x))

      (query0 (?X (no |http://a.com/ontology#p2|)) (?x))

      (query0 (?X (no |http://a.com/ontology#p3|)) (?x))

      ;;;
      ;;; Extended AN
      ;;;
	
      (query0 (?X (an |http://a.com/ontology#p1| integer)) (?x))

      (query0 (?X (an |http://a.com/ontology#p1| string)) (?x))

      (query0 (?X (an |http://a.com/ontology#p1| cardinal)) (?x))

      (query0 (?X (an |http://a.com/ontology#p1| real)) (?x))

      (query0 (?X (an |http://a.com/ontology#p1| boolean)) (?x))

      ;;;
      ;;;
	
      (query0 (?X (an |http://a.com/ontology#p2| integer)) (?x))

      (query0 (?X (an |http://a.com/ontology#p2| string)) (?x))

      (query0 (?X (an |http://a.com/ontology#p2| cardinal)) (?x))

      (query0 (?X (an |http://a.com/ontology#p2| real)) (?x))

      (query0 (?X (an |http://a.com/ontology#p2| boolean)) (?x))

      ;;;
      ;;;
	
      (query0 (?X (an |http://a.com/ontology#p3| integer)) (?x))

      (query0 (?X (an |http://a.com/ontology#p3| string)) (?x))

      (query0 (?X (an |http://a.com/ontology#p3| cardinal)) (?x))

      (query0 (?X (an |http://a.com/ontology#p3| real)) (?x))

      (query0 (?X (an |http://a.com/ontology#p3| boolean)) (?x))

      ;;;
      ;;; Extended ALL - Typing Check 
      ;;;
	
      (query0 (?X (all |http://a.com/ontology#p1| integer)) (?X))
	
      (query0 (?X (all |http://a.com/ontology#p2| string)) (?X))
	
      (query0 (?X (all |http://a.com/ontology#p3| boolean)) (?X))

      (query0 (?X (all |http://a.com/ontology#p3| string)) (?X))
	
      ;;;
      ;;; Extended Some 
      ;;;

      (query0 (?X (some |http://a.com/ontology#p1| integer)) (?x))

      (query0 (?X (some |http://a.com/ontology#p1| string)) (?x))

      ;;;
      ;;; Extended AT-LEAST
      ;;;

      (query0 (?X (at-least 1 |http://a.com/ontology#p1| (min 3))) (?x))

      (query0 (?X (at-least 1 |http://a.com/ontology#p2| (string= "STRING"))) (?x))

      (query0 (?X (at-least 1 |http://a.com/ontology#p3| (boolean= #T))) (?x))


      ;;;
      ;;; Extended Some - Complex Predicates 
      ;;; 

      (query0 (?X (some |http://a.com/ontology#p1| (equal 3)))
              (?x))

      (query0 (?X (some |http://a.com/ontology#p1| (or (equal 3)
                                                       (equal 2)
                                                       (equal 1)))) 
              (?x))

      (query0 (?X (some |http://a.com/ontology#p1| (or (equal 3)
                                                       (and (min 1)
                                                            (max 2)))))
              (?x))

      (query0 (?X (some |http://a.com/ontology#p1| (not (or (equal 3)
                                                            (and (min 1)
                                                                 (max 2))))))
              (?x))

      ;;;
      ;;; Rolle P1 wurde geschlossen: (at-most 3 p1) 
      ;;;

      (query0 (?X (all |http://a.com/ontology#p1| (or (equal 3)
                                                      (and (min 1)
                                                           (max 2)))))
              (?x))

      (query0 (?X (all |http://a.com/ontology#p2| (string= "STRING")))
              (?x))

      (query0 (?X (all |http://a.com/ontology#p3| (or (boolean= #T)
                                                      (boolean= #F))))
              (?x))

      (query0 (?x (at-most 2 |http://a.com/ontology#p1| (min 4)))
              (?x))

      ;;;
      ;;;
      ;;;
	
      (query0 (?x (at-least 3 |http://a.com/ontology#p1|))         
              (?x (:fillers (|http://a.com/ontology#p1| ?x))))
	
      (query0 (?x (at-least 3 |http://a.com/ontology#p1|))
              (?x (:datatype-fillers
                   (|http://a.com/ontology#p1| ?x))))
	
      )

    (progn 

      (full-reset)

      (racer-load-kb "told-values")

      (query0 (?x top)
              (?x (age ?x) 
                  (:told-value (age ?x))
                  (name ?x) 
                  (:told-value (name ?x))))

      (query0 (?x (and (an age) (string= name "Alice")))
              (?x (age ?x) 
                  (:told-value (age ?x))
                  (name ?x) 
                  (:told-value (name ?x)))))

    (progn 

      (full-reset)

      (racer-load-kb "negated-roles")

      (query0 (?x ?y r) (?x ?y))

      (query0 (not (?x ?y r))  (?x ?y))
	
      (query0 (?x ?y (not r)) (?x ?y))

      (query0 (not (?x ?y (not r))) (?x ?y))
	
      (query0 (?x ?y r) (?x ?y))
	
      (query0 (inv (?x ?y r)) (?x ?y))
	
      (query0 (?x (:has-known-successor r)) (?x))
	
      (query0 (not (?x (:has-known-successor r))) (?x))
	
      (query0 (?x (:has-known-successor (not r))) (?x))
	
      (query0 (not (?x (:has-known-successor (not r)))) (?x)))


    (progn

      (full-reset)

      (racer-load-kb "gemein")
	
      (query0 (?x ?y R) (?x ?y))
	
      ;;; (query0 (?x ?y S) (?x ?y)) 
      ;;; RACER BUG! SOLLTE DAS GLEICHE 
      ;;; WIE (?X ?Y R) LIEFERN!!!

      (query0 (?x ?y f) (?x ?y))
      (query0 (?x ?y g) (?x ?y))

      (query0 (?x ?y (inv f)) (?x ?y))
      (query0 (?x ?y (inv g)) (?x ?y))

      (query0 (?x ?y r1) (?x ?y))

      (query0 (?x ?y (:constraint h1 h1 =)) (?x ?y)))


    (progn 

      (full-reset)

      (owl-read-file "nrql:test-cases;annotation-test.owl")
	
      (query0  (?x (an |http://www.owl-ontologies.com/unnamed.owl#annot1|))
               ( (:annotations (|http://www.owl-ontologies.com/unnamed.owl#annot1| ?x)) ))

      (query0 (?x ?y |http://www.owl-ontologies.com/unnamed.owl#annot2|)
              (?x ?y)))

    (progn 

      (full-reset)

      (racer-load-kb "family-1-no-signature")

      (tbox-query (and (:same-as ?x woman)
                       (?x woman) 
                       (woman woman))
                  (?x))
	
      (tbox-query (and (?y sister) (?x woman) (?x ?y has-child))
                  (?x ?y))
	
      (tbox-query (and (?x top)) (?x))

      (tbox-query (and (top ?x)) (?x))

      (tbox-query (and (:top ?x)) (?x))

      (tbox-query (and (:top ?x)) (?x))
	
      (tbox-query (and (:top ?x) (?x top)) (?x))

      (tbox-query (and (woman woman)) (woman))

      (tbox-query (and (*top* top)) (*top*))

      (tbox-query (and (*top* *top*)) (*top*))
	
      (tbox-query (and (*bottom* bottom)) (*bottom*))

      ;;; (tbox-query (top) (and (top top))) 
      ;; TOP / BOTTOM sind reservierte Keywords! koennen nicht f. Ind-Namen verwendet werden!
	
      (tbox-query (and (not (?x top))) (?x))

      (tbox-query (and (?x woman) (?y sister) (?x ?y has-child)) (?x ?y))
	
      (tbox-query (?x woman) (?x))
	
      (tbox-query (and (?x woman) (?x ?y has-child)) (?x ?y))

      (tbox-query (and (?x woman) (?x ?y has-parent)) (?x ?y))

      (tbox-query (and (woman woman) (woman ?y has-child)) (?y))

      (tbox-query (and (woman sister) (woman ?y has-child)) (?y))

      (tbox-query (and (?x woman) (?x ?y has-descendant)) (?x ?y))
	
      (tbox-query (and (?x woman) (?x ?y has-ancestor)) (?x ?y))
	
      (tbox-query (and (?x woman) (?x ?y has-descendant) (?x ?z has-descendant)) (?x ?y ?z))

      (tbox-query (and (not (?x ?y has-descendant)) (not (?y ?x has-descendant))) (?x ?y))

      (tbox-query (and (not (?x ?y has-ancestor)) (not (?y ?x has-ancestor))) (?x ?y))

      (tbox-query (and (?x person) 
                       (?x ?y has-child)
                       (?x ?z has-child) (?y ?u has-descendant) 
                       (?z ?u has-descendant))
                  (?x ?y ?z ?u)))

    (progn 
       
      (full-reset)
	
      (racer-load-kb "family-1-no-signature")

      (tc (defquery mother (?x ?y) (and (?x woman) (?x ?y has-child))))
      (tc (defquery mother-with-male-child (?x ?child) (and (:substitute (mother ?x ?child)) (?child man))))
      (tc (retrieve (?a ?b) (and (:substitute (mother-with-male-child ?a ?b)) (?b uncle))))
      (tc (def-and-prep-query test (?a) (and (?a man) (:substitute (mother-with-male-child nil ?a)))))

      (tc (execute-all-queries :timeout *timeout*))
        
      (tc (get-all-answers) :test get-all-answers-equal-p))

    (let ((ts::*runtime-evaluation-p* t))

      (with-skipping-if-necessary

          (full-reset)
        
        (in-abox data-test)
        (enable-data-substrate-mirroring)
        (in-data-box data-test)

        (instance a test)
        (data-node a "huhu")
	
        (tc (defquery is-string (?*x) (and (?*x (:predicate stringp)))))
        (tc (def-and-prep-query test (?a ?*a) (and (?a test) (:substitute (is-string ?*a)))))
         
        (tc (execute-all-queries :timeout *timeout*))

        (tc (get-all-answers) :test get-all-answers-equal-p)))

    (progn 
       
      (full-reset)

      (racer-load-kb "family-1-no-signature")
	
      (instance doris (and woman
                           (at-least 2 has-child)))

      (instance jane woman)
      (instance tarzan man)
	
      (add-role-axioms 'family-ns 'married :symmetric t)
	
      (related tarzan jane married)
	
      ;;;
      ;;;
      ;;;
	
      (tc (prepare-abox-rule (and (?x mother) (?x (at-least 2 has-child)) (?x nil has-child))
	     
                             ((instance (new-ind first-child-of ?x) man)
                              (instance (new-ind second-child-of ?x) woman)
	      
                              (related ?x (new-ind first-child-of ?x) has-child)
                              (related (new-ind second-child-of ?x) ?x has-mother)

                              (constrained (new-ind first-child-of ?x)
                                           (new-ind age-of-first-child-of ?x)
                                           age)
	      
                              (constrained (new-ind second-child-of ?x)
                                           (new-ind name-of-second-child-of ?x)
                                           name)
	      
                              (constraints
                               (= (new-ind age-of-first-child-of ?x) 7)
                               (string= (new-ind name-of-second-child-of ?x) "Foobar")))))
	
      ;;;
      ;;; Tarzan & Jane get a baby: 
      ;;;
	
      (tc (prepare-abox-rule (and (?x woman) (?y man) (?x ?y married))

                             ((instance (new-ind child-of ?x ?y) human)
                              (instance ?x mother)
                              (instance ?y father)
                              (related  (new-ind child-of ?x ?y) ?x has-mother)
                              (related  (new-ind child-of ?x ?y) ?y has-father))))
        
      (tc (execute-all-rules :timeout *timeout*) :test equalp))

    (progn 

      (full-reset)
         
      (racer-load-kb "family-1-no-signature")
         
      (classify-tbox)
         
      (enable-query-realization)
	  
      (query0 (and (?x woman) (?y human) (?x human)) (?x))
	  
      (query0 (and (?x human) 
                   (?x (all has-descendant male))
                   (?x woman) (?x ?y has-child)) (?x ?y))
	  
      (query0 (and (?x human) 
                   (?x woman) 
                   (?x man)) (?x))
	  
      (query0 (and (?x human) 
                   (?x woman) 
                   (?x (:has-known-successor has-child))) (?x))
	  
      (query0 (and (?x woman) (:same-as ?x betty)) (?x))
	  
      (query0 (?x nil has-child) (?x))
      (query0 (?x ?y (:constraint age age <)) (?x ?y))
      (query0 (and (not (betty man))
                   (not (bind-individual betty)))
              (betty))
	  
      (query0 (and (:same-as ?x betty)
                   (betty woman)
                   (?x (some has-child person)))
              (betty))
	  
      (query0 (and (:same-as ?x betty)
                   (betty woman)
                   (?x (some has-child woman))
		      
                   (betty (all has-descendant man)))
	   
              (betty))
            
      (tc (describe-all-queries) :error-p nil :test equalp))

    (let ((ts::*runtime-evaluation-p* t))
       
      (with-skipping-if-necessary

          (full-reset)

        (in-tbox geo-example)
	
        (define-concrete-domain-attribute inhabitants :type cardinal)
	
        (define-concrete-domain-attribute has-name :type string)
	
        (define-primitive-attribute has-language)
	
        (in-abox geo-example)  
	
        (in-rcc-box geo-example :rcc5)
	
        (rcc-instance europe)
	
        (rcc-instance germany
                      (country germany) 
	 
                      (and country
                           (string= has-name "Germany")
                           (all has-language german) 
                           (some has-language language)
                           (= inhabitants 82600000)))

	
        (rcc-instance hamburg
                      (city hamburg)
                      (and city 
                           (some in-country germany)
                           (string= has-name "Hamburg")))
	
        (rcc-related europe germany :ppi)
        (rcc-related germany hamburg :ppi)
	
        (tc (retrieve (?*x ?x ?*y) (and (?x (and (string= has-name "Hamburg")
                                                 (some in-country germany)))
                                        (top ?*X)
                                        (not (?*y country))
                                        (?*x ?*y :pp))))))

    (let ((ts::*runtime-evaluation-p* t))
     
      (with-skipping-if-necessary
   
          (full-reset)

        (in-tbox test)
        (in-abox test)  
	
        (in-rcc-box test :rcc5)

        (rcc-related a b :ppi)
        (rcc-related b c :ppi)

        (tc (retrieve (?*x ?*y) (?*x ?*y :ppi)))))

    (let ((ts::*runtime-evaluation-p* t))

      (with-skipping-if-necessary 

          (full-reset)

        (in-data-box test)
	
        (data-node c ("michael") person)

        (data-edge c d 5)
        (data-edge c d 3)

        (data-node e 5)
        (data-node f 6)

        (del-data-node f)

        (del-data-edge x y)

        (data-node g ("michael" huhu))

        ;;; get the labels
	
        (tc (node-label g))
        (tc (edge-label c d))
        (tc (node-label xxx))
        (tc (edge-label c g))

        ;;; queries

        (tc (retrieve (?*x ?*y) (and (?*x (:predicate (search "mich")))
                                     (?*x ?*y ((:predicate (= 5))
                                               (:predicate (= 3)))))))

        (tc (retrieve (?*x ?*y) (and (?*x ?*y (:satisfies (:predicate string=))))))

        (tc (retrieve (?*x ?x) (and (?x person) (top ?*x))))

        (tc (retrieve (?*x ?*y) (and (?*x ?*y 3) 
                                     (?*y ( ((:predicate stringp) (:predicate numberp))))  (top ?*x))))
	
        (tc (retrieve (?*x) (?*x (( (:predicate stringp) (:predicate numberp))))))

        (tc (retrieve (?*x) (?*x (:has-known-successor 3))))

        (tc (retrieve (?*x) (not (?*x (:has-known-successor 3)))))))

    (let ((ts::*runtime-evaluation-p* t))

      (with-skipping-if-necessary

          (full-reset)

        (in-tbox test)

        (implies man human)
        (implies professor teacher)
        (implies teacher human)
	
        (in-abox test)
        (in-data-box test)
	
        (define-primitive-role collegue :inverse collegue)
	
        (data-node michael ("Michael Wessel" age 34) 
                   (and man
                        (some has-knowledge DL)))
	
        (data-node ralf ("Ralf Moeller" age 38) 
                   (and professor
                        (some has-knowledge DL)))
	
        (data-edge michael ralf
                   ("this is a description" has-collegue)
                   collegue)
	
        (tc (node-label michael))
        (tc (node-label ralf))
	
        (tc (edge-label michael ralf))

        (tc (retrieve (?x) (?x (and human
                                    (some collegue teacher)))))
	
        (tc (retrieve (?x ?*y) (and (?x (and human
                                             (some collegue teacher)))
                                    (?x ?y collegue)
                                    (?*y "Ralf Moeller"))))

        (tc (retrieve (?*y) (and (?*x (age 34))
                                 (?*x ?*y (has-collegue))
                                 (?*y "Ralf Moeller"))))


        (tc (retrieve (?*x) (and (?*x (age (:predicate (< 38)))))))

        (tc (retrieve (?*x ?*y) (and (?*x ?*y (has-collegue (:predicate (search "a description"))))
                                     (?*x ?*y (:satisfies (:predicate <)))
                                     (?*y (:predicate (search "Ralf"))))))

        (tc (retrieve (?x *michael) (and (bind-individual *michael)
                                         (*michael ?*x has-collegue)
                                         (?x professor)
                                         (?*x (age (:predicate (< 40)))))))))
    (let ((ts::*runtime-evaluation-p* t))
       
      (with-skipping-if-necessary

          (full-reset)

        (in-abox test)

        (define-primitive-role has-child :parent has-relative)
        (define-concrete-domain-attribute age :type integer)
        (define-concrete-domain-attribute name :type string)

        (in-mirror-data-box test)
        (enable-very-smart-abox-mirroring)

        (equivalent mother (and woman (some has-child human)))

        (implies woman human) 

        (instance alice woman)
        (instance betty woman)

        (related alice betty has-child)

        (constrained betty betty-age age)

        (constrained alice alice-age age)

        (constrained betty betty-name name)

        (constrained alice alice-name name)

        (constraints (equal betty-age 80))
        (constraints (equal alice-age 50))
        (constraints (string= betty-name "Betty"))
        (constraints (string= alice-name "Alice"))

        (tc (retrieve (?*x ?*y
                           ?*name-of-?*y
                           ?*age-of-?*y
                           ?*tv-1
                           ?*tv-2)
	     
                      (and (?*x (:abox-individual ))
                           (?x ?y has-child)
                           (?y woman)
		  
                           (?*y ?*age-of-?*y (:abox-attribute-relationship age))
                           (?*y ?*name-of-?*y (:abox-attribute-relationship name))

                           (?*age-of-?*y (:abox-concrete-domain-object))

                           (?*name-of-?*y (:abox-concrete-domain-object))
		  
                           (?*age-of-?*y ?*tv-1 (:abox-told-value-relationship))

                           (?*name-of-?*y ?*tv-2 (:abox-told-value-relationship))
		  
                           (?*tv-1 (:abox-concrete-domain-value (:predicate (< 90))))
                           (?*tv-2 (:abox-concrete-domain-value (:predicate (search "tty")))))))))

    (progn 
	
      (full-reset)

      (define-concrete-domain-attribute age :type cardinal)
	
      (constrained a a-age age)
      (constrained b b-age age)
      (constrained c c-age age)

      (constraints (= a-age b-age) (= b-age c-age) (= c-age 10))

      (tc (told-value 'a-age) :test equalp)
      (tc (told-value 'b-age) :test equalp)
      (tc (told-value 'c-age) :test equalp)

      (tc (retrieve (?x (age ?x) (told-value (age ?x))) (?x (an age)))))

    (without-nrql-timeout
      
     (requires-multiprocessing ()

                               (full-reset)

                               (in-tbox test)
	
                               (implies person top)
                               (implies man person)
                               (implies woman person)
	
                               (equivalent spouse (and woman (some married-to man)))
	
                               (in-abox test)
	
                               (instance doris spouse)
                               (instance betty spouse)
                               (instance adam man)
                               (instance eve woman)
                               (related eve adam married-to)
	
                               (set-nrql-mode 4)
	
                               (tc (describe-query-processing-mode))
	
                               (tc (retrieve (?x) (?x spouse)))

                               (tc (list (get-next-tuple :last)
                                         (get-next-tuple :last)))

                               (tc (get-next-tuple :last))
                               (tc (get-next-tuple :last))
                               (tc (get-next-tuple :last))
                               (tc (get-next-tuple :last))))

    (without-nrql-timeout

     (requires-multiprocessing ()

                               (full-reset)

                               (in-tbox test)
	
                               (define-primitive-attribute f1)
	
                               (define-primitive-role r1 :transitive t)
	
                               (define-primitive-attribute g1 :parents (f1 r1))
	
                               (in-abox test)
	
                               (related h i f1)
	
                               (related i j f1)
	
                               (instance h (some g1 top))
	
                               (instance i (some g1 top))
	
                               (set-nrql-mode 4)
                               (process-tuple-at-a-time)
	
                               (tc (retrieve (?x ?y) (?x ?y g1)))
                               (tc (get-next-tuple :last))
                               (tc (get-next-tuple :last))
                               (tc (get-next-tuple :last))))

    (without-nrql-timeout

     (requires-multiprocessing ()
     
                               (full-reset)

                               (in-tbox test)
	
                               (define-primitive-role r1 :transitive t)
	
                               (in-abox test)
	
                               (related h i r)
	
                               (related i j r)
	
                               (set-nrql-mode 4)
	
                               (process-tuple-at-a-time)

                               (tc (retrieve (?x ?y) (?x ?y r)))
                               (tc (get-next-tuple :last))
                               (tc (get-next-tuple :last))
                               (tc (get-next-tuple :last))
                               (tc (get-next-tuple :last))))

    (without-nrql-timeout

     (requires-multiprocessing ()

                               (full-reset)

                               (in-tbox test)
                               (define-primitive-attribute f1)
	
                               (define-primitive-role r1 :transitive t)
	
                               (define-primitive-attribute g1 :parents (f1 r1))

                               (in-abox test)
	
                               (related h i r1)
	
                               (related i j f1)
	
                               (instance h (some g1 top))
	
                               (instance i (some g1 top))

                               (set-nrql-mode 4)
                               (process-tuple-at-a-time)

                               (tc (retrieve (?x ?y) (?x ?y r1)))
                               (tc (get-next-tuple :last))
                               (tc (get-next-tuple :last))
                               (tc (get-next-tuple :last))
                               (tc (get-next-tuple :last))))

    (progn 

      (restore-standard-settings)

      (instance a c)
      (instance b e)
      (related b a r)

      (tc (retrieve-under-premise ( (instance a c) (instance b d) 
                                    (related a b r))
                                  (?x ?y)
                                  (and (?x c) (?y d)
                                       (?x ?y r)))))

    (progn 

      (restore-standard-settings)

      (instance a c)

      (tc (retrieve-under-premise ((instance a c))
                                  (?x)
                                  (and (?x c)))))
    
    (progn 
      
      (full-reset)
      
      (let ((thematic-substrate::*add-role-assertions-for-datatype-properties-p* t))
	
	(owl-read-file "nrql:test-cases;constraint-query-dtp.owl")
	
	(query0
         (?x ?y (constraint |http://www.owl-ontologies.com/unnamed.owl#age|
                            |http://www.owl-ontologies.com/unnamed.owl#age| =))
         (?x ?y))
	
	(query0
         (?x ?y (constraint |http://www.owl-ontologies.com/unnamed.owl#age| 
                            |http://www.owl-ontologies.com/unnamed.owl#age| >))
         (?x ?y))
	
	(query0
         (?x ?y (constraint |http://www.owl-ontologies.com/unnamed.owl#age| 
                            |http://www.owl-ontologies.com/unnamed.owl#age| <))
         (?x ?y))))

    (progn

      (full-reset)

      (define-concrete-domain-attribute age :type cardinal)

      (tc (firerule () ((instance (new-ind a) (an age)))))
       
      (tc (firerule () ((instance (new-ind b) (an age)))))

      (tc (firerule (and (?x (an age)) (?y (an age))) 
                    ((constrained ?x (new-ind age-of ?x) age)
                     (constrained ?y (new-ind age-of ?y) age))
                    :how-many 1))
       
      (tc (firerule (and (a (an age)) (b (an age)))
                    ((constraints (= (age b) (+ 30 (age a)))))))


      (tc (firerule (and (a (an age)) (b (an age)))
                    ((constraints (= (age a) 30)))))

      (tc (retrieve (?x) (?x (= age 30))))

      (tc (retrieve (?x (told-value (age ?x))) (?x (= age 30))))

      (tc (retrieve (?x (told-value (age ?x))) (?x (= age 60))))


      (tc 
       (firerule (a top) ((constraints (= (told-value (age a))
                                          (age a))))))

      (tc 
       (firerule (b top) ((constraints (= (told-value (age b))
                                          (age b))))))

      (tc 
       (firerule (and (a (an age)) (b (an age))) ((constraints (= (told-value (age b))
                                                                  (+ 30 (told-value (age a))))))))

      (tc (firerule (?x top) ((instance (new-ind test ?x) test)
                              (constrained (new-ind test ?x) (new-ind test-cd ?x) age)
                              (constraints
                               (= (new-ind test-cd ?x) 
                                  40)))))
       
      (tc (abox-consistent?)))))




	


(defconstant +concurrent-test-cases+
  `(requires-multiprocessing ()
    
                             (full-reset)
                             (process-tuple-at-a-time)
                             (enable-eager-tuple-computation)
                             ;; (enable-lazy-tuple-computation)
        
                             (racer-load-kb "family-1-no-signature")
         
                             (query1 (and (?x man) (not (:same-as ?x charles))) (?x))

                             (query1 (and (?x man) (:same-as ?x charles)) (?x))
	 
                             (query1 (betty top) (betty))

                             (query1 (and (?x top)) (?x))
	 
                             (query1 (and (?x woman)) (?x))
	 
                             (query1 (and (?x woman) (?y woman)) (?x))

                             (query1 (and (?x woman) (?y woman)
                                          (not (?x ?y has-descendant))
                                          (not (?y ?x has-descendant)))

                                     (?x ?y))

                             (query1 (or (betty (and woman))                       
                                         (and (?x woman)
                                              (betty top)
                                              (or (?x ?y has-sibling)
                                                  (?x ?y has-child))
                                              (?y man)))
		
                                     (?x ?y betty))


                             (query1 (or                       
                                      (betty (and woman))                       
                                      (and (?x woman)
                                           (betty top)
                                           (or (?x ?y has-sibling)
                                               (?x ?y has-child))
                                           (?y man)))
		
                                     (betty ?x ?y))

                             (query1 (or                       
                                      (and (?x woman)
                                           (?y human)
                                           (?z man)                   
                                           (?x ?y has-child)
                                           (?y ?z has-sibling)))                                                   
                                     (?x ?y ?z))

                             (query1 (and (charles top)
                                          (?x man))                    
                                     (?x charles))

                             (query1 (betty man)
                                     nil)

                             (query1 (AND (BETTY MAN)
                                          (NOT (?Y BETTY (INV HAS-CHILD))))
                                     (betty ?y))
         
                             (query1 (betty top)
                                     ())
	 
                             (query1 (betty top)
                                     (betty))

                             (query1 (and (not (?x ?y has-child))
                                          (not (?y ?x has-child)))                     
                                     (?x ?y))
	 
	 
                             (query1 (and (inv (?x ?y has-child)))                    
                                     (?x ?y))
	 
                             (query1 (and
                                      (betty woman)
                                      (and (?x woman)
                                           (?x man)))                       
                                     (betty))

                             (query1 (and
                                      (or (?x woman) (?x man)))
                                     (?x))

                             (query1 (or (?x woman) (?y man))
                                     (?x))

                             (query1 (?x man)
                                     (?x))

                             (query1 (not (?x man))
                                     (?x))

                             (query1 (?x (not c))
                                     (?x))

                             (query1 (not (?x c))
                                     (?x))

                             (query1 (not (?x ?y has-sibling))
                                     (?x ?y))

                             (query1 (?x ?y has-sibling)
                                     (?x ?y))

                             (query1 (?x top)
                                     (?x))

                             (query1 (and (betty woman) (?x woman))
                                     (?x))
	 
                             (query1 (not (and (betty woman)))
                                     (betty))

                             (query1 (or (top ?x) (top ?y))
                                     (?x ?y))
	 
                             (query1 (not (?x (not c)))
                                     (?x))

                             (query1 (or (?x c) (?x (not c)) )                         
                                     (?x))

                             (query1 (or (?x c) (?x (not c)))                         
                                     (?x))

                             (query1 (or (?x c) (not (?x c)))                         
                                     (?x))

                             (query1 (inv (and (?x ?y has-child) (?x woman) (?y man)))                         
                                     (?x ?y))
	 
                             (query1 (and (?x ?y has-child) (?x woman) (?y man))                         
                                     (?x ?y))
	 
                             (query1 (not (and (betty woman) (?x woman)))
                                     (?x))
	 
                             (query1 (and (?x woman) (?y man))
                                     (?y))
	 
                             (query1 (and (?x woman) (?x man))
                                     (?x))
	 
                             (query1 (and (?x (not woman))) 
                                     (?x))

                             (query1 (and (?x (not woman))) 
                                     (?x))
	 
                             (query1 (and (?x woman) (?y woman) (not (?x ?y has-descendant))) 
                                     (?x ?y))

                             (query1 (and (?x top)) (?x))
	 
                             (query1 (and (?a woman)) (?a))
	 
                             (query1 (and (?c mother)) (?c))
	 
                             (query1 (and (?x man)) (?x))

                             (query1 (and (?z brother)) (?z))

                             (query1 (and (?t uncle)) (?t))

                             (query1 (and (?x father)) (?x))

	 
                             (query1 (not (and (betty woman)))
                                     (betty))
	 
                             (query1 (or (?x c) (?x (not c)))                         
                                     (?x))
	 
                             (query1 (and (?x woman) (?y man))
                                     (?x ?y))

                             (query1 (and (?x (and c d)))
                                     (?x))

                             (query1 (not (and (betty woman)))
                                     (betty))

                             (query1 (or (?x human) (?x (not human)))                         
                                     (?x))

                             (query1 (and (?x woman) (?y man))
                                     (?x ?y))

                             (query1 (or (?x woman) (?y man))
                                     (?x ?y))

                             (query1 (and (?x top)) (?x))
	 
                             (query1 (and (?a woman)) (?a))
	 
                             (query1 (and (?b woman)) (?b))

                             (query1 (and (?c mother)) (?c))
	 
                             (query1 (and (?x woman) (?x ?y has-child)) (?x ?y))

                             (query1 (and (?x man)) (?x))

                             (query1 (and (?z brother)) (?z))

                             (query1 (and (?t uncle)) (?t))

                             (query1 (and (?x father)) (?x))
	 
                             (query1 (and (?a man) (?a ?b has-child)) (?a ?b))

                             (query1 (and (?c man) (?a ?b has-child) (?b ?c has-child) ) (?a ?b ?c))

                             (query1 (and (?x uncle)) (?x))

                             (query1 (?x (an age)) (?x))
	 
                             (query1 (?x (a name)) (?x))

                             (query1 (and (?x woman) (?x (> age 50))) (?x))

                             (query1 (and (?x man) (?x (> age 35))) (?x))

                             (query1 (and (?x human) (?x (> age 18))) (?x))

                             (query1 (and (?x human) (?x (< age 80))) (?x))

                             (query1 (and (?x woman) (?x (and (> age 70) (< age 100)))) (?x))

                             (query1 (and (?x (string= name "Alice Smith"))) (?x))
	 
                             (query1 (and (alice (string= name "Alice Smith"))) (alice))
	 
                             (query1 (and (alice (string= name "Alice Doe"))) (alice))

                             (query1 (and (?x (a name)) (?x (an age))
                                          (and (?x (or (string= name "Alice Smith")
                                                       (string= name "Betty Doe")))
                                               (?x (or (> age 50)
                                                       (< age 100)))))         
                                     (?x))
	 
                             (query1 (and (?x (a name)) (?x (an age))
                                          (or (?x (or (string= name "Alice Smith")
                                                      (string= name "Betty Doe")))
                                              (?x (or (> age 50)
                                                      (< age 100)))))
		
                                     (?x))

                             (query1 (?x ?y (:constraint age age <)) (?x ?y))

                             (query1 (?x ?x (:constraint (has-mother age) (has-father age) <)) (?x ?x))
	 
                             (query1 (and (?x ?mother has-mother) 
                                          (?x ?father has-father)
                                          (?mother ?father (:constraint age age <))) (?x ?mother ?father))

                             (query1 (and (?x ?x loves))
                                     (?x))
	 
                             (query1 (and (?x ?y loves))
                                     (?x ?y))

                             (query1 (and (?x ?x (:constraint age age <)))
                                     (?x))

                             (query1 (not (and (?x ?x (:constraint age age <))))
                                     (?x))
	 
                             (query1 (?x ?x (:constraint 
                                             (has-father has-mother age) 
                                             (has-mother has-mother age) =))
                                     (?x))

                             (query1 (and (?x ?mother has-mother) (?x ?father has-father)
                                          (?x ?y has-sibling))
                                     (?x ?mother ?father))

                             (query1 (and (?x ?mother has-mother) (?x ?father has-father)
                                          (or (?mother ?father has-brother)
                                              (?father ?mother has-sister)))
                                     (?x ?mother ?father))

                             (query1 (alice betty (:constraint (age) (age) >)) ())

                             (query1 (?x ?y (:constraint (age) (age) <)) (?x ?y))
	 
                             (query1 (alice ?y (:constraint (age) (age) <)) (alice ?y) )
	 
                             (query1 (inv (alice ?y (:constraint (age) (age) <))) (alice ?y))
	 
                             (query1 (inv (alice ?y (:constraint (age) (age) <))) ())

                             (query1 (alice ?y (:constraint (age) (age) >)) ())

                             (query1 (alice ?y (:constraint (age) (age) >)) (alice ?y))

                             (query1 (not (alice ?y (:constraint (age) (age) <))) (alice ?y))
	 
                             (query1 (and (?X ?m HAS-MOTHER)  (?x ?f has-father) (?m ?f (:constraint age age >))) (?x ?m ?f))
	 
                             (query1 (and (?X ?m HAS-MOTHER)  (?x ?f has-father) (?m ?f (:constraint age age <))) (?x ?m ?f))
	 
                             (query1 (and (?x ?m has-mother) (?x ?f has-father)) (?x ?m ?f))
	 
                             (query1 (not (and (?x ?m has-mother) (?x ?f has-father))) (?x ?m ?f))

                             (query1 (or (not (?x ?m has-mother)) (not (?x ?f has-father))) (?x ?m ?f))
	 
                             (query1 (and (?x ?m has-mother) (?x ?f has-father)) (?x))

                             (query1 (not (and (?x ?m has-mother) (?x ?f has-father))) (?x))

                             (query1 (or (not (?x ?m has-mother)) (not (?x ?f has-father))) (?x))

                             (query1 (and (?X ?m HAS-MOTHER)  
                                          (?x ?f has-father) 
                                          (?m ?f (:constraint age age <))) (?x ?m ?f))
	 
                             (query1 (not (and (?X ?m HAS-MOTHER)  
                                               (?x ?f has-father) 
                                               (?m ?f (:constraint age age <)))) (?x ?m ?f))

                             (query1 (or (not (?X ?m HAS-MOTHER))
                                         (not (?x ?f has-father))
                                         (not (?m ?f (:constraint age age <)))) (?x ?m ?f))

                             (query1 (?x ?x (:constraint (has-mother age) (has-father age) <)) (?x))

                             (query1 (?x ?x (:constraint (has-mother age) (has-father age) <)) (?x))

                             (query1 (not (?x ?x (:constraint (has-mother age) (has-father age) <))) (?x))
	 
                             (query1 (?x ?x (:constraint
                                             (has-mother age)
                                             (has-father age) <)) (?x))

                             (query1 (?x ?x (:constraint 
                                             (has-father has-mother age)
                                             (has-mother has-mother age) =))
                                     (?x))
	 
                             (query1 (not (?x ?x (:constraint 
                                                  (has-father has-mother age)
                                                  (has-mother has-mother age) =)))
                                     (?x))
	 
                             (query1 (and (?x top) (?y top))
                                     (?x ?y))

                             (query1 (?x ?y (:constraint 
                                             (has-father has-mother age)
                                             (has-mother has-mother age) =))
                                     (?x ?y))
	 
                             (query1 (not (?x ?y (:constraint 
                                                  (has-father has-mother age)
                                                  (has-mother has-mother age) =)))
                                     (?x ?y))
	 
                             (query1 (?x ?y (:constraint 
                                             (has-father has-mother age)
                                             (has-mother has-father age) >=))
                                     (?x ?y))
	 
                             (query1 (not (?x ?y (:constraint 
                                                  (has-father has-mother age)
                                                  (has-mother has-father age) >=)))
                                     (?x ?y))

                             (query1 (?x ?y (:constraint 
                                             (has-father has-mother age)
                                             (has-mother has-mother age) >=))
                                     (?x ?y))
	 
                             (query1 (not (?x ?y (:constraint 
                                                  (has-father has-mother age)
                                                  (has-mother has-mother age) >=)))
                                     (?x ?y))
	 
                             (query1 (?x eve (:constraint 
                                              (has-father has-mother age)
                                              (has-mother has-mother age) >=))
                                     (?x eve))

                             (query1 (not (?x eve (:constraint 
                                                   (has-father has-mother age)
                                                   (has-mother has-mother age) >=)))
                                     (?x eve))

                             (query1 (alice ?y (:constraint (age) (age) >)) ())

                             (query1 (alice ?y (:constraint (age) (age) >)) (alice ?y))

                             (query1 (alice ?y (:constraint (age) (age) >)) ())

                             (query1 (alice ?y (:constraint (age) (age) >)) (alice ?y))
	 

                             (query1 (?x ?y (:constraint (age) (age) <)) (?x ?y))

                             (query1 (?x ?y (:constraint (age) (age)
                                             (= age-1 (+ age-2 30))))
                                     (?x ?y))

                             (query1 (?x ?y (:constraint (age) (age)
                                             (<= (+ age-1 30) age-2 )))
                                     (?x ?y))

                             (query1 (?x ?y (:constraint (age) (age) =)) (?x ?y))
	 
                             (query1 ($?x $?y (:constraint (age) (age) =)) ($?x $?y))

                             (query1 (and (?x woman) (?y top) (?x (:has-known-successor has-child))) (?x))
	 
                             (query1 (not (and (?x woman) (?y top) (?x (:has-known-successor has-child)))) (?x))
	 
                             (query1 (and (?x mother) (?x ?y has-child)) (?x))

                             (execute-all-queries :timeout *timeout*)

                             (tc (get-all-answers) :test get-all-answers-equal-p)))




;;;
;;;
;;;

(defun run-nrql-tests (&key recreate-logs-p timeout)
  (let ((*mode* (if recreate-logs-p 
                    :logging
                  :verification))
        (*timeout* (or *timeout* 
                       timeout))
        (*package* (find-package :cl-user))
        ;(*tbox-verbose* nil)
        (*logfile-directory*
         "nrql:test-cases;results;" ))

    (reset1)
    
    (time (eval +concurrent-test-cases+))
    (setf ts::*syntactic-repository-p* nil)
    
   
    (when *tbox-verbose*
      (terpri) 
      (princ "1 -------------------------")
      (terpri) )
         
    (time 
     (dolist (case +test-cases+)
       (full-reset)
       (eval case)))

    (when *tbox-verbose*
      (terpri) 
      (princ "2 -------------------------")
      (terpri))
    
    (time 
     (dolist (case (subseq +test-cases+ 0 8))
       (full-reset)
       (enable-query-repository)
       (eval case)
       (when *tbox-verbose*
         (ts::show-current-qbox))))

    (when *tbox-verbose*
      (terpri) 
      (princ "3 -------------------------")
      (terpri))
    
    (time 
     (dolist (case (subseq +test-cases+ 0 8))
       (full-reset)
       (enable-query-repository)
       (setf ts::*syntactic-repository-p* t)
       (eval case)
       (when *tbox-verbose*
         (ts::show-current-qbox))))))
      

(defun run-nrql-power-test (&key timeout)
  (let ((*tbox-verbose* nil)
        (*timeout* timeout))
    (dotimes (i 100) 
      (run-nrql-tests))))

;;;
;;;
;;;

(defun perm-test-demo ()

  (dolist (perm (ts::permutations 
                 (list :query-1
                       :query-2
                       :query-3 
                       :query-4)))

    
    (without-nrql-timeout
         
     (full-reset)
     (process-set-at-a-time)
        
     (racer-load-kb "family-1-no-signature")
  
     (query1 (and (?x man) (?x (> age 35))) (?x))

     (query1 (and (?x human) (?x (< age 80))) (?x))

	
     (query1 (and (?x ?mother has-mother) 
                  (?x ?father has-father)
                  (?mother ?father (:constraint age age <)))
             (?x ?mother ?father))


     (query1 (?x ?x (:constraint (has-mother age) 
                     (has-father age) <)) (?x ?x)))

    (mapc #'execute-query
          (mapcar #'ts::find-query perm))

    (format t "Checking permutation ~A~%"  perm)

    (unless (every #'get-answer ts::*all-queries*)
      (error "!"))))

;;;;

#|

(FIRERULE
      (?X (A |http://www.owl-ontologies.com/unnamed.owl#name|))
      ((RELATED
        ?X
        (NEW-IND NAME-OF ?X)
        |http://www.owl-ontologies.com/unnamed.owl#name|)
       (CONSTRAINED
        (NEW-IND NAME-OF ?X)
        (NEW-IND STRING-VALUE-OF-NAME-OF ?X)
        RACER-INTERNAL%HAS-STRING-VALUE)
       (:CONSTRAINTS
        (STRING= (NEW-IND STRING-VALUE-OF-NAME-OF ?X)
                      (told-value (|http://www.owl-ontologies.com/unnamed.owl#name|
                                   ?X))))))

|# 


