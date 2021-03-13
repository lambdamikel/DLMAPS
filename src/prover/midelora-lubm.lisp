;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)


(setf (logical-pathname-translations "diss")
      '(("lubm;**;*.*.*" "~/diss/kap7/LUBM/**/*.*")
        ("benchmark-results;**;*.*.*" "~/diss/kap7/benchmark-results/**/*.*")))

(defconstant +no-of-runs-per-query+ 1)

(defvar *file-stream* t)

(defvar *output-stream* t)

(defvar *global-prep* 0)

(defvar *global-exec* 0) 

(defparameter *time* nil)


(defvar *load-time* nil)

(defvar *prep-time* nil)

(defvar *consistency-time* nil)

(defvar *inds* nil)

(defvar *results* nil)

(defvar *last-global* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ts::string-transform (string)
    string))


(defun reset ()
  (prover::delete-all-tboxes)
  (ts::delete-all-queries))

(defmacro mtime (&body body)
  `(if *time* 
       (time ,@body)
     ,@body))

(defmacro benchmark1 (universities departments no query vars)
  `(let ((id ',(intern (format nil "LUBM-QUERY-~A" no)))
	 (number-of-answers nil)
	 (prep-time 0)
	 (exec-time 0))
     
     (dotimes (i +no-of-runs-per-query+)
       (let ((t1 (get-internal-run-time)))
	 
	 (ts::midelora-prepare-query ,vars ,query :id id)
	 
	 ;; (format *output-stream* "~A" (thematic-substrate::describe-query id))
	 
	 (let ((t2 (get-internal-run-time)))
	   
	   (setf number-of-answers (length (ts::execute-query id)))
	   
	   (incf exec-time (/ (- (get-internal-run-time) t2)
			      internal-time-units-per-second))
	   (incf prep-time (/ (- t2 t1)
			      internal-time-units-per-second)))))
     
     
     (let ((mean-prep (/ prep-time +no-of-runs-per-query+))
	   (mean-exec (/ exec-time +no-of-runs-per-query+)))
       
       (incf *global-prep* mean-prep)
       (incf *global-exec* mean-exec)
       
       (format *output-stream* "~%*** Universities: ~2,D Max. Deps: ~2,D Query: ~2,A Answers: ~12,D Prep-Time: ~12,D Exec-Time: ~12,D Total: ~12,D~%" 
	       (1+ ,universities) 
	       (if (null ,departments)
		   :all
		 (1+ ,departments))
	       ,no
	       (if (null number-of-answers)
		   0
		 number-of-answers)
	       (float mean-prep)
	       (float mean-exec)
	       (+ (float mean-prep)
		  (float mean-exec)))
       
       
       (format *file-stream* 
	       "~2,D ~2,D ~2,A ~12,D ~12,D ~12,D ~12,D~%" 
	       (1+ ,universities) 
	       (if (null ,departments)
		   :all
		 (1+ ,departments))
	       ,no
	       (if (null number-of-answers)
		   0
		 number-of-answers)
	       (float mean-prep)
	       (float mean-exec)
	       (+ (float mean-prep)
		  (float mean-exec)))
       
       (force-output *file-stream*))))


(defmacro benchmark (universities departments no query vars)
  `(benchmark1 ,universities ,departments ,no ',query ',vars))

;;; ======================================================================

(defun load-kbs (&optional (univs 1) (departments 1))
  (declare (ignorable univs departments))

  (reset)
  
  ;(let ((*package* (find-package :racer-user)))
  ;  (load "~/diss/kap7/LUBM/lubm2.tbox"))

  (princ univs)
  (princ departments)

  (let ((name (if (= univs 0)
                  (if departments 
                      (format nil "lubm-0-~A" departments)
                    (format nil "lubm-1"))
                (format nil "lubm-~A" univs))))

    (princ name) (terpri)
  
    (let ((*package* (find-package :prover)))
      (load (format nil "/home/mi.wessel/diss/kap7/LUBM/~A.tbox" name))
    ;(load (format nil "/home/mi.wessel/diss/kap7/lubm.tbox.backup" name))
    
    ;(load (format nil "/home/mi.wessel/diss/kap7/LUBM/db-~A.abox" name))
      (load (format nil "/home/mi.wessel/diss/kap7/LUBM/~A.abox" name)))

    ;;; (delete-all-nodes) ;;; fuer DB! alle Knoten/Kanten "virtuell"
    
    (visualize-taxonomy)
  ;(in-abox lubm)
  ;  (instance i top)
    ))


(defun prepare-lubm-data-n-universities (check-abox-consistency
					 n-universities max-n-departments)
  (let ((t1a (get-internal-run-time)))
    
    #+:allegro (princ "Stack Cushion: ") 
    #+:allegro (princ (system:stack-cushion)) 
    #+:allegro (terpri)
    
    (mtime (load-kbs n-universities max-n-departments))
    

    (terpri)
    (princ (current-abox))
    (princ (current-abox))
    (terpri)
	    
    (let ((t1b (get-internal-run-time)))

      (setf *load-time* 
            (/ (- t1b t1a) internal-time-units-per-second))
            
      (let ((t2a (get-internal-run-time)))
        
        (format *output-stream* "~%~%ABox preparation ")
        (format *output-stream* "done.~%")
        
        (let ((t2b (get-internal-run-time)))
          
	  (when check-abox-consistency
            (format *output-stream* "~%~%ABox consistency checking... ")
	    
	    (terpri) 
	    (princ (current-abox))
	    (princ (current-abox))
	    (terpri) 
	    
            (mtime (prepare-abox-for-querying-and-sat-p (current-abox)))
            (format *output-stream* "done.~%"))

              

          (show-statistics)
          
          (let ((t2c (get-internal-run-time)))

            (setf *consistency-time* 
                  (/ (- t2c t2b) internal-time-units-per-second))


	    (format *output-stream* "~%Compute index structures... ")

            (ts::midelora-retrieve (?x) (?x top))
	    
            (format *output-stream* "done.~%")
	    
	    (let ((t2d (get-internal-run-time)))


              (setf *prep-time* 
                    (/ (- t2d t2c) internal-time-units-per-second))
              (setf *inds* (length (all-individuals)))

              #+:Allegro (excl:gc t)
	      (format *output-stream* 
                      #+:no-server
                      "~%Load: ~,20T~,4F ~%Preparation: ~,20T~,4F ~%Consistency: ~,20T~,4F ~%Index: ~,20T~,4F ~%Individuals: ~,20T~D ~%Concept Assertions: ~,20T~D ~%Role Assertions: ~,20T~D~%" 
                      #-:no-server
                      "~%Load: ~,20T~,4F ~%Preparation: ~,20T~,4F ~%Consistency: ~,20T~,4F ~%Index: ~,20T~,4F~%" 
		      (/ (- t1b t1a) internal-time-units-per-second)
                      (/ (- t2b t2a) internal-time-units-per-second)
		      (if check-abox-consistency
			  (/ (- t2c t2b) internal-time-units-per-second)
		        0)
		      (/ (- t2d t2c) internal-time-units-per-second)
                      #+:no-server (length (concept-instances top))
                      #+:no-server (length (all-concept-assertions))
                      #+:no-server (length (all-role-assertions))))))))))


;;; ======================================================================

(defun run-lubm-benchmark (benchmark-function 
			   check-abox-consistency
			   &optional (univs 0) (min-deps 0) (max-deps min-deps))
  
  
  (loop for deps1 from (if (null min-deps)
                           0
			 min-deps)
        to (if (or (null max-deps) (not (zerop univs)))
               0
             max-deps)
        do
	
	(format *output-stream* "~%Initializing...~%")
	
	(prepare-lubm-data-n-universities check-abox-consistency 
					  univs
					  (if (or (null max-deps) (not (zerop univs)))
					      nil
					    deps1))
	
	(format *output-stream* "~%Querying...~%")
	
	(funcall benchmark-function
		 univs (if (or (null max-deps) (not (zerop univs)))
			   nil
			 deps1))) 
  (values))

(defun original-lubm (universities departments)

  ;;; Query 1

  (with-abox-retrieval ( (current-abox) )

   (benchmark universities
              departments  1 (and                        
                              (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#GraduateStudent|)
                              (?x |http://www.Department0.University0.edu/GraduateCourse0|
                                  |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#takesCourse|))           
	     
              (?x))

   ;;; Query 2


   (benchmark universities
              departments  2 (and
                             
                              (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#GraduateStudent|)
                              (?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#University|)
                              (?z |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Department|)
			     
                              (?x ?z |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#memberOf|)
                              (?z ?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#subOrganizationOf|)
			     
                              (?x ?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#undergraduateDegreeFrom|))
	     
              (?x ?y ?z))

   ;;; Query 3

   (benchmark universities
              departments  3 (and
			     
                              (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Publication|)
                              (?x |http://www.Department0.University0.edu/AssistantProfessor0|
                                  |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#publicationAuthor|))
	     
              (?x |http://www.Department0.University0.edu/AssistantProfessor0|))

  
   ;;; Query 4

   (benchmark universities
              departments  4 (and (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Professor|)
                                  (?x |http://www.Department0.University0.edu|
                                      |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#worksFor|)
                                  (?x (a |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#name|))
                                  (?x (a |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#emailAddress|))
                                  (?x (a |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#telephone|))
                                  )
	     
              (?x ))

   ;;; Query 5

   (benchmark universities
              departments  5 (and (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Person|)
                                  (?x |http://www.Department0.University0.edu|
                                      |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#memberOf|))
	     
              (?x))

   ;;; Query 6 

   (benchmark universities
              departments  6 (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Student|)

              (?x))

   ;;; Query 7


   (benchmark universities
              departments  7 (and (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Student|)
                                  (?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Course|)
                                  (|http://www.Department0.University0.edu/AssociateProfessor0|
                                   ?y
                                   |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#teacherOf|)
                                  (?x ?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#takesCourse|))
	     
              (?x ?y))


   ;;; Query 8 

   (benchmark universities
              departments  8 (and (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Student|)
                                  (?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Department|)
                                  (?x ?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#memberOf|)
                                  (?y |http://www.University0.edu| 
                                      |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#subOrganizationOf|)
                                  (?x (a |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#emailAddress|)))
	     
              (?x ?y))

   ;;; Query 9 

   (benchmark universities
              departments  9 (and (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Student|)
                                  (?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Faculty|)
                                  (?z |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Course|)
                                  (?x ?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#advisor|)
                                  (?x ?z |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#takesCourse|)
                                  (?y ?z |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#teacherOf|))

              (?x ?y ?z))

   ;;; Query 10 

   (benchmark universities 
              departments  10 (and (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Student|)
                                   (?x |http://www.Department0.University0.edu/GraduateCourse0|
                                       |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#takesCourse|))
	     
              (?x))

   ;;; Query 11 

   (benchmark universities
              departments  11 (and (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#ResearchGroup|)
                                   (?x |http://www.University0.edu|
                                       |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#subOrganizationOf|))
	     
              (?x))

   ;;; Query 12 
				  
   (benchmark universities
              departments  12 (and (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Chair|)
                                   (?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Department|)
                                   (?x ?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#memberOf|)
                                   (?y |http://www.University0.edu|
                                       |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#subOrganizationOf|))

              (?x ?y))

   ;;; Query 13
   (benchmark universities
              departments  13 (and (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Person|)
                                   (|http://www.University0.edu|
                                    ?x
                                    |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#hasAlumnus|))

              (?x))

   ;;; Query 14

   (benchmark universities
              departments  14 (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#UndergraduateStudent|)

              (?x))

   ))

(defun simple-lubm (universities departments)


   (benchmark universities
              departments  2 (and
                             
                              (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#GraduateStudent|)
                              (?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#University|)
                              (?z |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Department|)
			     
                              (?x ?z |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#memberOf|)
                              (?z ?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#subOrganizationOf|)
			     
                              (?x ?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#undergraduateDegreeFrom|))
	     
              (?x ?y ?z)))

(defun subsumption-lubm (universities departments)

  (benchmark universities
	     departments  1 
	     (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Person|)
	     (?x))
  

  (benchmark universities
	     departments  2
	     (and (?x  |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Person|)
		  (?x ?y  |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#worksFor|)
		  (?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Organization|))
	     (?x ?y))

  
  (benchmark universities
	     departments  3
	     (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Professor|)
	     (?x))
  
  (benchmark universities
	     departments  4 
	     (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Chair|)
	     (?x))

  (benchmark universities
	     departments  5
	     (and (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Chair|)
		  (?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Department|)
		  (?x ?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#memberOf|))
	     (?x ?y))
  
  (benchmark universities
	     departments  6
	     (and (?x  |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Professor|)
		  (?x ?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#headOf|)
		  (?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Department|))
	     (?x ?y)))




(defun subsumption-lubm (universities departments)
  
  (benchmark universities
	     departments  1 
	     (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Person|)
	     (?x))
  

  (benchmark universities
	     departments  2
	     (and (?x  |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Person|)
		  (?x ?y  |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#worksFor|)
		  (?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Organization|))
	     (?x ?y))

  
  (benchmark universities
	     departments  3
	     (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Professor|)
	     (?x))


  (benchmark universities
	     departments  4
	     (?x |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Student|)
	     (?x))
	     
  (benchmark universities
	     departments  5
	     (and (?x  |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Student|)
		  (?x ?y  |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#takesCourse|)
		  (?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#Course|))
	     (?x ?y))
  
  (benchmark universities
	     departments  6
	     (and (?x  |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#GraduateStudent|)
		  (?x ?y  |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#takesCourse|)
		  (?y |http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#GraduateCourse|))
	     (?x ?y)))



;;;
;;;
;;;

(defun run-lubm-tests (benchmark-function univs 
                                          &key (max-deps-univ-1 nil)
                                          (check-abox-consistency t))
  
  (let ((*global-prep* 0)
	(*global-exec* 0))

    (when (and (> univs 1) max-deps-univ-1)
      (error "Maximum number of departments may only be specified ~
            if only one university is processed."))
    
    (pprint (ts::describe-query-processing-mode))
    
    (run-lubm-benchmark benchmark-function 
			check-abox-consistency
			(1- univs) (and max-deps-univ-1 (1- max-deps-univ-1)))
    

    (format *output-stream* "Total preparation time: ~12,D Total execution time: ~12,D  Rollback time: ~12,D~%" 
	    (float *global-prep*)
	    (float *global-exec*)
            (float (/ *time-spend-in-global-rollback* internal-time-units-per-second)))

    (format *file-stream* "Prep ~12,D Exec ~12,D Global Rollback ~12,D~%" 
	    (float *global-prep*)
	    (float *global-exec*)
            (float (/ *time-spend-in-global-rollback* internal-time-units-per-second)))))

;;;
;;;
;;;

(defun test (univs &optional deps)

  (setf *results* nil)

  (loop as deps from 1 to 1 do
        
        (full-reset)

        (let ((ts::*multiprocess-queries* nil)
              (settings 0))

          (dolist (setting (list
			 
                            ;; ( :query-optimization :optimizer-use-cardinality-heuristics   
                            ;;   :two-phase-query-processing-mode  :query-repository :compiler)  
			 
			 
                            (list t  t nil nil nil nil)))
	 
            (if (sixth setting)

                (setf ts::*compile-queries-p* t
                      ts::*compile-inline-p* t ;nil
                      ts::*runtime-evaluation-p* nil)
	   
              (setf ts::*compile-queries-p* nil
                    ts::*compile-inline-p* nil
                    ts::*runtime-evaluation-p* t))
	 
	 
            (incf settings)
	 
            (with-open-file (*file-stream* 
                             (format nil "diss:benchmark-results;result-univs-~A-deps-~A-setting-~A.res" 
                                     univs
                                     deps
                                     settings)
                             :direction :output 
                             :if-exists :supersede 
                             :if-does-not-exist :create)
	   
	   
              (reset)

	   
              (ts::eval-nrql-settings 
	    
               #'(lambda () 
                   (run-lubm-tests ;'original-lubm
                    'simple-lubm
                                   univs
                                   :check-abox-consistency t
                                   :max-deps-univ-1 deps
                                   ))
	    
               :mode 3
	    
               :query-optimization (first setting)
	    
               :optimizer-use-cardinality-heuristics (second setting)
	    
               :two-phase-query-processing-mode (third setting) 
	    
               :tuple-computation-mode :set-at-a-time
	    
               :query-repository (fourth setting) 

               :query-realization nil)

                 

              (push (list univs  deps
                          *inds*
                          (float *load-time*)
                          (float *consistency-time*)
                          (float *prep-time*)
                          (float (/ *time-spend-in-global-rollback* internal-time-units-per-second))
                          *last-global*)
                    *results*)))))

  (with-open-file
      (*file-stream* 
       (format nil 
	       "diss:benchmark-results;all-results.res")
       
       :direction :output 
       :if-exists :supersede 
       :if-does-not-exist :create)
    
    (pprint *results* *file-stream*)))



(defun subsumption-test ()
  (full-reset)

  (let ((ts::*multiprocess-queries* nil))

    #+:allegro (system:set-stack-cushion nil)
    
    (thematic-substrate::start-process 

     #+:allegro (system:set-stack-cushion nil)
     

     (let ((ts::*multiprocess-queries* nil))

       
       (setf ts::*compile-queries-p* nil
	     ts::*compile-inline-p* nil
	     ts::*runtime-evaluation-p* t)

       
       (with-open-file (*file-stream* 
			"diss:benchmark-results;subsumption-test-midelora-result-1.res"
			:direction :output 
			:if-exists :supersede 
			:if-does-not-exist :create)
	 
	 
	 (full-reset)
	 
	 (ts::with-nrql-settings (:mode 3
                                  :query-optimization t
                                  :query-repository nil)
	   (run-lubm-tests 'subsumption-lubm 0
			   :check-abox-consistency t
			   )))
       
       
       (with-open-file (*file-stream* 
			"diss:benchmark-results;subsumption-test-midelora-result-2.res"
			:direction :output 
			:if-exists :supersede 
			:if-does-not-exist :create)
	 
	 
	 (full-reset)
	 
	 (ts::with-nrql-settings (:mode 3
                                  :query-optimization t
                                  :query-repository t)
	   (run-lubm-tests 'subsumption-lubm 0
			   :check-abox-consistency t
			   )
	   
	   (ts::show-current-qbox)))))))
