;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SPATIAL-SUBSTRATE; Base: 10 -*-

(in-package spatial-substrate)

;;;
;;;
;;;

(defparameter *show-in-inspector-p* t)

(defmethod prepare-substrate-for-query-execution ((substrate map*) (query query))
  t)

;;;
;;;
;;;

(defmethod querying-started ((map map*) (query query))
  (when *show-in-inspector-p* 
    (when (result-inspector-running-p) 
      (lock-buttons))
    (unhighlight-all)))

(defmethod querying-ended ((map map*) (query query))
  (when *show-in-inspector-p* 
    (when (result-inspector-running-p) 
      (unlock-buttons))
    (dolist (binding (result-bindings query))
      (loop as var in (answer-pattern query)
            as object in binding do
            (progn 
              (etypecase object
                (symbol  ; Racer ABox-Individuum! 
                 (when (get-associated-substrate-node map object)               
                   (setf (geometry:bound-to (get-associated-substrate-node map object)) var)))
                (map-object
                 (setf (geometry:bound-to object) var))))))))

;;;
;;;
;;; 


(defmethod register-bindings ((map map*) (query map-query) (answer-pattern list) (new-bindings list))

  (when (and *show-in-inspector-p* 
             (result-inspector-running-p) 
             (not (ts::bottom-up-component-query-p query))
             (not (abort-search-p query)))
    (make-query-result answer-pattern new-bindings)))

;;;
;;;
;;;

(defun show-nodes (inds)
  (unhighlight-all)
  (highlight (mapcar #'(lambda (x) 
                         (if (is-map-object-p x)
                             x
                           (get-associated-substrate-node (get-current-map) x)))
                     (ensure-list inds))))

(defun from-tuples-to-inds (tuples)
  (apply #'append
         (mapcar #'(lambda (tuple)
              (mapcar #'second tuple))
          tuples)))

;;;;
;;;;
;;;;

(defmethod get-successors-of-type ((obj map-object) (type symbol))
  (in-substrate* (get-current-map))
  (let ((frame *result-inspector-frame*))
    (setf *result-inspector-frame* nil)

    (prog1
        (loop as binding in
              (with-dlmaps-standard-settings
                (answer-query `(,(intern (format nil "*~A" (name obj))) ?*x ,type)
                              '(?*x)))
              collect (second (first binding)))
      (setf *result-inspector-frame* frame))))
  
;;;
;;;
;;;

#|

(defmethod answer-query :around ((query map-query) 
                                 (res-args list)
                                 &rest args)
  (with-dlmaps-standard-settings
    (apply #'call-next-method query res-args args)))

|# 

;;;
;;;
;;;

(defmethod get-answer ((query map-query))
  (if (not (slot-value query 'query-satisfiable))
      nil
    (progn
      (get-all-remaining-tuples query)

      (if (answer-pattern query)
          (let* ((pat (answer-pattern query))
                 (res
                  (mapcar #'(lambda (binding) 
                              (mapcar #'(lambda (var val) 
                                          (list (get-textual-head-entry var) val))
                                      pat binding))
                          (result-bindings query))))
            (if (timeout-p query)
                (cons :timeout res)
              res))
        (or (eq (bindings-found-p query) t)
            (when (timeout-p query)
              :timeout))))))


;;;
;;; Test-Interface: 
;;;

(defvar *start-time* 0)

(defmacro with-timing ((var) &body body)
  `(let ((*start-time* (get-internal-run-time)))
     (unwind-protect 
         (progn ,@body)
       (incf ,var (- (get-internal-run-time) *start-time*)))))
       



(defun load-and-install-map (file &optional (type *kind-of-map*))
  (make-map-from-sqd-file file
                          :delete-if-exists-p t
                          :racer-package 'racer-user
                          :abox 
                          (intern 
                           (string-upcase
                            (pathname-name
                             (truename file))))
                          :tbox 
                          'oejendorf
                          :type type))


(defmacro answer-map-query (head body &optional (compile-p t))
  `(funcall #'(lambda ()
                (let ((*package* ;(racer-package *cur-substrate*)
                       (find-package :racer-user))
                      (*show-in-inspector-p* nil)
                      (time 0)

                      (*add-rcc-atoms-to-abox-for-reasoning* nil))
  

                  (let ((res 
                         (with-timing (time)
                           (WITH-NRQL-SETTINGS (:QUERY-OPTIMIZATION
                                                t 
                                                :TWO-PHASE-QUERY-PROCESSING-MODE
                                                nil
                                                :TUPLE-COMPUTATION-MODE
                                                :SET-AT-A-TIME

                                                :mode 3

                                                :EXCLUDE-PERMUTATIONS
                                                NIL
                                                :QUERY-REPOSITORY
                                                nil
                                                :REPORT-INCONSISTENT-QUERIES
                                                T
                                                :QUERY-REALIZATION
                                                nil)
                  
                             (RACER:WITH-UNIQUE-NAME-ASSUMPTION
                               (LET ((THEMATIC-SUBSTRATE::*RUNTIME-EVALUATION-P* ,(not compile-p))
                                     (THEMATIC-SUBSTRATE::*COMPILE-QUERIES-P* ,compile-p)
                                     (ts::*multiprocess-queries* nil))

                                 '(ANSWER-QUERY ',(change-package-of-description 
                                                   body
                                                   :racer-user)
                                                ',(change-package-of-description 
                                                   head
                                                   :racer-user))

                                 (ANSWER-QUERY ', body
                                               ',head)))))))
                    
                    (pprint (list res
                                  (length res)
                                  (float (/ time
                                            internal-time-units-per-second))))

                    (values res 
                            (length res)
                            (float (/ time
                                      internal-time-units-per-second))))))))


(defun diskql-test (n)

  (let* ((map (ecase n 
                (0 "~/maps/small.sqd")
                (1 "~/maps/va5.sqd")
                (2 "~/maps/oejendorf2.sqd")))
                         
         (type ; 'basic-explicit-racer-explicit-relations-map
          'midelora-explicit-relations-map))

    (time (load-and-install-map map type))

    ;(when (= n 1) (close-roles (get-current-map)))
    
    (case n
      (1 
       (racer:instance racer-user::ind-1215 racer-user::gewerbe)
       (racer:instance racer-user::ind-2951 racer-user::fabrik)
       (racer:instance racer-user::ind-423 racer-user::naturschutzgebiet))

      (2 

       (racer:instance racer-user::ind-9474 racer-user::gewerbe)

       (racer:instance racer-user::ind-19664 racer-user::fabrik)

       (racer:instance racer-user::ind-8765 racer-user::naturschutzgebiet)))

    (terpri)
    (princ "Prepare...") (terpri)
    (answer-map-query (?*x) (?*x racer-user::top))
    
    (terpri)
    (princ "Q1") (terpri)
    (answer-map-query (?*x) (?*x racer-user::wohnen))

    (terpri)
    (princ "Q2") (terpri)
    (answer-map-query (?x ?y)
                      (and (?*x racer-user::wohnen) (?x ?y :adjacent) (?*y racer-user::autobahn)))

    (terpri)
    (princ "Q3") (terpri)
    (answer-map-query (?w ?g ?e ?t)
                      (and (?*w racer-user::wohnen) (?*g racer-user::wiese-weide) 
                           (?*e racer-user::gewerbe)
                           (?*t (or racer-user::teich racer-user::see))
                           (?w ?g :adjacent) (?w ?e :adjacent) (?g ?t :contains)))

    (terpri)
    (princ "Q3 b)") (terpri)
    (answer-map-query (?w ?g ?e ?t)
                      (and (?*w racer-user::wohnen) (?*g racer-user::wiese-weide) 
                           (?*e racer-user::gewerbe)
                           (or (?*t racer-user::teich) (?*t  racer-user::see))
                           (?w ?g :adjacent) (?w ?e :adjacent) (?g ?t :contains)))

    

    (terpri)
    (princ "Q4") (terpri)
    (answer-map-query (?x)
                      (and (?*x (and racer-user::wohnen 
                                     (racer-user::at-least 8 racer-user::ntppi racer-user::gebaeude)))
                           (?x (:area (> area 30000)))))

    (terpri)
    (princ "Q5") (terpri)
    (answer-map-query (?x)
                      (and (?*x racer-user::wohnen) 
                           (neg (project-to (?x)
                                            (and (?x ?y :adjacent)
                                                 (?*y (or racer-user::industrie 
                                                          racer-user::gewerbe)))))))

    (terpri)
    (princ "Q5 (b)") (terpri)

    (racer-user::implies racer-user::gruenflaeche (and (not racer-user::industrie) (not racer-user::gewerbe)))
    (racer-user::implies racer-user::natuerliche-flaeche (and (not racer-user::industrie) (not racer-user::gewerbe)))
    (racer-user::implies racer-user::parkplatz (and (not racer-user::industrie) (not racer-user::gewerbe)))
    (racer-user::implies racer-user::wasser (and (not racer-user::industrie) (not racer-user::gewerbe)))
    (racer-user::implies racer-user::bab (and (not racer-user::industrie) (not racer-user::gewerbe)))
    (racer-user::implies racer-user::weg (and (not racer-user::industrie) (not racer-user::gewerbe)))
    (racer-user::implies racer-user::verkehrsbegleitgruen (and (not racer-user::industrie) (not racer-user::gewerbe)))
    (racer-user::implies racer-user::bab-randbegrenzung (and (not racer-user::industrie) (not racer-user::gewerbe)))


    (answer-map-query (?*x)
                      (and (?*x (and racer-user::wohnen (all racer-user::ec 
                                                             (not (or racer-user::industrie 
                                                                      racer-user::gewerbe)))))))

    (terpri)
    (princ "Q6") (terpri)
    (answer-map-query (?x ?y ?z ?u ?*f
                          )
                      (and (?*x racer-user::teich)
                           (?x ?y :inside) 
                           (?*y racer-user::naturschutzgebiet)
                           (?x ?z :flows-in)
                           (?*z racer-user::bach)
                           (?z ?u (:or :touches :crosses))
                           (?*u racer-user::gewerbe )
                           (?u ?f :contains)
                           (?*f racer-user::fabrik)
                           ))

    (terpri)
    (princ "Q7") (terpri)
    (answer-map-query (?x ?y)
                      (and (?x racer-user::wohnen)
                           (?y ?x (:inside-distance 750))
                           (?y racer-user::u-bahn-station-symbol*)))


    (terpri)
    (princ "Q8") (terpri)

    (answer-map-query (?w ?g ?e ?t ?ubahn)
                      (and (?*w racer-user::wohnen) 
                           (or (?*g racer-user::wiese-weide) 
                               (?*g racer-user::park))
                           (?*t racer-user::teich)
                           (?w ?g :adjacent) 
                           (?w ?e :adjacent)
                           (?g ?t :contains) 
                           (?ubahn ?w (:inside-distance 750)) (?*ubahn racer-user::u-bahn-station-symbol*)
                           (neg (project-to (?w)
                                            (and (?*w racer-user::wohnen) 
                                                 (?w ?x :adjacent)
                                                 (?*x racer-user::autobahn))))))))


(defun diskql-test2 (n)

  (let* ((map (ecase n 
                (0 "~/maps/small.sqd")
                (1 "~/maps/va5.sqd")
                (2 "~/maps/oejendorf2.sqd")))
                         
         (type 'basic-explicit-racer-explicit-relations-map))

    (time (load-and-install-map map type))

    (when (= n 1) (close-roles (get-current-map)))
    
    (case n
      (1 
       (racer:instance racer-user::ind-1215 racer-user::gewerbe)
       (racer:instance racer-user::ind-2951 racer-user::fabrik)
       (racer:instance racer-user::ind-423 racer-user::naturschutzgebiet))

      (2 

       (racer:instance racer-user::ind-9474 racer-user::gewerbe)

       (racer:instance racer-user::ind-19664 racer-user::fabrik)

       (racer:instance racer-user::ind-8765 racer-user::naturschutzgebiet)))

    (terpri)
    (princ "Prepare...") (terpri)
    (answer-map-query (?*x) (?*x racer-user::top))
    
    (terpri)
    (princ "Q1") (terpri)
    (answer-map-query (?*x) (?*x racer-user::wohnen))

    (terpri)
    (princ "Q2") (terpri)
    (answer-map-query (?*x ?*y)
                      (and (?*x racer-user::wohnen) (?*x ?*y :adjacent) (?*y racer-user::autobahn)))

    (terpri)
    (princ "Q3") (terpri)
    (answer-map-query (?*w ?*g ?*e ?*t)
                      (and (?*w racer-user::wohnen) (?*g racer-user::wiese-weide) 
                           (?*e racer-user::gewerbe)
                           (?*t (or racer-user::teich racer-user::see))
                           (?*w ?*g :adjacent) (?*w ?*e :adjacent) (?*g ?*t :contains)))

    (terpri)
    (princ "Q3 b)") (terpri)
    (answer-map-query (?*w ?*g ?*e ?*t)
                      (and (?*w racer-user::wohnen) (?*g racer-user::wiese-weide) 
                           (?*e racer-user::gewerbe)
                           (or (?*t racer-user::teich) (?*t  racer-user::see))
                           (?*w ?*g :adjacent) (?*w ?*e :adjacent) (?*g ?*t :contains)))

    

    (terpri)
    (princ "Q4") (terpri)
    (answer-map-query (?*x)
                      (and (?*x (and racer-user::wohnen 
                                     (racer-user::at-least 8 racer-user::ntppi racer-user::gebaeude)))
                           (?x (:area (> area 30000)))))

    (terpri)
    (princ "Q5") (terpri)
    (answer-map-query (?*x)
                      (and (?*x racer-user::wohnen) 
                           (neg (project-to (?*x)
                                            (and (?*x ?*y :adjacent)
                                                 (?*y (or racer-user::industrie 
                                                          racer-user::gewerbe)))))))

    (terpri)
    (princ "Q5 (b)") (terpri)

    (racer-user::implies racer-user::gruenflaeche (and (not racer-user::industrie) (not racer-user::gewerbe)))
    (racer-user::implies racer-user::natuerliche-flaeche (and (not racer-user::industrie) (not racer-user::gewerbe)))
    (racer-user::implies racer-user::parkplatz (and (not racer-user::industrie) (not racer-user::gewerbe)))
    (racer-user::implies racer-user::wasser (and (not racer-user::industrie) (not racer-user::gewerbe)))
    (racer-user::implies racer-user::bab (and (not racer-user::industrie) (not racer-user::gewerbe)))
    (racer-user::implies racer-user::weg (and (not racer-user::industrie) (not racer-user::gewerbe)))
    (racer-user::implies racer-user::verkehrsbegleitgruen (and (not racer-user::industrie) (not racer-user::gewerbe)))
    (racer-user::implies racer-user::bab-randbegrenzung (and (not racer-user::industrie) (not racer-user::gewerbe)))


    (answer-map-query (?*x)
                      (and (?*x (and racer-user::wohnen (all racer-user::ec 
                                                             (not (or racer-user::industrie 
                                                                      racer-user::gewerbe)))))))

    (terpri)
    (princ "Q6") (terpri)
    (answer-map-query (?*x ?*y ?*z ?*u ?*f
                          )
                      (and (?*x racer-user::teich)
                           (?*x ?*y :inside) 
                           (?*y racer-user::naturschutzgebiet)
                           (?*x ?*z :flows-in)
                           (?*z racer-user::bach)
                           (?*z ?*u (:or :touches :crosses))
                           (?*u racer-user::gewerbe )
                           (?*u ?*f :contains)
                           (?*f racer-user::fabrik)
                           ))

    (terpri)
    (princ "Q7") (terpri)
    (answer-map-query (?*x ?*y)
                      (and (?*x racer-user::wohnen)
                           (?y ?x (:inside-distance 750))
                           (?*y racer-user::u-bahn-station-symbol*)))


    (terpri)
    (princ "Q8") (terpri)

    (answer-map-query (?*w ?*g ?*e ?*t ?*ubahn)
                      (and (?*w racer-user::wohnen) 
                           (or (?*g racer-user::wiese-weide) 
                               (?*g racer-user::park))
                           (?*t racer-user::teich)
                           (?*w ?*g :adjacent) 
                           (?*w ?*e :adjacent)
                           (?*g ?*t :contains) 
                           (?ubahn ?w (:inside-distance 750))
                           (?*ubahn racer-user::u-bahn-station-symbol*)
                           (neg (project-to (?*w)
                                            (and (?*w racer-user::wohnen) 
                                                 (?*w ?*x :adjacent)
                                                 (?*x racer-user::autobahn))))))))






(defun bug (n)

  (let* ((map (ecase n 
                (1 "~/maps/va5.sqd")
                (2 "~/maps/oejendorf2.sqd")))
                         
         (type 'basic-explicit-racer-implicit-relations-map))

    (time (load-and-install-map map type))

    
    (ecase n
      (1 
       (racer:instance racer-user::ind-1215 racer-user::gewerbe)
       (racer:instance racer-user::ind-2951 racer-user::fabrik)
       (racer:instance racer-user::ind-423 racer-user::naturschutzgebiet))

      (2 

       (racer:instance racer-user::ind-9474 racer-user::gewerbe)

       (racer:instance racer-user::ind-19664 racer-user::fabrik)

       (racer:instance racer-user::ind-8765 racer-user::naturschutzgebiet)))
    

    (terpri)
    (princ "Q2") (terpri)
    (answer-map-query (?x ?y)
                      (and (?*x racer-user::wohnen) (?x ?y :adjacent) (?*y racer-user::autobahn)))))



;;;
;;;
;;; 



(defmacro midelora-answer-map-query (head body &optional (compile-p t))
  `(funcall #'(lambda ()
                (let ((*package* ;(racer-package *cur-substrate*)
                       (find-package :prover))
                      (*show-in-inspector-p* nil)
                      (time 0)

                      (*add-rcc-atoms-to-abox-for-reasoning* nil))
  

                  (let ((res 
                         (with-timing (time)
                           (WITH-NRQL-SETTINGS (:QUERY-OPTIMIZATION
                                                t 
                                                :TWO-PHASE-QUERY-PROCESSING-MODE
                                                nil
                                                :TUPLE-COMPUTATION-MODE
                                                :SET-AT-A-TIME

                                                :mode 3

                                                :EXCLUDE-PERMUTATIONS
                                                NIL
                                                :QUERY-REPOSITORY
                                                nil
                                                :REPORT-INCONSISTENT-QUERIES
                                                nil
                                                :QUERY-REALIZATION
                                                nil)
                  
                             (LET ((THEMATIC-SUBSTRATE::*RUNTIME-EVALUATION-P* ,(not compile-p))
                                   (THEMATIC-SUBSTRATE::*COMPILE-QUERIES-P* ,compile-p)
                                   (ts::*multiprocess-queries* nil))

                                 '(ANSWER-QUERY ',(change-package-of-description 
                                                   body
                                                   :prover)
                                                ',(change-package-of-description 
                                                   head
                                                   :prover))

                                 (ANSWER-QUERY ', body
                                               ',head))))))
                    
                    (pprint (list res
                                  (length res)
                                  (float (/ time
                                            internal-time-units-per-second))))

                    (values res 
                            (length res)
                            (float (/ time
                                      internal-time-units-per-second))))))))


(defun midelora-diskql-test (n)

  (let* ((map (ecase n 
                (0 "~/maps/small.sqd")
                (1 "~/maps/va5.sqd")
                (2 "~/maps/oejendorf2.sqd")))
                         
         (type ; 'basic-explicit-racer-explicit-relations-map
          'midelora-explicit-relations-map))

    (time (load-and-install-map map type))
    
    ;(when (= n 1) 
    ;(close-roles (get-current-map))

    (prover::implies prover::gruenflaeche (and (not prover::industrie) (not prover::gewerbe)))
    (prover::implies prover::natuerliche-flaeche (and (not prover::industrie) (not prover::gewerbe)))
    (prover::implies prover::parkplatz (and (not prover::industrie) (not prover::gewerbe)))
    (prover::implies prover::wasser (and (not prover::industrie) (not prover::gewerbe)))
    (prover::implies prover::bab (and (not prover::industrie) (not prover::gewerbe)))
    (prover::implies prover::weg (and (not prover::industrie) (not prover::gewerbe)))
    (prover::implies prover::verkehrsbegleitgruen (and (not prover::industrie) (not prover::gewerbe)))
    (prover::implies prover::bab-randbegrenzung (and (not prover::industrie) (not prover::gewerbe)))

    (case n
      (1 
       (prover:instance prover::ind-1232 prover::gewerbe)
       (prover:instance prover::ind-2951 prover::fabrik)
       (prover:instance prover::ind-423 prover::naturschutzgebiet))

      (2 

       (prover:instance prover::ind-9474 prover::gewerbe)

       (prover:instance prover::ind-19664 prover::fabrik)

       (prover:instance prover::ind-8765 prover::naturschutzgebiet)))

    (terpri)
    (princ "Prepare...") (terpri)
    (midelora-answer-map-query (?*x) (?*x prover::top))
    
    (prover::with-abox-retrieval ( (prover::current-abox) )

                                 (terpri)
                                 (princ "Q1") (terpri)
                                 (midelora-answer-map-query (?*x) (?*x prover::wohnen))

                                 (terpri)
                                 (princ "Q2") (terpri)
                                 (midelora-answer-map-query (?*x ?*y)
                                                            (and (?*x prover::wohnen) (?*x ?*y :adjacent) (?*y racer-user::autobahn)))


                                 (terpri)
                                 (princ "Q3") (terpri)
                                 (midelora-answer-map-query (?*w ?*g ?*e ?*t)
                                                            (and (?*w prover::wohnen) (?*g prover::wiese-weide) 
                                                                 (?*e prover::gewerbe)
                                                                 (?*t (or prover::teich prover::see))
                                                                 (?*w ?*g :adjacent) (?*w ?*e :adjacent) (?*g ?*t :contains)))

                                 (terpri)
                                 (princ "Q3 b)") (terpri)
                                 (midelora-answer-map-query (?*w ?*g ?*e ?*t)
                                                            (and (?*w prover::wohnen) (?*g prover::wiese-weide) 
                                                                 (?*e prover::gewerbe)
                                                                 (or (?*t prover::teich) (?*t  prover::see))
                                                                 (?*w ?*g :adjacent) (?*w ?*e :adjacent) (?*g ?*t :contains)))
                                 

                                 (unless (= n 2)
                                   (terpri)
                                   (princ "Q4") (terpri)
                                   (midelora-answer-map-query (?*x)
                                                              (and (?*x (and prover::wohnen 
                                                                             (prover::at-least 8 prover::ntppi prover::top)))
                                                                   (?x (:area (> area 30000))))))

                                 (terpri)
                                 (princ "Q5") (terpri)
                                 (midelora-answer-map-query (?*x)
                                                            (and (?*x prover::wohnen) 
                                                                 (neg (project-to (?*x)
                                                                                  (and (?*x ?*y :adjacent)
                                                                                       (?*y (or prover::industrie 
                                                                                                prover::gewerbe)))))))

                                 (terpri)
                                 (princ "Q5 (b)") (terpri)

                                 (midelora-answer-map-query (?*x)
                                                            (and (?*x (and prover::wohnen (all prover::ec 
                                                                                               (not (or prover::industrie 
                                                                                                        prover::gewerbe)))))))

                                 (terpri)
                                 (princ "Q6") (terpri)
                                 (midelora-answer-map-query (?*x ?*y ?*z ?*u ?*f
                                                                 )
                                                            (and (?*x prover::teich)
                                                                 (?*x ?*y :inside) 
                                                                 (?*y prover::naturschutzgebiet)
                                                                 (?*x ?*z :flows-in)
                                                                 (?*z prover::bach)
                                                                 (?*z ?*u (:or :touches :crosses))
                                                                 (?*u prover::gewerbe )
                                                                 (?*u ?*f :contains)
                                                                 (?*f prover::fabrik)
                                                                 ))

                                 (terpri)
                                 (princ "Q7") (terpri)
                                 (midelora-answer-map-query (?*x ?*y)
                                                            (and (?*x prover::wohnen)
                                                                 (?y ?x (:inside-distance 750))
                                                                 (?*y prover::u-bahn-station-symbol*)))


                                 (terpri)
                                 (princ "Q8") (terpri)

                                 (midelora-answer-map-query (?*w ?*g ?*e ?*t ?*ubahn)
                                                            (and (?*w prover::wohnen) 
                                                                 (or (?*g prover::wiese-weide) 
                                                                     (?*g prover::park))
                                                                 (?*t prover::teich)
                                                                 (?*w ?*g :adjacent) 
                                                                 (?*w ?*e :adjacent)
                                                                 (?*g ?*t :contains) 
                                                                 (?ubahn ?w (:inside-distance 750)) 
                                                                 (?*ubahn prover::u-bahn-station-symbol*)
                                                                 (neg (project-to (?*w)
                                                                                  (and (?*w prover::wohnen) 
                                                                                       (?*w ?*x :adjacent)
                                                                                       (?*x prover::autobahn)))))))))



