;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

(defun load-both (&optional (fn "test.kb") &key save-load-p (verify-p t) debug-p log-p)
  (declare (ignorable debug-p log-p))
  (reset-statistics)
  (racer:delete-all-tboxes)
  (delete-all-tboxes)
  ;(enable-racer-validation)
  ;(disable-racer-validation)

  (in-tbox foobar :delete-if-exists-p t)

  (let ((*package* (find-package :prover)))
    (format t "Loading ~A into DLMAPS...~%" fn)
    (load (format nil "dlmaps:prover;~A" fn)))
  ;(setf (language *cur-tbox*) (make-language 'alchf-rplus))
  
  (when save-load-p 
    (format t "Saving ~A!~%" "dlmaps:prover;test.tbox")
    (save-tbox *cur-tbox* "dlmaps:prover;test.tbox"))
  
  (in-tbox foobar)

  (when verify-p
    (let ((*package* :racer-user))
      (racer:in-tbox racer-user::foobar)
      (format t "Loading ~A into RACER...~%" fn)
      (if (not save-load-p)
          (load (format nil "dlmaps:prover;~A" fn))
        (load "dlmaps:prover;test.tbox")))
  
    (racer::set-current-tbox 'racer-user::foobar)
    
    (compare-taxonomies)
    
    ;(classify-cur-tbox debug-p log-p)
    ))


(defvar *stream*)


(defun enable-racer-validation ()
  (setf *racer-validation-p* t
        *logging-p* nil 
        *racer-tbox* 'racer-user::foobar))


(defun enable-logging ()
  (setf *logging-p* t))

(defun disable-logging ()
  (setf *logging-p* nil))

(defun disable-racer-validation ()
  (setf *racer-validation-p* nil))
        

(defun classify-cur-tbox (&optional (debug-p *debug-p*)  (logfile-p *logging-p*))
  (reset-sat-status *cur-tbox*)
  
  (let ((*debug-p* debug-p))
    
    (racer:classify-tbox 'racer-user::foobar)

    (with-open-file (stream "prover.log" :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      
      (let* ((*standard-output* 
             (if logfile-p 
                 stream
               *standard-output*))
             (*print-pretty* t)
             (*debug-io* *standard-output*)
             (*error-output* *standard-output*)
             (*trace-output* *standard-output*))

        (classify *cur-tbox* 
              ;:language +alchf+
                  :debug-p *debug-p*
                  :cache-models-p t
                  :use-cached-models-p t
                  :subtableau-caching-p t
                  :semantic-branching-p t
                  :delete-nodes-p nil
                  :use-unsat-cache-p t
                  :reuse-nodes-p nil 
                  :non-determinism-p nil)))
  
  ;(visualize-dag (taxonomy *cur-tbox*))

    ))


(defun compare-taxonomies ()

  (racer:classify-tbox)
  (classify-cur-tbox nil nil)

  (let* ((key #'(lambda (x) 
                  (format nil "~A" x)))
         (t1 (sort 
              (mapcar #'(lambda (x) 
                          (list (sort (ensure-list (first x)) #'string-lessp :key key)
                                (sort (ensure-list (second x)) #'string-lessp :key key)
                                (sort (ensure-list (third x)) #'string-lessp :key key)))
                      (racer:taxonomy))
              #'string-lessp
              :key #'(lambda (x) 
                       (format nil "~A" (caar x)))))
         (t2 (sort 
              (mapcar #'(lambda (x) 
                          (list (sort (ensure-list (first x)) #'string-lessp :key key)
                                (sort (ensure-list (second x)) #'string-lessp :key key)
                                (sort (ensure-list (third x)) #'string-lessp :key key)))
                      (taxonomy-list *cur-tbox*))
              #'string-lessp
              :key #'(lambda (x) 
                       (format nil "~A" (caar x)))))
         (okay t))
    
    ;(pprint t1) (terpri) (pprint t2) (terpri)

    (mapc #'(lambda (a b)
              (let ((a (read-from-string (format nil "~A" a)))
                    (b (read-from-string (format nil "~A" b))))

                ;; (format t "~%   Racer: ~A~%   DLMAPS: ~A~%" a b)
                
                (unless (set-equal (first a) (first b) :test #'(lambda (x y) (set-equal (ensure-list x) (ensure-list y))))
                  (format t "*** BAD CONCEPT ORDER: ~A ~A~%" (first a) (first b)))

                (unless (set-equal (second a) (second b) :test #'(lambda (x y) (set-equal (ensure-list x) (ensure-list y))))
                  (setf okay nil)
                  (format t "*** MISMATCH FOR PARENTS OF CONCEPT ~A
     TOO MANY: ~A
     TOO LESS: ~A~%~%"
                          (first a) 
                          (set-difference (second b) (second a) 
                                          :test #'(lambda (x y) (set-equal (ensure-list x) (ensure-list y))))
                          (set-difference (second a) (second b)
                                          :test #'(lambda (x y) (set-equal (ensure-list x) (ensure-list y))))))
                
                (unless (set-equal (third a) (third b) :test #'(lambda (x y) (set-equal (ensure-list x) (ensure-list y))))
                  (setf okay nil)
                  (format t "*** MISMATCH FOR CHILDREN OF CONCEPT ~A
     TOO MANY: ~A
     TOO LESS: ~A~%~%"
                          (first a) 
                          (set-difference (third b) (third a)
                                          :test #'(lambda (x y) (set-equal (ensure-list x) (ensure-list y))))
                          (set-difference (third a) (third b)
                                          :test #'(lambda (x y) (set-equal (ensure-list x) (ensure-list y))))))))

          t1 t2)

    (if okay
        'taxonomies-are-identical
      'error-taxonomies-are-different)))


;;;


(defun racer-subsumes (f g)
  (let ((*package* (find-package :racer-user)))
    (racer:concept-subsumes-p 
     (change-package-of-description f :racer-user)
     (change-package-of-description g :racer-user)
     'racer-user::foobar)))

(defun midelora-subsumes (f g &rest args) 
  (let ((*print-pretty* t))
    (apply #'concept-subsumes-p f g 
           :debug-p t
           args)))

