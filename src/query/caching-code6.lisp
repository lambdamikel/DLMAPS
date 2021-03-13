;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

(defconstant +cache-hash-table-size+ 100)

;;;
;;; Methoden fuer Caching
;;; Cache & Retrieve 
;;; 

;;;
;;; Caching fuer den Compiler
;;;

(defun get-atom-caching-code (query) 
  `(with-slots (bindings-found-p 
                bindings-hash 
                bindings) ,query
       
     (setf bindings-found-p t)
       
     (unless bindings-hash
       (setf bindings-hash
             (mht :size +cache-hash-table-size+ 
                  :test #'equal)))

     (let ((vals (mapcar #'bound-to ',(all-vois query))))

       (unless (gethash vals bindings-hash)
         (setf (gethash vals bindings-hash) t)
         (push vals bindings)))))

(defun get-and-caching-code (query) 
  (get-atom-caching-code query))


(defun get-or-caching-code (query) 
  (declare (ignore query))
  (to-be-implemented 'get-or-caching-code))

;;;
;;; Caching fuer Runtime (ohne Compiler) 
;;;

(defun atom-cache-binding (query) 
  (with-slots (bindings-found-p 
               bindings-hash 
               bindings
               all-vois) query
       
    (setf bindings-found-p t)
       
    (unless bindings-hash
      (setf bindings-hash
            (mht :size +cache-hash-table-size+ 
                 :test #'equal)))

    (let ((vals (mapcar #'bound-to all-vois)))
      (unless (gethash vals bindings-hash)
        (setf (gethash vals bindings-hash) t)
        (push vals bindings)))))

(defun and-cache-binding (query) 
  (atom-cache-binding query))

(defun or-cache-binding (query) 
  (declare (ignore query))
  (to-be-implemented 'or-cache-binding))

;;;
;;; fuer den Compiler
;;;

(defmethod get-cache-binding-code ((query atomic-query))
  (get-atom-caching-code query))

(defmethod get-cache-binding-code ((query binary-query))
  (get-atom-caching-code query))

(defmethod get-cache-binding-code ((query and-query))
  (get-and-caching-code query))

(defmethod get-cache-binding-code ((query or-query))
  (get-or-caching-code query))

;;;
;;; Runtime (ohne Compiler)
;;;

(defmethod cache-binding ((query atomic-query))
  (atom-cache-binding query))

(defmethod cache-binding ((query binary-query))
  (atom-cache-binding query))

(defmethod cache-binding ((query and-query))
  (and-cache-binding query))

(defmethod cache-binding ((query or-query))
  (or-cache-binding query))

;;;
;;; Methoden fuer Cache Retrieve
;;;

(defmethod needs-reordering-p ((query query))
  nil)

(defmethod needs-reordering-p ((query binary-query))
  (not (equal (vois query) (all-vois query))))

;;; Unary Atoms
;;;

(defvar *binding* nil)

;;;
;;; Check for Unary Atoms
;;; 

(defmethod get-code-check-if-cache-member-p ((query unary-query) (type (eql :exact)) body &rest args)
  (declare (ignorable args))
  `(let ((cur-binding (mapcar #'bound-to ',(all-vois query))))
     (when (gethash cur-binding (bindings-hash ,(exact-cache-reference query)))
       ,body)))


(defmethod check-if-cache-member-p ((query unary-query) (type (eql :exact)) continuation)
  (let ((*binding* (mapcar #'bound-to (all-vois query))))
    (when (gethash *binding*
                   (bindings-hash (exact-cache-reference query)))
      (apply continuation nil))))


(defmethod get-code-check-if-cache-member-p ((query unary-query) (type (eql :superset)) body &rest args)
  (with-superset-cache-as-exact-cache (query)
    (apply #'get-code-check-if-cache-member-p query :exact
           (let ((code 
                  (with-disabled-cache-entries (query)
                    (apply #'get-tester-code query body args))))
             `(when (=> (and ,(subset-cache-reference query)
                             (bindings-hash (subset-cache-reference query)))
                        (not (gethash cur-binding
                                      (bindings-hash 
                                       ,(subset-cache-reference query)))))
                ,code))
           args)))

(defmethod check-if-cache-member-p ((query unary-query) (type (eql :superset)) continuation)
  (with-superset-cache-as-exact-cache (query)
    (check-if-cache-member-p query :exact 
                             #'(lambda (&rest args)
                                 (declare (ignorable args))
                                 (when (=> (and (subset-cache-reference query)
                                                (bindings-hash 
                                                 (subset-cache-reference query)))
                                           (not (gethash *binding*
                                                         (bindings-hash 
                                                          (subset-cache-reference query)))))
                                   (with-disabled-cache-entries (query)
                                     (evaluate-tester query continuation)))))))
  
;;;
;;; Return for Unary Atoms
;;;

(defmethod get-code-return-cache-members ((query unary-query) (type (eql :exact)) body &rest args &key var &allow-other-keys)
  (declare (ignorable args))
  `(abortable-dolist (,var (bindings ,(exact-cache-reference query)))
     (setf (bindings-found-p ,query) nil)
     (let ((cur-binding ,var)
           (,var (first ,var)))
       (declare (ignorable cur-binding))

       ,body
       
       (when (and ,(member (voi query) (existential-vois query))
                  (bindings-found-p ,query))
         (throw 'abort-enumerator t)))))


(defmethod return-cache-members ((query unary-query) (type (eql :exact)) continuation)
  (abortable-dolist (var (bindings (exact-cache-reference query)))
    (setf (bindings-found-p query) nil)
    (let ((*binding* var)
          (var (first var)))
      (apply continuation :var var nil)
      (when (and (member (voi query) (existential-vois query))
                 (bindings-found-p query))
        (throw 'abort-enumerator t)))))


(defmethod get-code-return-cache-members ((query unary-query) (type (eql :superset)) body &rest args)
  (with-superset-cache-as-exact-cache (query)
    (apply #'get-code-return-cache-members query :exact 
           (let ((code (with-disabled-cache-entries (query)
                         (apply #'get-tester-code query body args))))
             `(when (=> (and ,(subset-cache-reference query)
                             (bindings-hash 
                              ,(subset-cache-reference query)))
                        (not (gethash cur-binding
                                      (bindings-hash
                                       ,(subset-cache-reference query)))))
                ,code))
           args)))

(defmethod return-cache-members ((query unary-query) (type (eql :superset)) continuation)
  
  (with-superset-cache-as-exact-cache (query)
    (return-cache-members query :exact 
                          #'(lambda (&rest args &key var &allow-other-keys) 
                              (declare (ignorable args))
                              (when (=> (and (subset-cache-reference query)
                                             (bindings-hash
                                              (subset-cache-reference query)))
                                        (not (gethash *binding*
                                                      (bindings-hash
                                                       (subset-cache-reference query)))))
                                (with-disabled-cache-entries (query)
                                  (evaluate-tester query continuation :var var)))))))

;;;
;;; Binary Atoms
;;;

;;;
;;; Check for Binary Atoms
;;; 

(defmethod get-code-check-if-cache-member-p ((query binary-query) (type (eql :exact)) body &rest args)
  (declare (ignorable args))
  `(let ((cur-binding (mapcar #'bound-to ',(all-vois query))))
     (when (gethash cur-binding
                    ;; Achtung! lexikografisch geordnet! entsp. variable-vectors-compatible-p 
                    (bindings-hash ,(exact-cache-reference query)))
       ,body)))

(defmethod check-if-cache-member-p ((query binary-query) (type (eql :exact)) continuation)
  (let ((*binding* (mapcar #'bound-to (all-vois query))))
    (when (gethash *binding*
                   (bindings-hash (exact-cache-reference query)))
      (apply continuation nil))))

(defmethod get-code-check-if-cache-member-p ((query binary-query) (type (eql :superset)) body &rest args)

  (with-superset-cache-as-exact-cache (query)
    (apply #'get-code-check-if-cache-member-p query :exact 
           (let ((code 
                  (with-disabled-cache-entries (query)
                    (apply #'get-tester-code query body args))))
             `(when (=> (and ,(subset-cache-reference query)
                             (bindings-hash 
                              ,(subset-cache-reference query)))
                        (not (gethash cur-binding
                                      (bindings-hash ,(subset-cache-reference query)))))
                ,code))
           args)))

(defmethod check-if-cache-member-p ((query binary-query) (type (eql :superset)) continuation)
  (with-superset-cache-as-exact-cache (query)
    (check-if-cache-member-p query :exact 
                             #'(lambda (&rest args)
                                 (declare (ignorable args))
                                 (when (=> (and (subset-cache-reference query)
                                                (bindings-hash
                                                 (subset-cache-reference query)))
                                           (not (gethash *binding*
                                                         (bindings-hash
                                                          (subset-cache-reference query)))))
                                   (with-disabled-cache-entries (query)
                                     (evaluate-tester query continuation)))))))

;;;
;;; Return for Binary Atoms
;;; 

(defmethod get-code-return-cache-members ((query binary-query) (type (eql :exact)) body &rest args &key from to &allow-other-keys)
  (declare (ignorable args))
  
  (unless (and from to) (nrql-error "Caching error for query ~A (6)" query))

  (if (eq (voi-from query) 
          (voi-to query))

      `(abortable-dolist (pair (bindings ,(exact-cache-reference query)))
         
         (setf (bindings-found-p ,query) nil)
         
         (let* ((,from (first pair))
                (,to ,from)
                (cur-binding (list ,from ,to)))

           (declare (ignorable ,from ,to cur-binding))

           ,body

           (when (and ,(member (voi-from query) (existential-vois query))
                      (bindings-found-p ,query))
             (throw 'abort-enumerator t))))
      
    `(abortable-dolist (pair (bindings ,(exact-cache-reference query)))
       (let* ((,from ,(if (needs-reordering-p query)
                          '(second pair)
                        '(first pair)))
              (,to ,(if (needs-reordering-p query)
                        '(first pair)
                      '(second pair)))
              (cur-binding (list ,from ,to)))

         (declare (ignorable ,from ,to cur-binding))
         
         (setf (bindings-found-p ,query) nil)
         
         ,body

         (when (and ,(and (member (voi-from query) (existential-vois query))
                          (member (voi-to query) (existential-vois query)))
                    (bindings-found-p ,query))
           (throw 'abort-enumerator t))))))


(defmethod return-cache-members ((query binary-query) (type (eql :exact)) continuation)
  (if (eq (voi-from query)
          (voi-to query))
      
      (abortable-dolist (pair (bindings (exact-cache-reference query)))
        (setf (bindings-found-p query) nil)
        (let* ((from (first pair))
               (*binding* (list from from)))
          ;;; korrekt!!! 
          (apply continuation :from from :to from nil))
        (when (and (member (voi-from query) (existential-vois query))
                   (bindings-found-p query))
          (throw 'abort-enumerator t)))
    
    (abortable-dolist (pair (bindings (exact-cache-reference query)))
      (setf (bindings-found-p query) nil)
      (let* ((from (if (needs-reordering-p query)
                       (second pair)
                     (first pair)))
             (to (if (needs-reordering-p query)
                     (first pair)
                   (second pair)))
             (*binding* (list from to)))
        (apply continuation :from from :to to nil)
        (when (and (member (voi-from query) (existential-vois query))
                   (member (voi-to query) (existential-vois query))
                   (bindings-found-p query))
          (throw 'abort-enumerator t))))))


(defmethod get-code-return-cache-members ((query binary-query) (type (eql :superset)) body &rest args)
  (declare (ignorable args))

  (with-superset-cache-as-exact-cache (query)
    (apply #'get-code-return-cache-members query :exact 
           (let ((code (with-disabled-cache-entries (query)
                         (apply #'get-tester-code query body args))))
             `(when (=> (and ,(subset-cache-reference query)
                             (bindings-hash 
                              ,(subset-cache-reference query)))
                        (not (gethash cur-binding
                                      (bindings-hash
                                       ,(subset-cache-reference query)))))
                ,code))
           args)))


(defmethod return-cache-members ((query binary-query) (type (eql :superset)) continuation)
  (with-superset-cache-as-exact-cache (query)
    (return-cache-members query :exact 
                          #'(lambda (&rest args &key from to &allow-other-keys)
                              (declare (ignorable args))
                              (when (=> (and (subset-cache-reference query)
                                             (bindings-hash 
                                              (subset-cache-reference query)))
                                        (not (gethash *binding*
                                                      (bindings-hash
                                                       (subset-cache-reference query)))))
                                (with-disabled-cache-entries (query)
                                  (evaluate-tester query continuation :from from :to to)))))))

;;;
;;;
;;;

(defmethod get-code-return-cache-members-from-bound ((query binary-query) (type (eql :exact)) body &rest args 
                                                     &key to &allow-other-keys)
  (declare (ignorable args))
  `(abortable-dolist (pair (bindings ,(exact-cache-reference query)))
     (setf (bindings-found-p ,query) nil)
     (when (eq ,(if (needs-reordering-p query)
                    `(second pair) 
                  `(first pair) )
               (bound-to ,(voi-from query)))

       (let* ((,to ,(if (needs-reordering-p query)
                        `(first pair)
                      `(second pair)))
              (cur-binding (list ,to ,to)))

         (declare (ignorable ,to cur-binding))

         ,body

         (when (and ,(member (voi-to query) (existential-vois query))
                    (bindings-found-p ,query))
           (throw 'abort-enumerator t))))))


(defmethod return-cache-members-from-bound ((query binary-query) (type (eql :exact)) continuation)
  (abortable-dolist (pair (bindings (exact-cache-reference query)))
    (setf (bindings-found-p query) nil)
    (when (eq (if (needs-reordering-p query)
                  (second pair) 
                (first pair) )
              (bound-to (voi-from query)))
      (let* ((var (if (needs-reordering-p query)
                      (first pair)
                    (second pair)))
             (*binding* (list var var)))

        (apply continuation :to var nil)

        (when (and (member (voi-to query) (existential-vois query))
                   (bindings-found-p query))
          (throw 'abort-enumerator t))))))


(defmethod get-code-return-cache-members-from-bound ((query binary-query) (type (eql :superset)) body &rest args 
                                                     &key to &allow-other-keys)
  (with-superset-cache-as-exact-cache (query)
    (apply #'get-code-return-cache-members-from-bound query :exact 
           (let ((code
                  (with-disabled-cache-entries (query)
                    (apply #'get-tester-code query body :to to args))))
             `(when (=> (and ,(subset-cache-reference query)
                             (bindings-hash 
                              ,(subset-cache-reference query)))
                        (not (gethash cur-binding
                                      (bindings-hash ,(subset-cache-reference query)))))
                ,code))
           args)))


(defmethod return-cache-members-from-bound ((query binary-query) (type (eql :superset)) continuation)
  (with-superset-cache-as-exact-cache (query)
    (return-cache-members-from-bound query :exact 
                                     #'(lambda (&rest args &key to &allow-other-keys)
                                         (declare (ignorable args))
                                         (when (=> (and (subset-cache-reference query)
                                                        (bindings-hash
                                                         (subset-cache-reference query)))
                                                   (not (gethash *binding*
                                                                 (bindings-hash
                                                                  (subset-cache-reference query)))))
                                           (with-disabled-cache-entries (query)
                                             (evaluate-tester query continuation :to to)))))))

;;;
;;;
;;;

(defmethod get-code-return-cache-members-to-bound ((query binary-query) (type (eql :exact)) body &rest args 
                                                   &key from &allow-other-keys)
  (declare (ignorable args))
  `(abortable-dolist (pair (bindings ,(exact-cache-reference query)))
     (setf (bindings-found-p ,query) nil)
     (when (eq ,(if (needs-reordering-p query) 
                    `(first pair) 
                  `(second pair))
               (bound-to ,(voi-to query)))
       (let* ((,from ,(if (needs-reordering-p query) 
                          `(second pair)
                        `(first pair)))
              (cur-binding (list ,from ,from)))

         (declare (ignorable ,from cur-binding))
         
         ,body

         (when (and ,(member (voi-from query) (existential-vois query))
                    (bindings-found-p ,query))
           (throw 'abort-enumerator t))))))


(defmethod return-cache-members-to-bound ((query binary-query) (type (eql :exact)) continuation)
  (abortable-dolist (pair (bindings (exact-cache-reference query)))
    (setf (bindings-found-p query) nil)
    (when (eq (if (needs-reordering-p query) 
                  (first pair) 
                (second pair))
              (bound-to (voi-to query)))
      (let* ((var (if (needs-reordering-p query) 
                      (second pair)
                    (first pair)))
             (*binding* (list var var)))
        
        (apply continuation :from var nil)

        (when (and (member (voi-from query) (existential-vois query))
                   (bindings-found-p query))
          (throw 'abort-enumerator t))))))



(defmethod get-code-return-cache-members-to-bound ((query binary-query) (type (eql :superset)) body &rest args 
                                                   &key from &allow-other-keys)
  (with-superset-cache-as-exact-cache (query)
    (apply #'get-code-return-cache-members-to-bound query :exact
           (let ((code 
                  (with-disabled-cache-entries (query)
                    (apply #'get-tester-code query body :from from args))))
             `(when (=> (and ,(subset-cache-reference query)
                             (bindings-hash 
                              ,(subset-cache-reference query)))
                        (not (gethash cur-binding
                                      (bindings-hash ,(subset-cache-reference query)))))
                ,code))
           args)))

(defmethod return-cache-members-to-bound ((query binary-query) (type (eql :superset)) continuation)
  (with-superset-cache-as-exact-cache (query)
    (return-cache-members-to-bound query :exact
                                   #'(lambda (&rest args &key from &allow-other-keys) 
                                       (declare (ignorable args))
                                       (when (=> (and (subset-cache-reference query)
                                                      (bindings-hash 
                                                       (subset-cache-reference query)))
                                                 (not (gethash *binding*
                                                               (bindings-hash
                                                                (subset-cache-reference query)))))
                                         (with-disabled-cache-entries (query)
                                           (evaluate-tester query continuation :from from)))))))

