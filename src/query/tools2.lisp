;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;; 
;;;

(defmacro with-new-marking-context (&body body)
  `(progn 
     ,@body))

#+(and :lracer (not :dlmaps))
(defmacro defpersistentclass (&rest rest)
  `(progn ,@(let ((name (first rest)))
	      (list 
	       `(DEFCLASS ,(first rest)
                          ,(second rest)
                          ,(mapcar #'(lambda (x)
                                       (remove :not-persistent x))
                                   (third rest)))
               `(defun ,(intern (string-upcase (format nil "is-~A-p" name))) (obj)
                  (typep obj ',name))))))

#+(or :racer-server (and :dlmaps (not :midelora)))
(defmacro defpersistentclass (&rest rest)
  `(progn ,@(let ((name (first rest)))
	      (list 
	       `(PERSISTENCE-MANAGER:DEFCLASS ,@rest)
               `(defun ,(intern (string-upcase (format nil "is-~A-p" name))) (obj)
                  (typep obj ',name))))))

;;;
;;;
;;;

(defun subclass-responsibility (method)
  (nrql-error "~A: Subclasses must implement!" method))

(defun to-be-implemented (method)
  (nrql-error "~A: To be implemented!" method))

;;;
;;;
;;;

(defun no ())

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

;;;
;;;
;;;

(defun tree-map (fn tree &rest args)
  (mapcar #'(lambda (item) 
              (if (consp item)
                  (apply #'tree-map fn item args)
                (apply fn item args)))
          tree))


(defun tree-find (tree x &rest args)
  (or (apply #'member x tree args)
      (some #'(lambda (sub)
		(and (consp sub)
		     (apply #'tree-find sub x args)))
	    tree)))

(defun newprod (list-of-args)
  (if (not (cdr list-of-args))
      list-of-args
    (let ((list (first list-of-args))
	  (res (newprod (rest list-of-args))))
      (if (not (cddr list-of-args))
	  (reduce #'nconc
                  (mapcar #'(lambda (elem1)
                              (mapcar #'(lambda (elem2)
                                          (list elem2 elem1))
                                      (first list-of-args)))
                          (second list-of-args)))
	(reduce #'nconc
                (mapcar #'(lambda (list2)
                            (mapcar #'(lambda (elem1)
                                        (cons elem1 list2))
                                    list))
                        res))))))


(defun tree-flatten (tree)
  (if (consp tree)
      (append (tree-flatten (car tree))
              (tree-flatten (cdr tree)))
    (when tree (list tree))))

;;;
;;;
;;;

(defun change-package-of-description (description &optional (package :cl-user) change-keyword-p)
  (when description 
    (typecase description
      (list
       (mapcar #'(lambda (x) 
                   (change-package-of-description x package change-keyword-p))
               description))
      (symbol 
       (if (and (keywordp description)
                (not change-keyword-p))
           description           
         (if (eq (symbol-package description)
                 (find-package :racer))
             ;;; Racer "interne" Symbole werden beibehalten!!!
             description
           (let ((*package* 
                  (if package (find-package package) 
                    *package*)))
             (read-from-string
              (concatenate 'string
                           "|" 
                           (let* ((string (format nil "~S" (symbol-name description)))
                                  (n (length string)))
                             (subseq string 1 (1- n)))
                           "|" ))))))
      (otherwise description))))

;;;
;;;
;;;

(defun simplify-boolean-expression (expr &optional recursively-p)
  ;;; (OP (OP c d)) -> (OP c d) etc.   
  (if (consp expr)
      (let ((op (intern (format nil "~A" (first expr))
                        :keyword)))
        (case op
          ((:not :neg)
           (if recursively-p
               `(:not ,(simplify-boolean-expression (second expr) t))
             expr))
          ((:and :or :intersection :union :cap :cup)
           (let ((args 
                  (remove-duplicates 
                   (if recursively-p 
                       (mapcar #'(lambda (x)
                                   (simplify-boolean-expression x t))
                               (rest expr))
                     (rest expr))
                   :test #'equal)))
             (if (not args)
                 (ecase op
                   ((:and :intersection :cap) 
                    (parser-error (format nil "Bad query term ~A" expr)))
                   ((:or :union :cup) 
                    (parser-error (format nil "Bad query term ~A" expr))))
               (if (not (cdr args))
                   (first args)
                 `(,(case op 
                      ((:and :intersection :cap) :and)
                      ((:or :union :cup) :or))
                   ,@(reduce #'append 
                             (mapcar #'(lambda (arg)
                                         (if (consp arg)
                                             (if (eq (first arg) op)
                                                 (rest arg)
                                               (list arg))
                                           (list arg)))
                                     args)))))))
          (otherwise expr)))
    expr))

                       
(defun get-boolean-dnf (expr)
  ;;;
  ;;; (AND (OR C (NOT D)) (OR E F)) (muss in NNF sein!) ->
  ;;; (OR (AND C E) (AND C F) (AND (NOT D) E) (AND (NOT D) F))
  ;;;

  (labels ((dnf (expr)
             (if (consp expr)
                 (let ((op (intern (format nil "~A" (first expr))
                                   :keyword)))
                   (if (member op '(:not :neg))
                       `(:not ,(second expr))
                     (let ((args (remove-duplicates (rest expr) :test #'equal)))
                       (cond ((member op '(:or :union :cup))
                              (simplify-boolean-expression 
                               (cons :or (mapcar #'dnf args))))
                             ((member op '(:and :intersection :cap))
                              (let* ((args (mapcar #'(lambda (x) 
                                                       (let ((x (dnf x)))
                                                         (if (or (member (first x) '(:neg :not))
                                                                 (not (member (first x)
                                                                              '(:and :or :cup :cap :intersection :union))))
                                                             (list :or x)
                                                           x)))
                                                   args))
                                     (args (remove-duplicates args :test #'equal))
                                     (or-terms (remove-if-not #'(lambda (x)
                                                                  (member (first x) '(:or :union :cup)))
                                                              args))
                                     (non-or-terms (remove-if #'(lambda (x)
                                                                  (member (first x) '(:or :union :cup)))
                                                              args)))
                                (simplify-boolean-expression
                                 (if or-terms 
                                     (let ((crosprod (newprod 
                                                      (mapcar #'rest or-terms))))
                                       (cons :or 
                                             (mapcar #'(lambda (or-term)
                                                         (simplify-boolean-expression 
                                                          (cons :and (append non-or-terms 
                                                                             or-term))))
                                                     crosprod)))
                                   (cons :and args)))))
                             (t expr)))))
               expr)))
    
    (dnf expr)))


;;;
;;;


(defun permutations (list &optional (n (length list)) (i 0)) 
  (perm list n i))

(defun perm (list &optional (n (length list)) (i 0))
  ;;; choose "k" out of "n" 
  (if (= i n) 
      '(nil)
    (remove-duplicates 
     (mapcan #'(lambda (a)
                 (let ((rest (remove a list :count 1)))
                   (mapcar #'(lambda (rest)
                               (cons a rest))
                           (perm rest n (1+ i)))))
             list)
     :test #'equal)))

