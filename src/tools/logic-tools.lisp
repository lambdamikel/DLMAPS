;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TOOLS -*-

(in-package :tools)

(defmethod macroreldef ((obj symbol))
  nil)

(defun nnf (concept &optional negate-p)
  (if (consp concept)
      (let ((op (first concept)))
        (if (eq op 'not)
            (nnf (second concept) (not negate-p))
          (case op
            (=> (nnf `(or (not ,(second concept))
                          ,(third concept))
                     negate-p))
            (<= (nnf `(or (not ,(third concept))
                          ,(second concept))
                     negate-p))
            (<=> (nnf `(and (or (not ,(third concept))
                                ,(second concept))
                            (or (not ,(second concept))
                                ,(third concept)))
                      negate-p))
            (and `(,(if negate-p 'or 'and)
                   ,@(mapcar #'(lambda (x) (nnf x negate-p)) (rest concept))))
            (or `(,(if negate-p 'and 'or) 
                  ,@(mapcar #'(lambda (x) (nnf x negate-p)) (rest concept))))
            (all 
             (cond ((consp (second concept))
                    (nnf `(and ,@(mapcar #'(lambda (role)
                                             `(all ,role ,(third concept)))
                                         (second concept)))
                       
                         negate-p))
                   ((macroreldef (second concept))
                    (nnf `(and ,@(mapcar #'(lambda (role)
                                             `(all ,role ,(third concept)))
                                         (macroreldef (second concept))))
                       
                         negate-p))
                   (t
                    (if (eq (second concept) 'id)
                        (nnf (third concept) negate-p)
                      `(,(if negate-p 'some 'all) 
                        ,(second concept) ,(nnf (third concept) 
                                                negate-p))))))
            (some 
             (cond ((consp (second concept))		
                    (nnf `(or ,@(mapcar #'(lambda (role)
                                            `(some ,role ,(third concept)))
                                        (second concept)))
                       
                         negate-p))
                   ((macroreldef (second concept))
                    (nnf `(or ,@(mapcar #'(lambda (role)
                                            `(some ,role ,(third concept)))
                                        (macroreldef (second concept))))
                       
                         negate-p))
                   (t 
                    (if (eq (second concept) 'id)
                        (nnf (third concept) negate-p)
                      `(,(if negate-p 'all 'some)
                        ,(second concept) ,(nnf (third concept) 						 
                                                negate-p)))))))))
    (if negate-p 
        `(not ,concept)
      concept)))

(defun flatten (concept)
  (if (consp concept)
      (let ((op (first concept)))
        (case op
          ((and or => <= <=>)
           (if (not (third concept))
               (flatten (second concept))
             (let ((args
                    (remove-duplicates 
                     (apply #'append
                            (mapcar #'(lambda (x)
                                        (let ((res (flatten x)))
                                          (if (consp res)
                                              (if (eq (first res) op)
                                                  (rest res)
                                                res)
                                            (list res))))
                                    (rest concept)))
                     :test #'(lambda (x y) (set-equal (ensure-list x) (ensure-list y))))))
               (if (cdr args)
                   `(,op ,@args)
                 (first args)))))
          ((some all)
           `(,op ,(second concept) 
                 ,(flatten (third concept))))
          (not
           `(,op ,(flatten (second concept))))))
    concept))



(defun dnf (concept)  
  (boolean-dnf concept))


(defun boolean-dnf (concept)  
  (labels ((simple-simplify (concept)
	     (if (not (consp concept))
		 concept
	       (let ((op (first concept)))
                 (if (member op '(and or))
                     (let ((args
                            (remove-duplicates 
                             (apply #'append
                                    (mapcar #'(lambda (x)
                                                (cond ((symbolp x) (list x))
                                                      ((and (consp x)
                                                            (eq (first x) 'not))
                                                       (list x))
                                                      ((and (consp x)
                                                            (eq (first x) op))
                                                       (rest x))
                                                      (t (list x))))
                                            (rest concept)))
                             :test #'(lambda (x y) (set-equal (ensure-list x) (ensure-list y))))))
                       (if (cdr args)
                           (cons op args)
                         (first args)))
                   concept))))
	   (rewrite (concept)
	     (if (or (and (consp concept)
			  (eq (first concept) 'not))
		     (not (consp concept)))
		 concept
	       (let ((op (first concept))
		     (args (rest concept)))
		 (if (eq op 'or)
		     (simple-simplify
		      (cons 'or (mapcar #'rewrite args)))
		   ;; op="and"; args kommen in DNF wieder => beginnen mir "or" 		   
		   (if (eq op 'and)
		       (let* ((args (mapcar #'(lambda (x) 
						(let ((x (rewrite x)))
						  (if (or (symbolp x)
							  (and (consp x)
							       (or (eq (first x) 'not)
								   (eq (first x) 'some)
								   (eq (first x) 'all))))
						      (list 'or x)
						    x)))
					    args))
			      (or-terms (remove-if-not #'(lambda (x)
							   (and (consp x)
								(eq (first x) 'or)))
						       args))
			      (non-or-terms (remove-if #'(lambda (x)
                                                           (and (consp x)
                                                                (eq (first x) 'or)))
						       args)))
                         (simple-simplify
                          (if or-terms 
                              (let ((crosprod (newprod 
                                               (mapcar #'rest or-terms))))
                                (cons 'or 
                                      (mapcar #'(lambda (or-term)
                                                  (simple-simplify 
                                                   (cons 'and (append non-or-terms 
                                                                      or-term))))
                                              crosprod)))
                            (cons 'and args))))
		     ;; op=some or op=all
		     (list op 
			   (second concept)
			   (rewrite (third concept)))))))))
				       
    (rewrite concept)))


;;;
;;;
;;;

(defun trafo (c)
  (if (symbolp c) 
      c
    (let ((op (first c))
          (args (rest c))
          (role (second c))
          (qual (third c)))
      (case op
        (not `(not ,(trafo (first args))))
        ((and or)
         `(,op ,@(mapcar #'trafo args)))
        (some
         (case role
           (eq
            (if (and (consp qual)
                     (eq (first qual) 'or))
                `(or ,@(mapcar #'(lambda (x) (trafo `(some eq ,x))) (rest qual)))
              (multiple-value-bind (mp bp)
                  (modal-and-boolean-part-of (nnf qual))
                `(or (and ,@(mapcar #'trafo mp)
                          ,@(when bp
                              `((some eq* (and EQN ,@bp)))))
                     ,(trafo qual)))))
           (otherwise
            `(some ,role ,(trafo `(some eq ,qual))))))
        (all
         (case role
           (eq
            (if (and (consp qual)
                     (eq (first qual) 'or))
                (let ((qual (nnf 
                             (cons 'and 
                                   (mapcar #'(lambda (x) 
                                               (list 'not x))
                                           (rest qual))))))                    
                  `(not ,(trafo `(some eq ,qual))))
              (multiple-value-bind (mp bp)
                  (modal-and-boolean-part-of qual)                    
                `(and (and ,@(mapcar #'trafo mp)
                           ,@(when bp
                               `((all eq* (=> EQN (and ,@bp))))))
                      ,(trafo qual)))))
           (otherwise 
            `(all ,role ,(trafo `(all eq ,qual))))))))))
               

(defun modal-and-boolean-part-of (c)
  (labels ((atomic (c)
             (or (symbolp c)
                 (and (consp c)
                      (eq (first c) 'not)
                      (symbolp (second c)))))
           (bool (x)
             (or (atomic x)
                 (and (consp x)
                      (or (eq (first x) 'and)
                          (eq (first x) 'or))))))
    (if (atomic c)
        (values nil (list c))
      (if (or (eq (first c) 'some)
              (eq (first c) 'all))
          (values (list c) nil)
        (if (eq (first c) 'not)
            (error "Bad concept! Not in NNF!")
          (let ((bp
                 (remove-if-not #'bool
                                (rest c)))
                (mp 
                 (remove-if-not
                  #'(lambda (x)
                      (and (consp x)
                           (or (eq (first x) 'some)
                               (eq (first x) 'all))))             
                  (rest c))))
            
            (values mp bp)))))))

;;;
;;;
;;;

(defun simplify-boolean-expression (expr &optional (recursively-p t))
  ;;; (OP (OP c d)) -> (OP c d) etc.   
  (if (consp expr)
      (let ((op (intern (format nil "~A" (first expr))
                        :keyword)))
        (case op
          ((:not :neg)
           (if recursively-p
               `(:not ,(simplify-boolean-expression (second expr)))
             expr))
          ((:and :or :intersection :union :cap :cup)
           (let ((args 
                  (remove-duplicates 
                   (if recursively-p 
                       (mapcar #'(lambda (x)
                                   (simplify-boolean-expression x))
                               (rest expr))
                     (rest expr))
                   :test #'equal)))
             (if (not args)
                 (ecase op
                   ((:and :intersection :cap) :top)
                   ((:or :union :cup) :bottom))
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
                               (cons :or (mapcar #'dnf args)) 
                               nil))
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
                                   (cons :and args))
                                 nil)))
                             (t expr)))))
               expr)))
    
    (dnf (simplify-boolean-expression expr))))

