;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

(defmethod save-tbox ((tbox tbox) filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream "(in-tbox ~A)" (name tbox))

    (dolist (role (get-all-roles tbox))
      (when (parse-name role) 
        (if (feature-p role) 
            (format stream "~%(define-primitive-attribute")
          (format stream "~%(define-primitive-role"))          
        (format stream " ~A"
                (parse-name role) )
        (when (superroles role) 
          (format stream " :parents ~A"
                  (mapcar #'parse-name (superroles role))))
        (when (parse-name (get-inverse-role role))
          (format stream " :inverse ~A" 
                  (parse-name (get-inverse-role role))))
        (unless (is-top-concept-p (role-domain role))
          (format stream " :domain ~A"
                  (unparse (role-domain role))))
        (unless (is-top-concept-p (role-range role))
          (format stream " :range ~A"
                  (unparse (role-range role))))
        (if (transitive-p role)
            (format stream " :transitive T)")
          (format stream ")"))))
    
    (dolist (axiom (get-simple-axioms tbox))
      (if (primitive-p axiom)          
          (format stream "~%(define-primitive-concept ")
        (format stream "~%(define-concept "))
      (format stream "~A ~A)" 
              (unparse (left axiom))
              (unparse (right axiom))))
    
    (dolist (axiom (get-gcis tbox))
      (unless (primitive-p axiom)
        (error "Found bad GCI ~A!" axiom))
      (format stream "~%(implies ~A ~A)"
              (unparse (left axiom))
              (unparse (right axiom))))))
