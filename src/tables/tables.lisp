;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun swap-first-args (entry)
    `(,(second entry) ,(first entry) ,@(cddr entry)))

 (defconstant +rcc1-roles+ '(sr))

  (defconstant +rcc1-rolebox+
    (create-rbox 'rcc1-rolebox 
                 +rcc1-roles+ 
                 :inverse-roles '((sr sr))
                 :reflexive-roles '(sr) 
                 :type 'jepd-rolebox
                 :axioms 
                 (mapcar #'swap-first-args
                         '((sr sr (sr))))))


 (defconstant +rcc2-roles+ '(o dr))

  (defconstant +rcc2-rolebox+
    (create-rbox 'rcc2-rolebox 
                 +rcc2-roles+ 
                 :inverse-roles '((o o) (dr dr))
                 :reflexive-roles '(o) 
                 :type 'jepd-rolebox
                 :axioms 
                 (mapcar #'swap-first-args
                         '((dr o (dr o))
                           (dr dr (dr o))
                           
                           (o dr (dr o))
                           (o o (dr o))))))

  (defconstant +rcc3-roles+ '(dr one eq))

  (defconstant +rcc3-rolebox+
    (create-rbox 'rcc3-rolebox 
                 +rcc3-roles+ 
                 :inverse-roles '((dr dr) (one one))
                 :reflexive-roles '(eq) 
                 :type 'jepd-rolebox
                 :axioms 
                 (mapcar #'swap-first-args
                         '((dr dr (dr one eq))
                           (dr one  (dr one))
                           (dr eq (dr))
                           
                           (one dr (dr one))
                           (one one  (dr one eq))
                           (one eq (one))
                           
                           (eq dr (dr))
                           (eq one (one))
                           (eq eq (eq))))))
  
  (defconstant +rcc5-roles+ '(dr po eq pp ppi))

  (defconstant +rcc5-rolebox+
    (create-rbox 'rcc5-rolebox
                 +rcc5-roles+ 
                 :inverse-roles '((dr dr) (po po) (pp ppi))
                 :reflexive-roles '(eq)
                 :type 'jepd-rolebox
                 :axioms 
                 (mapcar #'swap-first-args
                         `((dr dr ,+rcc5-roles+)
                           (dr po (dr po ppi))
                           (dr eq (dr))
                           (dr pp (dr))
                           (dr ppi (dr po ppi))
                         
                           (po dr (dr po pp))
                           (po po ,+rcc5-roles+)
                           (po eq (po))
                           (po pp (dr po pp))
                           (po ppi (po ppi))
	   
                           (eq dr (dr))
                           (eq po (po))
                           (eq eq (eq))
                           (eq pp (pp))
                           (eq ppi (ppi))
	           
                           (pp dr (dr po pp))
                           (pp po (po pp))
                           (pp eq (pp))
                           (pp pp (pp))
                           (pp ppi ,(remove 'dr +rcc5-roles+)) ; = overlap
	           
                           (ppi dr (dr))
                           (ppi po (dr po ppi))
                           (ppi eq (ppi))
                           (ppi pp ,+rcc5-roles+)
                           (ppi ppi (ppi))))))
  
  (defconstant +rcc8-roles+ '(dc ec po eq tpp tppi ntpp ntppi))
  
  (defconstant +rcc8-rolebox+ 
    (create-rbox 'rcc8-rolebox +rcc8-roles+ 
                 :reflexive-roles '(eq)
                 :inverse-roles '((dc dc) (ec ec) (po po)  
                                  (tpp tppi) (ntpp ntppi))
                 :type 'jepd-rolebox
                 :axioms 
                 (let ((top +rcc8-roles+)
                       (dc '(dc))
                       (ec '(ec))
                       (po '(po))
                       (tpp '(tpp))
                       (ntpp '(ntpp))
                       (tppi '(tppi))
                       (ntppi '(ntppi))
                       (eq '(eq))
                       (dr '(ec dc))
                       (pp '(tpp ntpp))
                       (ppi '(tppi ntppi)))
                   
                   (mapcar #'swap-first-args
                           `((dc dc    ,top)
                             (dc ec    (,@dr ,@po ,@ppi))
                             (dc po    (,@dr ,@po ,@ppi))
                             (dc tpp   ,dc)
                             (dc ntpp  ,dc)
                             (dc tppi  (,@dr ,@po ,@ppi))
                             (dc ntppi (,@dr ,@po ,@ppi))
                             (dc eq    ,dc)
                   
                             (ec dc    (,@dr ,@po ,@pp))
                             (ec ec    (,@dr ,@eq ,@po ,@tpp ,@tppi))
                             (ec po    (,@dr ,@po ,@ppi))
                             (ec tpp   ,dr)
                             (ec ntpp  ,dc)
                             (ec tppi  (,@ec ,@po ,@ppi))
                             (ec ntppi (,@po ,@ppi))
                             (ec eq    ,ec)
	           
                             (po dc    (,@dr ,@po ,@pp))
                             (po ec    (,@dr ,@po ,@pp))
                             (po po    (,@top))
                             (po tpp   (,@dr ,@po ,@pp))
                             (po ntpp  (,@dr ,@po ,@pp))
                             (po tppi  (,@po ,@ppi))
                             (po ntppi (,@po ,@ppi))
                             (po eq    (,@po))
	           
                             (tpp dc    (,@dr ,@po ,@pp))
                             (tpp ec    (,@ec ,@po ,@pp))
                             (tpp po    (,@po ,@pp))
                             (tpp tpp   ,pp)
                             (tpp ntpp  ,ntpp)
                             (tpp tppi  (,@po ,@eq ,@tpp ,@tppi))
                             (tpp ntppi (,@po ,@ppi))
                             (tpp eq    (,@tpp))
	           
                             (ntpp dc    (,@dr ,@po ,@pp))
                             (ntpp ec    (,@po ,@pp))
                             (ntpp po    (,@po ,@pp))
                             (ntpp tpp   (,@ntpp))
                             (ntpp ntpp  (,@ntpp))
                             (ntpp tppi  (,@po ,@pp))
                             (ntpp ntppi (,@po ,@eq ,@pp ,@ppi))
                             (ntpp eq    (,@ntpp))
	           
                             (tppi dc    ,dc)
                             (tppi ec    ,dr)
                             (tppi po    (,@dr ,@po ,@ppi))
                             (tppi tpp   (,@dr ,@eq ,@po ,@tpp ,@tppi))
                             (tppi ntpp  (,@dr ,@po ,@pp))
                             (tppi tppi  ,ppi)
                             (tppi ntppi ,ntppi)
                             (tppi eq    ,tppi)
	           
                             (ntppi dc    ,dc)
                             (ntppi ec    ,dc)
                             (ntppi po    (,@dr ,@po ,@ppi))
                             (ntppi tpp   (,@dr ,@po ,@ppi))
                             (ntppi ntpp  ,top)
                             (ntppi tppi  ,ntppi)
                             (ntppi ntppi ,ntppi)
                             (ntppi eq    ,ntppi)
	           
                             (eq dc ,dc)
                             (eq ec ,ec)
                             (eq po ,po)
                             (eq tpp ,tpp)
                             (eq ntpp ,ntpp)
                             (eq tppi ,tppi)
                             (eq ntppi ,ntppi)
                             (eq eq ,eq)))))))


