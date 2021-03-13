;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

(defpersistentclass dl ()
  ((name :accessor name :initarg :name)))

(defmethod initialize-instance :after ((lang dl) &rest initargs)
  (push lang *all-dls*))

;;;
;;;
;;;

(defpersistentclass needs-basic-abox-substrate ())

(defpersistentclass needs-rolebox-abox-substrate (needs-basic-abox-substrate))

(defpersistentclass needs-jepd-abox-substrate (needs-rolebox-abox-substrate))

;;;
;;; Hier werden nur Kombinationen bzw. Spezialisierungen definiert, die
;;; sinnvolle Spezialisierungen bzgl. des Proover-Codes ermoeglichen 
;;; Idee: für die hier drei definierten "Top"-Klassen werden Proover implementiert; 
;;; falls die Performanz unzureichend ist, koennen die get-code-Methoden des
;;; Provers entsp. spezialisiert werden (= effizienter gemacht fuer speziellere Sprachen)
;;; z.B. Blocking-Check aus fuer ALC, etc. Wichtig ist jedoch, dass der Top-Proover
;;; prinzipiell die mächtigste Sprache (also z.B. SHI-WICODIR) behandeln kann!
;;; Nur so werden die Code-Spezialisierungen optional. Daher ist die Vererbungshierarchie
;;; in "diese Richtung" angelegt
;;; 

(defpersistentclass dl-with-disjunctive-aboxes (dl))

;;;
;;;
;;;

(defpersistentclass dl-with-role-hierarchies (dl)) ;;; alles meine DLs haben role hierarchies

(defpersistentclass dl-with-domain-and-range-restrictions-on-roles (dl))

(defpersistentclass dl-with-disjunctive-roles (dl))

(defpersistentclass dl-with-conjunctive-roles (dl))

(defpersistentclass dl-with-inverse-roles (dl)) ; also symmetric roles 

(defpersistentclass dl-with-transitive-roles (dl))

(defpersistentclass dl-with-disjoint-roles (dl))

(defpersistentclass dl-with-rolebox (dl))

(defpersistentclass dl-with-jepd-property (dl-with-rolebox dl-with-disjoint-roles dl-with-inverse-roles))

;;;
;;;
;;;

(defpersistentclass boolean-dl (dl))

;;;
;;;
;;;

(defpersistentclass dl-with-somes (dl))

(defpersistentclass dl-with-features (dl))

(defpersistentclass dl-with-alls (dl))

;;;
;;;
;;;

(defpersistentclass alch-basis (boolean-dl dl-with-somes dl-with-alls 
                                           dl-with-domain-and-range-restrictions-on-roles
                                           dl-with-role-hierarchies))

;;;
;;;
;;;

(defpersistentclass dl-with-number-restrictions (dl))

(defpersistentclass dl-with-unqualified-number-restrictions (dl-with-number-restrictions))

(defpersistentclass dl-with-qualified-number-restrictions (dl-with-number-restrictions))

;;;
;;;
;;; 

(defpersistentclass dl-with-combined-some-all-rule (dl))

(defpersistentclass dl-with-blocking (dl))

(defpersistentclass dl-with-simple-blocking (dl-with-blocking))

(defpersistentclass dl-with-equal-blocking (dl-with-simple-blocking))

(defpersistentclass dl-with-subset-blocking (dl-with-simple-blocking))

(defpersistentclass dl-with-pairwise-blocking (dl-with-blocking))



(defpersistentclass dl-with-model-merging (dl))

;;;
;;; hier werden nur die Beweiser spezialisiert, 
;;; fuer die Implementationen existieren!
;;;


(defpersistentclass super-dl (alch-basis
                              dl-with-unqualified-number-restrictions
                              
                              dl-with-disjunctive-aboxes 
                              dl-with-inverse-roles
                              dl-with-transitive-roles
                              dl-with-features
                              
                              dl-with-pairwise-blocking
                              
                              needs-basic-abox-substrate))

;;;
;;; Basis DLs 
;;; 


(defpersistentclass alch (alch-basis
                          
                          dl-with-model-merging
                          dl-with-combined-some-all-rule
                          
                          needs-basic-abox-substrate))

(defpersistentclass alchi (alch-basis
                           
                           dl-with-inverse-roles

                           needs-basic-abox-substrate))

;;;
;;; DLs ohne inverse Rollen 
;;;

(defpersistentclass alchf (alch
                           dl-with-features))

(defpersistentclass alchf-rplus (alchf 
                                 dl-with-transitive-roles
                                 dl-with-subset-blocking))

(defpersistentclass alchn (alch
                           dl-with-unqualified-number-restrictions))

(defpersistentclass alchfn-rplus (alchf-rplus alchn))

;;;
;;; DLs mit inversen Rollen 
;;;


(defpersistentclass alchi-rplus (alchi
                                 dl-with-transitive-roles
                                 dl-with-equal-blocking))

(defpersistentclass alchif (alchi
                            dl-with-features))

(defpersistentclass alchif-rplus (dl-with-pairwise-blocking
                                  alchi-rplus alchif 
                                  ))

(defpersistentclass alchifn-rplus (alchif-rplus 
                                   dl-with-unqualified-number-restrictions))


;;;
;;; Rolebox/JEPD Substrate DLs 
;;;

(defpersistentclass alci-ra-minus (alch-basis

                                   dl-with-disjunctive-aboxes 
                                   dl-with-rolebox
                                   
                                   needs-rolebox-abox-substrate))

(defpersistentclass alci-ra-jepd (alci-ra-minus 
                                  dl-with-jepd-property

                                  needs-jepd-abox-substrate))

(defpersistentclass alci-rcc (alci-ra-jepd))

;;;
;;;
;;;

(eval 
 `(progn
    ,@(mapcar #'(lambda (dl)
                  `(defconstant ,(intern (format nil "+~A+" dl))
                     (make-instance (quote ,dl)
                                    :name (symbol-name ',dl))))
              
              '(dl 

                super-dl 
                
                alch alchf alchn 
                alchf-rplus alchfn-rplus

                alchi alchif 
                alchi-rplus alchif-rplus alchifn-rplus
                ;alchif-rplus 
                   
                alci-ra-minus alci-ra-jepd alci-rcc))))

;;;
;;;
;;;

(defmethod make-language ((type symbol))
  (or (loop as dl in *all-dls*
            when (eq (type-of dl) type)
            return dl)
      (make-instance type)))

(defmethod make-language ((type dl))
  type)

