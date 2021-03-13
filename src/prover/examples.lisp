;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

(reset-store *cur-store*)

;;;
;;; Dependency Directed Backtracking Example
;;;

(with-knowledge-base (test test :delete-if-exists-p t)

  (def* c a)
  
  (ins a c)
  
  (ins a (not a))

  (princ (abox-sat-p *cur-abox*)))

(progn

  (in-kb test test :delete-if-exists-p t)

  (def* c a)
  
  (ins a c)
  
  (ins a (not a))

  (princ (abox-sat-p *cur-abox*)))

(with-abox (test :delete-if-exists-p t)
  (ins a (and (or c1 d1)
              (or c2 d2)
              (or c3 d3)
              (or c4 d4)
              (or c5 d5)
              (or c6 d6)
              (or c7 d7)
              (or c8 d8)
              (or c9 d9)
              (some r (and c d))
              (all r (or (and d (not c)) (and c (not d))))))

  (setf *x* (list *cur-abox* *cur-substrate*))
  (princ (abox-sat-p *cur-abox* :semantic-branching-p t :debug-p t)))



(with-abox (test :delete-if-exists-p t)
  (ins a (and (all r bottom)
              (or (some r c)
                  (some s d))
              (all s (and e (not d)))))
  (princ (abox-sat-p *cur-abox* :semantic-branching-p nil :debug-p t :compute-all-completions-p t))
  (visualize *completions*))
  




(with-abox (test :delete-if-exists-p t)
  (ins a (and (some r (and c d))
              (all r (or (and d (not c)) 
                         (and c (not d))))))
  
  (setf *x* (list *cur-abox* *cur-substrate*))
  (princ (abox-sat-p *cur-abox* :semantic-branching-p t :debug-p t)))




(with-abox (test :delete-if-exists-p t)
  
  (ins a (and (some r (and c d))
              (all r (or (and d (not c))
                         (and c (not d))))))

  (setf *x* (list *cur-abox* *cur-substrate*))
  (princ (abox-sat-p *cur-abox* :semantic-branching-p t :debug-p t)))




;;;
;;; LTL (Linear Temporal Logic) Example
;;;

(with-rbox (rcc5-rolebox)
  (with-tbox (meta :delete-if-exists-p t)
    
    (def* linear-time
          (and (all dr bottom)
               (all ec bottom)
               (all po bottom)
               (all ppi linear-time)
               (all pp linear-time)))

    (def* dense 
          (=> (some ppi top)
              (some ppi (some ppi top))))
                   
                   
    (def* right-bounded 
          (some ppi (and last-node
                         (all ppi bottom))))

    (def* left-bounded
          (some pp (and first-node 
                        (all pp bottom))))


    (def* left-unbounded
          (all pp (some pp top)))
    
    (def* right-unbounded
          (all ppi (some ppi top)))
    
    (with-abox (test :delete-if-exists-p t)
      
      (ins a (and (some ppi (some ppi (some ppi (some ppi dead))))
                  linear-time
                  left-bounded
                  right-bounded))
      
      (princ (abox-sat-p *cur-abox* 
                         :reuse-nodes-p t
                         :compute-all-completions-p nil :debug-p t :break-p nil))
      
      (visualize *cur-abox*))))


;;;
;;; Basic ALCI ABox Consistency Checking 
;;; Auf Substrat-Ebene!
;;;


(with-substrate (test :type 'abox :delete-if-exists-p t)
    
  (node a (ncon (and a (some r c))))

  (abox-sat-p *cur-substrate* :compute-all-completions-p t)
 
  (visualize *completions*))


;;;
;;; "PO"-Free ALCI_RCC5 hat die Tree Model Property! 
;;; "Church Rosser"-Rauten ("Joins") müssen dann linear angeordnet werden, 
;;; weil nur noch (b,c) : PP oder (b,c) : PPI oder (b,c) : EQ möglich bleibt
;;;

(with-rbox (rcc5-rolebox)
  (in-tbox meta :delete-if-exists-p t)

    (setf *completions* nil)
    
    (impl top (all po bottom))

    (with-abox (test :delete-if-exists-p t)

      (ins a a)
      (ins b c)
      (ins c c)

      (rel a b pp)
      (rel a c pp)
      
      (princ (abox-sat-p *cur-abox* 
                         :compute-all-completions-p t 
                         :debug-p t 
                         :use-cached-models-p nil 
                         :cache-models-p nil
                         :break-p nil))

      (visualize *completions*))))

;;;
;;; Ladner-Baum-Konstruktion für ALCI_RCC5
;;; 


(with-rbox (rcc5-rolebox) ; tree models! LADNER-REDUCTION !!! PSPACE!!! 
  (kill)
  (with-tbox (meta :delete-if-exists-p t)
    (setf *completions* nil)
    
    (impl top (all po bottom))
    
    (impl q0 (and (not q1) (not q2) (not q3)
                  (all ppi (not q0*))))
                  
    (impl q1 (and (not q0) (not q2) (not q3)
                  (all ppi (not q1*))))
                  
    (impl q2 (and (not q0) (not q1) (not q3)
                  (all ppi (not q2*))))

    ;(impl q3 (and (not q0) (not q1) (not q2)
    ;              (all ppi (not q3))))

    (impl p1 (all eq p1)) 
    (impl p2 (all eq p2)) 
    (impl p3 (all eq p3)) 

    ;;;;     (impl qo (all eq q0)) 
    ;;;;     (impl q1 (all eq q1)) 
    ;;;;     (impl q2 (all eq q2)) 


    ;;;;     (impl (not p1) (all eq (not p1)) )
    ;;;;     (impl (not p2) (all eq (not p2)) )
    ;;;;     (impl (not p3) (all eq (not p3)) )

    ;;;;     (impl (not qo) (all eq (not q0)) )
    ;;;;     (impl (not q1) (all eq (not q1)) )
    ;;;;     (impl (not q2) (all eq (not q2)) )

    (def sp1 (and (=> p1 (all ppi p1))
                  (=> (not p1) (all ppi (not p1)))))

    (def sp2 (and (=> p2 (all ppi p2))
                  (=> (not p2) (all ppi (not p2)))))

    (def sp3 (and (=> p3 (all ppi p3))
                  (=> (not p3) (all ppi (not p3)))))
    
    (def b0 (=> q0 (and (some ppi (and q1 p1))
                        (some ppi (and q1 (not p1))))))
    (def b1 (=> q1 (and (some ppi (and q2 p2))
                        (some ppi (and q2 (not p2))))))
    (def b2 (=> q2 (and (some ppi (and q3 p3))
                        (some ppi (and q3 (not p3))))))
    
    
    (with-abox (test :delete-if-exists-p t)
      
      (ins a (and q0

                  b0
                  (all ppi b1)
                  ;(all ppi (all ppi b2))
                  
                  (all ppi sp1) (all ppi (all ppi sp1))
                  ;(all ppi (all ppi sp2))
                  
                  ))
      (setf *abox* *cur-abox*)
      (princ (abox-sat-p *abox* :compute-all-completions-p nil :break-p t :debug-p nil)))))





(with-rbox (rcc8-rolebox) ; tree models! LADNER-REDUCTION !!! PSPACE!!! 
  (kill)
  (with-tbox (meta :delete-if-exists-p t)
    (setf *completions* nil)
    
    (impl odd (all tppi even))
    (impl even (all tppi odd))

    (impl odd (not even))
    (impl even (not odd))
    
    
    (impl top (and (all po bottom)
                   (all ec bottom)))
    
    (impl q0 (and (not q1) (not q2) (not q3)
                  (all tppi (not q0))))
                  
    (impl q1 (and (not q0) (not q2) (not q3)
                  (all tppi (not q1))))
                  
    (impl q2 (and (not q0) (not q1) (not q3)
                  (all tppi (not q2))))
    
    (impl q3 (and (not q0) (not q1) (not q2)
                  (all tppi (not q3))))
    
    (impl p1 (all eq p1)) ; "make weak EQ semantics strong" (works only for trees!)
                            (impl p2 (all eq p2))
                            (impl p3 (all eq p3))
    

                            (def* sp1 (and (=> p1 (all tppi p1))
                                           (=> (not p1) (all tppi (not p1)))))

                            (def* sp2 (and (=> p2 (all tppi p2))
                                           (=> (not p2) (all tppi (not p2)))))

                            (def* sp3 (and (=> p3 (all tppi p3))
                                           (=> (not p3) (all tppi (not p3)))))
    
                            (def* b0 (=> q0 (and (some tppi (and q1 p1))
                                                 (some tppi (and q1 (not p1))))))

                            (def* b1 (=> q1 (and (some tppi (and q2 p2))
                                                 (some tppi (and q2 (not p2))))))

                            (def* b2 (=> q2 (and (some tppi (and q3 p3))
                                                 (some tppi (and q3 (not p3))))))
    
    
                            (with-abox (test :delete-if-exists-p t)
      
                              (ins a (and q0
                  
                                          even

                                          b0
                                          (all tppi b1)
                  ;(all tppi (all tppi b2))
                  
                                          (all tppi sp1) (all tppi (all tppi sp1))
                  ;(all tppi (all tppi sp2))
                  
                                          ))
                              (setf *abox* *cur-abox*)
                              (run #'(lambda ()
                                       (princ (abox-sat-p *abox* :compute-all-completions-p nil :debug-p nil))
                                       (visualize 
       ;(mapcar #'reorder-ppi
                ;(remove-if #'has-eq-edges-p 
                                        *completions*))))))



;;;
;;; ALCI_RCC5 "Geo Example" (Stadt / Land / Fluss)
;;;



(with-rbox (rcc5-rolebox)
  
  (with-tbox (geo-example :delete-if-exists-p t)
    
    (def* area)

    ;;; Die Konzepte "City", "River", "Lake" und "Mountain" sind spezielle "Flaechen"
    (def* country area)
    
    (def* city area)
  
    (def* river area)
    (def* lake area)
  
    (def* mountain area)

  
    ;;; "Deutschland" und die "Tschechische Republik" sind "Laender  "
    (def* germany country)
    (def* czech-republic country)

    ;;; Ein "lokaler Fluss" ueberlappt sich nicht mit "Laendern"

    (def local-river 
         (and river 
	      (not (some po country))))

    ;;; Ein "nicht-lokaler Fluss" ueberlappt sich mit "Laendern"
    (def non-local-river
         (and river
	      (some po country)))
  
    ;;; Ein "Fluss der in einen See fliesst" in ein Fluss der einen See ueberlappt
    (def river-flowing-into-a-lake
         (and river
	      (some po lake)))
  
    ;;; Ein deutscher Fluss ist ein Fluss der in Deutschland enthalten ist 
    ;;; und keine anderen Fluesse ueberlappt 
    (def german-river
         (and river
	      (some pp germany)
	      (all po (not country))))
  
    ;;; Eine deutsche Stadt ist eine Stadt die in Deutschland enthalten ist 
    (def german-city
         (and city
	      (all pp 
		   (=> country germany))))

    ;;; Ein Stadt an einem Fluss ist eine Stadt die einen Fluss ueberlappt
    (def city-at-river 
         (and city
	      (some po river)))

    ;;; Die Elbe ist ein Fluss der die Tschechische Republik und Deutschland kreuzt
    (def* elbe 
          (and river 
	       (some po czech-republic)
	       (some po germany)))
  

    ;;; Die "Binnen-Alster" ist ein Teich
    (def* alster-lake
          lake)
  
    ;;; Die Alster ist ein Fluss der in Deutschland enthalten ist 
    ;;; und die Binnen-Alster ueberlappt
    (def* alster 
          (and river 
	       (some pp germany)
	       (some po alster-lake)
	       (all po (not country))
	       (all pp (=> country germany))))

    ;;; Hamburg ist eine Stadt die an der Alster liegt
    (def* hamburg 
          (and city	   
	       (some po alster)))

    
    
    ;(princ (subsumes? german-city hamburg :type 'jepd-abox :debug-p t)) 
    
    (setf *dag* (taxonomy *cur-tbox*
                          :debug-p nil
                          :recompute-p t
                          :resuse-nodes-p nil 
                          :semantic-branching-p nil
                          :non-determinism-p t))
    
    (princ *dag*) 
    
    (when (member 'hamburg
                  (mapcar #'dag-node-name
                          (dag-node-children (find 'german-river 
                                                   (dag-nodes *dag*)
                                                   :key #'dag-node-name))))
      (break))

    
    (loop
     (princ "*")
     (unless (dag-isomorphic-p *dag* 
                               (setf *ldag* 
                                     (taxonomy *cur-tbox*
                                               ;:reuse-nodes-p (one-of '(t nil))
                                               :semantic-branching-p (one-of '(t nil))
                                               :non-determinism-p (one-of '(t nil))
                                               :use-cached-models-p nil
                                               :cache-models-p nil
                                               :recompute-p t)))
       (princ *ldag*)
       (break)))))


;;;
;;; Einfacher ALCI_RAMINUS-Test 
;;; Interessant!!!
;;; Muss unerfuellbar sein! 
;;; 


(with-rbox (test :roles (id r s) :inverse-roles ((r r)) :delete-if-exists-p t :reflexive-roles (id)
                 :axioms ((r r r)
                          (id r s)))

  (with-tbox (test :delete-if-exists-p t)
  
    (with-abox (test :delete-if-exists-p t )

      (ins a (and
              a
              (all s (not a))
              (some r c)))

      (princ (abox-sat-p *cur-abox* :debug-p t)))))
    

;;;
;;; einige einfache ALCI_RCC5 Tests
;;;


(with-rbox (rcc5-rolebox)
  (with-abox (test :delete-if-exists-p t :language +alci-rcc+)
    (ins a (and (some pp (some pp c)) (all pp e)))
    (princ (abox-sat-p *cur-abox*))
    (visualize *cur-abox*)))


(with-rbox (rcc5-rolebox)
  (with-abox (test :delete-if-exists-p t :language +alci-rcc+)
    (ins a (and a (all pp (not c)) (all eq (not c)) (all po (not c)) (all dr (not c)) (all ppi (not c))))
    (ins b b)
    (ins c c)
    (rel a b po)
    (rel b c po)
    (setf z *cur-abox*)
    
    (princ (abox-sat-p *cur-abox* :debug-p t :compute-all-completions-p t))
    (terpri)))

  

(loop
 (princ "+")
 (reset-store *cur-store*)
         
 (when (sat? (and (some po (some po (some po (some po c))))
                  (all po (not c))
                  (all dr (not c))
                  (all pp (not c))
                  (all ppi (not c))
                  (all eq (not c)))

             :language +alci-rcc+ 
             :rbox 'rcc5-rolebox
             :use-rbox-p t
             :break-p nil
             :semantic-branching-p (one-of '(t nil))
             :non-determinism-p (one-of '(t  nil))
             :reuse-nodes-p nil ; (one-of '(t  nil))
             :debug-p nil)
   (error "!"))

 (princ "-")
         
 (unless (sat? (and (some po (some po (some po (some po c))))
                    (all po (not c))
                    (all dr (not c))
                    (all pp (not c))
                    (all ppi (not c))
                    (all eq d))                       
               :language +alci-rcc+
               :rbox 'rcc5-rolebox
               :semantic-branching-p (one-of '(t nil))
               :use-rbox-p t
               :non-determinism-p (one-of '(t  nil))
               :reuse-nodes-p (one-of '(t  nil))
               :debug-p nil)
   (error "!")))




(with-rbox (rcc5-rolebox)
 
  (with-abox (test :delete-if-exists-p t :language +alci-rcc+)

    (ins a a)
    (ins b b)
    (ins c c)
    (rel a b dr)
    (rel b c ppi)
    (rel a c (or dr po pp eq ppi))

    (setf *x* *cur-abox*)
    
    (prepare *cur-abox* +alci-rcc+)

    (compute-minimal-label *cur-abox*)
    
    (visualize *cur-abox*)))
               


(with-rbox (rcc5-rolebox)
  (loop (princ "*")
        (unless (sat? (and (or (some dr (some pp c))
                               (some dr d))
                           (all po (not c))
                           (all dr (not d))
                           (all pp (not c)))
                      :semantic-branching-p t 
                      :non-determinism-p t
                      :type 'jepd-abox :compute-all-completions-p t :debug-p nil)
          (break)))
  (visualize *completions*))
    

    








(princ (sat?
        (and c d
             (some r a) (all r (=> a b))
      
             (or (and (all r (and c (some s d)))
                      (all r (=> c (all s (and (not d) (not e))))))

                 (all r (not b))
                 
                 ;;;xxx
                 
                 )) :debug-p t :semantic-branching-p t))




(princ (sat? (and c 
                  (some s 
                        (and d (all (inv s) (not c)))))
             :debug-p t))


(princ  (sat?
         (and c x 
              (or (some r (and d y (all (inv r) (not c))))                 
                  (some r (and d z
                               (or (all (inv r) (not c))

                                  
                                   (some s (and e u
                                                (or (all (inv s) (not d))
                                                    (some t (and f p
                                                                 (or (all (inv t) (not e))

                                                                     (all (inv t)
                                                                          (=> e (all (inv s) (=> d (all (inv r) 
                                                                                                        (or (not x) 
                                                                                                            ;;;xxx
                                                                                                            )))))))))))))))))
         :debug-p t
         :semantic-branching-p (one-of '(t nil))
         :non-determinism-p t))



;;;
;;; Konjunktive Aboxen! 
;;;


(with-abox (test :delete-if-exists-p t)

  (ins a (and a (or (all r (not b)) 
                    (all s (not b))
                    (all (and r s) 
                         (not b)))))

  (ins b (and b))

  (rel a b r)
  (rel a b s)

  (princ (abox-sat-p *cur-abox* :debug-p t)))





;;;
;;; Disjunktive Aboxen! 
;;;

(with-abox (test :delete-if-exists-p t)
  (ins a (and a (all r (not b)) (all s (or (not b) (all (inv (or r s)) (not a))))))

  (ins b (and b))
  (rel a b (or r s  ))
  (setf *x* *cur-abox*)
  (princ (abox-sat-p *cur-abox* :debug-p t)))



(with-abox (test :delete-if-exists-p t)
  (ins a (and a 
              (some (or r s) b)
              (all r (not b))
              (all s (not cb))))
  
  (setf *x* *cur-abox*)
  (princ (abox-sat-p *cur-abox* :debug-p t)))


(with-abox (test :delete-if-exists-p t)
  (ins a (and a 
              (all r (not b))
              (all s (not b))))
              
  (ins b (and b))
  (rel a b (or r s))
  
  (setf *x* *cur-abox*)
  (princ (abox-sat-p *cur-abox* :debug-p t)))





(progn
  (with-abox (test :delete-if-exists-p t)
    (ins a (and a
                (and (all ab1 (and (all bc1 (not c))
                                   (all bc2 (not c))))

                     (all ab2 (and (all bc1 (not c))
                                   (all bc2 (not c)))))))
    (ins b b)
    (ins c c)
  
    (rel a b (or ab1 ab2))
    (rel b c (or bc1 bc2))
  
    (princ (abox-sat-p *cur-abox* :debug-p t))))




(progn 
  
  (with-abox (test :delete-if-exists-p t)
    
    (ins a a)  
    (ins a1 a)  
    (ins a2 a)  
    (ins a3 a)
    
    (ins b (and b (all s (not c)) (all t (not c))))
    
    (ins c c)
    
    (rel a b (or r s))
  
    (rel a1 b (or r s u v w x y a b c))
    (rel a2 b (or r s u v w x y a b c))
    (rel a3 b (or r s u v w x y a b c))
    
    (rel b c (or s t))
  
    (princ (abox-sat-p *cur-abox* :debug-p t))))


;;;
;;;
;;;

(subrole r s)
(subrole s t)

(with-abox (test :delete-if-exists-p t)
  (ins a (and a
              (or (all s (not b))
                  (all r (not b))
                  (all t (not b))
                  
                  (some s 
                        (and (all (inv t) (not a))
                             (all (inv t) (not a)))))
                  
              (some r b)))
    
  (princ (abox-sat-p *cur-abox* :debug-p t)))


;;;
;;; ALCHI_R+ ! 
;;; 



(subrole r s)
(subrole s t)
(transitive s)

(with-abox (test :delete-if-exists-p t)
  (ins a (and (some r (some s (some s (some r (some r (and c 
                                                           (all (inv s) (not b))))))))
              (or (all s (not c))
                  b)))
  
    
  (princ (abox-sat-p *cur-abox* :debug-p t)))

;;;
;;; Demo: Rollen-Konjunktion / Disjunktion 
;;:

(princ (sat? (and (some (or r s) c) (all r (not c)) (all s d) (all (or r s) (=> d (not c))))))
(princ (sat? (and (some (or r s) c) (all r (not c)) (all s d) (all (and r s) (=> d (not c))))))
(princ (sat? (and (some (and r s) c) (all r (not c)))))
(princ (sat? (and (some (and r s) c) (all (and r s) (not c)))))
(princ (sat? (and (some (and r s) c) (all (or r s) (not c)))))


(princ (sat? (and (some (and r s) c) (all (and r s t) (not c)))))

(subrole r s)

(subrole t r)


(subrole t s)

(princ (sat? (and (some (and r s) c) (all (and r s t) (not c)))))

(princ (sat? (and (some (and r s) c) (all s (not c)))))






(subrole r t)

(princ (sat? (and (some (and s r) c) (all (and s r t) (not c))) :debug-p t))




(subrole s t)

(princ (sat? (and (some (and s r) c) (all (and s r t) (not c)))))







(subrole s t r) ;;; !!

(princ (sat? (and (some s c) (all (and r t) (not c)))))



(with-rbox (rcc5-rolebox)
  (with-abox (test :type 'rolebox-abox :delete-if-exists-p t)
    (ins a (and a))  
    (ins b (and b))
    (ins c (and c))
    (ins d (and d))  
    
    (rel a b (or r s))
    (rel b c (or r s))
    (rel c d (or r s))
    (rel b d (or r s))

    (visualize (get-all-atomic-configurations *cur-abox*))))
  


(princ 
 (sat? (some pp (some pp c))
       :debug-p t
       :type 'jepd-abox 
       :visualize-p t
       :rbox 'thematic-substrate::rcc5-rolebox ))



(with-abox (test :type 'abox :delete-if-exists-p t)
  (ins a (and auto
              (some has-part
                    (and (or motor
                             (some foobar huhu))
                         (all foobar 
                              (some has-part
                                    (or a b c 
                                        (some xz cdscdsc))))))))
  (princ (abox-sat-p *cur-abox* 
                     :compute-all-completions-p t))
  (visualize *completions*))
