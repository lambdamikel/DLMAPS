;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(defconstant +specials+
  '(*

    *TIME-SPEND-IN-GET-NODES*
    *TIME-SPEND-IN-GET-CACHE-SAT-NODES*
    *TIME-SPEND-IN-GET-OLD-NODES*
    *TIME-SPEND-IN-GET-LEAF-NODES*
    *TIME-SPEND-IN-GET-DEACTIVATED-NODES*
    *TIME-SPEND-IN-GET-BLOCKED-NODES*
    *TIME-SPEND-IN-GET-ACTIVE-NODES*

    *TIME-SPEND-IN-ABOX-NODES-ITERATOR*
    *TIME-SPEND-IN-ABOX-ACTIVE-NODES-ITERATOR*
    *TIME-SPEND-IN-ABOX-OLD-NODES-ITERATOR*
    *TIME-SPEND-IN-ABOX-LEAF-NODES-ITERATOR*
    *TIME-SPEND-IN-ABOX-CACHE-SAT-NODES-ITERATOR*
    *TIME-SPEND-IN-ABOX-BLOCKED-NODES-ITERATOR*
    *TIME-SPEND-IN-ABOX-DEACTIVATED-NODES-ITERATOR*   

    *TIME-SPEND-IN-GET-OLDEST-NODE-WITH-UNEXPANDED-ATTRIBUTE-EXISTS-CONCEPTS*
    *TIME-SPEND-IN-GET-OLDEST-NODE-WITH-UNEXPANDED-SOME-CONCEPTS*
    *TIME-SPEND-IN-GET-OLDEST-NODE-WITH-UNEXPANDED-OR-CONCEPTS*
    *TIME-SPEND-IN-GET-OLDEST-NODE-WITH-UNEXPANDED-AT-LEAST-CONCEPTS*
    

    *TIME-SPEND-IN-ABOX-EDGES-ITERATOR*
    *TIME-SPEND-IN-GET-EDGES*
    
    *

    *TIME-SPEND-IN-MAKE-CREATE-ACTION* 
    *TIME-SPEND-IN-MAKE-PUT-TO-UNEXPANDED-ACTION* 
    *TIME-SPEND-IN-MAKE-MOVE-FROM-UNEXPANDED-TO-EXPANDED* 
    *TIME-SPEND-IN-MAKE-REGISTER-ADDITIONAL-DEPENDENCIES* 
    *TIME-SPEND-IN-MAKE-CHANGE-STATE-ACTION* 

    *

    *TIME-SPEND-IN-UNDO*
    *TIME-SPEND-IN-UNDO-CREATE-EDGE-ACTION*
    *TIME-SPEND-IN-UNDO-CREATE-NODE-ACTION* 
    *TIME-SPEND-IN-UNDO-CHANGE-STATE-ACTION*
    *TIME-SPEND-IN-undo-PUT-TO-UNEXPANDED-ACTION* 
    *TIME-SPEND-IN-undo-MOVE-FROM-UNEXPANDED-TO-EXPANDED-ACTION*
    *TIME-SPEND-IN-undo-REGISTER-ADDITIONAL-DEPENDENCIES-ACTION*

    *
    
    *TIME-SPEND-IN-REGISTER-CHOICE-POINTS-FOR*
    *TIME-SPEND-IN-ADD-A-CHOICE-POINT*
    *TIME-SPEND-IN-SET-CHOICE-POINTS*

    *TIME-SPEND-IN-GET-CHOICE-POINTS*
    *TIME-SPEND-IN-DELETE-CHOICE-POINTS*
    *TIME-SPEND-IN-ADD-CHOICE-POINTS-FOR-EDGES*
    
    *    
    
    *TIME-SPEND-IN-REGISTER-BLOCKING-BLOCKED*
    *TIME-SPEND-IN-UNREGISTER-BLOCKING-BLOCKED*

    *TIME-SPEND-IN-ADJUST-BLOCKING-DEPENDENCIES*
    
    *
    
    *TIME-SPEND-IN-ADD-TO-UNEXPANDED*
    *TIME-SPEND-IN-ADD-TO-EXPANDED*
    *TIME-SPEND-IN-DELETE-FROM-UNEXPANDED*
    *TIME-SPEND-IN-DELETE-FROM-EXPANDED*
    
    *
    
    *TIME-SPEND-IN-REGISTER-AS-UNEXPANDED*
    
    *TIME-SPEND-IN-MOVE-FROM-UNEXPANDED-TO-EXPANDED* 
    *TIME-SPEND-IN-PUT-FROM-EXPANDED-TO-UNEXPANDED*
    
    *
    
    *TIME-SPEND-IN-DELETE-NODE*
    *TIME-SPEND-IN-DELETE-EDGE*

    *
    
    *TIME-SPEND-IN-REGISTER-ALREADY-KNOWN-TO-BE-SATISFIABLE*
    *TIME-SPEND-IN-REGISTER-LABEL-IS-STABLE*
    *TIME-SPEND-IN-REGISTER-ADDITIONAL-DEPENDENCIES*
    *TIME-SPEND-IN-REGISTER-DETERMINISTICALLY-EXPANDED*        
    *TIME-SPEND-IN-REGISTER-ALREADY-KNOWN-TO-BE-INCONSISTENT*
    *TIME-SPEND-IN-REGISTER-CHOICE-POINTS*
    *TIME-SPEND-IN-REGISTER-CACHE-SATISFIABLE*

    *TIME-SPEND-IN-UNREGISTER-DETERMINISTICALLY-EXPANDED*    
    
    *
    
    *TIME-SPEND-IN-DELETE-NODE*
    *TIME-SPEND-IN-MARK-DELETED*    
    *TIME-SPEND-IN-UNMARK-DELETED*
    
    
    *TIME-SPEND-IN-ACTIVATE-NODE*    
    *TIME-SPEND-IN-DEACTIVATE-NODE*
    
    *TIME-SPEND-IN-PUT-FROM-UNEXPANDED-TO-EXPANDED*
    *TIME-SPEND-IN-REGISTER-AS-EXPANDED*
    
    *TIME-SPEND-IN-IS-SUCCESSOR-P*
    *TIME-SPEND-IN-ADJUST-INDEX-STRUCTURES*
    
    
    *

    *TIME-SPEND-IN-DELETE-FROM-ALL-HEAPS*
    *TIME-SPEND-IN-DELETE-FROM-HEAPS*
    *TIME-SPEND-IN-ADD-TO-HEAPS*

    *

    *TIME-SPEND-IN-GET-YOUNGEST-NODE-SATISFYING* 
    *TIME-SPEND-IN-GET-YOUNGEST-NODE-SATISFYING1* 
        
    *TIME-SPEND-IN-GET-OLDEST-NODE-SATISFYING*
    *TIME-SPEND-IN-GET-OLDEST-NODE-SATISFYING1*
    
    *TIME-SPEND-IN-GET-OLDEST-OLD-THEN-YOUNGEST-SATISFYING* 

    *

    *TIME-SPEND-IN-ABOX-ENUMERATION* 
    *TIME-SPEND-IN-INITIAL-ABOX-SATURATION* 
    *TIME-SPEND-IN-ROLEBOX-APPLICATION* 
    *TIME-SPEND-IN-ABOX-COMPLETION* 
    
    *

    *TIME-SPEND-IN-IDENTIFY-STABLE-NODES*
    *TIME-SPEND-IN-MODEL-MERGING* 
    *TIME-SPEND-IN-MAKE-MODELS-FOR-NODES*
    *TIME-SPEND-IN-MAKE-MODELS-FOR-OLD-NODES*

    *

    *TIME-SPEND-IN-DELETE-NON-DET-ASSERTIONS*
    *TIME-SPEND-IN-COMPUTE-CORE-MODEL* 


    *

    *TIME-SPEND-IN-BLOCK-NODES* 
    *TIME-SPEND-IN-POP-ACTIVE-NODES-HEAP*
    
    *

    *TIME-SPEND-IN-DETERMINISTIC-EXPANSION*
    *TIME-SPEND-IN-SELECT-DET-NODE* 
    *TIME-SPEND-IN-FOCUSED-DETERMINISTIC-EXPANSION*
    *TIME-SPEND-IN-LOOK-FOR-ATOMS* 
    *TIME-SPEND-IN-LOOK-FOR-ANDS* 
    *TIME-SPEND-IN-LOOK-FOR-ORS* 
    *TIME-SPEND-IN-LOOK-FOR-SOMES* 
    *TIME-SPEND-IN-LOOK-FOR-AT-LEASTS* 
    *TIME-SPEND-IN-LOOK-FOR-ALLS* 

    *
    

    *TIME-SPEND-IN-SELECT-OR-CONCEPT* 
    *TIME-SPEND-IN-SELECT-OR-CONCEPT1* 
    *TIME-SPEND-IN-SELECT-OPEN-DISJUNCT*
    *TIME-SPEND-IN-SELECT-OPEN-DISJUNCT1*
    *TIME-SPEND-IN-OR-EXPANSION* 
    
    *

    *TIME-SPEND-IN-SELECT-SOME-CONCEPT* 
    *TIME-SPEND-IN-SELECT-SOME-CONCEPT1* 
    *TIME-SPEND-IN-SOME-EXPANSION* 
    *TIME-SPEND-IN-COMPUTE-NEW-SOME-SUCCESSOR-LABEL* 

    *
    
    *TIME-SPEND-IN-SELECT-ATTRIBUTE-EXISTS-CONCEPTS* 
    *TIME-SPEND-IN-SELECT-ATTRIBUTE-EXISTS-CONCEPTS1* 
    *TIME-SPEND-IN-FEATURE-EXPANSION* 
    *TIME-SPEND-IN-COMPUTE-NEW-FEATURE-SUCCESSOR-LABEL* 
    
    * 

    *TIME-SPEND-IN-SELECT-AT-LEAST-CONCEPT* 
    *TIME-SPEND-IN-SELECT-AT-LEAST-CONCEPT1* 
    *TIME-SPEND-IN-SIMPLE-AT-LEAST-EXPANSION* 

    *
    
    *TIME-SPEND-IN-SELECT-VIOLATED-AT-MOST-CONCEPT*
    *TIME-SPEND-IN-SIMPLE-AT-MOST-MERGING* 

    *
    
    *TIME-SPEND-IN-PROVER-INIT-ABOX-SAT-ALCH* 
    *TIME-SPEND-IN-PROVER-INIT-ABOX-SAT-ALCHF* 
    *TIME-SPEND-IN-PROVER-INIT-ABOX-SAT-ALCHF-RPLUS* 
    *TIME-SPEND-IN-PROVER-INIT-ABOX-SAT-ALCHI* 
    *TIME-SPEND-IN-PROVER-INIT-ABOX-SAT-ALCHN*
    

    *TIME-SPEND-IN-PROVER-INIT-BEFORE-MAIN-ABOX-SAT-ALCH* 
    *TIME-SPEND-IN-PROVER-INIT-BEFORE-MAIN-ABOX-SAT-ALCHF* 
    *TIME-SPEND-IN-PROVER-INIT-BEFORE-MAIN-ABOX-SAT-ALCHF-RPLUS* 
    *TIME-SPEND-IN-PROVER-INIT-BEFORE-MAIN-ABOX-SAT-ALCHI* 
    *TIME-SPEND-IN-PROVER-INIT-BEFORE-MAIN-ABOX-SAT-ALCHN*
    
    *TIME-SPEND-IN-PROVER-MAIN-ABOX-SAT-ALCH* 
    *TIME-SPEND-IN-PROVER-MAIN-ABOX-SAT-ALCHF* 
    *TIME-SPEND-IN-PROVER-MAIN-ABOX-SAT-ALCHF-RPLUS* 
    *TIME-SPEND-IN-PROVER-MAIN-ABOX-SAT-ALCHI* 
    *TIME-SPEND-IN-PROVER-MAIN-ABOX-SAT-ALCHN*
    

    *
    
    *TIME-SPEND-IN-PROVER-INIT-ABOX-SAT-ALCI-RA-JEPD*
    *TIME-SPEND-IN-PROVER-INIT-ABOX-SAT-ALCI-RA-MINUS* 

    *TIME-SPEND-IN-PROVER-INIT-CONCEPT-INSTANCES-DL*
    *TIME-SPEND-IN-PROVER-INIT-INDIVIDUAL-INSTANCE-P-DL*

    *TIME-SPEND-IN-PROVER-MAIN-ABOX-SAT-ALCI-RA-JEPD*
    *TIME-SPEND-IN-PROVER-MAIN-ABOX-SAT-ALCI-RA-MINUS* 

    *
    
    *TIME-SPEND-IN-INDIVIDUAL-INSTANCE-REGISTER-TOLD-CONCEPTS*
    *TIME-SPEND-IN-INDIVIDUAL-INSTANCE-INIT-ABOX*
    *TIME-SPEND-IN-INDIVIDUAL-INSTANCE-ABOX-SAT*
    *TIME-SPEND-IN-INDIVIDUAL-INSTANCE-ROLLBACK*
    *TIME-SPEND-IN-INDIVIDUAL-INSTANCE-ROLLBACK2*
    *TIME-SPEND-IN-GLOBAL-ROLLBACK*

    *))

(defun reset-statistics ()
  (setf *initial-concept-sat-cache-hits* 0)
  (setf *initial-concept-unsat-cache-hits* 0)
  (setf *initial-concept-sat-cache-queries* 0)
  (setf *initial-concept-unsat-cache-queries* 0)

  (setf *individual-instance-proofs* 0
        *obvious-instance-hits* 0 
        *obvious-non-instance-hits* 0
        *all-instance-tests* 0)
  

  (setf *true-abox-consistency-tests* 0)
  (setf *true-abox-subsumption-tests* 0)

  (setf *mm-queries* 0)
  (setf *mm-hits* 0)

  (setf *concept-queries* 0)
  (setf *true-concept-queries* 0)
  (setf *concept-mm-queries* 0)
  (setf *concept-mm-hits* 0)

  (setf *subsumes-queries* 0)
  (setf *true-subsumes-queries* 0)
  (setf *subsumes-mm-hits* 0)
  (setf *subsumes-ts-hits* 0)

  (setf *blocked-nodes* 0)
  (setf *created-nodes* 0)

  (setf *no-of-undos* 0
        *time-spend-in-undo* 0
        *time-for-node-creation* 0
        *time-for-edge-creation* 0
        *sat-cache-hits* 0 
        *sat-cache-queries* 0)
                

  (dolist (var +specials+)
    (unless (eq var '*)
      (setf (symbol-value var) 0))))


(defun show-statistics ()

  ;(describe-object *abox* t)

  (format t "~%*** Node: ~A, Concept : ~A ~%" *node* *concept*)

  (format t "~%*** Nodes: ~A Active: ~A Edges: ~A ~%" 
          (length (get-nodes *abox*))
          (length (get-active-nodes *abox*))
          (length (get-edges *abox*)))
          

  (format t "~%----------------------------------------------")

  (format t "~%*** ~7,D UNDOS IN ~7,D SECONDS, AVERAGE : ~20,10F" 
          *no-of-undos*
          (float (/ *time-spend-in-undo* internal-time-units-per-second))
          (float (/ (/ *time-spend-in-undo* internal-time-units-per-second) 
                    (max *no-of-undos* 1))))

  (format t "~%*** Time For Node Creation                    : ~10,D"
          (float (/ *time-for-node-creation* internal-time-units-per-second)))

  (format t "~%*** Time For Edge Creation                    : ~10,D"
          (float (/ *time-for-edge-creation* internal-time-units-per-second)))

  (format t "~%*** Blocked vs. Created Nodes                 : ~10,D of ~10,D / ~10,4F"  
          *blocked-nodes*
          *created-nodes*
          (float (/ *blocked-nodes* (max 1 *created-nodes*))))

  
  ;;;
  ;;; Interface (Konzept, Subsumption) 
  ;;; 

  (format t "~%----------------------------------------------")

  (format t "~%*** CONCEPT STATISTICS")

  (format t "~%*** Concept Sat Cache Performance             : ~10,D of ~10,D / ~10,4F" 
          (- *concept-queries* *true-concept-queries*) *concept-queries*
          (float (/ (- *concept-queries* *true-concept-queries*) (max 1 *concept-queries*))))
   
  (format t "~%*** AND Concept Sat Cache MM Performance      : ~10,D of ~10,D / ~10,4F" 
          *concept-mm-hits* *concept-mm-queries*
          (float (/ *concept-mm-hits* (max 1 *concept-mm-queries*))))

  (format t "~%----------------------------------------------")

  (format t "~%*** SUBSUMPTION STATISTICS")

  (format t "~%*** True vs. All Subsumption Tests            : ~10,D of ~10,D / ~10,4F" 
          *true-subsumes-queries*
          *subsumes-queries* 
          (float (/ *true-subsumes-queries* (max 1 *subsumes-queries*))))

  (format t "~%*** Told Subsumers Subsumptions Hits          : ~10,D of ~10,D / ~10,4F" 
          *subsumes-ts-hits* 
          *subsumes-queries* 
          (float (/ *subsumes-ts-hits* (max 1 *subsumes-queries*))))

  (format t "~%*** MM Performance of Rem. Subs. Tests        : ~10,D of ~10,D / ~10,4F" 
          *subsumes-mm-hits*
          *true-subsumes-queries* 
          (float (/ *subsumes-mm-hits* (max 1 *true-subsumes-queries*))))

  (format t "~%*** True ABox Subsumption Tests               : ~10,D" 
          *true-abox-subsumption-tests*)

  ;;;
  ;;; ABox (Subtableaux, Model-Merging) 
  ;;; 


  (format t "~%----------------------------------------------")
  (format t "~%*** ABOX STATISTICS")
  
  (format t "~%*** True ABox Consistency Tests               : ~10,D" 
          *true-abox-consistency-tests*)


  (format t "~%*** Total Unsat Cache Performance             : ~10,D of ~10,D / ~10,4F" 
          *initial-concept-unsat-cache-hits*
          *initial-concept-unsat-cache-queries*
          (float (/ *initial-concept-unsat-cache-hits* (max 1 *initial-concept-unsat-cache-queries*))))


  (format t "~%*** Total   Sat Cache Performance             : ~10,D of ~10,D / ~10,4F"  
          *sat-cache-hits*
          *sat-cache-queries*
          (float (/ *sat-cache-hits* (max 1 *sat-cache-queries*))))

  (format t "~%    Initial Concept Sat Cache Performance     : ~10,D of ~10,D / ~10,4F" 
          *initial-concept-sat-cache-hits*
          *initial-concept-sat-cache-queries*
          (float (/ *initial-concept-sat-cache-hits* (max 1 *initial-concept-sat-cache-queries*))))

  (format t "~%    Initial Concept MM Test Performance       : ~10,D of ~10,D / ~10,4F" 
          *mm-hits*
          *mm-queries*
          (float (/ *mm-hits* (max 1 *mm-queries*))))

  (format t "~%----------------------------------------------")
  (format t "~%*** INSTANCE STATISTICS")

  (format t "~%*** All Instance Tests                        : ~10,D" *all-instance-tests*)
    
  (format t "~%*** Obvious Instance Hits                     : ~10,D / ~10,4F" 
          *obvious-instance-hits* 
          (/ *obvious-instance-hits* (max 1 *all-instance-tests*)))

  (format t "~%*** Obvious Non Instance Hits                 : ~10,D / ~10,4F" 
          *obvious-non-instance-hits*
          (/ *obvious-non-instance-hits* (max 1 *all-instance-tests*)))

  (format t "~%*** True Instance Proofs                      : ~10,D / ~10,4F" 
          *individual-instance-proofs* 
          (/ *individual-instance-proofs* (max 1 *all-instance-tests*)))


  (format t "~%*** PROFILER STATISTICS")

  ;;;
  ;;;
  ;;;

  (dolist (var +specials+)
    (if (eq var '*)
        (format t "~%-------------------------------------------------------------------------------")
      (format t "~%*** ~80,A : ~10,4F " 
              var (float (/ (symbol-value var) internal-time-units-per-second)))))
  
  (terpri))


(reset-statistics)

