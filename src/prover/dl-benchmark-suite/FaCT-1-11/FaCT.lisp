;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;; FaCT description logic classifier                                    ;;;
;;; COPYRIGHT (C) IAN R. HORROCKS and THE UNIVERSITY OF MANCHESTER, 1997 ;;; 
;;;					                                 ;;;
;;; This program is free software; you can redistribute it and/or        ;;;
;;; modify it under the terms of the GNU General Public License          ;;;
;;; as published by the Free Software Foundation; either version 2       ;;;
;;; of the License, or (at your option) any later version.               ;;;
;;;                                                                      ;;;
;;; This program is distributed in the hope that it will be useful,      ;;;
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of       ;;;
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        ;;;
;;; GNU General Public License for more details.                         ;;;
;;;                                                                      ;;;
;;; You should have received a copy of the GNU General Public License    ;;;
;;; along with this program; if not, write to the Free Software          ;;;
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA            ;;;
;;; 02111-1307, USA.                                                     ;;;
;;;                                                                      ;;;
;;; Enquiries about FaCT should be directed to:                          ;;;
;;;                                                                      ;;;
;;; email: horrocks@cs.man.ac.uk                                         ;;;
;;; www:   http://www.cs.man.ac.uk/~horrocks                             ;;;
;;; smail: Ian Horrocks, Department of Computer Science, Oxford Road,    ;;;
;;;       Manchester 467M13 9PL, United Kingdom                          ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CL-USER")

(defconstant *reasoner* "FaCT")
(defconstant *version-number* "1.11")

#|
(export
 '(grail-define-relation grail-redefine-relation grail-define-implication
			 grail-define-concept grail-define-primconcept load-kb
			 reset-kb clear-kb dump-taxonomy export-kb
			 set-verbosity reset-verbosity set-debug reset-debug
			 set-features reset-features features set-profiling reset-profiling
			 alc-concept-coherent alc-test
			 defconcept-f defconcept defprimconcept-f defprimconcept
			 defprimrole-f defprimrole
			 defprimattribute-f defprimattribute implies-f implies disjoint-f disjoint
			 load-tkb classify-tkb init-tkb
			 direct-supers all-supers direct-subs all-subs
			 equivalences satisfiable subsumes 
			 equivalent-concepts disjoint-concepts 
			 add-concept-f add-concept classify-concept 
			 get-concept get-role get-all-concepts get-all-roles classified-tkb? 
			 what-is? is-primitive? is-concept? is-role? is-feature? name description))
|#

#|
(eval-when
    (compile)
  (PROCLAIM
   '(OPTIMIZE
     (SAFETY 0)				; Run time error checking level
     (SPEED 3)				; Speed of the compiled code
     (COMPILATION-SPEED 0)		; Speed of compilation
     (SPACE 0)				; Space of both intermidiate files and object
     #+(or ALLEGRO LISPWORKS) (DEBUG 0)
     ))
  #-LISPWORKS (proclaim '(inline grail-concept-definition grail-concept-primitive
			  system-concept-parents system-concept-children
			  system-concept-synonym system-concept-definition
			  system-concept-primitive system-concept-mark1
			  system-concept-visited system-concept-model
			  system-concept-classified system-concept-asserted-supers
			  system-concept-grail-name
			  role-parents role-ancestors role-functional
			  role-f-ancestors role-transfered-by role-transfers
			  role-grail-name role-grail-inv-name role-processed
			  constraints-c constraints-props constraints-all-rc
			  constraints-all-r+c constraints-r-y constraints-f-y
			  constraints-or-clauses constraints-bcp-cand constraints-some-fc-ux
			  constraints-some-rc-ux constraints-c-def-ux constraints-parent
			  c-grail-name-f r-functional-f r-inverse-f r-ancestors-f neg-con-f))
  (proclaim '(inline queue-contents make-queue enqueue dequeue
	      front empty-queue-p queue-nconc push-queue pop-queue
	      pos-lit pos-lit-p)))
|#


;;; ************** DATA STRUCTURES **************

(defstruct (grail-concept
	    (:print-function (lambda (c s k)
			       (declare (ignore k))
			       (format s "c[~A]" (grail-concept-name c)))))
  "structure for a grail concept node"
  (definition nil)
  (primitive nil)
  (name nil))

(defstruct (system-concept
	    (:print-function (lambda (c s k)
			       (declare (ignore k))
			       (format s "c[~A]"
				       (case (system-concept-grail-name c)
					     (:top '*TOP*)
					     (:bottom '*BOTTOM*)
					     (T (system-concept-grail-name c)))))))
  "structure for a system concept node"
  (parents nil)
  (children nil)
  (synonym nil)
  (definition nil)
  (primitive nil)
  (mark1 nil)
  (visited nil)
  (model nil)
  (classified 0)
  (asserted-supers nil)
  (grail-name nil))

(defstruct (role
	    (:print-function (lambda (r s k)
			       (declare (ignore k))
			       (format s "r[~A]" (role-grail-name r)))))
  "structure for a role"
  (parents nil)
  (ancestors nil)
  (functional nil)
  (f-ancestors nil)
  (transfered-by nil)
  (transfers nil)
  (grail-name nil)
  (grail-inv-name nil)
  (processed nil))

(defstruct constraints
  "structure of a costraint system for a single individual"
  (c nil)
  (props nil)
  (all-rc nil)
  (all-r+c nil)
  (r-y nil)
  (f-y nil)
  (or-clauses nil)
  (bcp-cand nil)
  (some-fc-ux nil)
  (some-rc-ux nil)
  (c-def-ux nil)
  (parent nil))



;;; ************** GLOBAL CONSTANTS **************

;;;(defconstant *max-n-concepts* 20000 "Maximum number of system concepts")
(defconstant *max-n-concepts* 100000 "Maximum number of system concepts")
(defconstant *max-n-roles* 10000 "Maximum number of roles")
(defconstant *TOP* 0 "System name of top concept")
(defconstant *BOTTOM* 1 "System name of bottom concept")



;;; ************** GLOBAL VARIABLES **************

(defvar *c-definitions* nil
  "List of defined GRAIL concepts")

(defvar *grail-concept-table* (make-hash-table :size *max-n-concepts*)
  "Maps GRAIL concept names to system concept names (fixnums)")

(defvar *system-concept-array* (make-array `(,(+ *max-n-concepts* *max-n-concepts*))
					   :element-type 'system-concept
					   :adjustable nil
					   :fill-pointer nil
					   :displaced-to nil)
 "Maps system concept names (fixnums) to definitions etc. (inc. GRAIL names if applicable)")

(defvar *next-available-concept* 2
  "Value of the next undefined system concept name (a fixnum)")

(defvar *concept-definition-table* (make-hash-table :test #'equal :size *max-n-concepts*)
  "Maps GRAIL concept names and system concept definitions to system concept names (fixnums)")

(defvar *kb-file-names* nil
  "List of kb files loaded since last clear-kb")


(defvar *relations* (make-hash-table :size *max-n-roles*)
  "Maps role names to data structures")
(defvar *r-definitions* nil
  "List of defined role names")
(defvar *transitive-roles* nil
  "Set to T (NIL) if there are (are not) transitive roles in KB")


(defvar *implications* nil
  "List of unprocessed implication axioms")
(defvar *grail-universal-constraints* nil
  "List of unencoded universal axioms (GCIs)")
(defvar *universal-constraint* nil
  "Encoded universal axiom - conjunction of all non-absorbed GCIs")


(defparameter *clash-level* nil
  "Dependency set returned after a clash")
(defparameter *or-level* 0
  "Current branching level (used to form dependency sets)")


;;; Variables controlling classifier features
(defparameter *transitivity* T
  "When T (NIL) enables (disables) transitive roles; default=T")
(defparameter *concept-eqn* T
  "When T (NIL) enables (disables) concept equations; default=T")
(defparameter *subset-s-equivalent* T
  "When T (NIL) enables (disables) enhanced subset blocking strategy; default=T")
(defparameter *backjumping* T
  "When T (NIL) enables (disables) dependency directed backtracking; default=T")
(defparameter *obvious-subs* T
  "When T (NIL) enables (disables) detection of obvious subsumption relations; default=T")
(defparameter *top-level-caching* T
  "When T (NIL) enables (disables) top-level caching in subsumption tests; default=T")
(defparameter *full-caching* T
  "When T (NIL) enables (disables) caching in satisfiability tests; default=T")
(defparameter *blocking* T
  "When T (NIL) enables (disables) blocking - essential for trans roles & cycles; default=T")
(defparameter *taxonomic-encoding* T
  "When T (NIL) enables (disables) concept encoding optimisation; default=T")
(defparameter *gci-absorption* T
  "When T (NIL) enables (disables) GCI absorbtion optimisation; default=T")
(defparameter *cyclical-definitions* T
  "When T (NIL) enables (disables) cyclical definitions for primitive concepts; default=T")
(defparameter *auto-configure* T
  "When T (NIL) enables (disables) auto-confuguration of features; default=T")
(defparameter *moms-heuristic* 0
  "Selects the search heuristic: 0 = Oldest+JW, 1 = MOMS, 2 = Dependencies+JW; default=0")
(defparameter *prefer-pos-lits* T
  "When T (NIL) MOMS heuristic prefers positive (negative) literals; default=T")
					; NOTE value restrictions are always neg-lits
(defparameter *minimise-clashes* T
  "When T (NIL) branch first on (negation of) highest priority literal; default=NIL")
					; NOTE pos-lit minimises clashes, neg-lit maximises pruning
(defparameter *sort-lists* T
  "When T (NIL) sorts successor list into oldest dependency order, otherwise reverses it; default=T")
;;; Features applicable to KSAT tests
(defparameter *auto-install-primitives* T
  "When T (NIL) enables (disables) auto-installation of primitive concepts & roles; default=T")
(defparameter *auto-install-classify* T
  "When T (NIL) enables (disables) classification of auto-installed concepts; default=T")
(defparameter *auto-install-transitive* nil
  "When T (NIL) auto-installed roles are (are not) transitive => modal K4 (modal K); default=NIL")
(defparameter *encode-reflexive* nil
  "When T (NIL) encoded concepts are (are not) reflexive => modal KT/S4 (modal K); default=NIL")

;;; Variables controlling debugging features
(defparameter *debugging* nil
  "When T (NIL) enables (disables) debugging features; default=NIL")
(defparameter *tsd* 0 "Caching recursion counter used in debugging output")
(defparameter *already-backtracking* nil "Backtracking flag used for debugging")

;;; Variables controlling verbosity features
(defparameter *verbosity*
  '(:rc-counts :test-counts
	       :cache-counts :warnings :synonyms :reclassifying)
  "List of enabled verbosity features")

;;; Variables controlling profiling features
(defparameter *profiling* 0
  "Profiling level (0=disabled); default=0")
(defparameter *profile-file* nil
  "When T (NIL) profiling output is written to file (console); default=NIL")
(defparameter *profile-fname* "profile.out"
  "Name of profiling output file; default=profile.out")

;;; Variables used to gather profiling data
(defparameter *start-time* 0)
(defparameter *run-time* 0)
(defparameter *model-size* 0)
(defparameter *max-model-size* 0)
(defparameter *model-depth* 0)
(defparameter *max-model-depth* 0)
(defparameter *search-space* 0)
(defparameter *cache-accesses* 0)
(defparameter *cache-hits* 0)
(defparameter *caching-sat-tests* 0)
(defparameter *total-sat-tests* 0)
(defparameter *total-subs-tests* 0)
(defparameter *cycle* nil)
;;; for KSAT tests
(defparameter *total-model-size* 0)
(defparameter *variable-assignments* 0)



;;; ************** DATA STRUCTURE ACCESSING **************

;;; Macro to access *SYSTEM-CONCEPT-ARRAY*
(defmacro s-concept (c) `(aref *system-concept-array* ,c))

;;; Macros to access SYSTEM-CONCEPT structures in *SYSTEM-CONCEPT-ARRAY*
(defmacro c-parents (c) `(system-concept-parents (s-concept ,c)))
(defmacro c-children (c) `(system-concept-children (s-concept ,c)))
(defmacro c-synonym (c) `(system-concept-synonym (s-concept ,c)))
(defmacro c-definition (c) `(system-concept-definition (s-concept ,c)))
(defmacro c-primitive (c) `(system-concept-primitive (s-concept ,c)))
(defmacro c-mark1 (c) `(system-concept-mark1 (s-concept ,c)))
(defmacro c-visited (c) `(system-concept-visited (s-concept ,c)))
(defmacro c-model (c) `(system-concept-model (s-concept ,c)))
(defmacro c-classified (c) `(system-concept-classified (s-concept ,c)))
(defmacro c-asserted-supers (c) `(system-concept-asserted-supers (s-concept ,c)))
(defmacro c-grail-name (c) `(system-concept-grail-name (s-concept ,c)))
;;; Functions to access SYSTEM-CONCEPT structures in *SYSTEM-CONCEPT-ARRAY*
(defun c-grail-name-f (c) (system-concept-grail-name (s-concept c)))

;;; Macro to access *GRAIL-CONCEPT-TABLE*
(defmacro g-concept (c) `(gethash ,c *grail-concept-table*))

;;; Macros to access GRAIL-CONCEPT structures in *GRAIL-CONCEPT-TABLE*
(defmacro g-definition (c) `(grail-concept-definition (g-concept ,c)))
(defmacro g-primitive (c) `(grail-concept-primitive (g-concept ,c)))

;;; Macro to access *CONCEPT-DEFINITION-TABLE*
(defmacro system-name (c) `(gethash ,c *concept-definition-table*))

;;; Macro to access *RELATIONS* hash-table
(defmacro r-defined (r) `(gethash ,r *relations*))

;;; Macros to access ROLE structures in *RELATIONS* hash-table
(defmacro r-parents (r) `(role-parents (gethash ,r *relations*)))
(defmacro r-ancestors (r) `(role-ancestors (gethash ,r *relations*)))
(defmacro r-f-ancestors (r) `(role-f-ancestors (gethash ,r *relations*)))
(defmacro r-trans-across (r) `(role-transfered-by (gethash ,r *relations*)))
(defmacro r-transfers (r) `(role-transfers (gethash ,r *relations*)))
(defmacro r-inverse (r) `(role-grail-inv-name (gethash ,r *relations*)))
(defmacro r-functional (r) `(role-functional (gethash ,r *relations*)))
(defmacro r-processed (r) `(role-processed (gethash ,r *relations*)))
;;; Functions to access ROLE structures in *RELATIONS* hash-table
(defun r-functional-f (r) (role-functional (gethash r *relations*)))
(defun r-inverse-f (r) (role-grail-inv-name (gethash r *relations*)))
(defun r-ancestors-f (r) (role-ancestors (gethash r *relations*)))
(defun r-processed-f (r) (role-processed (gethash r *relations*)))

;;; Macros to access (CONSTRAINT.DEPENDENCY-LIST) structures
(defmacro constraint (c-l) `(first ,c-l))
(defmacro level (c-l) `(second ,c-l))

;;; Macro to negate concepts - flips zeroth bit
(defmacro neg-con (c) `(logxor ,c 1))
;;; Function to negate concepts - flips zeroth bit
(defun neg-con-f (c) (logxor c 1))

 

;;; ************** QUEUE IMPLEMENTATION FUNCTIONS **************

;;; A queue is a (last . contents) pair

(defun queue-contents (q) (cdr q))

(defun make-queue ()
  "Build a new queue with no elements."
  (let ((q (cons nil nil)))
    (setf (car q) q)))

(defun enqueue (item q)
  "insert item at the end of the queue."
  (setf (car q)
	(setf (rest (car q))
	      (cons item nil)))
  q)

(defun dequeue (q)
  "Remove an item from the front of the queue."
  (pop (cdr q))
  (if (null (cdr q)) (setf (car q) q))
  q)

(defun front (q) (first (queue-contents q)))

(defun empty-queue-p (q) (null (queue-contents q)))

(defun queue-nconc (q list)
  "add the elements of LIST to the end of the queue."
  (setf (car q)
	(last (setf (rest (car q)) list))))

(defun push-queue (item q)
  "insert item at the end of the queue."
  (rplaca q
	  (cdr
	   (rplacd (car q)
		   (cons item nil))))
  item)

(defun pop-queue (q)
  "Remove an item from the front of the queue & return the item."
  (prog1
      (pop (cdr q))
    (if (null (cdr q)) (rplaca q q))))



;;; ************** GRAIL ERROR HANDLER **************

(defun error-handler (&optional (f nil) (c nil) (s nil))
  (if f (format t "~&!!ERROR!! in function ~S.~%" f)
    (format t "~&!!ERROR!!~%"))
  (cond
   ((eq c :bad-concept-defn)
    (format t
	    "Concept definition:~%   ~S~%is badly formed.~%" s))
   ((eq c :bad-role-defn)
    (format t
	    "Relation definition:~%   ~S~%is badly formed.~%" s))
   ((eq c :bad-concept-expansion)
    (format t
	    "Problem expanding concept.~%"))
   ((eq c :undefined-concept)
    (format t
	    "Concept:~%   ~S~%is undefined.~%" s))
   ((eq c :bad-relation-expansion)
    (format t
	    "Problem expanding relation.~%"))
   ((eq c :undefined-relation)
    (format t
	    "Relation:~%   ~S~%is undefined.~%" s))
   ((eq c :concept-redefinition)
    (format t
	    "Concept: ~S is already defined.~%" s))
   ((eq c :relation-redefinition)
    (format t
	    "Relation: ~S is already defined.~%" s))
   ((eq c :bad-name)
    (format t
	    "Unacceptable concept/relation name: ~S~%" s))
   ((eq c :non-primitive-r)
    (format t
	    "Tried to define non-primitive relation: ~S~%" s))
   ((eq c :is-is-primitive)
    (format t
	    "Can only be one of :is :is-primitive: ~S~%" s))
   ((eq c :implication)
    (format t
	    "Can't find primitive in implication antecedant ~S~%" s))
   ((eq c :recursive-definition)
    (format t
	    "Recursive definition ~S~%" s))
   (T
    (format t
	    "Unspecified error~%")))
  (break))



;;; ************** CONCEPT ENCODING **************

(defun decode-concept (c)
  (if (atom c)
      (if (c-grail-name c)
	  (c-grail-name c)
	(if (c-definition c)
	    (decode-concept (c-definition c))
	  (decode-concept (list :not (neg-con c)))))
    (case (car c)
	  ((:and :or) (cons (car c) (mapcar #'decode-concept (cdr c))))
	  ((:some :all) (list (car c) (second c) (decode-concept (third c))))
	  (:not (list (car c) (decode-concept (second c))))
	  (T (error "~&BAD CONCEPT DEFINITION - ~s~%" c)))))

(defun flatten (d &optional and-or)
  (if (listp d) 
      (case (car d)
	    ((:and :or)
	     (cond
	      ((null and-or)
	       (let ((d-flat (delete-duplicates (mapcan #'(lambda (x) (flatten x (car d))) (cdr d))
						:test #'equal)))
		 (if (eql (length d-flat) 1)
		     (car d-flat)
		   (cons (car d) d-flat))))
	      ((eq (car d) and-or)
	       (mapcan #'(lambda (x) (flatten x and-or)) (cdr d)))
	      (T
	       (list (flatten d)))))
	    ((:some :all)
	     (if and-or
		 (list (list (car d) (second d) (flatten (third d))))
	       (list (car d) (second d) (flatten (third d)))))
	    (:not
	     (if and-or
		 (list (list :not (flatten (second d))))
	       (list :not (flatten (second d)))))
	     (T))
    (if and-or
	(list d)
      d)))

(defun neg-normal (s)
  (cond
   ;; C => C
   ((atom s) s)
   ((eq (car s) :not)
    (cond
     ;; (:not :top) => :bottom
     ((eq (cadr s) :top) :bottom)
     ;; (:not :bottom) => :top
     ((eq (cadr s) :bottom) :top)
     ((listp (cadr s))
      (cond
       ;; (:not (:and C D)) => (:or (:not C) (:not D))
       ((eq (caadr s) :and)
	(cons :or (mapcar #'(lambda (x) (neg-normal (list :not x))) (cdadr s))))
       ;; (:not (:or C D)) => (:and (:not C) (:not D))
       ((eq (caadr s) :or)
	(cons :and (mapcar #'(lambda (x) (neg-normal (list :not x))) (cdadr s))))
       ;; (:not (:not C)) => c
       ((eq (caadr s) :not) (neg-normal (cadadr s)))
       ;; (:not (:all R C)) => (:some R (:not C))
       ((eq (caadr s) :all)
	(list :some
	      (neg-normal (cadadr s))
	      (neg-normal (list :not (car (cddadr s))))))
       ;; (:not (:some R C)) => (:all R (:not C))
       ((eq (caadr s) :some)
	(list :all
	      (neg-normal (cadadr s))
	      (neg-normal (list :not (car (cddadr s))))))
       (T (error-handler 'neg-normal :bad-concept-defn s))))
     ;; (:not C) => (:not C)
     (T s)))
   ;; if it doesn't start with a not neg-normal each component element
   (T (mapcar #'(lambda (x) (neg-normal x)) s))))

(defun neg-definition (d)
  (case (car d)
	(:and (cons :or (mapcar #'neg-con-f (cdr d))))
	(:or (cons :and (mapcar #'neg-con-f (cdr d))))
	(:some (list :all (second d) (neg-con (third d))))
	(:all (list :some (second d) (neg-con (third d))))
	(T (error "~&BAD CONCEPT TERM - ~S~%" d))))

(defun make-new-concept (c)
  (let ((n *next-available-concept*))
    (setf (s-concept n)
	  (make-system-concept :definition (when (listp c) c)
			       :synonym n))
    (setf (s-concept (1+ n))
	  (make-system-concept :definition (when (listp c) (neg-definition c))
			       :synonym (1+ n)))
    (when (and (listp c) (eq (car c) :and))
	  (setf (c-asserted-supers n) (cdr c))
	  (dolist (c-sup (cdr c))
		  (push (neg-con n) (c-asserted-supers (neg-con c-sup)))))
    (incf *next-available-concept* 2)
    n))

(defun install-concept (c)
  (let ((name (system-name c)))
    (if name name
      (setf (system-name c) (make-new-concept c)))))

(defun encode-and (l)
  (let ((e-l))
    (dolist (c (cdr l))
	    (let ((e-c (encode-concept-term c)))
	      (cond
	       ((or (eql e-c *BOTTOM*) (member (neg-con e-c) e-l))
		(return-from encode-and *BOTTOM*))
	       ((eql e-c *TOP*))
	       (T (pushnew e-c e-l)))))
    (cond
     ((cdr e-l) (install-concept (cons :and (sort e-l #'<))))
     (e-l (car e-l))
     (T *TOP*))))

(defun encode-or (l)
  (let ((e-l))
    (dolist (c (cdr l))
	    (let ((e-c (neg-con (encode-concept-term c))))
	      (cond
	       ((or (eql e-c *BOTTOM*) (member (neg-con e-c) e-l))
		(return-from encode-or *BOTTOM*))
	       ((eql e-c *TOP*))
	       (T (pushnew e-c e-l)))))
    (cond
     ((cdr e-l) (install-concept (cons :and (sort e-l #'<))))
     (e-l (car e-l))
     (T *TOP*))))

(defun encode-concept-term (c)
  (if (atom c)
      (let ((s-n (system-name c)))
	(if s-n s-n
	  (if *auto-install-primitives*
	      (if (and *auto-install-classify* (not (numberp c)))
		  (progn
		    (if (not (g-concept c))
			(grail-define-primconcept c))
;;;		    (install-concept c)
		    (encode-grail-concept c)
		    (setf s-n (system-name c)))
		(progn
		  (setf s-n (install-concept c))
		  (setf (c-primitive s-n) T)
		  (setf (c-grail-name s-n) c)
		  s-n))
	    (error "TRIED TO ENCODE UNDEFINED CONCEPT - ~S" c))))
    (case (car c)
	  (:and
	   (if *taxonomic-encoding*
	       (encode-and c)
	     (make-new-concept (cons :and (mapcar #'encode-concept-term (cdr c))))))
	  (:or
	   (neg-con
	    (if *taxonomic-encoding*
		(encode-or c)
	      (make-new-concept (cons :and (mapcar #'(lambda (c1)
						       (neg-con (encode-concept-term c1)))
						   (cdr c)))))))
	  (:some
	   (let ((ec (encode-concept-term (third c))))
	     (unless (r-defined (second c))
		     (if *auto-install-primitives*
			 (progn
			   (defprimrole-f (second c) :transitive *auto-install-transitive*)
			   (process-role (second c)))
		       (error "TRIED TO ENCODE TERM WITH UNDEFINED ROLE - ~S" c)))
	     (if (and *taxonomic-encoding* (eql ec *BOTTOM*))
		 *BOTTOM*
	       (if *taxonomic-encoding*
		   (if *encode-reflexive*
		       (neg-con
			(install-concept
			 (list :and (neg-con ec)
			       (neg-con (install-concept (list :some (second c) ec))))))
		     (install-concept (list :some (second c) ec)))
		 (if *encode-reflexive*
		     (neg-con
		      (make-new-concept
		       (list :and (neg-con ec)
			     (neg-con (make-new-concept (list :some (second c) ec))))))
		   (make-new-concept (list :some (second c) ec)))))))
	  (:all
	   (let ((ec (neg-con (encode-concept-term (third c)))))
	     (unless (r-defined (second c))
		     (if *auto-install-primitives*
			 (progn
			   (defprimrole-f (second c) :transitive *auto-install-transitive*)
			   (process-role (second c)))
		       (error "TRIED TO ENCODE TERM WITH UNDEFINED ROLE - ~S" c)))
	     (if (and *taxonomic-encoding* (eql ec *BOTTOM*))
		 *TOP*
	       (if *taxonomic-encoding*
		   (if *encode-reflexive*
		       (install-concept
			(list :and (neg-con ec)
			      (neg-con (install-concept (list :some (second c) ec)))))
		     (neg-con (install-concept (list :some (second c) ec))))
		 (if *encode-reflexive*
		     (make-new-concept
		      (list :and (neg-con ec)
			    (neg-con (make-new-concept (list :some (second c) ec)))))
		   (neg-con (make-new-concept (list :some (second c) ec))))))))
	  (:not
	   (neg-con (encode-concept-term (second c))))
	  (T
	   (error "BAD CONCEPT TERM - ~S" c)))))

(defun encode-grail-concept (c)
;;; do nothing if its already encoded or if it isn't a defined GRAIL concept - in this
;;; latter case encode-concept-term will install it as a primitive or signal and error
;;; depending on the setting of *auto-install-primitives*
  (unless (or (system-name c) (not (or (numberp c) (g-concept c))))
	  (verbosity '(:classify-1 :classify-2) "c")
;;; numbers are treated as atomic primitives
	  (if (numberp c)
	      (let ((s-n (install-concept c)))
		(setf (c-primitive s-n) t)
		(setf (c-grail-name s-n) c))
	    (let ((g-d (g-definition c)))
	      (if (g-primitive c)
;;; install c first in case its defn is cyclical
		  (let ((s-n (install-concept c)))
;;; recursively encode all concepts c refers to
		    (map nil #'encode-grail-concept (directly-refers-to g-d))
		    (let ((c-d (when g-d (encode-concept-term g-d))))
		      (cond ((and c-d (eql c-d *BOTTOM*))
			     (verbosity :warnings
					"~&!!WARNING!! ~A is INCOHERENT~%" c)
			     (setf (system-name c) *BOTTOM*)
			     (setf (c-definition s-n) *BOTTOM*)
			     (setf (c-synonym s-n) *BOTTOM*)
			     (setf (c-definition (neg-con s-n)) *TOP*)
			     (setf (c-synonym (neg-con s-n)) *TOP*))
			    (T
			     (setf (c-definition s-n) c-d)
			     (setf (c-primitive s-n) t)
			     (setf (c-classified s-n) 1)
			     (when c-d
				   (setf (c-asserted-supers s-n) (list c-d)))
			     (setf (c-grail-name s-n) c)))))
		(progn
;;; recursively encode all concepts c refers to
		  (map nil #'encode-grail-concept (directly-refers-to g-d))
		  (let ((c-d (encode-concept-term g-d)))
		    (when (eql c-d *BOTTOM*)
			  (verbosity :warnings "~&!!WARNING!! ~A is INCOHERENT~%" c))
		    (setf (system-name c) c-d)
		    (when (zerop (c-classified c-d))
			  (setf (c-classified c-d) 1))
		    (if (c-grail-name c-d)
			(when (not (eq c (c-grail-name c-d)))
			      (verbosity :synonyms "~&!!NOTE!! ~A is a SYNONYM for ~A~%"
					 c (c-grail-name c-d)))
		      (setf (c-grail-name c-d) c)))))))))

(defun encode-all-concepts ()
  (verbosity '(:classify-1 :classify-2) "~&~%Encoding concept terms: ")
  (map nil #'encode-grail-concept *c-definitions*)
  (when *grail-universal-constraints*
	(let ((e-u-c (encode-concept-term *grail-universal-constraints*)))
	  (unless (eql e-u-c *TOP*)
		  (if *universal-constraint*
		      (if (or (eql e-u-c *BOTTOM*) (eql *universal-constraint* *BOTTOM*)
			      (eql e-u-c (neg-con *universal-constraint*)))
			  (setf *universal-constraint* *BOTTOM*)
			(install-concept
			 (cons :and (sort (list e-u-c *universal-constraint*) #'<))))
		    (setf *universal-constraint* e-u-c))))
	(setf *grail-universal-constraints* nil)))



;;; ************** GRAIL interface **************

(defun directly-refers-to (c)
  (if (atom c)
      (unless (null c) (list c))
    (case (car c)
	  ((:and :or)
	   (mapcan #'directly-refers-to (cdr c)))
	  ((:some :all)
	   (directly-refers-to (third c)))
	  (:not
	   (directly-refers-to (second c)))
	  (T (error "~&BAD CONCEPT DEFINITION - ~S~%" C)))))

(defun refers-to (c1 c2 expand-primitives &optional (visited (gensym)))
  (cond
   ((or (null c1) (numberp c1))
    nil)
   ((atom c1)
    (unless (eq (get c1 'visited) visited)
	    (setf (get c1 'visited) visited)
	    (or (eql c1 c2)
		(and (g-concept c1) (or expand-primitives (not (g-primitive c1)))
		     (refers-to (g-definition c1) c2 expand-primitives visited)))))
   (T 
    (case (car c1)
	  ((:and :or)
	   (some #'(lambda (c) (refers-to c c2 expand-primitives visited)) (cdr c1)))
	  ((:some :all)
	   (refers-to (caddr c1) c2 expand-primitives visited))
	  (:not
	   (refers-to (cadr c1) c2 expand-primitives visited))
	  (T (error-handler 'refers-to :bad-concept-defn c1))))))

(defun grail-define-relation (r i-r p func i-func)
  (unless (verbosity :classify-2 "~&~A~%" r)
	  (verbosity :classify-1 (if func "A" "R")))
  (cond ((numberp r)
	 (error-handler 'grail-define-relation :bad-name r))
	((r-defined r)
	 (error-handler 'grail-define-relation :relation-redefinition r))
	(T
	 (setf *r-definitions* (nconc *r-definitions* (list r)))
	 (setf (gethash r *relations*) (make-role :grail-name r
						  :grail-inv-name i-r
						  :parents p
						  :functional func))
	 (unless (eql r i-r)
		 (setf (gethash i-r *relations*) (make-role :grail-name i-r
							    :grail-inv-name r
							    :functional i-func)))))
  (car (multiple-value-list (gethash r *relations*))))

(defun grail-redefine-relation (r &key (parents nil) (transitive-across nil))
  (verbosity :classify-1 "+")
  (cond	((r-defined r)
	 (let ((r-defn (gethash r *relations*)))
	   (when parents
		 (setf (role-parents r-defn)
		       (nunion parents (role-parents r-defn))))
	   (when transitive-across
		 (setf *transitive-roles* T)
		 (setf (role-transfered-by r-defn)
		       (nunion transitive-across (role-transfered-by r-defn))))))
	(T
	 (error-handler 'grail-define-relation :undefined-relation r)))
  (car (multiple-value-list (gethash r *relations*))))

(defun grail-define-implication (d i)
  (verbosity '(:classify-1 :classify-2) "I")
  (setf *implications* (nconc *implications* `((,d .,i))))
  d)

(defun grail-define-concept (n d)
  (cond ((numberp n)
	 (error-handler 'grail-define-concept :bad-name n))
	((g-concept n)
	 (error-handler 'grail-define-concept :concept-redefinition n))
;;; convert cyclical definitions into GCIs + primitive
	((refers-to d n (not *cyclical-definitions*))
	 (verbosity :notes
		    "~&!NOTE! cyclical concept ~S - definition converted to GCIs.~%" n)
	 (grail-define-implication n d)
	 (grail-define-implication d n)
	 (grail-define-primconcept n))
	(T
	 (unless (verbosity :classify-2 "~&~A~%" n)
		 (verbosity :classify-1 "C"))
	 (setf *c-definitions* (nconc *c-definitions* (list n)))
	 (setf (g-concept n) (make-grail-concept
			      :definition (flatten (neg-normal d))
			      :name n)))))

(defun grail-define-primconcept (n &optional (d nil))
  (cond ((numberp n)
	 (error-handler 'grail-define-primconcept :bad-name n))
	((g-concept n)
	 (error-handler 'grail-define-primconcept :concept-redefinition n))
	(T
	 (unless (verbosity :classify-2 "~&~A~%" n)
		 (verbosity :classify-1 "P"))
	 (when (refers-to d n (not *cyclical-definitions*))
	       (if *cyclical-definitions*
		   (verbosity :notes
			      "~&!NOTE! cyclical primitive concept ~S~%" n)
		 (progn
		   (verbosity :notes
			      "~&!NOTE! cyclical concept ~S - definition converted to GCI.~%" n)
		   (grail-define-implication n d)
		   (setf d nil))))
	 (setf *c-definitions* (nconc *c-definitions* (list n)))
	 (setf (g-concept n)
	       (make-grail-concept :definition (when d
						     (flatten
						      (neg-normal d)))
				   :primitive T
				   :name n)))))



;;; ************** ROLE & GCI PRE-PROCESSING **************

;;; IN-FaCT stuff

(defvar *double-blocking* nil)

(defun inv-r (r)
  (let ((pnr (symbol-name r)))
    (if (and (> (length pnr) 3) (equal (subseq pnr 0 3) "*I*"))
	(intern (subseq pnr 3))
      (intern (format nil "*I*~A" pnr)))))

(defun abs-r (r)
  (let ((pnr (symbol-name r)))
    (if (and (> (length pnr) 3) (equal (subseq pnr 0 3) "*I*"))
	(intern (subseq pnr 3))
      r)))

(defun r-check-installed (r)
  (if (not (r-defined (abs-r r)))
      (if *auto-install-primitives*
	  (defprimrole-f (abs-r r))
	(error "UNDEFINED ROLE - ~S" r))
    T))

(defun r-make-symetrical (r)
  (setf (r-parents r) (union (r-parents r) (mapcar #'r-inverse-f (r-parents (r-inverse r)))))
  (mapc #'r-check-installed (r-parents r))
  (setf (r-parents (r-inverse r)) (mapcar #'r-inverse-f (r-parents r))))

(defun r-get-ancestors (r &optional (a (list r)))
  (dolist (r1 (r-parents r) a)
    (if (not (member r1 a))
	(setf a (r-get-ancestors r1 (cons r1 a))))))

(defun r-get-equivalents (r)
  (mapcan #'(lambda (r1) (if (member r (r-get-ancestors r1)) (list r1)))
	  (r-get-ancestors r)))

(defun r-check-functional (r)
  (when (and (not (r-functional r)) (some #'r-functional-f (r-ancestors r)))
    (verbosity :warnings
	       "~&!!WARNING!! role ~S has attribute ancestor ~S - fixed.~%"
	       r (car (member-if #'r-functional-f (r-ancestors r))))
    (setf (role-functional r) T)))

(defun r-check-non-functional (r)
;;; For iFaCT, only leaves can be functional
  (if (string= *reasoner* "iFaCT")
      (let ((r-equivalents (r-get-equivalents r)))
	(dolist (r1 (r-ancestors r))
	  (when (and (r-functional r1) (not (member r1 r-equivalents)))
	    (verbosity :warnings
		       "~&!!WARNING!! role ~S has attribute ancestor ~S - fixed.~%" r r1)
	    (setf (r-functional r1) nil))))))

(defun r-check-transitive (r)
;;; For iFaCT, only leaves can be functional
  (when (and (r-functional r) (r-trans-across r))
    (verbosity :warnings
	       "~&!!WARNING!! attribute ~S is transitive - fixed.~%" r)
    (setf (r-trans-across r) nil)
    (setf (r-trans-across (r-inverse r)) nil)))

(defun process-role (r)
  (setf (r-ancestors r) (r-get-ancestors r))
  (setf (r-ancestors (r-inverse r)) (mapcar #'r-inverse-f (r-ancestors r)))
;;; force role to be functional if it has a functional ancestor
  (r-check-functional r)
  (r-check-functional (r-inverse r))
;;; force role to be non-functional if it isn't a leaf (iFaCT only)
  (r-check-non-functional r)
  (r-check-non-functional (r-inverse r))
;;; force functional roles to be non-transitive
  (r-check-transitive r)
  (r-check-transitive (r-inverse r))
;;; add r to (r-transfers r1) for every r1 in (r-trans-across r)
  (dolist (r1 (r-trans-across r))
    (pushnew r (r-transfers r1)))
  (dolist (r1 (r-trans-across (r-inverse r)))
    (pushnew r (r-transfers (r-inverse r1))))
;;; set f-ancestors for role
  (when (r-functional r)
    (setf (r-f-ancestors r)
      (delete-if-not #'r-functional-f (copy-list (r-ancestors r)))))
  (when (r-functional (r-inverse r))
    (setf (r-f-ancestors (r-inverse r))
      (delete-if-not #'r-functional-f (copy-list (r-ancestors (r-inverse r))))))
  (setf (r-processed r) T))

(defun classify-all-roles ()
  (verbosity '(:classify-1 :classify-2) "~&~%Pre-processing roles: ")
  (mapc #'R-MAKE-SYMETRICAL *r-definitions*)
  (mapc #'process-role *r-definitions*))

(defun reclassify-all-roles ()
  (dolist (r *r-definitions*)
	  (setf (r-processed r) nil))
  (classify-all-roles))

(defun get-index-p (d i)
  (let ((x-d (list (list :not d) i)) (p :top) c)
    (when
     *gci-absorption*
     (loop
      (setf c (car (member-if #'(lambda (x)
				  (and (listp x) (eq (first x) :not) (atom (second x))
				       (g-primitive (second x))))
			      x-d)))
      (when c
	    (setf p (second c))
	    (setf x-d (delete c x-d))
	    (return))
      (setf c nil)
      (setf x-d (mapcan #'(lambda (x)
			    (cond
			     (c (list x))
			     ((and (listp x) (eq (first x) :not) (atom (second x))
				   (not (g-primitive (second x))) (listp (g-definition (second x)))
				   (eq (car (g-definition (second x))) :and))
			      (setf c (cdr (neg-normal (list :not (g-definition (second x))))))
			      nil)
			     ((and (listp x) (eq (first x) :not) (atom (second x))
				   (not (g-primitive (second x))) (atom (g-definition (second x))))
			      (setf c (list (list :not (g-definition (second x)))))
			      nil)
			     ((and (atom x) (not (g-primitive x)) (listp (g-definition x))
				   (eq (car (g-definition x)) :or))
			      (setf c (cdr (g-definition x)))
			      nil)
			     ((and (atom x) (not (g-primitive x)) (atom (g-definition x)))
			      (setf c (list (g-definition x)))
			      nil)
			     ((and (listp x) (eq (first x) :not) (listp (second x))
				   (eq (first (second x)) :and))
			      (setf c (cdr (neg-normal (list :not (second x)))))
			      nil)
			     ((and (listp x) (eq (first x) :or))
			      (setf c (cdr x))
			      nil)
			     (T (list x))))
			x-d))
      (if c
	  (setf x-d (append c x-d))
	(return)))
     (when (eq p :top)
	   (verbosity
	    :notes
	    "~&!!NOTE!! GCI: ~S => ~S~%cannot be absorbed into primitive definition.~%" d i)))
    (if (cdr x-d)
	(values p (cons :or x-d))
      (values p (car x-d)))))

(defun reclassify-implication (d i)
  (let ((d-enc (encode-concept-term d)) (i-enc (encode-concept-term i)))
  (cond
;;; if i is an atomic primitive try to convert a primitive to a non-primitive.
   ((and (atom i) (g-primitive i) *gci-absorption* (g-definition i)
	 (eql (encode-concept-term (g-definition i)) d-enc)
	 (not (eql d :top)) (not (eql d :bottom))
	 (not (refers-to d i (not *cyclical-definitions*))))
    (verbosity '(:classify-1 :classify-2) "x")
    (setf (g-primitive i) nil)
;;; Changing definition of i so remove mapping to encoded concept
    (setf (system-name i) nil))
;;; if d is an atomic primitive add i to its definition.
   ((and (atom d) (g-primitive d) *gci-absorption*
	 (not (eql d :top)) (not (eql d :bottom))
	 (or *cyclical-definitions* (not (refers-to i d T))))
    (verbosity '(:classify-1 :classify-2) "p")
    (setf (g-definition d)
	  (flatten (neg-normal (if (g-definition d) `(:and ,(g-definition d) ,i) i))))
;;; Changing definition of d so remove mapping to encoded concept
    (setf (system-name d) nil))
   (T
    (multiple-value-bind
     (p c) (get-index-p d i)
     (if (or (eq p :top) (and (not *cyclical-definitions*) (refers-to c p T)))
	 (progn
	   (verbosity '(:classify-1 :classify-2) "i")
	   (when (not (eq p :top))
		 (verbosity
		  :notes
		  "~&!NOTE! cyclical implication ~S -> ~S not absorbed into primitive definition~%"
		  d i))
	   (setf *grail-universal-constraints*
		 (if *grail-universal-constraints*
		     `(:and ,*grail-universal-constraints* ,c)
		   c)))
       (progn
	 (verbosity '(:classify-1 :classify-2) "p")
;;; Changing definition of p so remove mapping to encoded concept
	 (setf (system-name p) nil)
	 (setf (g-definition p)
	       (flatten (neg-normal (if (g-definition p)
					`(:and ,(g-definition p) ,c)
				      c)))))))))))

(defun reclassify-all-implications ()
  (when *implications*
	(verbosity '(:classify-1 :classify-2) "~&~%Pre-processing General Inclusion Axioms: ")
;;; Need to re-do all encoding when definitions change
	(reset-kb)
	(let (rhp)
	  (dolist (i *implications*)
	    (if (and (atom (rest i)) (not (atom (first i))))
		(pushnew (rest i) rhp)
	      (reclassify-implication (first i) (rest i))))
	  (dolist (c rhp)
	    (reclassify-implication
	     (flatten (neg-normal (cons :or (mapcan #'(lambda (i)
							(if (eql c (rest i)) (list (first i))))
						    *implications*)))) c)))
	(setf *grail-universal-constraints*
	      (flatten (neg-normal *grail-universal-constraints*)))
	(setf *implications* nil)
;;; Need to re-do all encoding when definitions change
	(reset-kb)))



;;; ************** CLASSIFICATION **************

(defun c-insert-node (n)
  (let ((p (c-parents n))
	(c (c-children n)))
    (dolist (x p)
	    (setf (c-children x )
		  (cons n (set-difference (c-children x) c))))
    (dolist (x c)
	    (setf (c-parents x)
		  (cons n (set-difference (c-parents x) p)))))
  n)

(defun mark-all-ancestors (c v)
  (setf c (c-synonym c))
  (unless (eq (c-mark1 c) v)
	  (setf (c-mark1 c) v)
	  (dolist (p (c-parents c))
		  (mark-all-ancestors p v))))

(defun mark-all-descendants (c v)
  (setf c (c-synonym c))
  (unless (eq (c-mark1 c) v)
	  (setf (c-mark1 c) v)
	  (dolist (p (c-children c))
		  (mark-all-descendants p v)))
  v)

(defun simple-top-subs (y c subsumer non-subsumer)
  "Returns T if y subsumes c"
  (cond
   ((eq (c-mark1 y) non-subsumer) nil)
   ((eq (c-mark1 y) subsumer))
   ((every #'(lambda (p) (simple-top-subs p c subsumer non-subsumer)) (c-parents y))
    (cond
     ((test-subsumes y c)
      (setf (c-mark1 y) subsumer))
     (T
      (setf (c-mark1 y) non-subsumer)
      nil)))))

(defun direct-parents (root concept &optional (subsumer (gensym))
			    (non-subsumer (gensym)) (visited (gensym)))
  (setf (c-visited root) visited)
  (let ((s-c (remove-if-not #'(lambda (c) (simple-top-subs c concept subsumer non-subsumer))
			    (c-children root))))
    (cond
     ((null s-c)
      (list root))
     (T
      (mapcan #'(lambda (c) (unless (eq (c-visited c) visited)
				    (direct-parents c concept subsumer non-subsumer visited)))
	      s-c)))))

(defun mps-recurse (c possible-subsumee &optional (visited (gensym)) (new-p-s (gensym)))
  (cond
   ((or (eq (c-mark1 c) visited)
	(eq (c-mark1 c) new-p-s)))
;;; just return - we've been here before
   ((eq (c-mark1 c) possible-subsumee)
;;; a node which is a descendant of all so far
;;; ...mark it and all its descendants with the new possible-subsumee marker
    (mark-all-descendants c new-p-s))
   (T
;;; any other node not yet visited on this traversal
    (setf (c-mark1 c) visited)
;;; recursively search list of children
    (dolist (c-c (c-children c))
	    (mps-recurse c-c possible-subsumee visited new-p-s))))
  new-p-s)

(defun mark-possible-subsumees (c-list)
  (let ((possible-subsumee (gensym)))
;;; ...mark all descendants of 1st concept in list as possible subsumees
    (dolist (c (c-children (car c-list)))
	    (mark-all-descendants c possible-subsumee))
    (dolist (c (cdr c-list) possible-subsumee)
	    (setf possible-subsumee (mps-recurse c possible-subsumee)))))

(defun unmark-possible-subsumees (c possible-subsumee)
;;; when c is marked as a possible subsumee
  (when (eq (c-mark1 c) possible-subsumee)
;;; ...clear marker to nil
	(setf (c-mark1 c) nil)
;;; ...recursively unmark all parents
	(dolist (p (c-parents c))
		(unmark-possible-subsumees p possible-subsumee))))

(defun direct-children (p-list concept)
  (let ((search-q (enqueue *BOTTOM* (make-queue))) child-list
	(possible-subsumee (mark-possible-subsumees p-list))
	(marked-subsumee (gensym)) (visited-subsumee (gensym)))
;;; start searching through q
    (loop
;;; terminate when q is empty and return list of children
     (when (empty-queue-p search-q)
	   (return child-list))
;;; examine concept on top of q
     (let* ((c (pop-queue search-q)) s-c)
;;; unless it has parents which are subsumees ...
       (unless
;;; loop through list of parents
	(dolist (p (c-parents c) s-c)
		(cond
;;; if its a marked subsumee
		 ((eq (c-mark1 p) marked-subsumee)
;;; ...unless its already been visited
		  (unless (eq (c-visited p) visited-subsumee)
;;;    ...mark it as visited
			  (setf (c-visited p) visited-subsumee)
;;;    ...add it to the back of the search queue
			  (enqueue p search-q))
;;; ...set s-c flag to show subsumee parents were found
		  (setf s-c T))
;;; if its marked as a possible subsumee
		 ((eq (c-mark1 p) possible-subsumee)
;;; need to do a subsumption test
		  (cond
;;; if it is a subsumee...
		   ((test-subsumes concept p)
;;; ...mark all descendants as subsumees
		    (mark-all-descendants p marked-subsumee)
;;; ...mark it as a visited subsumee
		    (setf (c-visited p) visited-subsumee)
;;; ...add it to the back of the search queue
		    (enqueue p search-q)
;;; ...set s-c flag to show subsumee parents were found
		    (setf s-c T))
;;; if its not a subsumee...
		   (T
;;; ...unmark all ancestors as possible subsumees
		    (unmark-possible-subsumees p possible-subsumee))))
;;; if its outside the range of possible subsumees do nothing
		 (T)))
;;; ...(unless it has parents which are subsumees)
;;; push it onto the list of children
	(push c child-list))))))

(defun classify-con (c &key (install T))
  (let ((subsumer (gensym)) (start-time (get-internal-run-time))
	(start-sat *total-sat-tests*) (start-sub *total-subs-tests*))
    (when (> *profiling* 0)
	  (if *profile-file*
	      (format *profile-file* "(~S (" (c-grail-name c))
	    (format T "~&Classify - ~S" (c-grail-name c))))
    (map nil #'(lambda (c2) (mark-all-ancestors c2 subsumer))
	 (c-asserted-supers c))
    (setf (c-parents c) (direct-parents *TOP* c subsumer))
    (cond
;;; check if c is coherent
     ((eql (car (c-parents c)) *BOTTOM*)
      (verbosity :warnings "~&!!WARNING!! ~A is INCOHERENT~%" (decode-concept c))
      (setf (system-name (c-grail-name c)) *BOTTOM*)
      (setf (c-synonym c) *BOTTOM*)
      (setf (c-primitive c) nil)
      (setf (c-definition c) *BOTTOM*)
      (setf (c-asserted-supers c) (list *BOTTOM*))
      (setf (c-parents c) nil)
;;; If there is a  *universal-constraint* we have to be careful not to clear its
;;; definition when we discover that (not *universal-constraint*) = *BOTTOM*
      (unless (eql (neg-con c) *universal-constraint*)
	      (setf (c-synonym (neg-con c)) *TOP*)
	      (setf (c-primitive (neg-con c)) nil)
	      (setf (c-definition (neg-con c)) *TOP*)
	      (setf (c-asserted-supers (neg-con c)) nil)
	      (setf (c-parents (neg-con c)) nil)))
;;; check if c is a synonym
     ((and (= (length (c-parents c)) 1) (test-subsumes c (car (c-parents c))))
      (let ((s-c (car (c-parents c))))
	(unless (verbosity :synonyms "~&!!NOTE!! ~A is a SYNONYM for ~A~%"
			   (c-grail-name c) (c-grail-name s-c))
		(unless (verbosity :classify-2 "~&~A-S~%" (c-grail-name c))
			(verbosity :classify-1 "S")))
	(setf (system-name (c-grail-name c)) s-c)
;	(setf (c-synonym c) s-c)
;;;	(setf (c-definition c) s-c)
;	(setf (c-asserted-supers c) (list s-c))
;	(setf (c-parents c) nil)
;	(setf (c-synonym (neg-con c)) (neg-con s-c))
;;;	(setf (c-definition (neg-con c)) (neg-con s-c))
;	(setf (c-asserted-supers (neg-con c)) (list (neg-con s-c)))
;	(setf (c-parents (neg-con c)) nil)
	))
     (T
      (unless (verbosity :classify-2 (if (c-primitive c) "~&~A-P~%" "~&~A-C~%")
			 (c-grail-name c))
	      (verbosity :classify-1 (if (c-primitive c) "P" "C")))
      (setf (c-children c) (direct-children (c-parents c) c))
      (when install (c-insert-node c))))
    (when (> *profiling* 0)
	  (if *profile-file*
	      (format *profile-file* ")~%~,3F ~D ~D)~%"
		      (/ (- (get-internal-run-time) start-time) internal-time-units-per-second)
		      (- *total-subs-tests* start-sub) (- *total-sat-tests* start-sat))
	    (format T "~&~10F ~6D ~6D"
		    (/ (- (get-internal-run-time) start-time) internal-time-units-per-second)
		    (- *total-subs-tests* start-sub) (- *total-sat-tests* start-sat))))))

(defun ordered-classify-concept (c &optional subs)
  (when (member c subs)
	(return-from ordered-classify-concept))
  (unless (eql (c-classified c) 2)
	  (dolist (c1 (c-asserted-supers c))
		  (ordered-classify-concept c1 (cons c subs)))
	  (when (eql (c-classified c) 1)
		(classify-con c)
		(setf (c-classified c) 2))))

(defun auto-configure ()
  (when *auto-configure*
	(when (and *transitivity* (not *transitive-roles*))
	      (setf *transitivity* nil)
	      (verbosity :notes "~&!NOTE! transitivity switched OFF (no transitive roles)~%"))
;	(when (and *concept-eqn* (not *universal-constraint*))
;	      (setf *concept-eqn* nil)
;	      (verbosity :notes "~&!NOTE! GCI constraints switched OFF (no GCIs)~%"))
;	(when (and *blocking* (not *transitivity*) (not *concept-eqn*))
;	      (setf *blocking* nil)
;	      (verbosity :notes
;			 "~&!NOTE! blocking switched OFF (no transitive roles or GCIs)~%"))
))

(defun classify-all-concepts ()
  (when (> *profiling* 0) (profile-open))
  (progv '(*transitivity* *concept-eqn* *blocking*)
	 `(,*transitivity* ,*concept-eqn* ,*blocking*)
	 (setf *search-space* 0)
	 (setf *total-sat-tests* 0)
	 (setf *total-subs-tests* 0)
	 (setf *cache-accesses* 0)
	 (setf *cache-hits* 0)
	 (setf *caching-sat-tests* 0)
	 (auto-configure)
	 (if (> *profiling* 0) (features *profile-file*)
	   (when (verbosity :features "") (features)))
	 (verbosity :reclassifying "~&~%CLASSIFYING KNOWLEDGE BASE:~%")
	 (let ((start-time (get-internal-run-time)) r-t i-t e-t1 e-t2)
	   (classify-all-roles)
	   (setf r-t (get-internal-run-time))
;	   (encode-all-concepts)
	   (setf e-t1 (get-internal-run-time))
	   (reclassify-all-implications)
	   (setf i-t (get-internal-run-time))
	   (encode-all-concepts)
	   (setf e-t2 (get-internal-run-time))
	   (when (> *profiling* 0)
		 (format (if *profile-file* *profile-file* T)
			 "~&; process roles = ~F;  process GCIs = ~F;  encode concepts = ~F~%"
			 (/ (- r-t start-time) internal-time-units-per-second)
			 (/ (- i-t e-t1) internal-time-units-per-second)
			 (/ (+ (- e-t1 r-t) (- e-t2 i-t)) internal-time-units-per-second))))
	 (verbosity '(:classify-1 :classify-2) "~&~%Classifying concepts: ")
	 (dolist (c *c-definitions*)
		 (ordered-classify-concept (system-name c))))
  (when (> *profiling* 0) (profile-close))
  (verbosity :rc-counts "~&~%~D roles.~%~D concepts.~%"
	     (length *r-definitions*) (length *c-definitions*))
  (verbosity :test-counts "~&~%~D subsumption tests.~%~D satisfiability tests.~%"
	     *total-subs-tests* *total-sat-tests*)
  (verbosity
   :cache-counts
   "~&~%~D cache hits (~,1F%).~%~D cache misses (~,1F%).~%~D caching satisfiability tests~%"
   *cache-hits*
   (if (zerop *cache-accesses*) 0 (/ (* 100 *cache-hits*) *cache-accesses*))
   (- *cache-accesses* *cache-hits*)
   (if (zerop *cache-accesses*) 0
     (/ (* 100 (- *cache-accesses* *cache-hits*)) *cache-accesses*))
   *caching-sat-tests*)
;;;  (values)
;;; return T if KB is consistent, NIL otherwise
  (if (test-sat *TOP*) T nil))

(defun reclassify-all-concepts ()
  (reset-kb)
  (classify-all-concepts))



;;; ************** KB MANAGEMENT ************** 

(defun reset-kb ()
  "Re-initialise hierarchy, encoding and caching"
  (setf *next-available-concept* 2)
  (setf (s-concept *TOP*) (make-system-concept :children `(,*BOTTOM*) :classified 2
					       :synonym *TOP* :grail-name :top))
  (setf (s-concept *BOTTOM*) (make-system-concept :parents `(,*TOP*) :classified 2
						  :synonym *BOTTOM* :grail-name :bottom))
  (setf (g-concept :TOP) (make-grail-concept :name '*TOP* :definition '*TOP* :primitive T))
  (setf (g-concept :BOTTOM) (make-grail-concept :name '*BOTTOM* :definition '*BOTTOM* :primitive T))
  (clrhash *concept-definition-table*)
;;; Map GRAIL built in names for top and bottom concepts
  (setf (system-name :top) *TOP*)
  (setf (system-name :bottom) *BOTTOM*)
  (setf *tsd* 0))

(defun clear-kb ()
  "Initialise KB to contain only *TOP* and *BOTTOM*"
  (setf *kb-file-names* nil)
  (clrhash *grail-concept-table*)
  (setf *c-definitions* nil)
  (setf *next-available-concept* 2)
  (setf (s-concept *TOP*) (make-system-concept :children `(,*BOTTOM*) :classified 2
					       :synonym *TOP* :grail-name :top))
  (setf (s-concept *BOTTOM*) (make-system-concept :parents `(,*TOP*) :classified 2
						  :synonym *BOTTOM* :grail-name :bottom))
  (setf (g-concept :TOP) (make-grail-concept :name '*TOP* :definition '*TOP* :primitive T))
  (setf (g-concept :BOTTOM) (make-grail-concept :name '*BOTTOM* :definition '*BOTTOM* :primitive T))
  (clrhash *concept-definition-table*)
;;; Map GRAIL built in names for top and bottom concepts
  (setf (system-name :top) *TOP*)
  (setf (system-name :bottom) *BOTTOM*)
  (clrhash *relations*)
  (setf *r-definitions* nil)
  (setf *transitive-roles* nil)
  (setf *universal-constraint* nil)
  (setf *grail-universal-constraints* nil)
  (setf *implications* nil)
  (values))

(defun list-kb ()
  (format t "~&CONCEPTS: ~S~%RELATIONS: ~S~%" *c-definitions* *r-definitions*)
  (values))

(defun load-kb (&optional (i-fname "galen-core.lisp"))
  (clear-kb)
  (push (namestring (probe-file i-fname)) *kb-file-names*)
  (load i-fname))

(defun grail-name-list (l)
  (mapcar #'(lambda (c) (if (c-grail-name c) (c-grail-name c)
			  (error "~&!!ERROR!! No grail name - ~S~%" c))) l))

(defun dump-taxonomy (&optional (o-fname "taxonomy.dump") &key (features t))
  (let ((o-f (open o-fname :direction :output :if-exists :supersede)))
    (when features (features o-f))
    (print (list :top (grail-name-list (c-parents *TOP*))
		 (grail-name-list (c-children *TOP*))) o-f)
    (dolist (d *c-definitions*)
	    (let ((d-s (system-name d)))
	      (when (or (c-parents d-s) (c-children d-s))
		  (print (list d (grail-name-list (c-parents d-s))
			       (grail-name-list (c-children d-s))) o-f))))
    (print (list :bottom (grail-name-list (c-parents *BOTTOM*))
		 (grail-name-list (c-children *BOTTOM*))) o-f)
    (terpri o-f)
    (close o-f)))

(defun dump-roles (&optional (o-fname "roles.dump") &key (features nil))
  (let ((o-f (open o-fname :direction :output :if-exists :supersede)))
    (when features (features o-f))
    (dolist (r *r-definitions*)
	    (print (list r (r-parents r) (r-ancestors r) (r-functional r) (r-trans-across r)
			 (r-transfers r) (r-inverse r)) o-f)
	    (let ((i-r (r-inverse r)))
	      (print (list i-r (r-parents i-r) (r-ancestors i-r) (r-functional i-r)
			   (r-trans-across i-r) (r-transfers i-r) (r-inverse i-r)) o-f)))
    (close o-f)))

(defparameter *synonyms* nil)
(defparameter *scramble* nil)
(defparameter *scnum* 0)
(defparameter *cname-hash* (make-hash-table))
(defparameter *srnum* 0)
(defparameter *rname-hash* (make-hash-table))
(defparameter *w-hash* (make-hash-table))

(defun scramble-con (c)
  (unless *synonyms* (setf c (if (system-name c) (c-grail-name (system-name c)) c)))
  (if *scramble*
      (or (gethash c *cname-hash*)
	  (setf (gethash c *cname-hash*) (intern (format nil "C~S" (incf *scnum*)))))
    c))

(defun scramble-role (r)
  (if *scramble*
      (or (gethash r *rname-hash*)
	  (setf (gethash r *rname-hash*) (intern (format nil "R~S" (incf *srnum*)))))
    r))

(defun transitive-disjunction (s-a r c &optional expanded)
  (cond
   ((member r expanded) nil)
   (T
    (let ((x-r
	    (delete nil (mapcar #'(lambda (s)
				    (let ((t-d (transitive-disjunction s-a s c
								       (cons r expanded))))
				      (when t-d (list s-a (scramble-role r) t-d))))
				(r-trans-across r)))))
      (if x-r (list* (if (eq s-a 'some) 'or 'and) (list s-a (scramble-role r) c) x-r)
	(list s-a (scramble-role r) c))))))

(defun kris-concept (c &optional (refinement nil))
  (cond
   ((listp c)
    (case (car c)
	  ((:and and) (cons 'and (mapcar #'(lambda (x) (kris-concept x refinement)) (cdr c))))
	  ((:or or) (cons 'or (mapcar #'(lambda (x) (kris-concept x refinement)) (cdr c))))
	  ((:some some)
	   (if refinement
	       (transitive-disjunction 'some (second c) (kris-concept (third c) refinement))
	     (list 'some (scramble-role (second c)) (kris-concept (third c) refinement))))
	  ((:all all)
	   (if refinement
	       (transitive-disjunction 'all (second c) (kris-concept (third c) refinement))
	     (list 'all (scramble-role (second c)) (kris-concept (third c) refinement))))
	  ((:not not) (list 'not (kris-concept (second c) refinement)))
	  (t (error "UN-TRANSLATABLE CONCEPT - ~S" c))))
   (T
    (case c
	  (:top '*TOP*)
	  (:bottom '*BOTTOM*)
	  (t (scramble-con c))))))

(defun export-kris-role (r o-f alc trans)
  (format o-f "(defprim~A ~S"
	  (if (and (r-functional r) (not alc)) "attribute" "role") (scramble-role r))
  (if trans
      (if (member r (r-trans-across r))
	  (format o-f " :parents ~S :transitive T)~%" (mapcar #'scramble-role (r-parents r)))
	(format o-f " :parents ~S)~%" (mapcar #'scramble-role (r-parents r))))
    (format o-f ")~%")))

(defun ordered-export-concept (c o-f r w &optional subs numbers)
  (if (atom c)
      (progn
	(when (numberp c)
	  (unless (or (not numbers) (eq (gethash c *w-hash*) w))
	    (setf (gethash c *w-hash*) w)
	    (format o-f "(defprimconcept ~S)~%"
		    (scramble-con c)))
	      (return-from ordered-export-concept))
	(when (member c subs)
	      (verbosity :warnings "~&!!WARNING!! cyclical definition: ~S~%" c)
	      (return-from ordered-export-concept))
	(when (and (not (eq (c-grail-name (system-name c)) c)) (not *synonyms*))
	      (format T "~&!!NOTE!! ~S is a synonym for ~S - Discarded.~%"
		      c (c-grail-name (system-name c)))
	      (ordered-export-concept (c-grail-name (system-name c)) o-f r w subs numbers)
	      (return-from ordered-export-concept))
	(unless (eq (gethash c *w-hash*) w)
		(let ((d (g-definition c)))
		  (when d
			(push c subs)
			(if (atom d)
			    (ordered-export-concept d o-f r w subs numbers)
			  (case (car d)
			    ((:and :or)
			     (dolist (cr (cdr d))
			       (ordered-export-concept cr o-f r w subs numbers)))
			    ((:some :all)
			     (ordered-export-concept (third d) o-f r w subs numbers))
			    (:not (ordered-export-concept (second d) o-f r w subs numbers))
			    (T (error "BAD CONCEPT - ~S" d))))))
		(setf (gethash c *w-hash*) w)
		(format o-f "(def~Aconcept ~S"
			(if (g-primitive c) "prim" "") (scramble-con c))
		(when (g-definition c)
		      (format o-f " ~S" (kris-concept (g-definition c) r)))
		(format o-f ")~%")))
    (case (car c)
	  ((:and :or) (dolist (cr (cdr c))
			      (ordered-export-concept cr o-f r w subs numbers)))
	  ((:some :all) (ordered-export-concept (third c) o-f r w subs numbers))
	  (:not (ordered-export-concept (second c) o-f r w subs numbers))
	  (T (error "BAD CONCEPT - ~S" c)))))

(defun export-implication (a c o-f r)
  (format o-f "(implies ~S ~S)~%"
	  (kris-concept (flatten a) r)
	  (kris-concept (flatten c) r)))

(defun export-kris-kb (fname &key alc cycles transitivity refinement synonyms gci scramble numbers)
  (let ((o-f (open (if fname fname "kris.tbox") :direction :output :if-exists :supersede))
	(save-cd *cyclical-definitions*)
	(save-ps *print-circle*)
	(written (gensym)))
    (setf *print-circle* nil)
    (setf *cyclical-definitions* cycles)
    (setf *scramble* scramble)
    (setf *synonyms* synonyms)
    (clrhash *w-hash*)
    (when scramble
	  (setf *scnum* 0)
	  (clrhash *cname-hash*)
	  (setf *srnum* 0)
	  (clrhash *rname-hash*))
    (reclassify-all-roles)
    (unless gci
	    (reclassify-all-implications))
    (encode-all-concepts)
    (dolist (r *r-definitions*)
	    (export-kris-role r o-f alc transitivity)
	    (unless (eq r (r-inverse r))
		    (export-kris-role (r-inverse r) o-f alc transitivity)))
    (dolist (c *c-definitions*)
	    (ordered-export-concept c o-f refinement written nil numbers))
    (when gci
	  (dolist (i *implications*)
		  (export-implication (car i) (cdr i) o-f refinement)))
    (setf *cyclical-definitions* save-cd)
    (setf *print-circle* save-ps)
    (close o-f)))

(defun loom-concept (c &key alc)
  (cond
   ((listp c)
    (case (car c)
	  ((:and and) (cons :and (mapcar #'(lambda (c) (loom-concept c :alc alc)) (cdr c))))
	  ((:or or) (cons :or (mapcar #'(lambda (c) (loom-concept c :alc alc)) (cdr c))))
	  ((:some some) 
	   (if (and (r-functional (second c)) (not alc))
	       (list :the (second c) (loom-concept (third c) :alc alc))
	   (list :some (second c) (loom-concept (third c) :alc alc))))
	  ((:all all) (list :all (second c) (loom-concept (third c) :alc alc)))
	  ((:not not) (list :not (loom-concept (second c) :alc alc)))
	  (t (error "UN-TRANSLATABLE CONCEPT - ~S" c))))
   (T
    (case c
	  (:top 'Thing)
	  (:bottom '*BOTTOM*)
	  (t c)))))

(defun export-loom-kb (fname &key alc)
  (let ((o-f (open (if fname fname "loom.tbox") :direction :output :if-exists :supersede))
	(cycles *cyclical-definitions*))
    (setf *cyclical-definitions* nil)
    (reclassify-all-roles)
    (reclassify-all-implications)
    (encode-all-concepts)
    (dolist (r *r-definitions*)
	    (let ((i-r (r-inverse r)))
	      (format o-f "(defrelation ~S)~%" r)
	      (unless (eq r i-r)
	      (format o-f "(defrelation ~S)~%" i-r))))
    (dolist (c *c-definitions*)
	    (if (not (eq (c-grail-name (system-name c)) c))
		(format T "~&!!NOTE!! ~S is a synonym for ~S - Discarded.~%"
			c (c-grail-name (system-name c)))
	      (format o-f "(defconcept ~S ~A)~%" c
		      (if (g-primitive c)
			  (if (g-definition c)
			      (format nil ":is-primitive ~S"
				      (loom-concept (g-definition c) :alc alc))
			    "")
			(format nil ":is ~S"
				(loom-concept (g-definition c) :alc alc))))))
    (setf *cyclical-definitions* cycles)
    (close o-f)))

(defmacro export-kb (fname &key (system 'kris) (alc nil))
  `(case ',system
     (kris (export-kris-kb ,fname :alc ,alc))
     (fact (export-kris-kb ,fname :cycles  T :transitivity T :synonyms T :gci T))
     (loom (export-loom-kb ,fname :alc ,alc))
     (T (format t "~&!!ERROR!! Unknown system \"~S\".~%" ,system))))



;;; ************** SUBSUMPTION TESTING **************
    
(defun add-all-r+c (c-s r c ddb-label)
  "Propogates transitive (ALL R C) constraints"
  (let ((r-c (list r c)))
    (cond
;;; if r-c already in all-r+c do nothing
     ((member r-c (constraints-all-r+c c-s) :test #'equal :key #'first))
     (T
;;; add new r+c constraint
      (push (list r-c ddb-label) (constraints-all-r+c c-s))))))

(defun expand-all-rc (c-s r c ddb-label trans p-list)
  "expands x:(:all R C) + xRy constraints"
  (and
;;; add regular value restriction y:C 
   (add-constraint c-s c ddb-label)
   (if *transitivity*
;;; if triggered by x:(:all+ R C) add y:(:all+ R C)
       (if trans (add-all-r+c c-s r c ddb-label)
;;; otherwise, add y:(:all+ S C) for each S s.t. S is transitive, R subsumes S,
;;; and S subsumes P, where P is the role connecting x and y
	 (every #'(lambda (s)
		    (if (and (r-trans-across s) (member r (r-ancestors s)))
			(add-all-r+c c-s s c ddb-label)
		      T))
		p-list))
     T)))

(defun add-constraint (c-s concept level)
  "Add a new constraint (concept level) to S; return nil if there is a clash."
  (dbg :addc-entry "~&   ADD-C ~S ~S" concept level)
  (cond
;;; if concept is an atom...
   ((atom concept)
    (add-literal c-s concept level))

;;; if concept = (:and c1 ... cn)...
   ((eq (car concept) :and)
    (every #'(lambda (c)
;;; add c constraints for every c in the :and list
	       (add-constraint c-s c level))
;;; constraint is the :and list of concepts
	   (cdr concept)))

;;; if concept = (:or c1 ... cn)...
   ((eq (car concept) :or)
;;;   add or clause
    (add-or-clause c-s (cdr concept) level))

;;; if concept = (:all r c)...
   ((eq (car concept) :all)
;;;   expand :all constraint
    (push (list (cdr concept) level) (constraints-all-rc c-s)))

;;; otherwise
   (T
;;;  its and ERROR - a bad concept expression
    (error-handler 'add-constraint :bad-concept-expansion))))

(defun add-xry (c-s r y-c-s level)
  "Add xry to x-r-y - check all-rc constraints and any partial expansions."
;;; build a new r-y constraint
  (let* ((r-a (r-ancestors r)))
    (and
;;; check if any (:all s c) constraints are triggered - s in ancestors of r.
     (every #'(lambda (s-c)
		(if (member (first (constraint s-c)) r-a)
		    (expand-all-rc y-c-s (first (constraint s-c)) (second (constraint s-c))
				   (union level (level s-c)) nil r-a)
		  t))
	    (constraints-all-rc c-s))
;;; check if any (:all s+ c) constraints are triggered - s in ancestors of r.
     (every #'(lambda (s-c)
		(if (member (first (constraint s-c)) r-a)
		    (expand-all-rc y-c-s (first (constraint s-c)) (second (constraint s-c))
				   (union level (level s-c)) t r-a)
		  t))
	    (constraints-all-r+c c-s)))))

(defun set-equal (s1 s2 &key (test #'eql))
  "returns T if the two sets s1 and s2 are equal"
  (and
   (= (length s1) (length s2))
   (every #'(lambda (e) (member (first e) s2 :test test :key #'first)) s1)))

(defun s-equivalent (x-c y-c)
  "True if x and y are s-equivalent"
  (if *subset-s-equivalent*
      (and (subsetp (constraints-c x-c) (constraints-c y-c))
	   (subsetp (constraints-all-r+c x-c) (constraints-all-r+c y-c)
		    :test #'equal :key #'first)
	   (setf *cycle* T))
    (and (endp (set-exclusive-or (constraints-c x-c) (constraints-c y-c)))
	 (set-equal (constraints-all-r+c x-c) (constraints-all-r+c y-c) :test #'equal)
	 (setf *cycle* T))))

(defun find-s-equivalent (c-s)
  "Finds and returns an s-equivalent variable to x if one exists"
;;; can switch off if blocking is not required - e.g. for alc
  (when *blocking*
	(let ((s-equiv
	       (do ((y-c-s (constraints-parent c-s) (constraints-parent y-c-s)))
		   ((or (null y-c-s) (s-equivalent c-s y-c-s)) y-c-s))))
	  (when s-equiv
		(dbg :sat-sequiv "~&     S-equivalent ~S = ~S" c-s s-equiv))
	  s-equiv)))

(defun not-fully-solved (c-s)
  (some #'(lambda (c) (plusp (car c))) (constraints-or-clauses c-s)))

(defun not-expanded (c-s)
  "returns T if the constraint system c-s is not expanded"
;;; return logical or of the unexpanded constraint lists
  (or 
   (constraints-some-fc-ux c-s)
   (constraints-some-rc-ux c-s)
   (constraints-c-def-ux c-s)
   (not-fully-solved c-s)))

(defun needs-expanding (c-s)
  (and (not-expanded c-s)
       (not (find-s-equivalent c-s))
       (not (mergable-constraints c-s))))

(defun sorted-successors (s-list)
  (if *sort-lists*
      (stable-sort (reverse s-list)
	    #'(lambda (x y)
		(< (if (cdr x) (reduce #'max (cdr x)) -1)
		   (if (cdr y) (reduce #'max (cdr y)) -1))))
    (reverse s-list)))

(defun expand-f-roles (c-s)
  "Expand (:some f C) constraints on c-s; return nil if there was a clash"
;;; only adds one node at a time to model size
  (when (and (constraints-some-fc-ux c-s) (> (incf *model-size*) *max-model-size*))
	(setf *max-model-size* *model-size*))
;;; loop through fc-ux list until empty - reverse it so as to process oldest first
  (do ((f-list (sorted-successors (constraints-some-fc-ux c-s)) (cdr f-list)))
;;; return c-s if we got through the whole list without a clash
      ((endp f-list) c-s)
;;; when list entry is non-nil - might be nil if absorbed into previous f
      (when (car f-list)
;;; initialise dependency set, set of f-ancestors and new constraints set
;;; (car f-list) is: ((R C) . d-set)
	    (let ((d-set (cdar f-list))
		  (f-set (r-f-ancestors (caaar f-list)))
		  (y-cs (make-constraints :parent c-s)))
;;; unless constraining concept is added ok, return nil - clash
	      (unless (and (add-constraint y-cs (cadaar f-list) d-set)
;;; add the universal constraint due to GCIs
			   (if *universal-constraint*
			       (add-constraint y-cs *universal-constraint* nil)
			     T)
			   (add-xry c-s (caaar f-list) y-cs d-set))
		      (return-from expand-f-roles nil))
;;; look for other f-roles which need to be absorbed into this one
	      (do ((not-changed nil))
;;; exit when no more found
		  (not-changed)
;;; set not-changed to true - it is set back to nil if an f-role is absorbed in next loop
		  (setf not-changed T)
;;; loop through the rest of the f-list
		  (do ((f-l (cdr f-list) (cdr f-l)))
		      ((endp f-l))
;;; when list entry is non-nil - might be nil if absorbed into previous f
		      (when (car f-l)
;;; when the f has some f-ancestor which is in f-set
			    (when (intersection (r-f-ancestors (caaar f-l)) f-set)
;;; diff := ancestors of f not already in f-set
				  (let ((diff (set-difference (r-f-ancestors (caaar f-l)) f-set)))
;;; if there were any new f-ancestors
				    (when diff
;;; not-changed:=nil - need to check list again
					  (setf not-changed nil)
;;; add new f-ancestors onto f-set
					  (setf f-set (nconc diff f-set)))
;;; dependency set is union of all d-sets
				    (setf d-set (union d-set (cdar f-l)))
;;; unless constraining concept is added ok, return nil - clash
				    (unless (and (add-constraint y-cs (cadaar f-l) d-set)
						 (add-xry c-s (caaar f-l) y-cs d-set))
					    (return-from expand-f-roles nil))
;;; set list entry to nil to show this f-role has been dealt with
				    (rplaca f-l nil))))))
;;; when y-cs needs expanding
	      (when (needs-expanding y-cs)
;;; unless resulting constraint system is satisfiable return nil
		    (unless (sat y-cs)
			    (return-from expand-f-roles nil)))))))

(defun expand-r-roles (c-s)
  "Expand (:some R C) constraints on c-s; return nil if there was a clash"
;;; list of already expanded constraint systems to check for duplicate expansions
  (let (r-c-list)
;;; loop through rc-ux list until empty - reverse it so as to process oldest first
    (dolist (r-c (sorted-successors (constraints-some-rc-ux c-s)) c-s)
	    (let ((r (caar r-c))
		  (c (cadar r-c)))
	      (when (notany #'(lambda (r-cs)
				(and (member r (r-ancestors (car r-cs)))
				     (member c (constraints-c (cdr r-cs)))))
			    r-c-list)
		    (let ((y-cs (make-constraints :parent c-s))
			  (d-set (cdr r-c)))
;;; increment model-size and set max size
		      (when (> (incf *model-size*) *max-model-size*)
			    (setf *max-model-size* *model-size*))
;;; unless constraining concept is added ok, return nil - clash
		      (unless (and (add-constraint y-cs c d-set)
;;; add the universal constraint due to GCIs
				   (if *universal-constraint*
				       (add-constraint y-cs *universal-constraint* nil)
				     T)
				   (add-xry c-s r y-cs d-set))
			      (return-from expand-r-roles nil))
;;; when y-cs needs expanding
		      (when (needs-expanding y-cs)
;;; unless its expanded ok return nil
			    (unless (sat y-cs)
				    (return-from expand-r-roles nil)))
;;; add r-cs to list
		      (push (cons r y-cs) r-c-list)))))))

(defun sat (c-s)
  "Returns T if constraint system is satisfiable, nil otherwise"
  (unless (expand-defined-concepts c-s)
	  (return-from sat nil))
  (if (not-fully-solved c-s)
      (add-ux-or-constraints c-s)
;;; preserve model depth and size
    (progv
     '(*model-depth* *model-size*) `(,*model-depth* ,*model-size*)
     (when (and (or (constraints-some-rc-ux c-s) (constraints-some-fc-ux c-s))
		(> (incf *model-depth*) *max-model-depth*))
	   (setf *max-model-depth* *model-depth*))
     (and (expand-f-roles c-s)
	  (expand-r-roles c-s)))))
		
(defun full-sat (c-s)
  (and
;;; add the universal constraint due to GCIs
   (if *universal-constraint*
	 (add-constraint c-s *universal-constraint* nil)
     T)
   (sat c-s)))

(defun simple-sat (c-s)
  (progv '(*universal-constraint*) `(nil)
	 (sat c-s)))

(defun test-sat (exp)
  (dbg :test-sat "~&Test sat: ~S~%" (decode-concept exp))
  (incf *total-sat-tests*)
  (let ((c-s (make-constraints))
	sat-result recursive-run-time mmd mms
	(s-s *search-space*) (c-a *cache-accesses*) (c-h *cache-hits*))
    (setq *search-space* 0 *cache-accesses* 0 *cache-hits* 0)
;;; set *or-level* to 0 using progv so we can handle recursive calls to test-sat
    (progv '(*or-level* *model-size* *max-model-size*
			 *model-depth* *max-model-depth*
			 *start-time* *cycle*)
	   `(0 1 1 1 1 ,(get-internal-run-time) nil)
	   (setf recursive-run-time *start-time*)
	   (setf sat-result 
		 (and
		  (add-constraint c-s exp nil)
		  (if *concept-eqn* (full-sat c-s) (simple-sat c-s))))
	   (when (> *profiling* 2)
;;; time taken by this test EXCLUDING any recursive caching tests
		 (setf *run-time* (- (get-internal-run-time) *start-time*))
		 (profile-out exp (not (null sat-result))))
	   (setf mmd *max-model-depth*)
	   (setf mms *max-model-size*)
;;; time taken by this test INCLUDING any recursive caching tests
	   (setf recursive-run-time (+ *run-time* (- *start-time* recursive-run-time))))
    (setf *max-model-size* (max *max-model-size* (+ *model-size* mms)))
    (setf *max-model-depth* (max *max-model-depth* (+ *model-depth* mmd)))
    (incf *search-space* s-s)
    (incf *cache-accesses* c-a)
    (incf *cache-hits* c-h)
    (incf *start-time* recursive-run-time)
    sat-result))

(defun test-and-cache (c)
  "Check if c has a model and, if so, store relevant details in c-model"
  (dbg :test-and-cache "~&~ATest&cache ~S"
       (make-string *tsd* :initial-element #\Space) c)
;;; set model to *BOTTOM* (no model) to prevent infinite recursive calls
  (setf (c-model c) *BOTTOM*)
;;; NOTE *tsd* is a recursion depth counter for debugging
  (incf *tsd*)
;;; calls test-sat to return model of c
  (let ((c-s (test-sat c)))
    (decf *tsd*)
;;; if c is satisfiable
    (if c-s
;;;  keep a note of relevant parts of top node of model
	(setf (c-model c) (make-constraints
;;; keep a note of all x:C constraints
			   :c (constraints-c c-s)
;;; keep a note of all Rs in all R.C constraints
			   :all-rc (delete-duplicates (mapcar #'caar (constraints-all-rc c-s)))
;;; keep a note of all Rs in xRy constraints
			   :r-y (delete-duplicates (nconc (mapcar #'caar
								  (constraints-some-rc-ux c-s))
							  (mapcar #'caar
								  (constraints-some-fc-ux c-s))))
;;; keep a note of all fs in xfy constraints
			   :f-y (when (constraints-some-fc-ux c-s)
				      (delete-duplicates
				       (apply #'append (mapcar #'(lambda (a)
								   (r-f-ancestors (caar a)))
							       (constraints-some-fc-ux c-s)))))))
;;; otherwise if c isn't satisfiable
      (progn
	(dbg :test-and-cache-incoherent "~&~AINCOHERENT ~S"
	     (make-string *tsd* :initial-element #\Space) c)
	(verbosity :notes "~&!!NOTE!! ~A is INCOHERENT~%" (decode-concept c))
;;; set c to a synonym for :bottom and NOT c to a synonym for :top
	(when (c-grail-name c)
	      (setf (system-name (c-grail-name c)) *BOTTOM*))
	(setf (c-synonym c) *BOTTOM*)
	(setf (c-asserted-supers c) (list *BOTTOM*))
	(setf (c-definition c) *BOTTOM*)
	(setf (c-primitive c) nil)
	(when (c-grail-name (neg-con c))
	      (setf (system-name (c-grail-name (neg-con c))) *TOP*))
;;; If there is a  *universal-constraint* we have to be careful not to clear its
;;; definition when we discover that (not *universal-constraint*) = *BOTTOM*
	     (unless (eql (neg-con c) *universal-constraint*)
		     (setf (c-synonym (neg-con c)) *TOP*)
		     (setf (c-asserted-supers c) nil)
		     (setf (c-definition (neg-con c)) *TOP*)
		     (setf (c-primitive (neg-con c)) nil))
;;; return nil
	nil))))

(defun get-cached-model (c)
  (cond
;   ((eql c *TOP*) T)
   ((eql c *BOTTOM*) nil)
   (T
    (let ((m (c-model c)))
      (if m (unless (eql m *BOTTOM*) m)
	(test-and-cache c))))))

(defun notany-member (l1 l2)
  (notany #'(lambda (c)
	      (member c l2)) l1))

(defun notany-neg-member (l1 l2)
  (notany #'(lambda (c)
	      (member (neg-con c) l2)) l1))

(defun mergable (c1 c2)
  (let ((mc1 (get-cached-model c1)) (mc2 (get-cached-model c2)))
    (and
     mc1 mc2
     (notany-neg-member (constraints-c mc1) (constraints-c mc2))
     (every #'(lambda (r)
		(notany-member (r-ancestors r) (constraints-all-rc mc2)))
	    (constraints-r-y mc1))
     (every #'(lambda (r)
		(notany-member (r-ancestors r) (constraints-all-rc mc1)))
	    (constraints-r-y mc2))
     (notany-member (constraints-f-y mc1) (constraints-f-y mc2)))))

(defun no-possible-clash (c1 all-r+c c-list)
  (let ((mc1 (get-cached-model c1)))
    (when mc1
	  (let ((r-y-mc1 (constraints-r-y mc1))
		(all-rc-mc1 (constraints-all-rc mc1)))
	    (and
	     (every #'(lambda (r)
			(notany #'(lambda (s)
				    (member s all-r+c :key #'caar))
				(r-ancestors r)))
		    r-y-mc1)
	     (every #'(lambda (c2)
			(let* ((mc2 (get-cached-model c2)))
			  (and
			   mc2
			   (let ((r-y-mc2 (constraints-r-y mc2))
				 (all-rc-mc2 (constraints-all-rc mc2)))
			     (and
			      (notany-neg-member (constraints-c mc1) (constraints-c mc2))
			      (every #'(lambda (r)
					 (notany-member (r-ancestors r) all-rc-mc2))
				     r-y-mc1)
			      (every #'(lambda (r)
					 (notany-member (r-ancestors r) all-rc-mc1))
				     r-y-mc2)
			      (notany-member (constraints-f-y mc1) (constraints-f-y mc2)))))))
		    c-list))))))

(defun mergable-constraints (c-s)
  "Check if models of constraints in c-s can be merged"
  (when *full-caching*
	(let ((s-t *total-sat-tests*) (c-s-t *caching-sat-tests*)
	      (all-r+c (constraints-all-r+c c-s)))
	  (incf *cache-accesses*)
	  (dbg :cache-merging "~&Merge ~S? - "
	       (constraints-c c-s))
	  (cond
	   ((do ((c-list (constraints-c c-s) (cdr c-list))
		 (res T (no-possible-clash (car c-list) all-r+c (cdr c-list))))
		((or (endp c-list) (not res)) res))
	    (dbg :cache-merging "YES~%")
	    (setf *caching-sat-tests* (+ c-s-t (- *total-sat-tests* s-t)))
	    (incf *cache-hits*))
	   (T
	    (dbg :cache-merging "NO~%")
	    (setf *caching-sat-tests* (+ c-s-t (- *total-sat-tests* s-t)))
	    nil)))))

(defun obvious-non-subs (c1 c2)
;;; Spots obvious non-subsumptions via easily satisfiable cases of (C2 & not(C1)).
  (when (and *top-level-caching*
	     (or
;;; One such case is if C1 is :bottom and C2 has a model...
	      (and (eql c1 *BOTTOM*) (get-cached-model c2))
;;; another case is if C2 is :top and (:not C1) has a model (i.e. C1/=top)
	      (and (eql c2 *TOP*) (get-cached-model (neg-con c1)))
;;; another case is if c2 and (:not c1) both have models & are mergeable
	      (mergable c2 (neg-con c1))))
	(dbg :found-obvious-non-subs "~&OBVIOUS NON-SUBSUMPTION - not(~S > ~S)" c1 c2)
	T))

(defun obvious-subs (c1 c2)
;;; obvious subsumption if c1=top or c2=bottom
;;; or (:not c1)=incoherent (i.e. c1=top) or c2=incoherent (i.e. c2=bottom)
  (when *obvious-subs*
	(or (eql c1 *TOP*) (eql c2 *BOTTOM*) (not (get-cached-model (neg-con c1)))
	    (not (get-cached-model c2)))))

(defun test-subsumes (c1 c2)
  (incf *total-subs-tests*)
  (dbg :subs-entry-1 "~&     ~S > ~S ?" c1 c2)
  (dbg :subs-entry-2 ".")
  (when (> *profiling* 1)
	(if *profile-file*
	    (format *profile-file* "~%(~S ~S ("
		    (decode-concept c1) (decode-concept c2))
	  (format T "~&Subsumption test - ~S > ~S ?"
		  (decode-concept c1) (decode-concept c2))))
;;; c1 > c2 if c2 and not c1 is not satisfiable. If c1 is :bottom and c2 has a model -
;;; so c2 is known to be coherent - can just return nil
  (let ((subs-result
	 (if (obvious-subs c1 c2) T
	   (unless (obvious-non-subs c1 c2)
		   (setq *model-size* 1 *max-model-size* 1
			 *model-depth* 1 *max-model-depth* 1)
		   (not (test-sat `(:and ,c2 ,(neg-con c1))))))))
    (when (and (> *profiling* 1) *profile-file*)
	  (format *profile-file* "))"))
    subs-result))

(defun expand-defined-concepts (c-s)
  "Expand defined concepts in c-s"
;;; expand all defined concepts
;;; need outer loop because new ones might be added by inner loop
  (loop
;;; get local copy of unexpanded defined concepts lisp
   (let ((def-ux (constraints-c-def-ux c-s)))
;;; clear the list in c-s
     (setf (constraints-c-def-ux c-s) nil)
;;; return nil unless all the definitions are added OK
     (unless (every #'(lambda (constraint)
			(add-constraint c-s (c-definition (first constraint))
					(second constraint)))
		    def-ux)
	     (return nil)))
;;; exit loop with T if no more to be processed
   (when (endp (constraints-c-def-ux c-s)) (return T))))


;;; ************** POSIT code ************** 

(defun pos-lit (p)
  (logxor (logior p 1) 1))

(defun pos-lit-p (p)
  (zerop (logand p 1)))

(defun shift-left (i dist)
  (dpb i (byte 16 dist) 0))

(defun alpha (pos-p neg-p)
  (+ (shift-left (* pos-p neg-p) 16) pos-p neg-p))

(defun jw-weight (clause-len max-len)
  (shift-left 1 (- max-len clause-len)))

(defun dependencies (lit-list)
  "Return a dependency set from lit-list"
;;; d-set is the union of all the d-sets from propositions corresponding to literals in lit-list
  (let ((d-set (fifth (second (car lit-list)))))
    (dolist (lit (cdr lit-list) d-set)
	    (setf d-set (union d-set (fifth (second lit)))))))

(defun add-or-clause (c-s clause d-set)
  "Add disjunctive clause to constraint system"
;;; initialise open-literal-count, list of literals in clause and list of new
;;; proposition entries
  (let ((o-l-c 0) lit-list new-props)
;;; loop through literals in clause
    (dolist (lit clause)
;;; set p=proposition (positive literal); p-entry=corresponding entry in (constraints-props c-s)
	    (let* ((p (pos-lit lit))
		   (p-entry (car (member p (constraints-props c-s) :key #'first))))
;;; if an entry in (constraints-props c-s) was found
	      (if p-entry
		  (progn
;;; if lit already satisfied bomb out with T
		    (when (eql lit (fourth p-entry))
			  (return-from add-or-clause T))
;;; if lit is open then increment open literal count
		    (unless (fourth p-entry)
;;; increment open literal count
			    (incf o-l-c))
;;; add list (literal p-entry) to lit-list
		    (push (list lit p-entry) lit-list))
;;; if there isn't an existing p-entry, make a new one
;;; p-entry = (proposition pos-lits neg-lits theta ddb-set) where:
;;;    proposition = name of proposition (a positive literal)
;;;    pos-lits = list of clauses containing positive proposition
;;;    neg-lits = list of clauses containing negative proposition
;;;    theta = +p, -p or nil if p is open
;;;    d-set = dependency set; should be nil if theta is nil
		(let ((new-p (list p nil nil nil nil)))
;;; add new-p to list of new propositions - don't add directly to (constraints-props c-s) because
;;; clause may be satisfied already
		  (push new-p new-props)
;;; add list (literal new-p) to lit-list
		  (push (list lit new-p) lit-list)
;;; increment open literal count
		  (incf o-l-c)))))
;;; if there are no open literals then its a clash
    (when (zerop o-l-c)
;;; set *clash-set* to union of d-sets from clause and all its literals
	  (setf *clash-level* (union d-set (dependencies lit-list)))
;;; return nil
	  (return-from add-or-clause nil))
;;; add new-props to (constraints-props c-s)
    (setf (constraints-props c-s) (nconc new-props (constraints-props c-s)))
;;; build new clause record = (olc lits d-set) where:
;;;    olc = number of open literals or -1 if clause satisfied
;;;    lits = list of (lit p-entry) pairs for each literal in the clause
;;;    d-set = dependency set
    (let ((clause-rec (list o-l-c lit-list d-set)))
;;; for each literal in lit-list
      (dolist (p lit-list)
;;; add clause-rec to pos-lit or neg-lit lists in p-entry
	      (if (pos-lit-p (car p))
		  (push clause-rec (second (second p)))
	      (push clause-rec (third (second p)))))
;;; add clause record to (constraints-or-clauses c-s)
      (push clause-rec (constraints-or-clauses c-s))
;;; if open lit count is 1, add clause record to bcp candidates list
      (when (eql o-l-c 1)
	    (push clause-rec (constraints-bcp-cand c-s)))))
;;; return T - clause was added without causing a clash
  T)

(defun extend-prop (c-s p-entry lit d-set)
  "Extend the solution by setting truth value of p-entry to lit"
;;; set proposition truth val to lit
  (setf (fourth p-entry) lit)
;;; set proposition dependency set to d-set
  (setf (fifth p-entry) d-set)
;;; when lit is *BOTTOM* set *clash-level* and bomb out
  (when (eql lit *BOTTOM*)
	(setf *clash-level* d-set)
	(return-from extend-prop nil))
;;; add lit to constraints-c list for use in blocking etc
  (push lit (constraints-c c-s))
;;; if lit is a synonym
  (if (/= lit (c-synonym lit))
;;;   unless synonym literal is added OK
      (unless (add-literal c-s (c-synonym lit) d-set)
;;;     bomb out returning nil (clash)
	      (return-from extend-prop nil))
;;;   otherwise get the literals definition
    (let ((c-def (c-definition lit)))
;;;   and when it has a definition add it to some-rc-ux or c-def-ux for future expansion
      (when c-def
	    (if (and (listp c-def) (eq (car c-def) :some))
		(if (r-functional (second c-def))
		    (push (cons (cdr c-def) d-set) (constraints-some-fc-ux c-s))
		  (push (cons (cdr c-def) d-set) (constraints-some-rc-ux c-s)))
	      (push (list lit d-set) (constraints-c-def-ux c-s))))))
;;; if its a positive literal
  (if (pos-lit-p lit)
      (progn
;;; for each clause in neg-lits list
	(dolist (clause (third p-entry))
;;; decrement open literals count
		(decf (car clause))
;;; if open literals count is zero, its a clash
		(when (zerop (car clause))
;;; set *clash-set* to union of d-sets from clause and all its literals
		      (setf *clash-level* (union (third clause) (dependencies (second clause))))
;;; return nil
		      (return-from extend-prop nil))
;;; if open literals count is one, clause is a candidate for BCP
		(when (eql (car clause) 1)
		      (push clause (constraints-bcp-cand c-s))))
;;; for each clause in pos-lits list, set open literals count to -1 = satisfied
	(dolist (clause (second p-entry))
		(setf (car clause) -1)))
;;; if its a negative literal
      (progn
;;; for each clause in pos-lits list
	(dolist (clause (second p-entry))
;;; decrement open literals count
		(decf (car clause))
;;; if open literals count is zero, its a clash
		(when (zerop (car clause))
;;; set *clash-set* to union of d-sets from clause and all its literals
		      (setf *clash-level* (union (third clause) (dependencies (second clause))))
;;; return nil
		      (return-from extend-prop nil))
;;; if open literals count is one, clause is a candidate for BCP
		(when (eql (car clause) 1)
		      (push clause (constraints-bcp-cand c-s))))
;;; for each clause in neg-lits list, set open literals count to -1 = satisfied
	(dolist (clause (third p-entry))
		(setf (car clause) -1))))
;;; return T - there was no clash
  T)

(defun add-literal (c-s lit d-set)
  "Add literal to constraint system"
;;; p-entry := proposition record in constraints-props
  (let ((p-entry (car (member (pos-lit lit) (constraints-props c-s) :key #'first))))
;;; if there was an entry
    (if p-entry
;;; the fourth item in the entry is its truth-val = lit, not lit or nil
	(let ((truth-val (fourth p-entry)))
;;; if lit=truth-val nothing to be done - return T
	  (when (eql lit truth-val)
		(return-from add-literal T))
;;; if truth val is non-nil it must be not lit -> clash: set ddb set & return nil
	  (when truth-val
		(setf *clash-level* (union d-set (fifth p-entry)))
		(return-from add-literal nil))
;;; return the result of calling call extend-prop to assign truth value
	  (and (extend-prop c-s p-entry lit d-set)
;;;   followed by b-c-p
	       (b-c-p c-s)))
;;; if there was no p-entry, make a new one
      (let ((new-p (list (pos-lit lit) nil nil nil nil)))
;;; add it to constraints-props
	(push new-p (constraints-props c-s))
;;; and set its truth value
	(extend-prop c-s new-p lit d-set)))))

(defun b-c-p (c-s)
  "Expand disjunctions with only one open literal"
;;; loop until candidates stack is empty or there is a clash
  (loop
;;; return T when candidates stack is empty
   (when (endp (constraints-bcp-cand c-s))
	 (return-from b-c-p T))
;;; pop next candidate off the stack - its a clause record = (olc lits d-set)
   (let ((candidate (pop (constraints-bcp-cand c-s))))
;;; when olc still = 1
     (when (eql (car candidate) 1)
;;; look through lits list to find open lit - fourth elt in p-entry is nil
	   (let ((lit (find-if #'(lambda (l) (not (fourth (second l)))) (second candidate))))
;;; its an error if we didn't find it
	     (unless lit
		     (error "~&!!ERROR!! - open literal not found in BCP~%"))
;;; extend c-s by setting literal in p-entry; dependencies = union of d-sets from other lits
	     (unless (extend-prop c-s (second lit) (first lit)
				  (union (third candidate) (dependencies (second candidate))))
;;; return nil if extend-prop caused a clash
		     (return-from b-c-p nil)))))))

(defun pure-lit-candidates (c-s)
  "Return sorted list of propositions which appear in binary clauses and their priorities"
  (let (candidates (max-priority -1))
;;; loop through propositions list and return candidates
    (dolist (prop (constraints-props c-s) candidates)
;;; only consider open propositions
	    (when (and (not (fourth prop)) (or (endp (second prop)) (endp (third prop))))
		  (let ((pos-clauses (length (second prop)))
			(neg-clauses (length (third prop))))
		    (let ((priority (max pos-clauses neg-clauses)))
		      (when (>= priority max-priority)
			    (when (> priority max-priority)
				  (setf max-priority priority)
				  (setf candidates nil)))
;;; add to candidates a list of (prop pos-occurences neg-occurences priority)
		      (push (list prop pos-clauses neg-clauses) candidates)))))))

(defun mom-candidates (c-s)
  "Return sorted list of propositions which appear in binary clauses and their priorities"
  (let (candidates (max-priority -1))
;;; loop through propositions list and return candidates
    (dolist (prop (constraints-props c-s) candidates)
;;; only consider open propositions
	    (when (not (fourth prop))
		  (let ((pos-bin 0) (neg-bin 0))
;;; count the number of times the proposition appears positively in binary clauses
		    (dolist (clause (second prop))
			    (when (eql (car clause) 2) (incf pos-bin)))
;;; count the number of times the proposition appears negatively in binary clauses
		    (dolist (clause (third prop))
			    (when (eql (car clause) 2) (incf neg-bin)))
;;; unless both are zero
		    (unless (and (zerop pos-bin) (zerop neg-bin))
			    (let ((priority (alpha pos-bin neg-bin)))
			      (when (>= priority max-priority)
				    (when (> priority max-priority)
					  (setf max-priority priority)
					  (setf candidates nil)))
;;; add to candidates a list of (prop pos-occurences neg-occurences priority)
			      (push (list prop pos-bin neg-bin) candidates))))))))

(defun clause-lengths (c-s)
  "Return a dotted pair (min-clause-len . max-clause-len)"
  (let ((min-len most-positive-fixnum) (max-len 0))
    (dolist (clause (constraints-or-clauses c-s))
	    (let ((c-len (car clause)))
	      (when (plusp c-len)
		    (when (> c-len max-len)
			  (setf max-len c-len))
		    (when (< c-len min-len)
			  (setf min-len c-len)))))
    (cons min-len max-len)))

(defun prop-jw-weights (p max-clause-len)
  "Return a dotted pair (pos-jw-weight . neg-jw-weight) for proposition p"
  (let ((pos-wt 0) (neg-wt 0))
;;; calculate weights from clauses where the proposition appears positively
    (dolist (clause (second p))
	    (incf pos-wt (jw-weight (car clause) max-clause-len)))
;;; calculate weights from clauses where the proposition appears negatively
    (dolist (clause (third p))
	    (incf neg-wt (jw-weight (car clause) max-clause-len)))
    (cons pos-wt neg-wt)))

(defun jw-candidates (c-s)
  "Return sorted list of propositions and their Jereslow & Wang priorities"
  (let ((max-clause-len (cdr (clause-lengths c-s))) candidates (max-priority -1))
;;; loop through propositions list
    (dolist (prop (constraints-props c-s) candidates)
;;; only consider open propositions
	    (when (not (fourth prop))
;;; calculate j&w weights
		  (let* ((jww (prop-jw-weights prop max-clause-len))
			 (priority (alpha (car jww) (cdr jww))))
		    (when (>= priority max-priority)
			  (when (> priority max-priority)
				(setf max-priority priority)
				(setf candidates nil)))
;;; add to candidates a list of (prop pos-wt neg-wt priority)
		    (push (list prop (car jww) (cdr jww)) candidates))))))

(defun full-moms-heuristic (c-s)
  (let ((candidates (mom-candidates c-s)))
;;; if candidates from binary clauses were found
    (if candidates
;;; if there is more than one candidate (with the same priority)
	(if (cdr candidates)
	    (let (final-candidate (max-priority -1)
				  (max-clause-len (cdr (clause-lengths c-s))))
;;; loop through candidates and return final-candidate
	      (dolist (c candidates final-candidate)
;;; evaluate Jereslow & Wang positive and negative weightings for candidate and its total priority
		      (let* ((jww (prop-jw-weights (car c) max-clause-len))
			     (priority (alpha (car jww) (cdr jww))))
;;; when the priority is >= the max-priority
			(when (>= priority max-priority)
;;; if the priority = max-priority
			      (if (= priority max-priority)
;;; if positive occurences outnumber negative occurences
				  (if (> (car jww) (cdr jww))
;;;   and when positive literals are preferred
				      (when *prefer-pos-lits*
;;;   set the final candidate to the new candidate
					  (setf final-candidate
						(list (car c) (car jww) (cdr jww))))
;;; if positive occurences didn't outnumber negative occurences,
;;; and positive literals are not prefered
				    (unless *prefer-pos-lits*
;;;   set the final candidate to the new candidate
					  (setf final-candidate
						(list (car c) (car jww) (cdr jww)))))
;;; when priority > max-priority, set the max-priority and the final candidate
				(progn
				  (setf max-priority priority)
				  (setf final-candidate
					(list (car c) (car jww) (cdr jww)))))))))
;;; return first member of original candidates list if there was only one candidate
	  (car candidates))
;;; if there were no mom candidates use J & W on all propositions
      (progn
	(setf candidates (jw-candidates c-s))
;;; if there are any candidates
	(if candidates
;;; if there is more than one candidate
	    (if (cdr candidates)
;;; look for a candidate where...
		(let ((pos-cand
		       (find-if #'(lambda (c)
;;; positive literals are preferred and positive weight is greater than negative
				    (or (and *prefer-pos-lits* (> (second c) (third c)))
;;; or positive literals are nnot preferred and negative weight is greater than positive
					(and (not *prefer-pos-lits*) (> (second c) (third c)))))
				candidates)))
;;; if one was found return it, otherwise just return first candidate
		  (if pos-cand pos-cand (car candidates)))
;;; if there was only one candidate return it
	    (car candidates))
;;; otherwise its an error
	  (error "~&!!ERROR!! no candidate for SAT expansion~%"))))))

(defun max-clash (clash-list)
  (if clash-list (reduce #'max clash-list) 0))

(defun select-disjunction (c-s)
  (if (= 2 *moms-heuristic*)
      (let ((max-dep most-positive-fixnum) oldest)
	(dolist (dj (constraints-or-clauses c-s) oldest)
	  (when (plusp (first dj))
	    (let ((md (if (third dj) (max (third dj)) -1)))
	      (cond ((< md max-dep)
		     (setf oldest (second dj))
		     (setf max-dep md))
		    ((= md max-dep)
		     (dolist (c (second dj))
		       (unless (member c oldest) (setf oldest (cons c oldest))))))))))
    (second (car (member-if #'(lambda (c) (plusp (car c))) 
		    (constraints-or-clauses c-s))))))

(defun simple-moms-heuristic (c-s)
  (let* ((uxc (select-disjunction c-s))
	 (max-clause-len (cdr (clause-lengths c-s))) candidate (max-priority -1))
    (dolist (c uxc candidate)
	    (when (not (fourth (second c)))
		  (let* ((jww (prop-jw-weights (second c) max-clause-len))
			 (priority (alpha (car jww) (cdr jww))))
		    (when (> priority max-priority)
			  (setf max-priority priority)
			  (setf candidate (list (second c) (car jww) (cdr jww)))))))))

(defun add-ux-or-constraints (c-s)
  "Expand disjunction clauses by assigning truth values to their propositions"
;;; Chose a candidate proposition according to some heuristic
  (let ((candidate (if (= 1 *moms-heuristic*) (full-moms-heuristic c-s) (simple-moms-heuristic c-s))))
;;; prop:=p-entry in constraints-props list
    (let ((prop (car candidate))
;;; split-lit:=litteral to add; add -ve first if pos-weight>neg-weight
	  (split-lit (if (> (second candidate) (third candidate))
			 (if *minimise-clashes* (caar candidate) (neg-con (caar candidate)))
		       (if *minimise-clashes* (neg-con (caar candidate)) (caar candidate)))))
;;; For debugging purpouses:-
      (setf *already-backtracking* nil)
      (dbg :split-lit "~A~A/~A " (if (pos-lit-p split-lit) "+" "-")
	   (pos-lit split-lit) *model-depth*)
;;; save the constraint system state
      (let* ((saved-cs (save-c-s c-s))
;;; res is true if adding split-lit to c-s is OK and the resulting c-s was satisfiable
;;; note that dependency level *or-level* is incremented
	     (res (and (extend-prop c-s prop split-lit (list (incf *or-level*)))
		       (b-c-p c-s)
		       (sat c-s))))
	(debug-if :backtrack
		  (and (not res) *backjumping* (not *already-backtracking*)
		       (< (max-clash *clash-level*) *or-level*))
		  "BT ~A->~A;" *or-level* (max-clash *clash-level*))
	(setf *already-backtracking* T)
;;; unless we already have a solution or we can backjump
	(unless (or res (and *backjumping* (< (max-clash *clash-level*) *or-level*)))
;;; restore the constraint system state
		(restore-c-s c-s saved-cs)
;;; save the dependency set from the first clash
		(let ((first-clash *clash-level*))
		  (dbg :split-lit "<~A~A/~A "
		       (if (pos-lit-p split-lit) "-" "+")
		       (pos-lit split-lit)  *model-depth*)
		  (incf *search-space*)
;;; res is true if adding (not split-lit) to c-s is OK and the resulting c-s was satisfiable
		  (setf res (and (extend-prop c-s prop (neg-con split-lit) (list *or-level*))
				 (b-c-p c-s)
				 (sat c-s)))
;;; unless we have a solution *clash-level*:=union of two clash dependency sets - current level
		  (unless (or res (and *backjumping*
				       (< (max-clash *clash-level*)
					  *or-level*)))
			  (setf *clash-level*
				(remove *or-level* (union first-clash *clash-level*))))))
;;; decrement dependency level
	(decf *or-level*)
;;; return res
	res))))

(defun save-c-s (c-s)
  "Save the state of the constraint system"
  (let ((y (copy-constraints c-s)))
    (setf (constraints-props y)
	  (cons (constraints-props y) (mapcar #'copy-list (constraints-props c-s))))
    (setf (constraints-or-clauses y)
	  (cons (constraints-or-clauses y) (mapcar #'car (constraints-or-clauses c-s))))
    y))

(defun restore-c-s (c-s saved-c-s)
  (setf (constraints-c c-s) (constraints-c saved-c-s))
  (setf (constraints-all-rc c-s) (constraints-all-rc saved-c-s))
  (setf (constraints-all-r+c c-s) (constraints-all-r+c saved-c-s))
  (setf (constraints-bcp-cand c-s) (constraints-bcp-cand saved-c-s))
  (setf (constraints-some-fc-ux c-s) (constraints-some-fc-ux saved-c-s))
  (setf (constraints-some-rc-ux c-s) (constraints-some-rc-ux saved-c-s))
  (setf (constraints-c-def-ux c-s) (constraints-c-def-ux saved-c-s))
  (setf (constraints-props c-s) (car (constraints-props saved-c-s)))
  (map nil #'(lambda (p1 p2)
	       (setf (second p1) (second p2))
	       (setf (third p1) (third p2))
	       (setf (fourth p1) (fourth p2))
	       (setf (fifth p1) (fifth p2)))
       (constraints-props c-s) (cdr (constraints-props saved-c-s)))
  (setf (constraints-or-clauses c-s) (car (constraints-or-clauses saved-c-s)))
  (map nil #'(lambda (p1 p2)
	       (setf (first p1) p2))
       (constraints-or-clauses c-s) (cdr (constraints-or-clauses saved-c-s))))



;;; ************** VERBOSITY DEBUGGING & PROFILING ************** 

(defmacro verbosity (level f-string &rest v-list)
  `(when (and *verbosity* (if (listp ,level)
			      (some #'(lambda (l) (member l *verbosity*)) ,level)
			    (member ,level *verbosity*)))
	 ,(nconc `(funcall #'format T ,f-string) v-list)
	 (force-output)
	 T))

(defun set-verbosity (&rest l)
  (dolist (li l *verbosity*)
	  (pushnew li *verbosity*)))

(defun reset-verbosity (&rest l)
  (if l
      (dolist (li l *verbosity*)
	      (setf *verbosity* (delete li *verbosity*)))
    (setf *verbosity* nil)))

(defmacro dbg (level f-string &rest v-list)
  `(when (and *debugging* (member ,level *debugging*))
	 ,(nconc `(funcall #'format T ,f-string) v-list)
	 (force-output)))

(defmacro debug-if (level cond f-string &rest v-list)
  `(when (and *debugging* (member ,level *debugging*) ,cond)
	 ,(nconc `(funcall #'format T ,f-string) v-list)
	 (force-output)))

(defun set-debug (&rest l)
  (dolist (li l *debugging*)
	  (pushnew li *debugging*)))

(defun reset-debug (&rest l)
  (if l
      (dolist (li l *debugging*)
	      (setf *debugging* (delete li *debugging*)))
    (setf *debugging* nil)))

(defun feature-list (l val)
  (dolist (f l)
	  (case f
		(:transitivity (setf *transitivity* val))
		(:concept-eqn (setf *concept-eqn* val))
		(:subset-s-equivalent (setf *subset-s-equivalent* val))
		(:backjumping (setf *backjumping* val))
		(:obvious-subs (setf *obvious-subs* val))
		(:top-level-caching (setf *top-level-caching* val))
		(:full-caching (setf *full-caching* val))
		(:blocking (setf *blocking* val))
		(:taxonomic-encoding (setf *taxonomic-encoding* val))
		(:gci-absorption (setf *gci-absorption* val))
		(:cyclical-definitions (setf *cyclical-definitions* val))
		(:auto-configure (setf *auto-configure* val))
		(:moms-heuristic (setf *moms-heuristic* val))
		(:prefer-pos-lits (setf *prefer-pos-lits* val))
		(:minimise-clashes (setf *minimise-clashes* val))
		(:auto-install-primitives (setf *auto-install-primitives* val))
		(:auto-install-transitive (setf *auto-install-transitive* val))
		(T (verbosity :warnings "~&!!WARNING!! unknown feature - ~S~%" f))))
  (mapcan #'(lambda (f) (when (cdr f) (list (car f))))
	  `((:transitivity . ,*transitivity*)
	    (:concept-eqn . ,*concept-eqn*)
	    (:subset-s-equivalent . ,*subset-s-equivalent*)
	    (:backjumping . ,*backjumping*)
	    (:obvious-subs . ,*obvious-subs*)
	    (:top-level-caching . ,*top-level-caching*)
	    (:full-caching . ,*full-caching*)
	    (:blocking . ,*blocking*)
	    (:taxonomic-encoding . ,*taxonomic-encoding*)
	    (:gci-absorption . ,*gci-absorption*)
	    (:cyclical-definitions . ,*cyclical-definitions*)
	    (:auto-configure . ,*auto-configure*)
	    (:moms-heuristic . ,*moms-heuristic*)
	    (:prefer-pos-lits . ,*prefer-pos-lits*)
	    (:minimise-clashes . ,*minimise-clashes*)
	    (:auto-install-primitives . ,*auto-install-primitives*)
	    (:auto-install-transitive . ,*auto-install-transitive*))))

(defun set-features (&rest l)
  (feature-list l T))

(defun reset-features (&rest l)
  (feature-list l nil))

(defun set-profiling (&key (file "profile.out") (level 1))
  (setf *profiling* level)
  (setf *profile-file* (not (null file)))
  (setf *profile-fname* file)
  (if (> level 0)
      (format T "~&Profiling level ~1D => ~A~%" level (if file file "*TERMINAL-IO*"))
    (format T "~&Profiling OFF~%"))
  (values))

(defun reset-profiling ()
  (setf *profiling* 0)
  (format T "~&Profiling OFF~%")
  (values))

(defun todays-date ()
  (multiple-value-bind
   (sec min hour date month year day bst zone)
   (get-decoded-time)
   (declare (ignore day))
   (format nil "~2,'0D:~2,'0D:~2,'0D ~A  ~D/~D/~D"
	   (+ hour zone) min sec (if bst "BST" "GMT") date month year)))

(defun features (&optional (f t))
  (format f "~&;FaCT version ~A  ~A~%" *version-number* (todays-date))
  (format f ";   ~A ~A  ~A ~A~%" (lisp-implementation-type) (lisp-implementation-version)
	  (if (machine-type) (machine-type) "386") (software-type))
  (format f ";   Loaded TBox files:~%")
  (map nil #'(lambda (n) (format f ";      ~A~%" n)) (reverse *kb-file-names*))
  (format f ";Features and optimisations:~%")
  (format f ";   Transitivity:                         ~A~%" (if *transitivity* "ON" "OFF"))
  (format f ";   Concept Inclusions:                   ~A~%" (if *concept-eqn* "ON" "OFF"))
  (format f ";   Blocking                              ~A~%" (if *blocking* "ON" "OFF"))
  (when *blocking*
	(format f ";   Subset S-equivalence:                 ~A~%"
		(if *subset-s-equivalent* "ON" "OFF")))
  (format f ";   Encoding & Normalisation:             ~A~%" (if *taxonomic-encoding* "ON" "OFF"))
  (format f ";   GCI absorption:                       ~A~%" (if *gci-absorption* "ON" "OFF"))
  (format f ";   Backjumping:                          ~A~%" (if *backjumping* "ON" "OFF"))
  (format f ";   Obvious Subsumption Detection:        ~A~%" (if *obvious-subs* "ON" "OFF"))
  (format f ";   Use caching in subsumption tests:     ~A~%" (if *top-level-caching* "ON" "OFF"))
  (format f ";   Use caching in satisfiability tests:  ~A~%" (if *full-caching* "ON" "OFF"))
  (format f ";   SAT branch to minimise clashes:       ~A~%" (if *minimise-clashes* "ON" "OFF"))
  (format f ";   Branching heuristic:                  ~A~%" (case *moms-heuristic*
							       (0 "OLDEST+JW")
							       (1 "MOMS")
							       (2 "DEPENDENCIES+JW")))
  (when (= *moms-heuristic* 1)
	(format f ";   MOMS heuristic prefers positive lits: ~A~%"
		(if *prefer-pos-lits* "ON" "OFF")))
  (format f ";   Cyclical primitive definitions:       ~A~%"
	  (if *cyclical-definitions* "ON" "OFF"))
  (format f ";   Profiling:                            ~A~%" (if *profiling* 
								 (if *profile-file*
								     (format nil "Level ~1D => ~A"
									     *profiling*
									     *profile-fname*)
								   "ON")
							       "OFF"))
  (format f "~%"))

(defun profile-open ()
  (setf *profile-file*
	(when *profile-fname*
	      (open *profile-fname* :direction :output :if-exists :supersede))))

(defun profile-close ()
  (when *profile-fname*
	(close *profile-file*)
	(setf *profile-file* nil)))

(defun profile-hdr ()
  (unless *profile-file*
	  (format T "~&    search    m-size   m-depth  c-access     c-hit")
	  (format T "  run-time sat cyc")))

(defun profile-out (exp sat-result)
  (cond
   (*profile-file*
    (format *profile-file* "~%(~S ~S ~S ~S ~S ~S ~F ~S ~S)"
	    exp *search-space* *max-model-size* *max-model-depth* *cache-accesses* *cache-hits*
	    (/ *run-time* internal-time-units-per-second) sat-result *cycle*))
   (T
    (format T "~&Satisfiable - ~A ?" (decode-concept exp))
    (profile-hdr)
    (format T "~&~10@S~10@S~10@S~10@S~10@S~10F~4@S~4@S"
	    *search-space* *max-model-size* *max-model-depth* *cache-accesses* *cache-hits*
	    (/ *run-time* internal-time-units-per-second)
	    sat-result *cycle*))))



;;; ************** KSAT INTERFACE ************** 

(defun translate-role (r)
  (if (listp r)
      (if (and (string= *reasoner* "iFaCT") (= (length r) 2)
	       (member (first r) '(inverse inv :inverse :inv)))
	  (inv-r (translate-role (second r)))
	(error "UN-TRANSLATABLE ROLE - ~S" r))
    r))

(defun translate-concept (c)
  (cond
   ((listp c)
    (rplaca c
	    (case (car c)
		  ((and :and) :and)
		  ((or :or) :or)
		  ((not :not) :not)
		  ((some :some) :some)
		  ((all :all) :all)
		  (t (error "UN-TRANSLATABLE CONCEPT - ~S" c))))
    (case (car c)
	  ((:and :or)
	   (do ((l (cdr c) (cdr l)))
	       ((endp l))
	       (rplaca  l (translate-concept (car l)))))
	  ((:some :all)
	   (rplaca (cddr c) (translate-concept (caddr c))))
	  (T
	   (rplaca (cdr c) (translate-concept (cadr c)))))
    c)
   (T
    (case c
	  (*TOP* :top)
	  (*BOTTOM* :bottom)
	  (t c)))))

(defun alc-concept-coherent (c &key (k4 nil) (logic 'k))
  (unless (member logic '(k kt k4 s4))
	  (error "Logic must be K, KT, K4 or S4 - ~A not supported~%" logic))
  (when (and k4 (not logic)) (setf logic 'k4))
  (clear-kb)
  (setf *search-space* 0) ; backtracking search space
  (setf *max-model-size* 1) ; max model size
  (progv '(*encode-reflexive* *auto-install-transitive* *transitivity* 
			      *auto-install-primitives* *blocking* 
			      *concept-eqn* *full-caching* *profiling*
			      *verbosity*)
	 `(,(not (null (member logic '(kt s4))))     ;;; reflexive
	   ,(not (null (member logic '(k4 s4))))     ;;; transitive
	   ,(not (null (member logic '(k4 s4))))     ;;; transitive
	   T                                         ;;; auto install
	   ,(not (null (member logic '(k4 s4))))     ;;; blocking
	   nil                                       ;;; concept eqn
	   ,(if *auto-configure* nil *full-caching*)
	   ,(if *auto-configure* 0 *profiling*)
	   ,(if *auto-configure* nil *verbosity*))
	 (let* ((encoded-concept (encode-concept-term (translate-concept c)))
		(start-time (get-internal-run-time)))
	   (cond ((eql encoded-concept *BOTTOM*)
		  (setf *run-time* 0)
		  (setf *variable-assignments* 0)
		  (setf *total-model-size* 0)
		  (values nil
			  *run-time*
			  *variable-assignments*
			  *total-model-size*))
		 (T
		  (let ((res (test-sat encoded-concept)))
		    (setf *run-time* (/ (- (get-internal-run-time) start-time)
					internal-time-units-per-second))
		    (setf *variable-assignments* *search-space*)
		    (setf *total-model-size* *max-model-size*)
		    (values (when res T)
			    *run-time*
			    *variable-assignments*
			    *total-model-size*)))))))

(defun load-ksat-concept (&optional fname)
  (declare (special con))
  (progv '(con) '(nil)
    (unless fname
	    (princ "Concept File? ")
	    (setf fname (read)))
    (unless (stringp fname)
	    (setf fname (string-downcase (symbol-name fname))))
    (load (probe-file (concatenate 'string "~/lisp/ksat/Grail/Data/" fname)))
    con))

(defun alc-test (&key (c nil) (k4 nil))
  (when (or (null c) (stringp c)) (setf c (load-ksat-concept c)))
  (let* ((st (get-internal-run-time)) (res (alc-concept-coherent c :k4 k4))
	 (et (get-internal-run-time)))
    (format T "~&~A ~10,2F ~10D ~10D ~10,2F~%"
	    (if res "S" "U")
	    (/ (- et st) internal-time-units-per-second)
	    *variable-assignments*
	    *total-model-size*
	    *run-time*)))



;;; ************** KRIS INTERFACE **************

(defun defconcept-f (n d)
  (grail-define-concept n (translate-concept d)))

(defmacro defconcept (n d)
  `(grail-define-concept ',n ',(translate-concept d)))

(defun defprimconcept-f (n &optional d)
  (grail-define-primconcept n (when d (translate-concept d))))

(defmacro defprimconcept (n &optional d)
  `(grail-define-primconcept ',n ',(when d (translate-concept d))))

(defun defprimrole-f (r &key (parents nil) (supers nil) (transitive nil))
  (setf parents (union parents supers))
  (when (and parents (not (listp parents)))
	(error "~&!!ERROR!! BAD :PARENTS LIST - ~S~%" parents))
  (grail-define-relation r r
;			 (intern (format nil "I*~A" r))
			 parents nil nil)
  (when (eq transitive T)
	(grail-redefine-relation r :transitive-across `(,r)))
  (car (multiple-value-list (r-defined r))))

(defmacro defprimrole (r &key (parents nil) (supers nil) (transitive nil))
  `(defprimrole-f ',r :parents ',parents :supers ',supers :transitive ,transitive))

(defun defprimattribute-f (r &key (parents nil) (supers nil))
  (setf parents (union parents supers))
  (when (and parents (not (listp parents)))
	(error "~&!!ERROR!! BAD :PARENTS LIST - ~S~%" parents))
  (grail-define-relation r r
;			 (intern (format nil "I*~A" r))
			 parents T T))

(defmacro defprimattribute (r &key (parents nil) (supers nil))
  `(defprimattribute-f ',r :parents ',parents :supers ',supers))

(defun implies-f (a c)
  (grail-define-implication (translate-concept a) (translate-concept c))
  (kris-concept a))

(defmacro implies (a c)
  `(implies-f ',a ',c))

(defun disjoint-f (&rest c-list)
  (do ((d-list (mapcar #'translate-concept c-list) (cdr d-list)))
      ((endp (cdr d-list)))
      (grail-define-implication (first d-list) (list :not (cons :or (cdr d-list))))))

(defmacro disjoint (&rest c-list)
  `(apply #'disjoint-f ',c-list))

(defun load-tkb (filename &key (verbose t) (overwrite nil))
  (progv '(*verbosity*) '((:warnings))
	 (when verbose (set-verbosity :classify-1))
	 (when overwrite (clear-kb))
	 (let ((f (probe-file filename)))
	   (if f
	       (progn
		 (push (namestring f) *kb-file-names*)
		 (load f))
	     (progn
	       (verbosity :warnings "~&!!WARNING!! file not found - ~S~%" filename)
	       nil)))))

(defun classify-tkb (&key (mode :nothing))
  (progv '(*verbosity*) '((:warnings :synonyms))
	 (case mode
	       (:stars (set-verbosity :classify-1))
	       (:names (set-verbosity :classify-2))
	       (:count (set-verbosity :test-counts))
	       (T))
	 (classify-all-concepts)))

(defun init-tkb ()
  (clear-kb))

(defun direct-supers (concept)
  (when (typep concept 'grail-concept) (setf concept (grail-concept-name concept)))
  (setf concept (translate-concept concept))
  (when (system-name concept)
	(mapcar #'(lambda (c) (g-concept (c-grail-name c)))
		(c-parents (system-name concept)))))

(defun all-supers (concept)
  (when (typep concept 'grail-concept) (setf concept (grail-concept-name concept)))
  (setf concept (translate-concept concept))
  (when (system-name concept)
	(let (a (m (gensym)))
	  (mark-all-ancestors (system-name concept) m)
	  (setf (c-mark1 (system-name concept)) nil)
	  (dotimes (c *next-available-concept* a)
		   (when (eq (c-mark1 c) m) (push (g-concept (c-grail-name c)) a))))))

(defun direct-subs (concept)
  (when (typep concept 'grail-concept) (setf concept (grail-concept-name concept)))
  (setf concept (translate-concept concept))
  (when (system-name concept)
	(mapcar #'(lambda (c) (g-concept (c-grail-name c)))
		(c-children (system-name concept)))))

(defun all-subs (concept)
  (when (typep concept 'grail-concept) (setf concept (grail-concept-name concept)))
  (setf concept (translate-concept concept))
  (when (system-name concept)
	(let (a (m (gensym)))
	  (mark-all-descendants (system-name concept) m)
	  (setf (c-mark1 (system-name concept)) nil)
	  (dotimes (c *next-available-concept* a)
		   (when (eq (c-mark1 c) m) (push (g-concept (c-grail-name c)) a))))))

(defun equivalences (concept)
  (when (typep concept 'grail-concept) (setf concept (grail-concept-name concept)))
  (setf concept (translate-concept concept))
  (when (system-name concept)
	(let ((s-n (system-name concept)))
	  (delete (g-concept concept)
		  (mapcan #'(lambda (c) (when (eql (system-name c) s-n)
					      (list (g-concept c))))
			  (list* :top :bottom *c-definitions*))))))

(defun satisfiable (concept)
  (progv '(*auto-install-primitives*) '(T)
	 (when (test-sat (encode-concept-term (translate-concept concept))) T)))

(defun subsumes (c1 c2)
  (progv '(*auto-install-primitives*) '(T)
	 (unless (test-sat (encode-concept-term (translate-concept `(and ,c2 (not ,c1))))) T)))

(defun equivalent-concepts (c1 c2)
  (and (subsumes c1 c2) (subsumes c2 c1)))

(defun disjoint-concepts (c1 c2)
  (not (satisfiable `(and ,c1 ,c2))))

(defun add-concept-f (c-name definition &key (primitive nil))
  (if (eq primitive t)
      (defprimconcept-f c-name definition)
    (defconcept-f c-name definition))
  (classify-tkb)
  (car (multiple-value-list (g-concept c-name))))

(defmacro add-concept (c-name definition &key (primitive nil))
  `(add-concept-f ',c-name ',definition :primitive ,primitive))

(defun classify-concept (concept)
  (let ((e-c (encode-concept-term (translate-concept concept))))
    (unless (eql (c-classified e-c) 2)
	    (classify-con e-c :install nil))
    (when (c-synonym e-c)
	  (setf e-c (c-synonym e-c)))
    (if (eql (c-classified e-c) 2)
	(values
	 (mapcar #'(lambda (c) (g-concept (c-grail-name c))) (c-parents e-c))
	 (mapcar #'(lambda (c) (g-concept (c-grail-name c))) (c-children e-c))
	 (delete (g-concept concept)
		 (mapcan #'(lambda (c) (when (eql (system-name c) e-c)
					     (list (g-concept c))))
			 (list* :top :bottom *c-definitions*))))
      (values
       (mapcar #'(lambda (c) (g-concept (c-grail-name c))) (c-parents e-c))
       (mapcar #'(lambda (c) (g-concept (c-grail-name c))) (c-children e-c))
       nil))))

(defun get-concept (name)
  (setf name (translate-concept name))
  (car (multiple-value-list (g-concept name))))

(defun get-role (name)
  (car (multiple-value-list (r-defined name))))

(defun get-all-concepts ()
  (mapcar #'get-concept (list* :top :bottom *c-definitions*)))

(defun get-all-roles ()
  (mapcar #'get-role *r-definitions*))

(defun classified-tkb? ()
  "Return T if the TBox is fully processed and classified; NIL otherwise."
  (when (and
;;; every role has been pre-processed
	 (every #'r-processed-f *r-definitions*)
;;; there are no unprocessed implications
	 (endp *implications*)
;;; there are no unencoded universal constraints
	 (endp *grail-universal-constraints*)
;;; every defined concept is encoded and classified
	 (every #'(lambda (c)
;;; concept is encoded - it has a system name 
		    (and (system-name c)
;;; concept is classified - c-classified=2
			 (= (c-classified (system-name c)) 2)))
		*c-definitions*))
	T))

(defun what-is? (obj)
  (typecase obj
	    (grail-concept
	     (if (grail-concept-primitive obj) 'primitive 'concept))
	    (role
	     (if (role-functional obj) 'feature 'role))))

(defun is-primitive? (c)
  (and (typep c 'grail-concept) (grail-concept-primitive c)))

(defun is-concept? (c)
  (and (typep c 'grail-concept) (not (grail-concept-primitive c))))

(defun is-role? (r)
  (and (typep r 'role) (not (role-functional r))))

(defun is-feature? (r)
  (and (typep r 'role) (role-functional r)))

(defun name (obj)
  (typecase obj
	    (grail-concept (grail-concept-name obj))
	    (role (role-grail-name obj))))

(defun description (obj)
  (typecase obj
	    (grail-concept
	     (let ((d (grail-concept-definition obj)))
	       (if (grail-concept-primitive obj)
		   (if d
		       (kris-concept
			(flatten (list :and
				       (intern (format nil "~A*" (grail-concept-name obj)))
				       d)))
		     (car (multiple-value-list
			   (intern (format nil "~A*" (grail-concept-name obj))))))
		 (kris-concept d))))
	    (role 
	     (let (d)
	       (when (role-transfered-by obj) (push t d) (push :transitive d))
	       (when (role-parents obj) (push (role-parents obj) d) (push :supers d))
	       (push (role-grail-name obj) d)))))



;;; ************** USER INTERFACE **************

(defun display-concept (c)
  (if (g-concept c)
      (let* ((s-n (system-name c)))
	(unless (eq (c-grail-name s-n) c)
		(format t "~&!!NOTE!! ~S is a SYNONYM for ~S.~%" c (c-grail-name s-n)))
	(format t "~A - ~S:~%"
		(if (c-primitive s-n)
		    "PRIMITIVE-CONCEPT" "NON-PRIMITIVE-CONCEPT")
		(c-grail-name s-n))
	(format t "  DEFINITION: ~S~%" (c-definition s-n))
	(format t "     PARENTS: ~S~%" (c-parents s-n))
	(format t "    CHILDREN: ~S~%" (c-children s-n)))
    (format t "~&!!WARNING!! ~S is UNDEFINED.~%" c))
  (values))

(defun display-role (r)
  (cond
   ((not (member r *r-definitions*))
    (format t "~S is UNDEFINED~%" r))
   (T
    (format t "PRIMITIVE-ROLE - ~S:~%" r)
    (when (r-parents r)
	  (format t "         DEFINITION: ~S~%" (cons 'and (r-parents r))))
    (when (r-trans-across r)
	  (format t "  TRANSITIVE-ACROSS: ~S~%" (r-trans-across r)))))
  (values))



;;; ***************** GNU STUFF ****************

(defconstant gnu-warranty
    "
NO WARRANTY

BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO
WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE
LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS
AND/OR OTHER PARTIES PROVIDE THE PROGRAM \"AS IS\" WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,
THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE
PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME
THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN
WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY
AND/OR REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU
FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR
CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE
PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF
SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH
DAMAGES.

See the accompanying GNU general public license for further details.

")

(defconstant gnu-conditions
    "
You may copy and distribute the Program (or a work based on it, under
Section 2) in object code or executable form under the terms of
Sections 1 and 2 above provided that you also do one of the following:

      a) Accompany it with the complete corresponding machine-readable
      source code, which must be distributed under the terms of
      Sections 1 and 2 above on a medium customarily used for software
      interchange; or,

      b) Accompany it with a written offer, valid for at least three
      years, to give any third party, for a charge no more than your
      cost of physically performing source distribution, a complete
      machine-readable copy of the corresponding source code, to be
      distributed under the terms of Sections 1 and 2 above on a
      medium customarily used for software interchange; or,

      c) Accompany it with the information you received as to the
      offer to distribute corresponding source code. (This alternative
      is allowed only for noncommercial distribution and only if you
      received the program in object code or executable form with such
      an offer, in accord with Subsection b above.)

See the accompanying GNU general public license for further details.

")

(defconstant gnu-copyright
    "Copyright (C) 1997 and 1998, Ian R. Horrocks and the University of Manchester.
FaCT comes with ABSOLUTELY NO WARRANTY; for details type `(warranty)'.
This is free software, and you are welcome to redistribute it
under certain conditions; for details type `(conditions)'.
")

(defun warranty ()
  "display GNU (lack of) warranty information"
  (princ gnu-warranty)
  (values))

(defun conditions ()
  "display GNU redistribution conditions"
  (princ gnu-conditions)
  (values))

(defun copyright ()
  "display GNU copyright declaration"
  (format T "~&FaCT description logic classifier v~A~%" *version-number*)
  (princ gnu-copyright)
  (values))



;;; ************** INITIALISATION **************

(eval-when (load eval)
  (copyright)
  (clear-kb)
  (setf *print-circle* t))
