;;;
;;; $Header: /home/gene/library/website/docsrc/lisp-heap/RCS/test.lisp,v 201.1 2004/08/31 06:49:54 gene Exp $
;;;
;;; Copyright (c) 2002 by Gene Michael Stover.
;;; All rights reserved.
;;; Permission to copy, store, & view this document unmodified &
;;; in its entirety is granted.
;;;

(require "heap" '("heap.lisp"))

(defvar *tests* nil)
(setq *tests* nil)

(defmacro deftest (name &rest args-doc-body)
  `(progn
     ;; I don't understand why the SETQ is necessary.  NCONC is
     ;; destructive, it should affect *TESTS* inherently.  NCONC
     ;; has the expected behaviour when I experiment with it in
     ;; interactive mode, but experience shows that SETQ is
     ;; needed here.
     (setq *tests* (nconc *tests* (list ',name)))
     (defun ,name ,@args-doc-body)))

(deftest test0000 ()
  "Verifies that we can create a heap without crashing."
  (create-heap #'<))

(deftest test0010 ()
  "Verifies that we can insert some things into a heap without
crashing."
  (let ((h (create-heap #'<)))
    (dotimes (i 10)
	     (heap-insert h i))
    h))

(deftest test0012 ()
  "Verifies that we can insert some things & then remove as many
things & that the heap is then empty."
  (let ((h (create-heap #'<)))
    (dotimes (i 10)
	     (heap-insert h i))
    (dotimes (i 10)
	     (heap-remove h))
    (heap-empty-p h)))

(deftest test0013 ()
  "Places some numbers in a heap, in order, & then verifies that
the numbers are removed in that order & that the heap is then empty."
  (let ((h (create-heap #'<)))
    (dotimes (i 10)
	     (heap-insert h i))
    (dotimes (i 10)
	     (assert (eql (heap-remove h) i)))
    (heap-empty-p h)))

(deftest test0014 (&optional (count 10))
  "Places some random numbers in a heap & verifies that they are
removed in the correct order & that the heap is then empty."
  (let ((h (create-heap #'<))
	(lst (loop for i from 1 to count collect (random 100))))
    (mapc #'(lambda (x) (heap-insert h x)) lst)
    (mapc #'(lambda (x) (assert (eql (heap-remove h) x))) (sort lst #'<))
    (heap-empty-p h)))

(deftest test0015 ()
  "Performs TEST0014 many times & with larger COUNTs."
  (dotimes (i 100)
	   (assert (test0014 (* 3 i))))
  t)

(deftest test0016 ()
  "Verify that we can create a heap & specify its INITIAL-CONTENTS
without crashing."
  (create-heap #'< :initial-contents '(28 20 19 18 15 14 13 12 11)))

(deftest test0017 ()
  "Verify that a heap created with an INITIAL-CONTENTS is in order."
  (let ((h (create-heap #'< :initial-contents (loop for i from 99 downto 0
						    collect i))))
    (loop for i from 0 to 99
	  do (assert (eql (heap-remove h) i))))
  t)

(labels ((expect (lst less-fn)
		 "Put the items from LST into a heap in order, then
verify that they are removed from the heap in the correct order.
The heap orders by LESS-FN.  The list is sorted by LESS-FN before
the removal/comparisons."
		 (let ((h (create-heap less-fn)))
		   (mapc #'(lambda (x) (heap-insert h x)) lst)
		   (mapc #'(lambda (x)
			     (unless (equal x (heap-remove h))
			       (return-from expect nil)))
			 (sort lst less-fn))
		   (heap-empty-p h))))

	(deftest test0018 ()
	  "Put some strings in a heap, in order, make sure they come
out of the heap in that order."
	  (expect '("a" "b" "c") #'string-lessp))

	(deftest test0019 ()
	  "Put some strings in a heap, in reverse order, make sure
they come out of the heap in order."
	  (expect '("c" "b" "a") #'string-lessp)))

(deftest test0030 ()
  "Test that HEAP-CLEAR changes a non-empty heap into an
empty heap."
  (let ((h (create-heap #'>)))
    (heap-insert h 3)
    (heap-insert h 5)
    (heap-insert h 2)
    (heap-clear h)
    (heap-empty-p h)))

(deftest test0031 ()
  "Test that HEAP-CLEAR is faster than removing every element."
  (let ((h (create-heap #'>))
	(lst (loop for i from 1 to 10000 collect (random 10000))))
    (dolist (x lst) (heap-insert h x))
    (let* ((start-clear (get-internal-run-time))
	   (end-clear (progn (heap-clear h) (get-internal-run-time)))
	   (time-clear (- end-clear start-clear)))
      (dolist (x lst) (heap-insert h x))
      (let* ((start-loop (get-internal-run-time))
	     (end-loop (do ()
			   ((heap-empty-p h) (get-internal-run-time))
			   (heap-remove h)))
	     (time-loop (- end-loop start-loop)))
	(unless (< time-clear time-loop)
	  (format t "~&Time to HEAP-CLEAR should be less than time to")
	  (format t "~&remove all elements with HEAP-REMOVE.  Instead,")
	  (format t "~&~A is ~A, & ~A is ~A." 'time-clear time-clear
		  'time-loop time-loop))
	(< time-clear time-loop)))))

(deftest test0035 ()
  "Test using HEAP-DELETE to remove the single element from a heap
with one element.  The heap has one element, we DELETE it, & then
we check that the heap is empty."
  (let ((h (create-heap #'>)))
    (heap-insert h 42)
    (and (heap-remove h #'(lambda (h x) (eql x 42)))
	 (heap-empty-p h))))

(deftest test0037 ()
  "Test that interleaved INSERTs & REMOVEs return the right
elements, in the right order, & finally leave the heap empty."
  (let ((h (create-heap #'<)))
    (and (eql (heap-insert h 5) 5)
	 (eql (heap-insert h 11) 11)
	 (eql (heap-remove h) 5)
	 (eql (heap-insert h 17) 17)
	 (eql (heap-remove h) 11)
	 (eql (heap-insert h 21) 21)
	 (eql (heap-remove h) 17)
	 (eql (heap-remove h) 21)
	 (heap-empty-p h))))    

(defun check (lst)
  "Executes all the tests in LST until they all execute or one of
them fails."
  (do ((rc t    (funcall (first x)))
       (x  lst  (rest x)))
      ((or (endp x) (not rc)) (progn (unless rc
				       (format t "~&failure"))
				     rc))
      (print (first x))))

(defun demo-numbers (&optional (count 10000))
  "Fills a heap with COUNT random numbers, then removes them all.
Prints the number of seconds required for each operation."
  (let* ((h (create-heap #'<))
	 (start-time (get-internal-run-time)))
    (format t "~%inserting ~A numbers" count)
    (dotimes (i count) (heap-insert h (random count)))
    (format t "~&removing ~A numbers" count)
    (dotimes (i count) (heap-remove h))
    (assert (heap-empty-p h))
    (let* ((end-time (get-internal-run-time))
	   (second (/ (- end-time start-time) internal-time-units-per-second))
	   (rate (/ count second)))
      (format t "~&inserted, then removed, ~A items" count)
      (format t "~&duration ~,2G seconds" second)
      (format t "~&~,2G item per second" rate)
      rate)))

(defun demo-game (&optional (count 1000))
  "Simulate a game with an event loop to see how quickly a
game could process events.  Return the number of messages
per second.  Approximates a game's event queue by putting
10,000 events in the queue, then inserts & removes elements
one at a time for a while.  Elements are random integers.
    My laptop, Plague, a 750 MHz Pentium 3, running clisp in
interpreted mode, does nearly 1,500 insert/remove operations
per second.  If a game frame is 1/30th of a second, that is
about 50 events per frame, if that's all the game does,
which of course it is not.  In an a-life program, if the
event loop handles less than 1,500 messages per simulation
second, the simulation will run faster than real-time."
  (let ((h (create-heap #'<)))
    ;; Prime the queue.
    (dotimes (i 10000) (heap-insert h (random 10000)))
    (let ((start (get-internal-run-time)))
      (dotimes (i count)
	       (heap-remove h)
	       (heap-insert h (random 10000)))
      (let* ((end (get-internal-run-time))
	     (second (/ (- end start) internal-time-units-per-second))
	     (rate (/ count second)))
	rate))))
      
;;; --- end of file ---
