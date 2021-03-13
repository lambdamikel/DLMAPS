;;;
;;; $Header: /home/gene/library/website/docsrc/lisp-heap/RCS/ex11.lisp,v 201.1 2004/08/31 06:49:54 gene Exp $
;;;
;;; Copyright (c) 2003 by Gene Michael Stover.
;;; All rights reserved.
;;; Permission to copy, store, & view this document unmodified &
;;; in its entirety is granted.
;;;

(require "heap" '("heap.lisp"))

(defun heap-sort-basic (lst lessp)
  (let ((h (create-heap lessp)))
    ;; Copy the elements of LST into the heap.
    (dolist (x lst) (heap-insert h x))
    ;; Remove the elements from the heap,
    ;; collecting them into a new list.
    (loop while (not (heap-empty-p h)) collect (heap-remove h))))

(defun heap-sort-smart (lst lessp)
  (let ((h (create-heap lessp :initial-contents lst)))
    (loop while (not (heap-empty-p h)) collect (heap-remove h))))

(defun ex11 ()
  "Compare HEAP-SORT-BASIC & HEAP-SORT-SMART."
  (let ((lst (loop for i from 1 to 10000 collect (random 100000))))
    (dolist (fn '(heap-sort-basic heap-sort-smart))
	    (let* ((start-time (get-internal-run-time))
		   (ignored (funcall fn lst #'<))
		   (end-time (get-internal-run-time))
		   (second (/ (- end-time start-time)
			      internal-time-units-per-second)))
	      (format t "~&~16A  ~5,2F" fn second)))))

;;; --- end of file ---
