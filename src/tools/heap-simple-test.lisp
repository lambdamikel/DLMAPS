;;;
;;; $Header: /home/gene/library/website/src/htdocs/lisp-heap/RCS/test.lisp,v 1.3 2003/03/21 15:30:58 gene Exp gene $
;;;
;;; Copyright (c) 2003 by Gene Michael Stover.
;;; All rights reserved.
;;; Permission to copy, store, & view this document unmodified &
;;; in its entirety is granted.
;;;

(require "heap" '("heap.lisp"))

;;;
;;; Create a heap to hold integers \& nothing
;;; else.  Order the integers from small to
;;; large.
;;;
(defvar *h* (heap::create-heap #'<))

(pprint (heap:heap-empty-p *h*))

;;;
;;; Insert some hard-coded integers into the heap.
;;; Notice that they are out of order.
;;;
(dolist (i '(5 3 8))
	(heap::heap-insert *h* i))

(pprint (heap:heap-empty-p *h*))

(pprint (list (heap:heap-items *h*)))
	     
;;;
;;; Remove the items from the heap & print them.
;;; It should print 3, then 5, then 8.
;;;


(pprint (list (heap:heap-find *h* #'(lambda (h x) (= x 3)))
              (heap:heap-find *h* #'(lambda (h x) (= x 13)))))

(pprint (list (heap:heap-items *h*)))
	     
(print (list (heap:heap-remove *h*)
             (heap:heap-remove *h*)))

(pprint (list (heap:heap-items *h*)))

(pprint (heap:heap-empty-p *h*))

(print (list (heap:heap-remove *h*)
             (heap:heap-remove *h*)))

(pprint (heap:heap-empty-p *h*))

;;; --- end of file ---
