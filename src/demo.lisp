;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CL-USER -*-
 
(in-package :cl-user)

(defun dlmaps-demo ()

  (when (probe-file "q-queries.lisp")
    (let ((*package* (find-package :spatial-substrate)))
      (load "q-queries.lisp")))

  (dlmaps::map-viewer)
  (dlmaps::result-inspector))


