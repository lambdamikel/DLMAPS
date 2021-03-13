;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SPATIAL-SUBSTRATE; Base: 10 -*-

(in-package spatial-substrate)

(defun load-geo-ontology ()
  #+:midelora
  (let ((*package* (find-package :prover)))
    (prover::delete-all-tboxes)
    
    (load "dlmaps:ontology;ontology.lisp")

    (prover::set-current-tbox 'prover::oejendorf))

  #+:ignore
 (let ((*package* (find-package :racer-user)))
    (racer:delete-all-tboxes)
    (load "dlmaps:ontology;ontology.lisp")
    ;(load "ontology.lisp")

    (racer:set-current-tbox 'racer-user::oejendorf)))

 


  
  
  