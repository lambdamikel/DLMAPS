;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SPATIAL-SUBSTRATE; Base: 10 -*-

(in-package spatial-substrate)

;;;
;;;
;;;

#+:midelora
(defmethod matches-p ((query map-simple-rcc-or-edge-query) (description midelora-map-edge-description))
  (member (change-package-of-description
           (prover::unparse (textual-description description))
           :keyword)
          (textual-description query)))
      
