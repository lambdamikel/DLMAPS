;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SPATIAL-SUBSTRATE; Base: 10 -*-

(in-package spatial-substrate)

(defconstant +secret+ (intern (format nil "~A" (gensym "TEMP")) :cl-user))

;;;
;;;
;;;

(defun rcc-network-consistent-p (query conjuncts)
  (let* ((rcc-type (case (rcc-type query)
                     (:rcc8 +rcc8-rolebox+)
                     ;;; diese Klauseln werden momentan nicht 
                     ;;; angesprochen, weil durch das Rewriting
                     ;;; der map-queries alles auf RCC8 gebracht wird
                     ;;; der raeumliche Index unterstuetzt momentan 
                     ;;; nur RCC8
                     (:rcc5 +rcc5-rolebox+)
                     (:rcc3 +rcc3-rolebox+)
                     (:rcc2 +rcc2-rolebox+)
                     (:rcc1 +rcc1-rolebox+)
                     (otherwise +rcc8-rolebox+)))
         (temp 
          (create-substrate +secret+
                            :delete-if-exists-p t 
                            :type 'jepd-substrate 
                            :rbox rcc-type))
         
         (edge-class (get-standard-edge-description-class temp))
         (node-class (get-standard-node-description-class temp)))

    (dolist (conjunct conjuncts)      
      (let* ((from (textual-description (voi-from conjunct)))
             (to (textual-description (voi-to conjunct)))
             (node-from (find-node temp from :error-p nil))
             (node-to (find-node temp to :error-p nil)))
        (unless node-from 
          (create-node temp
                       from 
                     (make-node-description node-class from)                                          
                     :delete-if-exists-p nil
                     :create-reflexive-edges-p t
                     :error-p nil))
        (unless node-to 
          (create-node temp
                     to
                     (make-node-description node-class to)
                     :delete-if-exists-p nil
                     :create-reflexive-edges-p t
                     :error-p nil))      
        (create-edge temp
                     from
                     to 
                     (make-edge-description edge-class
                                            (mapcar #'(lambda (x)
                                                        (intern (format nil "~A" 
                                                                        (case x 
                                                                          (:borders :ec)
                                                                          (:bordered-by :ec)
                                                                          (otherwise x)))
                                                                :thematic-substrate))
                                                    (ensure-list (textual-description conjunct))))
                     :error-p nil
                     :create-inverse-p nil)))
    
    (add-missing-edges temp)
    (consistent-p temp :strict-p nil)))

;;;
;;;
;;;

(defmethod query-consistent-p ((query map-hybrid-and-query) &rest args)
  (let ((res (apply #'call-next-method query
                    :package
                    (if (slot-exists-p query 'racer-package)
                        (racer-package (substrate query))
                      (find-package :racer-user))
                    :tbox
                    (if (slot-exists-p query 'racer-tbox) 
                        (tbox (substrate query))
                      (racer::current-tbox))
                    args)))
    (if (eq res nil)
        nil
      (let ((rcc-network-consistent-p 
             (rcc-network-consistent-p 
              query 
              (remove-if-not #'is-map-rcc-edge-query-p (subqueries query)))))
        (if (eq rcc-network-consistent-p nil)            
            nil
          res)))))

;;;
;;;
;;;

(defmethod semantically-rewrite-query ((query map-hybrid-or-query) (parser generic-map-parser)
                                       &rest args)
  (call-next-method))


(defmethod semantically-rewrite-query ((query map-hybrid-and-query) (parser generic-map-parser)
                                       &rest args)
  (call-next-method))
