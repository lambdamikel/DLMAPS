;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(defrule initial-abox-saturation (dl-with-combined-some-all-rule abox)
  
  ;;; das muss so gemacht werden!
  ;;; Denn sonst ist z.B. die ABOx
  ;;;
  ;;; ((instance a (or (all r c) (all r d)))
  ;;;  (related a b r)
  ;;;  (instance b (and (not c) (not d))))
  ;;; 
  ;;; consistent, wegen combined-some-all-rule-p-
  ;;; Kopplung

  (let ((start-time2 nil))

    (labels ((do-it ()
               (perform (deterministic-expansion :language-type dl)
                 (:body 
                  (if clashes 
                      (handle-clashes)     
                    (perform (or-expansion)
                      (:positive 
                       (if clashes 
                           (handle-clashes)           
                         (do-it)))
                      (:negative 

                       (unless start-time2 (break "no start time2!"))
                     
                       (incf *time-spend-in-initial-abox-saturation* 
                             (- (get-internal-run-time)
                                start-time2))

                       +insert-body-code+)))))))
      #| 

;;; geht so nicht, weiteres Markierungsattribut erforderlich

    (every #'(lambda (node)
               (deactivate-all-nodes abox)

               (spreading-activation node)

               (if (zerop (1- (no-of-nodes abox)))
                   (progn 
                     (loop-over-abox-nodes (node abox)
                       (register-label-is-stable node))
                     +insert-body-code+)
                   
                 (let ((*combined-some-all-rule-p* nil))
                     
                   (perform (abox-enumeration)
                     (:body 
                      (do-it))
                     (:after-successful-clash-repair 
                      (do-it))))))

           (compute-cluster abox))

|# 

      (with-strategy (+abox-saturation-strategy+)
        (perform (abox-enumeration)
          (:body 
           (setf start-time2 (get-internal-run-time))
           (do-it))
          (:after-successful-clash-repair 
           (setf start-time2 (get-internal-run-time))
           (do-it)))))))
    
;;;
;;;
;;;

#|

(defmethod compute-cluster ((abox abox))
  ;;; fuer demnaechst

  (deactivate-all-nodes abox)

  (let ((cluster nil))
    (loop-over-abox-nodes (node abox)
      (unless (active-p node)
        (push node cluster)
        (spreading-activation node)))
  
    cluster))

(defmethod deactivate-all-nodes ((abox abox))
  (loop-over-abox-nodes (node abox)
    (when (active-p node)
      (deactivate node))))

(defmethod spreading-activation ((node abox-node))
  (unless (active-p node)

    (activate node)

    (dolist (out (outgoing node))
      (spreading-activation (to out)))

    (dolist (in (incoming node))
      (spreading-activation (from in)))))

|#

