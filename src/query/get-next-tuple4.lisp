;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;;
;;;

(defmethod get-next-set-of-rule-consequences ((query query))
  (get-next-tuple query))

(defmethod get-next-tuple ((query query))
  (if (not (slot-value query 'query-satisfiable))
      :inconsistent
    (with-slots (process get-next-tuple-p 
                         bindings-found-p 
                         bindings-queue
                         current-bindings 
                         last-queue-item
                         substrate) query

      (labels ((wait-for-next-tuple ()
		                  
		 (process-wait 
                  (or (not process)
                      bindings-queue))

                 (when (runtime-error query)
                   (nrql-runtime-error (runtime-error query))))
	       	       
               (return-no-more-tuples ()
                 (if (timeout-p query)
                     :timeout

                   (progn 
                     (last-tuple-has-been-delivered query)
                     :exhausted))))

        ;;; (princ (bindings-queue query)) (terpri)

        (wait-for-next-tuple)
        
        (let ((res-tuple
             
               (if (not bindings-queue)
          
                   (return-no-more-tuples)
               
                 (let ((tuple
                        (pop bindings-queue)))
            
                   (unless bindings-queue
                     (setf last-queue-item nil))
            
                   (if (not (proactive-tuple-computation-p query))
              
                       (progn 
                         (setf get-next-tuple-p t)
                         tuple)

                     tuple)))))

          (setf current-bindings res-tuple)
              
          res-tuple)))))



(defmethod get-next-tuple ((query nrql-query))
  (if (not (slot-value query 'query-satisfiable))
      :inconsistent
    (with-slots (process get-next-tuple-p 
                         deliver-phase-two-warning-tokens-p
                         deliver-kb-has-changed-warning-tokens-p
                         two-phase-processing-p
                         bindings-found-p 
                         bindings-queue
                         current-bindings 
                         last-queue-item
                         substrate
                         kb-changed-token-delivered-p) query

      (labels ((wait-for-next-tuple ()
                 (process-wait (or (not process)
                                   bindings-queue))

                 (when (runtime-error query)
                   (nrql-runtime-error (runtime-error query))))
	       	       
               (return-no-more-tuples ()
                 (if (timeout-p query)
                     :timeout

                   (progn 
                     (last-tuple-has-been-delivered query)
                     :exhausted))))

        ;;; (princ (bindings-queue query)) (terpri)

        (if (and (not kb-changed-token-delivered-p)
                 deliver-kb-has-changed-warning-tokens-p
                 (not (query-accurate-p query)))
          
            (let ((token :warning-kb-has-changed))
              (setf kb-changed-token-delivered-p t)
              (setf current-bindings token)
              token)

          (progn 

            (wait-for-next-tuple)
      
            (let ((res-tuple
             
                   (if (not bindings-queue)
          
                       (return-no-more-tuples)
               
                     (let ((tuple
                            (pop bindings-queue)))
            
                       (unless bindings-queue
                         (setf last-queue-item nil))
            
                       (if (not (proactive-tuple-computation-p query))
              
                           (case tuple 
                             (:warning-expensive-phase-two-starts
                              (if (and deliver-phase-two-warning-tokens-p
                                       two-phase-processing-p)
                                  (progn
                                    (push :warning-expensive-phase-two-starts-2 bindings-queue)
                                    tuple)
                                (progn
                                  (setf get-next-tuple-p t)
                                  (get-next-tuple query))))
                       
                             (:warning-expensive-phase-two-starts-2
                              (setf get-next-tuple-p t)
                              (get-next-tuple query))
                       
                             (otherwise 
                              (setf get-next-tuple-p t)
                              tuple))
              
                         (case tuple 
                           (:warning-expensive-phase-two-starts
                      
                            ;;; im eager mode wird das Token nicht geliefert!
                      
                            (wait-for-next-tuple)
                 
                            (if bindings-queue
                     
                                (pop bindings-queue)
                   
                              (return-no-more-tuples)))
                           (otherwise tuple)))))))

              (setf current-bindings res-tuple)
            
              res-tuple)))))))
        
;;;
;;;
;;;

(defmethod next-tuple-available-p ((query query))
  (when (bindings-queue query)
    t))

(defmethod next-set-of-rule-consequences-available-p ((query query))
  (next-tuple-available-p query))

;;;
;;;
;;;

(defmethod get-current-tuple ((query query))
  (current-bindings query))

(defmethod get-current-set-of-rule-consequences ((query query))
  (current-bindings query))

  
