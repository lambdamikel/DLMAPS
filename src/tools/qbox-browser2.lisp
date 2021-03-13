(in-package :TOOLS)

(defvar *node* nil)

(define-application-frame qbox-browser (dag-browser)
  ((show-equivalents-p :initform t)
   (variable-vectors-p :initform t)))

(defmethod accept-options ((frame qbox-browser) stream)
  
  (with-slots (show-equivalents-p 
               variable-vectors-p
               show-top-p show-bottom-p) frame
    
    (formatting-table (stream :multiple-columns t)
      (formatting-row (stream)
	(formatting-cell (stream)
	  (multiple-value-bind (object)
	      (accept 'boolean
		      :prompt "Show Equivalents"
		      :prompt-mode :raw
		      :stream stream 
		      :default show-equivalents-p)
	    (setf show-equivalents-p object)))

        (formatting-cell (stream)
	  (multiple-value-bind (object)
	      (accept 'boolean
		      :prompt "Show Variable Vectors"
		      :prompt-mode :raw
		      :stream stream 
		      :default variable-vectors-p)
	    (setf variable-vectors-p object)))

        (formatting-cell (stream)
	  (multiple-value-bind (object)
	      (accept 'boolean
		      :prompt "Show Top"
		      :prompt-mode :raw
		      :stream stream 
		      :default show-top-p)
	    (setf show-top-p object)))

        (formatting-cell (stream)
	  (multiple-value-bind (object)
	      (accept 'boolean
		      :prompt "Show Bottom"
		      :prompt-mode :raw
		      :stream stream 
		      :default show-bottom-p)
	    (setf show-bottom-p object)))))))

(defmethod draw-dag-node ((object thematic-substrate::query) stream)
  (with-slots (variable-vectors-p show-equivalents-p) *application-frame*
    (labels ((draw-it (stream)
               (let ((*print-right-margin* 40)
                     (*print-pretty* t))
              
                 (if show-equivalents-p 
                     (if variable-vectors-p 
                         (format stream "~A~%~A~%~A"
                                 (thematic-substrate::all-vois object) 
                                 (thematic-substrate::unparse-query object)
                                 (thematic-substrate::equivalents object))
                       (format stream "~A~%~A"
                               (thematic-substrate::unparse-query object)
                               (thematic-substrate::equivalents object)))
                   (if variable-vectors-p 
                       (format stream "~A~%~A"
                               (thematic-substrate::all-vois object) 
                               (thematic-substrate::unparse-query object))
                     (format stream "~A" 
                             (thematic-substrate::unparse-query object)))))))

      (with-output-as-presentation (stream object 'node)
        (if (not (thematic-substrate::unique-name-assumption-p object))
            (surrounding-output-with-border        
                (stream :shape :rectangle)
              (surrounding-output-with-border        
                  (stream :shape :rectangle)
                (draw-it stream)))
          (surrounding-output-with-border        
              (stream :shape :rectangle)
            (draw-it stream)))))))

