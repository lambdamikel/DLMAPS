(in-package :TOOLS)

(defvar *node* nil)

(define-application-frame qbox-browser ()
  ((qbox :initarg :qbox
         :accessor qbox)
   (show-equivalents-p :initform t)
   (variable-vectors-p :initform t)   
   (show-top-p :initform t)   
   (show-bottom-p :initform t)   
   (app-stream :initform nil :accessor app-stream))
  (:panes
   (display :application
            :display-function 'draw-display
            :display-after-commands t)
   (info :application
         :scroll-bars :both
         :end-of-line-action :allow
         :end-of-page-action :allow
         :display-after-commands t)
   (options :accept-values
	    :scroll-bars nil
	    :display-function
	    `(accept-values-pane-displayer
	      :displayer ,#'(lambda (frame stream)
			      (accept-options frame stream)))))

  (:layouts
   (:defaults
    (vertically () 
      (1/10 options)           
      (5/10 display)
      (4/10 info)))))
 

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


(defmethod draw-display ((frame qbox-browser) stream)
  (with-slots (show-bottom-p show-top-p qbox) frame
    (format-graph-from-roots 
     (if show-top-p
         (list (thematic-substrate::top-query qbox))
       (thematic-substrate::children (thematic-substrate::top-query qbox)))                     
     #'draw-node
     #'(lambda (x) 
         (if (not show-bottom-p)
             (remove-if #'thematic-substrate::is-bottom-query-p 
                        (thematic-substrate::children x))
           (thematic-substrate::children x)))
     :center-nodes t
     :orientation :down
     :stream stream
     :graph-type :directed-acyclic-graph
     :merge-duplicates t
     :arc-drawer 
     #'(lambda (stream from-object
                       to-object x1 y1
                       x2 y2 
                       &rest
                       drawing-options)
         (declare (dynamic-extent
                   drawing-options))
         (declare (ignore from-object
                          to-object))
         (apply #'draw-arrow* stream
                x1 y1 x2 y2 drawing-options))
     :merge-duplicates t)
    (setf (app-stream frame) stream)))
  
(define-presentation-type node ())

(defun draw-node (object stream)
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
                  
                
                                   
  
(define-qbox-browser-command (exit :menu "Exit") ()
  (frame-exit *application-frame*))


(define-qbox-browser-command (inspect-node
                              :menu "Inspect"
                              :name "Inspect")
    ((node 'node :gesture :select))
  (let ((*standard-output* (get-frame-pane *application-frame* 'info))
        (*print-length* nil)
        (*print-depth* nil)
        (*print-pretty* t))
    (setf *node* node)
    (window-clear *standard-output*)
    (describe-object node *standard-output*)))

(defun qbox-browser (qbox &optional (port (find-port)))
  (let ((qbox-browser (make-application-frame 'qbox-browser
                        :frame-manager
                        (find-frame-manager :port port)
                        :width 800
                        :height 600
                        :qbox qbox)))
    (run-frame-top-level qbox-browser)))


(defun show-qbox (qbox)
  (when qbox 
    (qbox-browser qbox)))
