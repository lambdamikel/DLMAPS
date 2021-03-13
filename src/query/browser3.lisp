;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;; warum muss ich hier die Panes und das Layout erneut definieren?!
;;;

(defvar *node*)

(defconstant +qbox-browser-text-style+
  (make-text-style
   :sans-serif :roman :small))

(define-application-frame qbox-browser (dag-browser)
  ((show-equivalents-p :initform nil)
   (show-bodies-p :initform t)
   (variable-vectors-p :initform t)
   (orientation :accessor orientation :initform :down))

   
  (:panes
   (dag-display :application
                :text-style +qbox-browser-text-style+
                :display-function 'draw-dag-display
                :display-after-commands t)
   (dag-info :application
             :scroll-bars :both
             :end-of-line-action :allow
             :end-of-page-action :allow
             :display-after-commands t)
   (dag-options :accept-values
	        :scroll-bars nil
	        :display-function
	        `(accept-values-pane-displayer
	          :displayer ,#'(lambda (frame stream)
			          (accept-options frame stream)))))
  
  (:layouts
   (:defaults
    (vertically () 
      (1/10 dag-options)           
      (5/10 dag-display)
      (4/10 dag-info)))))

;;;
;;;
;;; 

(defmethod accept-options ((frame qbox-browser) stream)
  (with-slots (show-equivalents-p 
               show-bodies-p 
               variable-vectors-p
               show-top-p show-bottom-p
               orientation) frame
    
    (formatting-table (stream :multiple-columns t)
      (formatting-row (stream)
	(formatting-cell (stream)
	  (multiple-value-bind (object)
	      (accept 'clim:boolean
		      :prompt "Show Bodies"
		      :prompt-mode :raw
		      :stream stream 
		      :default show-bodies-p)
	    (setf show-bodies-p object)))

        (formatting-cell (stream)
	  (multiple-value-bind (object)
	      (accept 'clim:boolean
		      :prompt "Show Equivalents"
		      :prompt-mode :raw
		      :stream stream 
		      :default show-equivalents-p)
	    (setf show-equivalents-p object)))

        (formatting-cell (stream)
	  (multiple-value-bind (object)
	      (accept 'clim:boolean
		      :prompt "Show Variable Vectors"
		      :prompt-mode :raw
		      :stream stream 
		      :default variable-vectors-p)
	    (setf variable-vectors-p object)))

        (formatting-cell (stream)
	  (multiple-value-bind (object)
	      (accept 'clim:boolean
		      :prompt "Show Top"
		      :prompt-mode :raw
		      :stream stream 
		      :default show-top-p)
	    (setf show-top-p object)))

        (formatting-cell (stream)
	  (multiple-value-bind (object)
	      (accept 'clim:boolean
		      :prompt "Show Bottom"
		      :prompt-mode :raw
		      :stream stream 
		      :default show-bottom-p)
	    (setf show-bottom-p object)))
        
        (formatting-cell (stream)
          (multiple-value-bind (object)
              (accept 'completion
                      :prompt nil
                      :stream stream 
                      :default orientation
                      :view `(option-pane-view :items (:down :right)))
            (setf orientation object)))))))

;;;
;;;
;;;

(defmethod draw-dag-node ((object query) stream)
  (with-slots (variable-vectors-p 
               show-bodies-p show-equivalents-p) *application-frame*
    (labels ((draw-it (stream)
               (let ((*print-right-margin* 40)
                     (*print-pretty* t))
              
                 (if show-equivalents-p 
                     (if variable-vectors-p 
                         (format stream "~A~%~A~%~A"
                                 (all-vois object) 
                                 (if show-bodies-p 
                                     (unparse-query object)
                                   (iterator-id object))
                                 (equivalents object))
                       (format stream "~A~%~A"
                               (if show-bodies-p 
                                   (unparse-query object)
                                 (iterator-id object))
                               (equivalents object)))
                   (if variable-vectors-p 
                       (format stream "~A~%~A"
                               (all-vois object) 
                               (if show-bodies-p 
                                   (unparse-query object)
                                 (iterator-id object)))
                     (format stream "~A" 
                             (if show-bodies-p 
                                 (unparse-query object)
                               (iterator-id object))))))))
      
      (with-output-as-presentation (stream object (type-of object))
        (if t ;; (not (use-unique-name-assumption-p object))
            (surrounding-output-with-border        
                (stream :shape :rectangle)
              (surrounding-output-with-border        
                  (stream :shape :rectangle)
                (draw-it stream)))
          (surrounding-output-with-border        
              (stream :shape :rectangle)
            (draw-it stream)))))))

;;;
;;;
;;;

(defmethod visualize-dag ((dag qbox) &rest args)
  (apply #'call-next-method dag :type 'qbox-browser args))

;;;
;;; Commands - auch diese werden nicht vererbt?!
;;;

(define-qbox-browser-command (com-exit :menu "Exit") ()
  (frame-exit *application-frame*))

(define-qbox-browser-command (com-inspect-node
                              :menu nil)
    ((node 'query :gesture :select))
  
  (let ((stream (get-frame-pane *application-frame* 'dag-info)))
        
    (setf *node* node)
    
    (window-clear stream)
    
    (let ((slots 
                   
           '(iterator-id 
             answer-pattern
             all-vois 
                   
             substrate                   
                   
             query-satisfiable
             query-tautological
             entails
             entailed-by
             equivalents

             in-dnf-p 
             is-optimized-p 
             active-p 
             
             cache-bindings-p
             
             superset-cache-reference
             exact-cache-reference
             subset-cache-reference

             bindings-found-p)))
      
      (format stream "Query ~A is a ~A, with~%~%" node (type-of node))
      
      (dolist (slot slots)
        (format stream "~A~1,40T~A~%" slot (slot-value node slot)))
      (format stream "~%~%~A~%" (unparse-query node)))))

(define-qbox-browser-command (com-make-subqbox-from-node :menu nil)
    ((node 'query :gesture :select))
  (com-make-subdag-from-node node))

(define-qbox-browser-command (com-exchange-query-positions :menu nil)
    ((node 'query :gesture :select))
  (com-exchange-child-node-positions node))

