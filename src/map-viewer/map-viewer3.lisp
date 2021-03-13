;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SPATIAL-SUBSTRATE; Base: 10 -*-

(in-package spatial-substrate)

(defvar *map-viewer-frame*)

(defvar *selected-object*)

;(defvar *kind-of-map* 'basic-explicit-racer-explicit-relations-map)

(defvar *kind-of-map* 'midelora-explicit-relations-map)

#+(or mcl lispworks)
(defmacro outl (&body body)
  `(spacing (:thickness 2)
     (outlining (:thickness 2)
       (spacing (:thickness 2)
         ,@body))))

;;;
;;; Text Styles and Colors etc.
;;;

(defconstant +highlight-thickness+ 10)

(defconstant +highlight-line-thickness+ 6)

(defconstant +gravity-field+ nil)

(defconstant +gravity-field-thickness+ 15)

(defconstant +gravity-field-thickness2+ 5)

(defconstant +info-text-style+
  (make-text-style :serif :roman :small))

(defconstant +text-height+ 10)

(defconstant +command-listener-text-style+
  (make-text-style
   :sans-serif :roman :normal))



(defconstant +bound-to-text-style+
  (parse-text-style '(:sans-serif :bold :LARGE)))

(defconstant +bound-to-text-ink+
  (make-rgb-color 0 0 0))

(defconstant +bound-to-ink+
  +yellow+)

(defconstant +map-viewer-text-style+
  (make-text-style :sans-serif
                   :roman
                   :normal))


(defconstant +non-map-object-color+
  (make-rgb-color 0.7 0.7 0.7))

(defconstant +non-map-object-bound-to-color+ 
  (make-rgb-color 0.8 0.8 0))

(defconstant +marker-color+
  (make-rgb-color 1.0 1.0 0.0))

(defconstant +text-color+
  (make-rgb-color 0 0 1))

(defconstant +grayed-item-ts+
  (parse-text-style '(:sans-serif nil :small)))

(defconstant +grayed-item-i+ 
  (make-gray-color 0.7))

(defconstant +selected-item-ts+
  (parse-text-style '(:sans-serif (:bold :italic) :small)))

(defconstant +selected-item-i+ 
  (make-gray-color 0.0))

(defconstant +unselected-item-ts+
  (parse-text-style '(:sans-serif :bold  :small)))

(defconstant +unselected-item-i+ 
  (make-gray-color 0.4))

;;;
;;;
;;;

(defvar *unknown-os-keys* nil)

;;;
;;;
;;;

(defparameter *highlight-bindings* t)

(defparameter *display-binding-names-mode* t)

(defparameter *warn-mode* nil)

(defparameter *solid-areas* t)

(defparameter *draw-contours* t)

(defparameter *autotracking-mode* t)

(defparameter *display-map-text-mode* nil)

(defparameter *display-nodes-mode* nil)

(defparameter *sensitive-objects-mode* t)

(defparameter *single-box-mode* nil)

(defparameter *use-query-processor* t)

(defparameter *display-only-bound-objects* nil)

;;;
;;;
;;;

(defvar *draw-overview* nil)

(defvar *draw-components* nil)

;;;
;;;
;;;

(defmacro with-pretty-map-output (&body body)
  `(let ((*display-map-text-mode* nil)
	 (*display-binding-names-mode* nil)
	 (*display-nodes-mode* nil)
	 (*sensitive-objects-mode* nil)
         (*highlight-bindings* nil)
         (*solid-areas* t)
         (*draw-contours* nil))
     ,@body))

;;;
;;;
;;;

(defmethod dont-display ((obj t))
  t)

;;;
;;;
;;;

(defmethod label-pos ((obj geom-thing))
  (values (x (centroid obj)) (y (centroid obj))))


(defmacro with-map-viewer-frame ((name) &body body)
  `(let ((,name *map-viewer-frame*))
     ,@body))

(defmethod highlighted-p ((obj si-geom-thing))
  (eq (bound-to obj) t))

;;;
;;:
;;;

(defmethod highlight ((obj t) &key &allow-other-keys)
  t)

(defmethod highlight ((obj si-geom-thing) &key components-p set-focus-p) 
  (setf (bound-to obj) t)
  (when components-p
    (highlight (get-direct-components obj) :components-p t))
  (when (and set-focus-p t)
    (with-map-viewer-frame (frame)
      (with-slots  (current-map-radius) frame        
        (typecase obj
          (bounding-box-mixin
           (set-current-map-position-and-radius (x (centroid obj))
                                                (y (centroid obj))                                      
                                                ;current-map-radius
                                                (radius obj)))
          (otherwise       
           (set-current-map-position-and-radius (x obj)
                                                (y obj)
                                                (/ current-map-radius 20))))))))


(defmethod highlight ((obj list) &key components-p)
  (dolist (obj obj)
    (highlight obj :components-p components-p)))

;;;
;;;
;;;

(defmethod unhighlight ((obj t) &key &allow-other-keys)
  t)

(defmethod unhighlight ((obj si-geom-thing) &key components-p)
  (setf (bound-to obj) nil)
  (when components-p
    (unhighlight (get-direct-components obj) :components-p t)))

(defmethod unhighlight ((obj list) &key components-p)
  (dolist (obj obj)
    (unhighlight obj :components-p components-p)))

;;;
;;;
;;;

(defun select (obj)
  (highlight (or (get-topmost-master obj)
                 obj)
             :components-p t))

(defun unselect (obj)
  (unhighlight (or (get-topmost-master obj)
                   obj)
               :components-p t))

;;;
;;;
;;;

(define-presentation-type os-key-item ())

(define-presentation-type current-map-range ())

;;;
;;;
;;;

(define-gesture-name :select-os-key-item :pointer-button :left)

(define-gesture-name :unselect-os-keys-of-object :pointer-button (:left :shift))

(define-gesture-name :unselect-all-but-os-keys-of-object :pointer-button (:left :control))
  
(define-gesture-name :highlight-os-keys-of-object :pointer-button :middle)
  
(define-gesture-name :hide-object :pointer-button (:left :meta))

(define-gesture-name :adjust-map-center :pointer-button (:left))

(define-gesture-name :adjust-map-extent :pointer-button (:shift :left))

(define-gesture-name :full-map-extent :pointer-button :middle)

(define-gesture-name :select-object :pointer-button (:shift :right))

(define-gesture-name :describe-object :pointer-button :left)


;;;
;;;
;;;

(define-command-table file-table
                      :menu (("Install Current Map" :command (com-install-as-current-mapviewer-map))
                             ("divide1" :divider nil)	                     
                             ("Load SQD Map" :command (com-load-sqd-map))
	                     ("divide2" :divider nil)
	                     ("Load Map" :command (com-load-map))
	                     ("Save Map As" :command (com-save-map-as))
                             ("Save Map" :command (com-save-map))
                             ("divide3" :divider nil)
                             ("Restart Inspector" :command (com-restart-inspector))
	                     ("divide4" :divider nil)
	                     ("Quit" :command (com-quit))))


(define-command-table map-table
                      :menu (("Show Map Infos" :command (com-show-map-infos))
                             ("divide1" :divider nil)
                             ("Redraw Map" :command (com-redraw))                             
                             ("Clear Display" :command (com-clear-display))
	                     ("Reset Map" :command (com-reset-map))
	                     ("divide2" :divider nil)
                             ("Unhighlight" :command (com-unhighlight))
                             ("Invert Highlighted" :command (com-invert-highlighted))
                             ("Highlight in Current Range" :command (com-highlight-objects-in-current-range))
                             ("Highlight Hidden Objects" :command (com-highlight-hidden-objects))
	                     ("divide3" :divider nil)                                               
                             ("Delete Highlighted Objects" :command (com-delete-highlighted-objects))                      
                             ("divide4" :divider nil)                  
                             ("Crop Map" :command (com-crop-map))
                             ("Close Roles" :command (com-close-roles))
                             ("Classify Map" :command (com-classify-map))))


(define-command-table keys-table
                      :menu (("Undo Nothing" :command (com-undo))
	                     ("divide1" :divider nil)
	                     ("Lookup Key" :command (com-lookup-os-key))
	                     ("Select All Keys" :command (com-highlight-all-os-keys))
	                     ("Unselect All Keys" :command (com-unselect-all-os-keys))))

(define-command-table spatial-table
                      :menu (("Highlight Intersecting Objects" :command (com-highlight-intersecting-objects*))
	                     ("Highlight Touching Objects" :command (com-highlight-touching-objects*))
                             ("Highlight Contained Objects" :command (com-highlight-contained-objects*))
                             ("Highlight Strictly Contained Objects" :command (com-highlight-strictly-contained-objects*))
	                     ("Highlight Convered Objects" :command (com-highlight-covered-objects*))                             
                             ("Highlight Overlaping Objects" :command (com-highlight-overlapping-objects*))
                             ("Highlight Bordered Objects" :command (com-highlight-bordered-objects*))))

;;;
;;;
;;;

#+:allegro
(defun generate-name-for-undo-command ()  
  (remove-command-from-command-table 
   'com-undo 'keys-table)
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (unselect-stack) map-viewer-frame
      (let ((first (first unselect-stack)))
	(if first
	    (add-command-to-command-table 'com-undo
					  'keys-table
					  :menu (list 
						 (if (typep first 'map-object)
						     (format nil "Undo Hide ~A"
							     (id first))
						   "Undo Last Selection")
						 :after :start))
	  (add-command-to-command-table 'com-undo
					'keys-table
					:menu (list 
					       "Undo Nothing"
					       :after :start)))))))

#+:lispworks
(defun generate-name-for-undo-command ()  
  t)

(defun register-selected-keys-for-undo ()
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (unselect-stack selected-os-keys) map-viewer-frame	
      (push (copy-list selected-os-keys) unselect-stack)
      (generate-name-for-undo-command)
      (redisplay-frame-pane map-viewer-frame 'os-selector))))


(defun register-hide-for-undo (object)
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (unselect-stack) map-viewer-frame	
      (push object unselect-stack)
      (generate-name-for-undo-command))))

;;;
;;;
;;;

(defclass overview-pane
          (application-pane)
  ())

(defclass display-pane
          (application-pane)
  ())


(define-application-frame map-viewer ()
  ( (map :initform nil)
    
    (unselect-stack :initform nil)
    
    (current-map-range :initform nil :accessor current-map-range)
    
    (current-map-radius :initform 100)
    (current-map-position :initform nil)
    (current-map-transformation :initform nil)

    (overview-pattern :initform nil)
    (old-overview-vp-width :initform nil)
    (old-overview-vp-height :initform nil)    
    (overview-transformation :initform nil)

    (present-os-keys :initform nil)
    (selected-os-keys :initform nil))
  
  (:command-table (map-viewer
		   :inherit-from (file-table map-table keys-table spatial-table)
		   :menu (("File" :menu file-table)
			  ("Map" :menu map-table)
			  ("Key Control" :menu keys-table)
			  ("Spatial Querying" :menu spatial-table))))

  (:panes         
   
   (display (make-pane 'display-pane		       
		       :scroll-bars nil
		       :end-of-line-action :allow
		       :end-of-page-action :allow
		       :textcursor nil))

   (button-undo (make-pane 'push-button
			   :label "Undo"	
			   :activate-callback 'button-undo))

   (button-unhighlight (make-pane 'push-button
                                  :label "Unhighlight"
                                  :activate-callback 'button-unhighlight))

   (button-clear (make-pane 'push-button
			    :label "Clear"			   
			    :activate-callback 'button-clear))
   
   (button-redraw (make-pane 'push-button
			     :label "Redraw"			   
			     :activate-callback 'button-redraw))

   (button-infos (make-pane 'push-button
			    :label "Infos"			   
			    :activate-callback 'button-infos))
   
   (button-reset (make-pane 'push-button
			    :label "Reset"
			    :activate-callback 'button-reset))
   
   (button-delete (make-pane 'push-button
                             :label "Delete"			   
                             :activate-callback 'button-delete))
   
   (overview (make-pane 'overview-pane			
			:scroll-bars nil
			:end-of-line-action :allow
			:end-of-page-action :allow
			:textcursor nil
                        :display-after-commands nil
			:display-function #'draw-overview))
   
   (infos :application
	  :label "Map Infos"
	  :textcursor nil
	  :end-of-line-action :allow
	  :end-of-page-action :allow
	  :text-style +info-text-style+
	  :scroll-bars :both)
   
   (coordinates :application
		:label "Map Coordinates"
		:textcursor nil
		:end-of-line-action :allow
		:end-of-page-action :allow
		:text-style +info-text-style+
		:scroll-bars nil		
		:display-function #'show-coordinates)

   (buttons :accept-values
	    :scroll-bars nil
 	    :min-height :compute :height :compute :max-height :compute 
	    :display-function
	    `(accept-values-pane-displayer
	      :displayer ,#'(lambda (frame stream)
			      (accept-buttons
			       frame stream))))

   (os-selector :application
		:incremental-redisplay t
		:scroll-bars :both
		:end-of-line-action :allow
		:end-of-page-action :allow
		:display-function #'accept-os-selection)
   
   (type :accept-values
         :scroll-bars nil
         :min-height :compute :height :compute :max-height :compute 
         
         :display-function
         `(accept-values-pane-displayer
           :displayer ,#'(lambda (frame stream)
                           (declare (ignore frame))
                           (multiple-value-bind (object ptype changed)
                               (accept 'completion
                                       :query-identifier 'kind-of-map
                                       :prompt nil
                                       :prompt-mode :raw
                                       :stream stream 
                                       :default *kind-of-map*
                                       :view `(option-pane-view :items
                                                                ,(reverse
                                                                  (remove-if-not (lambda (x) (search "MIDELORA" x)) 
                                                                                (mapcar #'class-name
                                                                                        (clos:class-direct-subclasses
                                                                                         (find-class 'final-class))) :key #'symbol-name))))
                             (declare (ignore ptype))	    
                             (when changed (setf *kind-of-map* object))))))
   
   (command :interactor
	    :label nil
	    :text-style +command-listener-text-style+
	    :scroll-bars :vertical
	    :min-height '(4 :line)
	    :max-height '(4 :line)
	    :height '(4 :line))
   
   (pointer-documentation-pane
    (make-clim-stream-pane :type 'pointer-documentation-pane
      :foreground +white+
      :background +black+
      :text-style (make-text-style
                   :sans-serif :bold :small)
      :scroll-bars nil
      :min-height '(1 :line)
      :max-height '(1 :line)
      :height '(1 :line))))
  
  (:layouts
   (:default
    (vertically ()	 
      #+(or allegro lispworks) (outl buttons)
      #-(or allegro lispworks) (outl buttons)
      (horizontally ()
        #+(or allegro lispworks)
        (1/4 (spacing (:thickness 2)
               (outl os-selector)))
        #-(or allegro lispworks)
        (1/4 (spacing (:thickness 2)
               (outl os-selector)))
        (2/4
         (spacing (:thickness 2)
           #+allegro
           (vertically ()
             (outlining ()
               (labelling (:label "Current Range")
                 display))
             (30 (horizontally ()
                   (1/7 button-undo)		    
                   (1/7 button-unhighlight)		    
                   (1/7 button-delete)
                   (1/7 button-clear)
                   (1/7 button-reset)
                   (1/7 button-redraw)
                   (1/7 button-infos))))
           #-allegro
           (vertically ()
             (outl (labelling (:label "Current Range")
                     display))
             (horizontally ()
               (1/7 button-undo)		    
               (1/7 button-unhighlight)		    
               (1/7 button-clear)
               (1/7 button-reset)
               (1/7 button-redraw)
               (1/7 button-delete)
               (1/7 button-infos)))))
        (1/4 
         (spacing (:thickness 2) 
           #+allegro
           (vertically ()
             (1/15 type)
             (2/15 coordinates)
             (6/15 (outlining ()
                     (labelling (:label "Map Overview")
                       overview)))
             (6/15 infos))
           #-allegro
           (vertically ()
             (1/15 (outl type))
             (2/15 (outl coordinates))
             (6/15 (outl 
                     (labelling (:label "Map Overview")
                       overview)))
             (6/15 (outl infos))))))
      
      #-lispworks
      command
      #+lispworks
      (outl command)
      #+allegro
      pointer-documentation-pane
      #+mcl
      (outl pointer-documentation-pane)))))


;;;
;;;
;;;

(defun button-unhighlight (button)
  (declare (ignore button))
  (com-unhighlight))

(defun button-undo (button)
  (declare (ignore button))
  (com-undo))

(defun button-clear (button)
  (declare (ignore button))
  (com-clear-display))

(defun button-redraw (button)
  (declare (ignore button))
  (com-redraw))

(defun button-infos (button)
  (declare (ignore button))
  (com-show-map-infos))

(defun button-reset (button)
  (declare (ignore button))
  (com-reset-map))

(defun button-delete (button)
  (declare (ignore button))
  (com-delete-highlighted-objects))


;;;
;;;
;;;

(define-map-viewer-command (com-reset-map :name "Reset Map")
    ()  
  (reset-map-range)
  (com-redraw))

(define-map-viewer-command (com-unhighlight :name "Unhighlight")
    ()  
  (unhighlight-all)
  (com-redraw))

(defun unhighlight-all ()
  (with-map-viewer-frame (frame)
    (with-slots (map) frame
      (when map
        (unhighlight (elements (spatial-index map)))))))

;;;
;;;
;;;

(define-map-viewer-command (com-undo :name "Undo")
    ()
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (unselect-stack selected-os-keys) map-viewer-frame	
      (let ((first (pop unselect-stack)))
	(when first
	  (cond ((typep first 'si-geom-thing) ; undo hide object
		 (setf (dont-display first) nil)
		 (com-clear-display)
		 (com-draw-current-map))
		(t			
		 (setf selected-os-keys first)))
	  (generate-name-for-undo-command)
	  (com-draw-current-map))))))


(defmethod accept-buttons ((frame map-viewer) stream)
  (let ((changed nil))
    
    (formatting-table (stream :x-spacing '(2 :character))
      
      (formatting-row (stream)
	
	(formatting-cell (stream :align-x :center)
	  (multiple-value-bind (bool ptype changed1)
	      (accept 'boolean
		      :prompt "Autotracking"
		      :stream stream :default *autotracking-mode*		    
		      :query-identifier 'autotracking)
	    (declare (ignore ptype))
	    (setf changed (or changed changed1))
	    (when changed1 (setf *autotracking-mode* bool))))
	

	(formatting-cell (stream :align-x :center)	
	  (multiple-value-bind (bool ptype changed4)
	      (accept 'boolean
		      :prompt "Sensitive"
		      :stream stream :default *sensitive-objects-mode*		    
		      :query-identifier 'sensitive-objects-mode)
	    (declare (ignore ptype))
	    (setf changed (or changed changed4))
	    (when changed4 (setf *sensitive-objects-mode* bool))))
		
	(formatting-cell (stream :align-x :center)
	  (multiple-value-bind (bool ptype changed2)
	      (accept 'boolean
		      :prompt "Text"
		      :stream stream :default *display-map-text-mode*		    
		      :query-identifier 'display-text)
	    (declare (ignore ptype))
	    (setf changed (or changed changed2))	
	    (when changed2 (setf *display-map-text-mode* bool))))
	
	(formatting-cell (stream :align-x :center)
	  (multiple-value-bind (bool ptype changed3)
	      (accept 'boolean
		      :prompt "Nodes"
		      :stream stream :default *display-nodes-mode*		    
		      :query-identifier 'display-nodes)
	    (declare (ignore ptype))
	    (setf changed (or changed changed3))	
	    (when changed3 (setf *display-nodes-mode* bool))))

	
        (formatting-cell (stream :align-x :center)	
          (multiple-value-bind (bool ptype changed6)
              (accept 'boolean
                      :prompt "Contours"
                      :stream stream :default *draw-contours*		    
                      :query-identifier 'contours)
            (declare (ignore ptype))	    
            (setf changed (or changed changed6))
            (when changed6 (setf *draw-contours* bool))))

	(formatting-cell (stream :align-x :center)
	  (multiple-value-bind (bool ptype changed8)
	      (accept 'boolean
		      :prompt "Areas"
		      :stream stream :default *solid-areas*		    
		      :query-identifier 'solid-areas)
	    (declare (ignore ptype))
	    (setf changed (or changed changed8))
	    (when changed8 (setf *solid-areas* bool))))
	
	(formatting-cell (stream :align-x :center)
	  (multiple-value-bind (bool ptype changed9)
	      (accept 'boolean
		      :prompt "Highlight Bound"
		      :stream stream :default *highlight-bindings*
		      :query-identifier 'highlight-bindings)
	    (declare (ignore ptype))	    
	    (setf changed (or changed changed9))
	    (when changed9 (setf *highlight-bindings* bool))))

	(formatting-cell (stream :align-x :center)
	  (multiple-value-bind (bool ptype changed10)
	      (accept 'boolean
		      :prompt "Show Bindings"
		      :stream stream :default *display-binding-names-mode*
		      :query-identifier 'display-bindings)
	    (declare (ignore ptype))	    
	    (setf changed (or changed changed10))
	    (when changed10 (setf *display-binding-names-mode* bool))))
	

	(formatting-cell (stream :align-x :center)	
	  (multiple-value-bind (bool ptype changed5)
	      (accept 'boolean
		      :prompt "Draw Only Bound"
		      :stream stream :default *display-only-bound-objects*
		      :query-identifier 'display-only-bound-objects)
	    (declare (ignore ptype))
	    (setf changed (or changed changed5))
	    (when changed5 (setf *display-only-bound-objects* bool))))

	(formatting-cell (stream :align-x :center)	
	  (multiple-value-bind (bool ptype changed11)
	      (accept 'boolean
		      :prompt "Use Query Processor"
		      :stream stream :default *use-query-processor*
		      :query-identifier 'use-query-processor)
	    (declare (ignore ptype))	    
	    (setf changed (or changed changed11))
	    (when changed11 (setf *use-query-processor* bool))))

	(when (and changed *autotracking-mode*)
	  (com-draw-current-map))))))

(defmethod accept-os-selection ((frame map-viewer) stream)
  (with-map-viewer-frame (map-viewer-frame)        
    (let ((y 0))
      (flet ((set-item (item)
	       (let ((item (lookup-os item)))
		 (draw-text* stream 
			     (format nil "~A ~A" (first item) (third item))
			     0 y)))
	     (set-unknown-item (item)               
	       (draw-text* stream 
			   (format nil "??? ~A ???" item)
			   0 y)))
	
	(with-slots (present-os-keys selected-os-keys) map-viewer-frame
	  (dolist (item *unknown-os-keys*)
	    (incf y 14)
	    (let ((selected (member item selected-os-keys :test #'equal)))	    	
	      (updating-output (stream :unique-id item
				       :cache-value (list y selected)
				       :cache-test #'equal)
		(with-output-as-presentation 
		    (stream item 'os-key-item)
		  (if selected
		      (with-drawing-options (stream :text-style +selected-item-ts+
						    :ink +selected-item-i+)
			(set-unknown-item item))
		    (with-drawing-options (stream :text-style +unselected-item-ts+
						  :ink +unselected-item-i+)
		      (set-unknown-item item)))))))
	  
	  (dolist (item *os*)
	    (let ((item (second item)))
	      (incf y 14)
	      (let ((selected (member item selected-os-keys :test #'equal)))
		(updating-output (stream :unique-id item
					 :cache-value (list y selected) 
					 :cache-test #'equal)
		  (if (member item present-os-keys :test #'equalp)
		      (with-output-as-presentation 
			  (stream item 'os-key-item)
			(if selected
			    (with-drawing-options (stream :text-style +selected-item-ts+
							  :ink +selected-item-i+)
			      (set-item item))
			  (with-drawing-options (stream :text-style +unselected-item-ts+
							:ink +unselected-item-i+)
			    (set-item item))))
		    (with-drawing-options (stream :text-style +grayed-item-ts+
						  :ink +grayed-item-i+)	    
		      (set-item item))))))))))))

;;;
;;;
;;;

(define-map-viewer-command (com-highlight-os-key-item :name "Select Key")
    ((object 'os-key-item))  
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (present-os-keys selected-os-keys) map-viewer-frame	
      (register-selected-keys-for-undo)      
      (setf selected-os-keys	
	    (if (member object selected-os-keys :test #'equalp)
	        (remove object selected-os-keys :test #'equalp)
	      (cons object selected-os-keys)))
      (when *autotracking-mode*
	(com-draw-current-map)))))

(define-presentation-to-command-translator select-os-key-item
    (os-key-item com-highlight-os-key-item map-viewer
		 :gesture :select-os-key-item)		   
    (object)
  (list object))

;;;
;;;
;;;

(define-map-viewer-command (com-highlight-all-os-keys :name "Select All Keys")
    ()
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (present-os-keys selected-os-keys) map-viewer-frame
      (register-selected-keys-for-undo)
      (setf selected-os-keys (copy-tree present-os-keys))
      (com-draw-current-map))))


(define-map-viewer-command (com-unselect-all-os-keys :name "Unselect All Keys")
    ()
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (present-os-keys selected-os-keys) map-viewer-frame	
      (register-selected-keys-for-undo)
      (setf selected-os-keys nil)
      (com-draw-current-map))))

;;;
;;;
;;;


(define-map-viewer-command (com-unselect-os-keys-of-object :name "Unselect OS Keys Of Object")
    ((object 'map-object))  
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (present-os-keys selected-os-keys) map-viewer-frame	
      (register-selected-keys-for-undo)
      (dolist (os (all-os object))
	(setf selected-os-keys
	      (delete os selected-os-keys :test #'equalp)))
      (com-draw-current-map))))

(define-map-viewer-command (com-unselect-all-but-os-keys-of-object :name "Unselect All But OS Keys Of Object")
    ((object 'map-object))
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (present-os-keys selected-os-keys) map-viewer-frame
      (register-selected-keys-for-undo)
      (setf selected-os-keys
	    (all-os object))
      (com-draw-current-map))))


(define-presentation-to-command-translator unselect-os-keys-of-object
    (map-object com-unselect-os-keys-of-object map-viewer
	        :gesture :unselect-os-keys-of-object)
    (object)
  (list object))

(define-presentation-to-command-translator unselect-all-but-os-keys-of-object
    (map-object com-unselect-all-but-os-keys-of-object map-viewer
	        :gesture :unselect-all-but-os-keys-of-object)
    (object)
  (list object))


;;;
;;;
;;;


(define-map-viewer-command (com-highlight-os-keys-of-object :name "Highlight OS Keys Of Object")
    ((object 'map-object))
  (with-map-viewer-frame (map-viewer-frame)
    (let ((os (all-os object)))
      (with-slots (map) map-viewer-frame	
        (dolist (obj (objects map))
          (when (intersection (all-os obj) os)
            (if (highlighted-p obj)
                (unselect obj)
              (select obj)))))))
  (com-draw-current-map))



(define-presentation-to-command-translator highlight-os-keys-of-object
    (map-object com-highlight-os-keys-of-object map-viewer
	        :gesture :highlight-os-keys-of-object)
    (object)
  (list object))

;;;
;;;
;;;

(define-map-viewer-command (com-lookup-os-key :name "Lookup OS Key")
    ()  
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (unselect-stack) map-viewer-frame	
      (let* ((stream (get-frame-pane map-viewer-frame 'display))
	     (os-number
	      (accepting-values (stream :own-window t :label "Lookup OS Key:")
		(accept 'integer :stream stream :view 'gadget-dialog-view))))
	(window-clear *standard-output*)
	(terpri)
	(format t "  ~A~%" 
		(lookup-os os-number))))))

;;;
;;;
;;;

(define-map-viewer-command (com-hide-object :name "Hide Object")
    ((object 'si-geom-thing))
  (register-hide-for-undo object) 
  (setf (dont-display object) t)
  (com-clear-display)
  (com-draw-current-map))

(define-presentation-to-command-translator hide-object
    (si-geom-thing com-hide-object map-viewer
	           :gesture :hide-object)   
    (object)
  (list object))

;;;
;;;
;;;


(defun show-current-map-range (&key (record t)
				    (coordinates t)
				    stream
				    transformation)
  (with-map-viewer-frame (frame)
    (let ((stream (or stream (get-frame-pane frame 'overview))))
      (with-slots  (map overview-transformation
		        current-map-range 
		        current-map-position current-map-radius) frame
	(flet ((draw-it ()
                 (with-correct-clipping (stream)
                   (with-drawing-options (stream :transformation 
                                                 (or transformation overview-transformation))
		   
                     (draw-circle* stream
                                   (first current-map-position)
                                   (second current-map-position)
                                   (- current-map-radius 4)
                                   :ink +flipping-ink+
                                   :line-thickness 2
                                   :filled nil)
		   
                     (draw-rectangle* stream
                                      (x (pmin current-map-range))
                                      (y (pmin current-map-range))
                                      (x (pmax current-map-range))
                                      (y (pmax current-map-range))			  
                                      :line-thickness 2
                                      :ink +flipping-ink+
                                      :filled t)))))
	  
	  (when map
	    (when coordinates 
	      (show-coordinates frame (get-frame-pane frame 'coordinates)))
	    (if record
		(with-output-as-presentation (stream current-map-range
						     'current-map-range)
		  (draw-it))
	      (with-output-recording-options (stream :draw t :record nil)
		(draw-it)))))))))

(define-map-viewer-command (com-adjust-map-center :name "Adjust Map Center")
    ((object 'current-map-range))
  (declare (ignore object))
  (adjust-map-center))

(define-map-viewer-command (com-adjust-map-extent :name "Adjust Map Extent")
    ((object 'current-map-range))
  (declare (ignore object))
  (adjust-map-extent))

(define-map-viewer-command (com-full-map-extent :name "Full Map Extent")
    ((object 'current-map-range))
  (declare (ignore object))
  (full-map-range))

(define-presentation-to-command-translator adjust-map-center
    (current-map-range com-adjust-map-center map-viewer
		       :gesture :adjust-map-center)
    (object)
  (list object))


(define-presentation-to-command-translator adjust-map-extent
    (current-map-range com-adjust-map-extent map-viewer
		       :gesture :adjust-map-extent)
    (object)
  (list object))

(define-presentation-to-command-translator full-map-extent
    (current-map-range com-full-map-extent map-viewer
		       :gesture :full-map-extent)
    (object)
  (list object))


(defmethod set-current-map-position-and-radius (xcenter ycenter r)
  (with-map-viewer-frame (frame)
    (with-slots (current-map-position current-map-radius) frame
      (setf current-map-position 
	    (list xcenter ycenter))
      (setf current-map-radius r))
    (let ((*autotracking-mode* nil))
      (adjust-current-map-range))))

(defun full-map-range ()
  (with-map-viewer-frame (frame)
    (with-slots (map old-overview-vp-width 
		     old-overview-vp-height) frame
      (when map
        (with-slots (xmin ymin xmax ymax) (spatial-index map)
          (set-current-map-position-and-radius 
           (floor (+ xmin xmax) 2)
           (floor (+ ymin ymax) 2)
           (- xmax (floor (+ xmin xmax) 2)))
          (when *autotracking-mode* 
            (adjust-current-map-range)))))))


(defun reset-map-range ()
  (with-map-viewer-frame (frame)
    (with-slots (map current-map-radius current-map-position) frame
      (when map 
	(with-slots (xmin ymin xmax ymax) (spatial-index map)
	  (setf current-map-radius (floor (- xmax xmin) 5))
	  (setf current-map-position
	        (list (floor (+ xmin xmax) 2)
		      (floor (+ ymin ymax) 2)))
	  (let ((*autotracking-mode* nil))
	    (adjust-current-map-range)))))))

(defun adjust-current-map-range ()
  (with-map-viewer-frame (frame)
    (with-slots (current-map-range 
		 current-map-position current-map-radius) frame
      (let ((xcenter (first current-map-position))
	    (ycenter (second current-map-position)))
	
	(if (not current-map-range)
	    (setf current-map-range
	          (make-bounding-box 
	           (- xcenter current-map-radius) (- ycenter current-map-radius)
	           (+ xcenter current-map-radius) (+ ycenter current-map-radius)))
	  (with-slots (pmin pmax) current-map-range
	    (setf (x pmin) (- xcenter current-map-radius) 
		  (y pmin) (- ycenter current-map-radius)
		  (x pmax) (+ xcenter current-map-radius) 
		  (y pmax) (+ ycenter current-map-radius))))
	
	(recalculate-transformation (get-frame-pane frame 'display))
	
	(when *autotracking-mode*
	  (com-draw-current-map))))))


(defun adjust-map-center ()  
  (with-map-viewer-frame (map-viewer-frame)
    (let ((stream (get-frame-pane map-viewer-frame 'overview)))
      (with-slots (current-map-position 
		   map
		   old-overview-vp-width 
		   old-overview-vp-height) map-viewer-frame  
	
	(when map
	  
	  (block track-pointer
	    (with-output-recording-options (stream :draw t :record nil)
	      (tracking-pointer (stream)
		(:pointer-motion (x y)
                 (with-slots (xmin ymin xmax ymax) (spatial-index map)
                   (let* ((xscale (/ old-overview-vp-width  (- xmax xmin)))
                          (yscale (/ old-overview-vp-height (- ymax ymin)))
                          (xcenter	     
                           (+ xmin (* (/ 1 xscale) x)))
                          (ycenter
                           (+ ymax (* (- (/ 1 yscale)) y))))
                     (show-current-map-range :record nil)
                     (setf current-map-position 
                           (list xcenter ycenter))
                     (adjust-current-map-range)				 				 
                     (show-current-map-range :record nil))))
		(:pointer-button-press ()
                 (return-from track-pointer))))))))))


(defun adjust-map-extent ()  
  (with-map-viewer-frame (map-viewer-frame)
    (let ((stream (get-frame-pane map-viewer-frame 'overview)))
      (with-slots (current-map-radius
		   current-map-position
		   map
		   old-overview-vp-width 
		   old-overview-vp-height) map-viewer-frame  
	
	(when map
	  (block track-pointer
	    (with-output-recording-options (stream :draw t :record nil)
	      (tracking-pointer (stream)
		(:pointer-motion (x y)			       
                 (with-slots (xmin ymin xmax ymax) (spatial-index map)
                   (let* ((xscale (/ old-overview-vp-width  (- xmax xmin)))
                          (yscale (/ old-overview-vp-height (- ymax ymin)))
                          (xcenter (first current-map-position))
                          (ycenter (second current-map-position))
                          (xr	     
                           (- xcenter (+ xmin (* (/ 1 xscale) x))))
                          (yr		       
                           (- ycenter (+ ymax (* (- (/ 1 yscale)) y))))
                          (r (sqrt (+ (* xr xr) (* yr yr)))))
                     (unless (zerop r)
                       (show-current-map-range :record nil)
                       (setf current-map-radius r)
                       (adjust-current-map-range)					       
                       (show-current-map-range :record nil)))))
		(:pointer-button-press ()			
                 (return-from track-pointer))))))))))


(defmethod show-coordinates ((frame map-viewer) stream)
  (with-slots (current-map-range map) frame
    (when map
      (window-clear stream)
      ;(terpri stream)
      (with-slots (xmin ymin xmax ymax) (spatial-index map)
	(format stream "  Full Map: (~A,~A) - (~A,~A)"
		(round xmin) (round ymin)
		(round xmax) (round ymax)))
      (terpri stream)
      (with-slots (pmin pmax) current-map-range
	(format stream "  Map Range: (~A,~A) - (~A,~A)"
		(round (x pmin)) (round (y pmin))
		(round (x pmax)) (round (y pmax)))
	(terpri stream)
	(let ((size (abs (round (- (x pmin) (x pmax))))))
	  (format stream "  Range: ~A * ~A meters" size size))))))

;;;
;;;
;;;

(defmethod selectedp ((object map-object))
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (selected-os-keys) map-viewer-frame
      (some #'(lambda (os)
                (member os selected-os-keys :test #'equal))
            (all-os object)))))

(defmethod selectedp ((obj si-geom-thing))
  t)

;;;
;;;
;;;

(defmethod draw-current-map ((frame map-viewer) stream &key 
                             overview 
                             fast-p
                             (clear-p t))
  (with-slots (current-map-range
	       current-map-transformation
	       selected-os-keys map) frame    
    (with-slots (pmin pmax) current-map-range

      (let ((*draw-overview* overview)
            (*draw-components* nil))

        (labels ((draw-it (cur-obj)
                   (when (not (dont-display cur-obj))
                     (if (and (=> (not *display-only-bound-objects*)
                                  (selectedp cur-obj))
                              (=> *display-only-bound-objects*
                                  (bound-to cur-obj)))
                             
                         (draw cur-obj stream)
                     
                       (when *warn-mode* (warn "~%Object not drawn! ~A" cur-obj))))))

          (when clear-p (window-clear stream))
          (when map
            (with-correct-clipping (stream)
              (with-drawing-options (stream :transformation current-map-transformation)
                (with-selected-objects (cur-obj current-map-range
                                                (and (typep cur-obj 'si-geom-polygon) 
                                                     (primary-p cur-obj)
                                                     (not (bound-to cur-obj)))
                                                :intersects)
                  (draw-it cur-obj))

                ;;;
                ;;; highlighted objects liegen immer "oben"!
                ;;;
            	  
                (when fast-p 
              
                  ;;; ausser den Polygonen werden nur noch die
                  ;;; gehighlighteten gemalt ("fast")              

                  (with-selected-objects (cur-obj current-map-range
                                                  (bound-to cur-obj)
                                                  :intersects)
                    (draw-it cur-obj)))

                (unless fast-p

                  (with-selected-objects (cur-obj current-map-range
                                                  (and (typep cur-obj 'si-geom-polygon) 
                                                       (primary-p cur-obj)
                                                       (bound-to cur-obj))
                                                  :intersects)
                    (draw-it cur-obj))
              
                  (with-selected-objects (cur-obj current-map-range
                                                  (and (not (typep cur-obj 'si-geom-polygon))
                                                       (primary-p cur-obj))
                                                  :intersects)
                    (draw-it cur-obj))

                  (when *draw-contours*              
                  (with-selected-objects (cur-obj current-map-range
                                                  (and (typep cur-obj 'si-geom-polygon)
                                                       (primary-p cur-obj))
                                                  :intersects)
                    (dolist (segment (segments cur-obj))
                      (when (and (not (dont-display segment))
                                 (selectedp segment))
                        (draw-it segment))))))
            
                (when *display-binding-names-mode*
                  (dolist (object (elements (spatial-index map)))
                    (when (and (bound-to object)
                               (not (eq (bound-to object) t)))
                      (multiple-value-bind (lx ly)
                          (label-pos object)
                        (with-output-as-presentation
                            (stream object (type-of object)
                                    :single-box *single-box-mode* :allow-sensitive-inferiors t)
                          (draw-text* stream 
                                      (format nil "~A" (bound-to object))
                                      lx ly
                                      :ink +bound-to-text-ink+ 
                                      :text-style +bound-to-text-style+))))))
          
          
                (dolist (cur-obj (text-objects map))
                  (when (box-overlaps-box-p (bbox cur-obj)
                                            current-map-range)
                    (draw cur-obj stream)))))))))))

(defun draw-current-map-to-foreign-stream (stream &key width height overview fast-p)
  (with-map-viewer-frame (map-viewer)
    (recalculate-transformation stream :width width :height height)
    (draw-current-map map-viewer stream 
                      :fast-p fast-p
		      :clear-p nil
		      :overview overview)
    (recalculate-transformation (get-frame-pane map-viewer 'display))))

;;;
;;;
;;;

#+allegro
(defmethod draw-overview ((frame map-viewer) stream)
  (with-slots (overview-pattern current-map-range
	                        overview-transformation 
	                        map) frame
    (when map
      
      (multiple-value-bind (viewport-width viewport-height)
	  (bounding-rectangle-size (bounding-rectangle (window-viewport stream)))
	(unless overview-pattern
	  (setf overview-pattern 
	        (make-pattern-from-pixmap 
	         (with-output-to-pixmap (stream 
				         (sheet-medium (get-frame-pane frame 'overview))
				         :width viewport-width
				         :height viewport-height)
	           (with-drawing-options (stream :transformation overview-transformation)		    
		     (with-pretty-map-output 
                       (let ((*draw-overview* t))
                         (dolist (obj (elements (spatial-index map)))
                           (when (primary-p obj)
                             (draw obj stream))))))))))
	
        (draw-pattern* stream overview-pattern 0 0)
	(show-current-map-range)))))


#+(and mcl antiker-mac)
(defmethod draw-overview ((frame map-viewer) stream)
  (with-slots (overview-pattern current-map-range
	                        overview-transformation 
	                        map) frame
    (show-current-map-range)))


#+(or (and mcl (not antiker-mac)) lispworks)
(defmethod draw-overview ((frame map-viewer) stream)
  (with-slots (overview-pattern current-map-range
	                        overview-transformation 
	                        map) frame
    (when map      
      (multiple-value-bind (viewport-width viewport-height)
	  (bounding-rectangle-size (bounding-rectangle (window-viewport stream)))
	(unless overview-pattern
	  (setf overview-pattern 
	        (with-output-to-pixmap (stream 
				        (sheet-medium (get-frame-pane frame 'overview))
				        :width viewport-width
				        :height viewport-height)
                  (draw-rectangle* stream 0 0 viewport-width viewport-height
                                   :ink +white+)
	          (with-drawing-options (stream :transformation overview-transformation)
		    (with-pretty-map-output
                      (let ((*draw-overview* t))
                        (dolist (obj (elements (spatial-index map)))
                          (when (primary-p obj)
                            (draw obj stream)))))))))
	(draw-pixmap* stream
                      overview-pattern
                      0 0)
	(show-current-map-range)))))

;;;
;;;
;;;

(define-presentation-method highlight-presentation ((type si-geom-thing) record stream state) 
  (with-map-viewer-frame (frame)
    (with-slots (current-map-transformation map) frame
      (with-correct-clipping (stream)         
        (with-drawing-options (stream :transformation current-map-transformation)
          (highlight-object (presentation-object record) stream state))))))


(defmethod highlight-object ((object si-geom-chain) stream state)
  (dolist (segment (segments object))
    (draw-line* stream
                (x (p1 segment))
                (y (p1 segment))
                (x (p2 segment))
                (y (p2 segment))                 
                :ink +flipping-ink+
                :line-thickness +highlight-thickness+)))

(defmethod highlight-object ((object si-geom-polygon) stream state)
  (draw-polygon* stream (xy-list object)
                 :filled t
                 :ink +flipping-ink+
                 :line-thickness +highlight-thickness+))    

(defmethod highlight-object ((object si-geom-point) stream state)
  (with-slots (x y) object
    (if (primary-p object)
        (progn
          (draw-circle*
           stream
           x y 10
           :ink +flipping-ink+
           :line-thickness +highlight-thickness+
           :filled nil)
          (draw-circle*
           stream
           x y 8
           :line-thickness +highlight-thickness+
           :filled t
           :ink +flipping-ink+))
      (draw-circle*
       stream
       x y (if (bound-to object) 
               10
             6)
       :line-thickness +highlight-thickness+
       :filled t
       :ink +flipping-ink+))))

(defmethod highlight-object ((object si-geom-line) stream state)
  (with-slots (p1 p2) object
    (draw-line*
     stream
     (x p1) (y p1)
     (x p2) (y p2)
     :ink +flipping-ink+
     :line-thickness +highlight-thickness+)))

;;;
;;;
;;;

(defmethod draw :around ((object si-geom-thing) stream)
  (labels ((draw-it ()
	     (if *sensitive-objects-mode*
		 (with-output-as-presentation
		     (stream object (type-of object)
			     :single-box *single-box-mode* :allow-sensitive-inferiors t)
		   (call-next-method))
	       (call-next-method))))
    (if (typep object 'map-object)       
        (with-drawing-options (stream :ink (if (and *highlight-bindings*
                                                    (bound-to object))
                                               +bound-to-ink+
                                             (map-color object))
                                      :line-thickness (if (and *highlight-bindings*
                                                               (bound-to object))
                                                          +highlight-line-thickness+
                                                        1))
          (draw-it))
      (with-drawing-options (stream :ink (if (and *highlight-bindings*
                                                  (bound-to object))
                                             +bound-to-ink+
                                           +non-map-object-color+)
                                    :line-thickness (if (and *highlight-bindings*
                                                             (bound-to object))
                                                        +highlight-line-thickness+
                                                      1))
        (draw-it)))))

(defmethod draw ((object si-geom-line) stream)
  (with-slots (p1 p2) object
    
    (when (and *sensitive-objects-mode* +gravity-field+)
      (draw-line*
       stream
       (x p1) (y p1)
       (x p2) (y p2)
       :ink +transparent-ink+
       :line-thickness +gravity-field-thickness2+))

    (draw-line*
     stream
     (x p1) (y p1)
     (x p2) (y p2))
    
    (when (or *display-nodes-mode* *draw-components*)
      
      (draw p1 stream)
      (draw p2 stream))))


(defmethod draw ((object si-geom-chain) stream)
  (when (selectedp object)
    (unless *draw-overview*
      (draw-marker* stream
                    (x (centroid object))
                    (y (centroid object))
                    5
                    :ink +marker-color+))

    (when (and *sensitive-objects-mode* +gravity-field+)
      (dolist (segment (segments object))
        (draw-line* stream
                    (x (p1 segment))
                    (y (p1 segment))
                    (x (p2 segment))
                    (y (p2 segment))
                    :ink +transparent-ink+
                    :line-thickness +gravity-field-thickness+)))
    
    (dolist (segment (segments object))
      (draw-line* stream
                  (x (p1 segment))
                  (y (p1 segment))
                  (x (p2 segment))
                  (y (p2 segment))))
    
    (when *draw-components*
      (dolist (segment (segments object))
        (when (and (not (dont-display segment))
                   (selectedp segment))
          (draw segment stream))))))

(defmethod draw ((object si-geom-polygon) stream)
  (when (selectedp object)    

    (when (and *sensitive-objects-mode* +gravity-field+)
      (draw-polygon* stream (xy-list object)
                     :filled nil
                     :ink +transparent-ink+
                     :line-thickness +gravity-field-thickness+))
   
    (draw-polygon* stream (xy-list object)
                   :filled *solid-areas*)
   
    (dolist (hole (holes object))
      (draw-polygon* stream (xy-list hole)
                     :filled *solid-areas*
                     :ink +white+))

    (unless *draw-overview*
      (draw-marker* stream
                    (x (centroid object))
                    (y (centroid object))
                    5
                    :ink +marker-color+))
   
    (when *draw-components*
      (dolist (segment (segments object))
        (when (and (not (dont-display segment))
                   (selectedp segment))
          (draw segment stream))))))

(defmethod draw ((object si-geom-point) stream)
  (with-slots (x y name) object   
    (draw-circle*
     stream
     x y (if (and (bound-to object) 
                  *highlight-bindings*)
             8
           4)
     :filled t)))


(defmethod draw ((object basic-map-symbol) stream)
  (with-slots (x y name) object
    (draw-circle*
     stream
     x y 8
     :filled nil)
    (draw-circle*
     stream
     x y 6
     :filled nil)
    
    (draw-line* stream 
                (- x 5) (- y 5)
                (+ x 5) (+ y 5))
    (draw-line* stream 
                (- x 5) (+ y 5)
                (+ x 5) (- y 5))))
    

(defmethod draw ((object map-text) stream)
  (when *display-map-text-mode*
    (with-drawing-options (stream :transformation (matrix object)
				  :ink +text-color+)
      (with-slots (text) object
	(draw-vector-text text stream)))))

;;;
;;;
;;;

(defun measure-and-draw-map ()
  (with-map-viewer-frame (frame)
    (with-slots (overview-transformation
	         present-os-keys 
	         overview-pattern
	         current-map-position current-map-radius map) frame
    
      (with-slots (xmin ymin xmax ymax) (spatial-index map)
        (window-clear *standard-output*)
      
        (dolist (text-obj (text-objects map))
          (let ((stream (get-frame-pane frame 'display))
                (*display-map-text-mode* t))
            (init text-obj stream)))


        (setf overview-pattern nil)
        (setf present-os-keys nil)
      
        (setf *unknown-os-keys* nil)
      
        (labels ((insert-os-key (object)	      
		   (dolist (item (all-os object))
		     (if (lookup-os item)
		         (pushnew item
				  present-os-keys)
		       (pushnew item
			        *unknown-os-keys*)))
		 
		   (if (or (typep object 'basic-map-polygon)
                           (typep object 'basic-map-chain))
		       (dolist (object (segments object))
                         (when (typep object 'map-object)
		           (insert-os-key object))))))
	
	  (dolist (obj (objects map))
	    (insert-os-key obj))

          (show-map-infos)
	
	  (change-os-key-selection)
	
	  (recalculate-transformation (get-frame-pane frame 'overview) :force t)
	
	  (reset-map-range)

	  (com-draw-current-map)

	  (draw-overview frame (get-frame-pane frame 'overview)))))))



(defmethod recalculate-transformation (sheet &key width height)
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (map current-map-range current-map-transformation) map-viewer-frame
      (when map
	(let* ((vbb
		(window-viewport sheet))
	       (vb-width
		(or width (bounding-rectangle-width vbb)))
	       (vb-height
		(or height (bounding-rectangle-height vbb)))
	       
	       (xscale (/ vb-width (- (x (pmax current-map-range)) (x (pmin current-map-range)))))
	       
	       (yscale (/ vb-height (- (y (pmax current-map-range)) (y (pmin current-map-range)))))
	       
	       (trans1 (make-translation-transformation (- (x (pmin current-map-range)))
							(- (y (pmax current-map-range)))))
	       (trans2 (make-scaling-transformation  
			xscale 
			(- yscale)))
	       (trans 
		(compose-transformations trans2 trans1)))
	  
	  (setf current-map-transformation trans))))))


(defmethod recalculate-transformation ((pane overview-pane) &key force)
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (map
		 overview-transformation 
		 old-overview-vp-width 
		 old-overview-vp-height) map-viewer-frame  
      (when map
	(with-slots (xmin ymin xmax ymax) (spatial-index map)
	  (let* ((vpbb
		  (window-viewport 
		   (get-frame-pane map-viewer-frame 'overview)))
		 (vp-width
		  (bounding-rectangle-width vpbb))
		 (vp-height
		  (bounding-rectangle-height vpbb)))
	    
	    (when (or force
		      (not old-overview-vp-width) (not old-overview-vp-height)
		      (not (and (= old-overview-vp-width vp-width)
				(= old-overview-vp-height vp-height))))
	      (let*
		  ((trans1 (make-translation-transformation (- xmin) (- ymax)))
		   (trans2 (make-scaling-transformation  
			    (/ vp-width (- xmax xmin))
			    (- (/ vp-height (- ymax ymin)))))		   
		   (trans (compose-transformations trans2 trans1)))
		
		(setf old-overview-vp-width vp-width
		      old-overview-vp-height vp-height)
		(setf overview-transformation trans)))))))))

(defmethod calculate-transformation ((pane application-pane))
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (map) map-viewer-frame  
      (when map
	(with-slots (xmin ymin xmax ymax) (spatial-index map)
	  (let* ((vpbb
		  (window-viewport pane))
		 (vp-width
		  (bounding-rectangle-width vpbb))
		 (vp-height
		  (bounding-rectangle-height vpbb))
		 
		 (trans1 (make-translation-transformation (- xmin) (- ymax)))
		 (trans2 (make-scaling-transformation  
			  (/ vp-width (- xmax xmin))
			  (- (/ vp-height (- ymax ymin))))))
	    (compose-transformations trans2 trans1)))))))

;;;
;;;
;;;


(define-map-viewer-command (com-show-map-infos :name "Show Map Infos")
    ()
  (window-clear *standard-output*)
  (show-map-infos))

(defun show-map-infos (&optional (stream *standard-output*))    
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (map present-os-keys) map-viewer-frame
      (when map
	(fresh-line stream)
	(terpri stream)
	(format stream "  Infos For Map ~A:~%" (name map))
        (format stream "  ~A,~%" map)
	(format stream "  ~A Objects in Map, ~%" (length (objects map)))
        (format stream "  ~A Objects in Spatial Index, ~%" (length (elements (spatial-index map))))	
	(format stream "  ~A Unknown Object OS Keys,~%" (length *unknown-os-keys*))
	(format stream "  ~A Known Object OS Keys. " (length present-os-keys))))))

;;;
;;;
;;;

(define-map-viewer-command (com-invert-highlighted :name "Invert Highlighted")
    ()
  (invert-highlighted)
  (com-redraw))


(defun invert-highlighted ()
  (with-map-viewer-frame (map-viewer-frame)    
    (with-slots (map) map-viewer-frame
      (dolist (object (elements (spatial-index map)))
        (setf (bound-to object) (not (bound-to object))))
      (dolist (object (elements (spatial-index map)))
        (when (for-some-component-holds-p object #'bound-to)
          (setf (bound-to object) t))))))

(define-map-viewer-command (com-delete-highlighted-objects :name "Delete Highlighted Objects")
    ()
  (with-map-viewer-frame (map-viewer-frame)
    (invert-highlighted)
    (with-slots (map) map-viewer-frame
      (with-slots (xmin ymin xmax ymax) (spatial-index map)
        (recompute-map xmin ymin xmax ymax)))))

  
(define-map-viewer-command (com-highlight-hidden-objects :name "Highlight Hidden Objects")
    ()
  (with-map-viewer-frame (map-viewer-frame)    
    (with-slots (map) map-viewer-frame
      (dolist (obj (elements (spatial-index map)))
        (when (dont-display obj)
          (select obj)
          (setf (dont-display obj) nil)))))
  (com-redraw))

(define-map-viewer-command (com-crop-map :name "Crop Map")
    ()
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (map current-map-range) map-viewer-frame
      (unhighlight-all)
      (with-selected-objects (cur-obj current-map-range
                                      t
                                      :truly-inside)          
        (setf (bound-to cur-obj) t))
      (recompute-map (x (pmin current-map-range))
                     (y (pmin current-map-range))
                     (x (pmax current-map-range))
                     (y (pmax current-map-range))))))
                     
(define-map-viewer-command (com-highlight-objects-in-current-range :name "Highlight Objects in Current Range")
    ()
  (with-map-viewer-frame (map-viewer-frame)    
    (with-slots (current-map-range map) map-viewer-frame
      (with-selected-objects (cur-obj current-map-range
                                      t
                                      :truly-inside)
        (select cur-obj))))
  (com-redraw))

(defun recompute-map (xmin ymin xmax ymax)
  (with-map-viewer-frame (map-viewer-frame)    
    (with-slots (current-map-range map) map-viewer-frame
      (let* ((objects (remove-if-not #'(lambda (x) 
                                         (and (highlighted-p x)
                                              (for-each-master-holds-p x #'highlighted-p)))
                                     (elements (spatial-index map)))))
        
        (dolist (obj (set-difference (objects map) objects))
          (delete-node map obj))
           
        (setf (text-objects map)
              (remove-if-not #'(lambda (x)
                                 (box-overlaps-box-p (bbox x)
                                                     current-map-range))
                             (text-objects map)))

        (set-client-bb xmin ymin xmax ymax)
                
        (dolist (obj objects)
          (insert-into-spatial-index obj))
        
        (sort-current-spatial-index)
        
        (unhighlight (elements (spatial-index map)))

        (measure-and-draw-map)))))

(defun change-os-key-selection ()  
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (present-os-keys selected-os-keys) map-viewer-frame
      (setf selected-os-keys present-os-keys))))


;;;
;;;
;;;

(define-map-viewer-command (com-classify-map :name "Classify Map")    
    ()
  t)


;;;
;;;
;;;

(define-map-viewer-command (com-close-roles :name "Close Roles")    
    ()
  (close-roles (get-current-map)))

;;;
;;;
;;;

(define-map-viewer-command (com-load-sqd-map :name "Load SQD Map")
    ()
  (with-map-viewer-frame (map-viewer-frame)    
    (let ((stream (get-frame-pane map-viewer-frame 'infos)))      
      (let ((file (file-selector "Load SQD Map" "maps:" "sqd")))
        (when file	
          (window-clear stream)
	  (terpri stream)	
          (format stream " Attempting to load SQD map ~A!" file)
          (install-as-current-mapviewer-map (make-map-from-sqd-file file
                                                                    :delete-if-exists-p t
                                                                    :racer-package 'racer-user
                                                                    :abox 
                                                                    (intern 
                                                                     (string-upcase
                                                                      (pathname-name
                                                                       (truename file))))
                                                                    :tbox 
                                                                    (unless (eq *kind-of-map* 'alcrcc-explicit-relations-map)
                                                                      'oejendorf)
                                                                    :type *kind-of-map*)))))))


(defmethod install-as-current-mapviewer-map ((obj map*))
  (with-map-viewer-frame (frame)
    (with-slots (map) frame
      (setf map obj)
      (setf *kind-of-map*
            (type-of map))
      (measure-and-draw-map))))


(defun reinstall-map-viewer-map ()
  (install-as-current-map (slot-value *map-viewer-frame* 'map)))

;;;
;;;
;;;

(define-map-viewer-command (com-install-as-current-mapviewer-map :name "Install Current Map DB")
    ()
  (when (get-current-map)
    (install-as-current-mapviewer-map (get-current-map))))

;;;
;;;
;;;


(define-map-viewer-command (com-load-map :name "Load Map")
    ()
  (let ((file (file-selector "Load Map" "maps:" "map")))      
    (when file
      (install-as-current-mapviewer-map (load-map file)))))


(define-map-viewer-command (com-save-map-as :name "Save Map As")
    ()
  (with-map-viewer-frame (map-viewer-frame)
    (let ((file (file-selector "Save Map" "maps:" "map" :save t)))
      (when file
	(with-slots (map) map-viewer-frame
          (save-map map file))))))


(define-map-viewer-command (com-save-map :name "Save Map")
    ()
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (map) map-viewer-frame        
      (let ((file (format nil "~A-~A-~A-~A.map" 
                          (let* ((name (format nil "~A" (name map)))
                                 (pos (position #\/ name :from-end t))
                                 (pos2 (position #\. name :from-end t)))
                            (if pos
                                (subseq name (1+ pos)
                                        (or pos2 (length name)))
                              name))
                          (type-of map)
                          (if (dc-edges-p map)
                              'with-dc
                            'no-dc)
                          (when (typep map 'racer-map) 
                            (if (closed-roles-p map)
                                'closed-roles
                              'open-roles)))))
        (save-map map file)))))

;;;
;;;
;;;

(defun com-draw-current-map ()
  (with-map-viewer-frame (map-viewer-frame)
    (draw-current-map map-viewer-frame (get-frame-pane map-viewer-frame 'display))
    (redisplay-frame-pane map-viewer-frame 'os-selector)))

;;;
;;;
;;;


(define-map-viewer-command (com-highlight-object :name "Highlight Object")
    ((object 'map-object))
  (if (bound-to object)
      (unselect object)
    (select object))
  (setf *selected-object* object)
  (com-redraw))



(define-presentation-to-command-translator select-object
    ((map-object) com-highlight-object map-viewer 
     :gesture :select-object)
    (object)
  (list object))


(define-map-viewer-command (com-describe-object :name "Describe Object")
    ((object 'si-geom-thing))
  (labels ((describe-this-object (object level &key slave)
             (when (typep object 'map-object)
	       (dolist (os (all-os object))
	         (multiple-value-bind (os-info found)
		     (lookup-os os)
                   (if found			
		       (my-format *standard-output* level
			          "~A  ~A"
			      	  (thematic-substrate::name object)
                                  os-info)
                     (my-format *standard-output* level
                                "~A  Unknown Object!"
                                (thematic-substrate::name object))))))
	     (when (and (not slave)
			(or (typep object 'si-geom-chain)
			    (typep object 'si-geom-polygon)))
	       (fresh-line *standard-output*)
	       (my-format *standard-output* level "Has Segments:")
	       (dolist (segment (segments object))
		 (terpri *standard-output*)
		 (describe-this-object segment (1+ level)
				       :slave slave)))
	     (when (and slave (part-of object))
	       (dolist (part (part-of object))
		 (fresh-line *standard-output*)
		 (my-format *standard-output* level "Part Of:~%")
		 (describe-this-object part (1+ level)
				       :slave slave)))))
    
    (window-clear *standard-output*)
    (terpri)
    
    (setf *selected-object* object)

    (describe-this-object object 0
			  :slave
			  (part-of object))))

(define-presentation-to-command-translator describe
    ((si-geom-thing) com-describe-object map-viewer 
     :gesture :describe-object)
    (object)
  (list object))


;;;
;;;
;;;

(define-map-viewer-command (com-clear-display :name "Clear Map Display")
    ()
  (with-map-viewer-frame (map-viewer-frame)  
    (let ((stream (get-frame-pane map-viewer-frame 'display)))
      (window-clear stream))))

(define-map-viewer-command (com-redraw :name "Redraw Current Range")
    ()
  (with-map-viewer-frame (frame)
    (redisplay-frame-pane frame 'overview)
    (com-clear-display)
    (com-draw-current-map)
    (redisplay-frame-pane frame 'os-selector)))

;;;
;;;
;;;

(define-map-viewer-command (com-restart-inspector :name "Restart Inspector")
    ()
  (result-inspector))

(define-map-viewer-command (com-quit :name "Quit")
    ()
  (with-map-viewer-frame (map-viewer-frame)
    (let ((yes-or-no (notify-user map-viewer-frame
				  "Quit selected! Are you sure?"
				  :style :question)))
      (when yes-or-no
        (setf *map-viewer-frame* nil)    
	(frame-exit map-viewer-frame)))))

;;;
;;; SPATIAL QUERYING
;;;

(define-map-viewer-command (com-highlight-intersecting-objects* :name "Highlight Intersecting Objects")
    ()
  (let ((object
	 (accept 'si-geom-thing)))
    (show-intersecting-objects object)))

(define-map-viewer-command (com-highlight-intersecting-objects)
    ((object 'si-geom-thing :gesture nil)) 
  (show-intersecting-objects object))

(defmethod show-intersecting-objects ((object si-geom-thing))
  (unhighlight-all)
  (if (and *use-query-processor* (typep object 'map-object))
      (mapc #'highlight (get-successors-of-type object :po))
    (with-selected-objects (obj object (not (eq obj object)) :intersects)
      (highlight obj)))
  (com-draw-current-map))

;;;
;;;
;;;

(define-map-viewer-command (com-highlight-contained-objects* :name "Highlight Contained Objects")
    ()
  (let ((object
	 (accept 'si-geom-polygon)))
    (show-contained-objects object)))

(define-map-viewer-command (com-highlight-contained-objects)
    ((object 'si-geom-polygon :gesture nil))  
  (show-contained-objects object))

(defmethod show-contained-objects ((object si-geom-polygon))
  (unhighlight-all)
  (if (and *use-query-processor* (typep object 'map-object))
      (progn 
        (mapc #'highlight (get-successors-of-type object :tppi))
        (mapc #'highlight (get-successors-of-type object :ntppi)))
    (with-selected-objects (obj object t :inside)
      (highlight obj)))
  (com-draw-current-map))

;;;
;;;
;;;

(define-map-viewer-command (com-highlight-strictly-contained-objects* :name "Highlight Strictly Contained Objects")
    ()
  (let ((object
	 (accept 'si-geom-polygon)))
    (show-strictly-contained-objects object)))

(define-map-viewer-command (com-highlight-strictly-contained-objects)
    ((object 'si-geom-polygon :gesture nil))  
  (show-strictly-contained-objects object))

(defmethod show-strictly-contained-objects ((object si-geom-polygon))
  (unhighlight-all)
  (if (and *use-query-processor* (typep object 'map-object))
      (mapc #'highlight (get-successors-of-type object :ntppi))         
    (with-selected-objects (obj object t :truly-inside)
      (highlight obj)))
  (com-draw-current-map))

;;;
;;;
;;;

(define-map-viewer-command (com-highlight-covered-objects* :name "Highlight Covered Objects")
    ()
  (let ((object
	 (accept 'si-geom-polygon)))
    (show-covered-objects object)))

(define-map-viewer-command (com-highlight-covered-objects)
    ((object 'si-geom-polygon :gesture nil))  
  (show-covered-objects object))

(defmethod show-covered-objects ((object si-geom-polygon))
  (unhighlight-all)
  (if (and *use-query-processor* (typep object 'map-object))
      (mapc #'highlight (get-successors-of-type object :tppi))
    (with-selected-objects (obj object t :covered-by)
      (highlight obj)))
  (com-draw-current-map))

;;;
;;;
;;;


(define-map-viewer-command (com-highlight-touching-objects* :name "Highlight Touching Objects")
    ()
  (let ((object
	 (accept 'si-geom-thing)))
    (show-touching-objects object)))

(define-map-viewer-command (com-highlight-touching-objects)
    ((object 'si-geom-thing :gesture nil))  
  (show-touching-objects object))

(defmethod show-touching-objects ((object si-geom-thing))
  (unhighlight-all)
  (if (and *use-query-processor* (typep object 'map-object))
      (mapc #'highlight (get-successors-of-type object :ec))
    (with-selected-objects (obj object t :touches)
      (highlight obj)))
  (com-draw-current-map))

;;;
;;;
;;;

(define-map-viewer-command (com-highlight-overlapping-objects* :name "Highlight Overlapping Objects")
    ()
  (let ((object
	 (accept 'si-geom-polygon)))
    (show-overlapping-objects object)))

(define-map-viewer-command (com-highlight-overlapping-objects)
    ((object 'si-geom-polygon :gesture nil))
  (show-overlapping-objects object))

(defmethod show-overlapping-objects ((object si-geom-polygon))
  (unhighlight-all)
  (if (and *use-query-processor* (typep object 'map-object))
      (mapc #'highlight (get-successors-of-type object :po))
    (with-selected-objects (obj object t :overlaps)
      (highlight obj)))
  (com-draw-current-map))

;;;
;;;
;;;

(define-map-viewer-command (com-highlight-bordered-objects* :name "Highlight Bordered Objects")
    ()
  (let ((object
	 (accept 'si-geom-thing)))
    (show-bordered-objects object)))

(define-map-viewer-command (com-highlight-bordered-objects)
    ((object 'si-geom-thing :gesture nil))
  (show-bordered-objects object))

(defmethod show-bordered-objects ((object si-geom-thing))
  (unhighlight-all) 
  (if (and *use-query-processor* (typep object 'map-object))
      (mapc #'highlight (get-successors-of-type object :borders))
    (with-selected-objects (obj object (not (eq obj object)) :lies-on)
      (highlight obj)))
  (com-draw-current-map))

;;;
;;;
;;;

#+allegro
(defmethod note-frame-iconified ((manager tk-silica::motif-frame-manager) (frame map-viewer)) 
  nil)

#|
(defmethod resize-sheet :after ((sheet display-pane) width height)
  (declare (ignore width height))
  (recalculate-transformation sheet)
  (com-draw-current-map))

(defmethod resize-sheet :after ((sheet overview-pane) width height)
  (declare (ignore width height))
  (recalculate-transformation sheet)
  (draw-overview (pane-frame sheet) sheet))

|#

(defun map-viewer (&key (force t)
			(process t)
			left
			top width height
		        &allow-other-keys)
  (let ((port (find-port)))

    #+allegro
    (setf (clim:text-style-mapping port 
				   +map-viewer-text-style+)
          "-*-lucida-medium-r-normal-*-12-*-*-*-*-*-*-*")

    (when (or force (null *map-viewer-frame*))
      (unless left
	(multiple-value-bind (screen-width screen-height)
	    (bounding-rectangle-size 
	     (sheet-region (find-graft :port port)))
	  (setf left 0
		top 0
		width screen-width 
		height screen-height)))

      (setf *map-viewer-frame*
	    (make-application-frame
	        'map-viewer
	      :left left
	      :top  top
	      :width (- width 40)
	      :height (- height 100)))

      #+allegro
      (if process
	  (mp:process-run-function
	   "Map-Viewer"
	   #'(lambda ()
	       (run-frame-top-level *map-viewer-frame*)))
	(run-frame-top-level *map-viewer-frame*))
      #+mcl
      (if process
	  (ccl:process-run-function
	   "Map Viewer"
	   #'(lambda ()
	       (run-frame-top-level *map-viewer-frame*)))
	(run-frame-top-level *map-viewer-frame*))
      #+lispworks 
      (if process
	  (mp:process-run-function
	   "Map-Viewer"
           '(:size 250000) ;;; set Stack Size! 
	   #'(lambda ()
	       (run-frame-top-level *map-viewer-frame*)))
	(run-frame-top-level *map-viewer-frame*))
      

      *map-viewer-frame*)))

;;;
;;;
;;;

(defmethod frame-standard-input ((frame map-viewer))
  (get-frame-pane frame 'command))

(defmethod frame-standard-output ((frame map-viewer))
  (get-frame-pane frame 'infos))

(defmethod frame-error-output ((frame map-viewer))
  (get-frame-pane frame 'infos))
