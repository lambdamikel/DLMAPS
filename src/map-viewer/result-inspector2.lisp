;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SPATIAL-SUBSTRATE; Base: 10 -*-

(in-package spatial-substrate)

(defconstant +x-no-of-thumbnails+ 6)

(defconstant +y-no-of-thumbnails+ 6)

(defconstant +spacing+ 10)

(defconstant +offset-radius+ 100)

(defconstant +light-blue+ (make-rgb-color 0.5 0.5 1))

(defconstant +inspector-text-style+ 
  (make-text-style :sans-serif :bold :large))  

(defconstant +inspector-interactor-text-style+
  (make-text-style :sans-serif :bold :normal))

;;;
;;; Achtung: die Spatial Atoms (s. spatial-substrate;compiler5.lisp) 
;;; können NUR compiliert werden! -> *runtime-evaluation* = NIL
;;; ts::*compile-queries-p* = T bedeutet, dass die erzeugten 
;;; Lambdas Lisp-compiliert werden; bei NIL werden sie EVALUIERT
;;; (aber letztlich wurde die Query "compiliert")
;;; 

(defparameter *searching-active* nil)

(defparameter *buttons* nil)

(defparameter *draw-thumbnails* t)

(defparameter *inspector-solid-areas* t)

(defparameter *experimental* nil)

(defparameter *exclude-permutations* nil)

(defparameter *inspector-display-binding-names-mode* t)

;;;
;;;
;;;

(defvar *result-inspector-frame* nil)

(defvar *inspector-process*)

;;;
;;;
;;;

(defvar +button-abort+)

(defvar +button-draw-thumbnails+)

(defvar +button-solid-areas+)

(defvar +button-experimental+)

(defvar +button-permutations+)

(defvar +button-show-bindings+)

(defvar +button-next+)

(defvar +button-previous+)

(defvar +button-delete-all-pages+)

(defvar +button-delete-page+)

(defvar +button-delete-selected+)

(defvar +button-delete-unselected+)

(defvar +button-unselect-all+)

;;;
;;;
;;;

(defmacro with-result-inspector ((name) &body body)
  `(let ((,name *result-inspector-frame*))
     ,@body))

(defmacro with-query-result-output (&body body)
  `(let ((*display-binding-names-mode* 
          *inspector-display-binding-names-mode*)
	 (*solid-areas* 
          *inspector-solid-areas*)
	 (*display-nodes-mode* nil)
	 (*sensitive-objects-mode* nil)
         (*highlight-bindings* t))
     ,@body))

(defmacro with-overview-output (&body body)
  `(let ((*display-map-text-mode* nil)
	 (*display-nodes-mode* nil)
	 (*sensitive-objects-mode* nil)
         (*highlight-bindings* t)
         (*solid-areas* *inspector-solid-areas*)
         (*display-binding-names-mode* 
          *inspector-display-binding-names-mode*))	 
     ,@body))

;;;
;;;
;;;
  
(defun result-inspector-running-p ()
  *result-inspector-frame*)
       


(define-command-table query-table
                      :menu (("Answer Query" :command (com-inspector-answer-query))))


(define-application-frame inspector ()
  ((pages :accessor pages :initform nil)
   (selected-page :accessor selected-page :initform nil)
   (active-page :accessor active-page :initform nil)
   (selected-query-result :accessor selected-query-result :initform nil)

   (map-on-display-p :accessor map-on-display-p :initform nil)
   
   (all-matches :accessor all-matches :initform nil) 
   
   (last-percent :initform nil)
   (last-time :initform nil))
  
  (:command-table (inspector
                   :inherit-from (query-table)
		   :menu (("Query" :menu query-table))))
  (:menu-bar nil)
  
  (:panes            
   (query-results :application
		  :label 
		  "Query Results"
		  :initial-cursor-visibility :inactive
		  :display-after-commands nil
		  :textcusor nil
		  :scroll-bars nil
		  :display-function #'draw-query-results)
   
   (infos :application
          :label "Infos"
          :text-style +inspector-text-style+
          :scroll-bars :both
          :textcursor nil
          :initial-cursor-visibility :inactive
          :end-of-line-action :allow
          :end-of-page-action :follow)

   (progress-bar :application
		 :label nil
		 :borders nil
		 :scroll-bars nil
		 :textcursor nil
		 :initial-cursor-visibility :inactive)


   (command :interactor
            :text-style +inspector-interactor-text-style+
            :scroll-bars :vertical)

   (button-abort (setf +button-abort+
		       (make-pane 'push-button
			          :label "Abort Search"			   
			          :activate-callback 'abort-search)))
   
   (button-draw-thumbnails (setf +button-draw-thumbnails+
			         (make-pane 'toggle-button
					    :value *draw-thumbnails*
                                            :default *draw-thumbnails*
					    :label "DT"
					    :value-changed-callback 'button-draw-thumbnails)))

   (button-show-bindings (setf +button-show-bindings+
                               (make-pane 'toggle-button
                                          :value *inspector-display-binding-names-mode*
                                          :label "SB"
                                          :default *inspector-display-binding-names-mode*
                                          :value-changed-callback 'button-show-bindings)))
   
   (button-solid-areas (setf +button-solid-areas+
                             (make-pane 'toggle-button
                                        :value *inspector-solid-areas*
                                        :label "SA"
                                        :default *inspector-solid-areas*
                                        :value-changed-callback 'button-solid-areas)))

   (button-experimental (setf +button-experimental+
                              (make-pane 'toggle-button
                                         :value *experimental*
                                         :default *experimental*
                                         :label "ES"
                                         :value-changed-callback 'button-experimental)))

   (button-permutations (setf +button-permutations+
                              (make-pane 'toggle-button
                                         :value *exclude-permutations*
                                         :default *exclude-permutations*
                                         :label "EP"
                                         :value-changed-callback 'button-permutations)))


   (button-previous (setf +button-previous+
		          (make-pane 'push-button
				     :label "<< Previous Page <<"
				     :activate-callback 'previous-page)))

   (button-next (setf +button-next+
		      (make-pane 'push-button
			         :label ">> Next Page >>"
			         :activate-callback 'next-page)))

   (button-delete-all-pages (setf +button-delete-all-pages+
			          (make-pane 'push-button
					     :label "Delete All Pages"
					     :activate-callback 'delete-all-pages)))

   (button-delete-page (setf +button-delete-page+
			     (make-pane 'push-button
				        :label "Delete Page"
				        :activate-callback 'delete-page)))
   
   (button-delete-selected (setf +button-delete-selected+
			         (make-pane 'push-button
					    :label "Delete Selected"
					    :activate-callback 'delete-selected)))

   (button-delete-unselected (setf +button-delete-unselected+
			           (make-pane 'push-button
					      :label "Delete Unselected"
					      :activate-callback 'delete-unselected)))   

   (button-unselect-all (setf +button-unselect-all+
			      (make-pane 'push-button
				         :label "Unselect All"
				         :activate-callback 'unselect-all)))
   
   (page-nr :application
	    :label nil
	    :borders nil
	    :scroll-bars nil
	    :textcursor nil
	    :initial-cursor-visibility :inactive
	    :display-function #'show-page-nr))
  (:layouts
   (:default    
    #+(and :lispworks :win32)
    (horizontally ()
      (1/2 (outl 
             (vertically ()
               (1/26 progress-bar)
               (1/26 (horizontally () (1 button-abort) +fill+))
               (1/26 (horizontally ()
                       (1/2 button-previous)
                       (1/2 button-next)
                       +fill+))
               (1/26 page-nr)
               (19/26 query-results)
               (1/26 (horizontally ()
                       (1/2 button-delete-page)
                       (1/2 button-delete-all-pages)
                       +fill+))
               (1/26 (horizontally ()
                       (1/3 button-unselect-all)
                       (1/3 button-delete-selected)
                       (1/3 button-delete-unselected)
                       +fill+))
               (1/26 (horizontally ()
                       (1/5 button-draw-thumbnails)
                       (1/5 button-solid-areas)
                       (1/5 button-show-bindings)
                       (1/5 button-experimental)
                       (1/5 button-permutations)
                       +fill+)))))
      (1/2 
       (vertically ()
         (1/2 (outl infos))
         (1/2 (outl 
                (labelling (:label "Query Processor")
                  command))))))
    #-(and :lispworks :win32)
    (horizontally ()
      (1/2 (outl 
             (vertically ()
               (1/26 progress-bar)
               (1/26 button-abort)
               (1/26 (horizontally ()
                       (1/2 button-previous)
                       (1/2 button-next)))
               (1/26 page-nr)
               (19/26 query-results)
               (1/26 (horizontally ()
                       (1/2 button-delete-page)
                       (1/2 button-delete-all-pages)))
               (1/26 (horizontally ()
                       (1/3 button-unselect-all)
                       (1/3 button-delete-selected)
                       (1/3 button-delete-unselected)))
               (1/26 (horizontally ()
                       (1/5 button-draw-thumbnails)
                       (1/5 button-solid-areas)
                       (1/5 button-show-bindings)
                       (1/5 button-experimental)
                       (1/5 button-permutations))))))
                       
      (1/2 
       (vertically ()
         (1/2 (outl infos))
         (1/2 (outl 
                (labelling (:label "Query Processor")
                  command)))))))))
    

(defun result-inspector (&key (force t)
                              left
                              top 
                              width height
                              &allow-other-keys)
  (let ((port (find-port)))
      
    (when (or force (null *map-viewer-frame*))
      (unless left
	(multiple-value-bind (screen-width screen-height)
	    (bounding-rectangle-size 
	     (sheet-region (find-graft :port port)))
	  (setf left 0
		top 0
		width screen-width 
		height screen-height)))

      (setf *result-inspector-frame*
	    (make-application-frame
	        'inspector

	      :left left
	      :top  top
	      :width (- width 40)
	      :height (- height 100)
              :pretty-name "Inspector"))

      (setf *inspector-process* 
	    (mp:process-run-function
	     "Result Inspector"
             nil
	     #'(lambda ()
	         (run-frame-top-level *result-inspector-frame*))))

      *result-inspector-frame*)))

(defmethod frame-standard-output ((frame inspector))
  (get-frame-pane frame 'infos))

(defmethod frame-error-output ((frame inspector))
  (get-frame-pane frame 'infos))

(defmethod frame-query-io ((frame inspector))
  (get-frame-pane frame 'command))

;;;
;;;
;;;

(defun show-search-progress (percent &key force-p)
  (with-result-inspector (frame)
    (let* ((stream (get-frame-pane frame 'progress-bar))
	   (time (get-universal-time)))
      (with-slots (last-percent last-time) frame 
	(when (or (not last-percent)
		  (and (not (= last-percent percent))
		       (or force-p 
                           (> (- time last-time) 1))))

          
          (multiple-value-bind (width height)
              (bounding-rectangle-size (bounding-rectangle stream))

	    (with-output-recording-options (stream :draw t :record nil)
	      (when last-percent
		(draw-rectangle* stream 0 0 (* (/ last-percent 100) width) height
				 :ink +background-ink+))
	      (setf last-percent percent)
	      (draw-rectangle* stream 0 0 (* (/ percent 100) width) height
			       :ink +red+)
	      (setf last-time time))))))))


(defmethod run-frame-top-level :before ((frame inspector) &key)
  (lock-buttons)
  (deactivate-gadget +button-abort+))

;;;
;;;
;;;

(defclass query-result ()
  ((bindings :accessor bindings :initarg :bindings)
   (selected :accessor selected :initform nil)
   
   (map-radius :accessor map-radius :initarg :map-radius)
   (map-xcenter :accessor map-xcenter :initarg :map-xcenter)
   (map-ycenter :accessor map-ycenter :initarg :map-ycenter)))
  
(defclass query-page ()
  ((objects :accessor objects :initarg :objects :initform nil)))

;;;
;;;
;;;

(defmethod full ((obj query-page))
  (= (length (objects obj))
     (* +x-no-of-thumbnails+
	+y-no-of-thumbnails+)))

(defun make-new-page ()
  (with-result-inspector (frame)
    (let ((obj (make-instance 'query-page)))
      (pushend obj (pages frame))
      (setf (active-page frame) obj)
      (setf (selected-page frame) obj)
      (update-buttons)
      obj)))

(defun get-position (obj)
  (with-result-inspector (frame)
    (when (and obj (selected-page frame))
      (let ((pos (position obj (objects (selected-page frame)))))
	(when pos
	  (multiple-value-bind (w h)
	      (thumbnail-size frame)
	    (let ((y (floor pos +x-no-of-thumbnails+))
		  (x (mod pos +x-no-of-thumbnails+)))
	      (values (+ (* w x) +spacing+) 
		      (+ (* h y) +spacing+)
		      (- (* w (1+ x)) +spacing+)
		      (- (* h (1+ y)) +spacing+)))))))))

(defmethod thumbnail-size ((frame inspector))
  (multiple-value-bind (width height)
      (window-inside-size (get-frame-pane frame 'query-results))
    (values (/ (- width 1) +x-no-of-thumbnails+)
	    (/ (- height 1) +y-no-of-thumbnails+))))

;;;
;;;
;;;

(defclass foobar () ())

(defmethod draw-result ((result query-result) stream w h &key (fast-p t) text-p full-view)
  (set-current-map-position-and-radius (map-xcenter result)
				       (map-ycenter result)
				       (+ (map-radius result) 50))
  
  (select-query-result result)

  (labels ((draw-it ()	    
	     (when *draw-thumbnails*
               (with-query-result-output 
                 (let ((*display-map-text-mode* text-p))
               
                   (draw-current-map-to-foreign-stream stream
                                                       :fast-p fast-p
                                                       :overview t
                                                       :width w
                                                       :height h))))))
	       
    (let ((bgink +background-ink+)
          (fgink +black+))
      
      (if (not full-view)	

          (progn
        
            (when (selected result)
              (setf bgink +red+)
              (setf fgink +black+))
            
            (with-output-as-presentation (stream result 'query-result
                                                 :single-box t
                                                 :allow-sensitive-inferiors nil)
              
              (draw-rectangle* stream -7 -7 (+ 7 w) (+ 7 h)
                               :ink bgink)
              
              (draw-rectangle* stream 0 0 w h
                               :ink fgink
                               :filled nil))
            
            (with-drawing-options (stream
                                   :clipping-region
                                   (make-bounding-rectangle 0 0 (1+ w) (1+ h)))

              (with-output-as-presentation (stream result 'foobar
                                                   :single-box nil
                                                   :allow-sensitive-inferiors nil)
                  
                (draw-it))))
                 
        
        (draw-it)))))


(defmethod draw-overview ((frame inspector) stream)
  (with-slots (overview-pattern current-map-range
	                        overview-transformation) frame
    (multiple-value-bind (width height)
	(bounding-rectangle-size (bounding-rectangle (window-viewport stream)))
      (when (get-current-map)      
        (with-overview-output
          (full-map-range)
          (recalculate-transformation stream :width width :height height)
          (with-map-viewer-frame (map-viewer)
            (draw-rectangle* stream 0 0 width height
                             :ink +white+)
            (draw-current-map map-viewer stream
                              :overview t
                              :fast-p t
                              :clear-p nil)))))))


(defmethod draw-thumbnail ((result query-result))
  (with-result-inspector (frame)
    (let ((stream (get-frame-pane frame 'query-results)))
      (when (map-on-display-p frame)
        (window-clear stream))
      (setf (map-on-display-p frame) nil)
      (multiple-value-bind (xf yf xt yt)
	  (get-position result)
        (when (and xf yf xt yt)
          (with-translation (stream xf yf)
            (draw-result result stream (- xt xf) (- yt yf))))))))

;;;
;;;
;;;

(define-presentation-method highlight-presentation ((obj query-result) record stream state)
  (declare (ignore state))
  (let* ((obj (presentation-object record)))
    (multiple-value-bind (xf yf xt yt)
        (get-position obj)
      (when xf				; get-position kann NIL liefern
        (draw-rectangle* stream 
                         xf yf xt yt
			 :filled nil
			 :ink +flipping-ink+
			 :line-thickness 10)))))

#|
(define-presentation-method presentation-refined-position-test ((obj query-result) record stream state)
  (break) 
  
  |#

(defun get-page-nr ()
  (with-result-inspector (frame)
    (if (selected-page frame)
	(1+ (position (selected-page frame) (pages frame)))
      0)))

;;;
;;;
;;;

(defun abort-search (&optional button)
  (declare (ignore button))
  (thematic-substrate::abort-all-queries))

(defun previous-page (&optional button)
  (declare (ignore button))
  (with-result-inspector (frame)
    (setf (map-on-display-p frame) nil)
    (when (selected-page frame)
      (let ((pos (position (selected-page frame) (pages frame))))
	(unless (zerop pos)
	  (setf (selected-page frame)
	        (nth (1- pos) (pages frame)))
	  (update-buttons))))))

(defun next-page (&optional button)
  (declare (ignore button))
  (with-result-inspector (frame)
    (setf (map-on-display-p frame) nil)
    (let ((rest (rest (member (selected-page frame) (pages frame)))))
      (when rest	
	(setf (selected-page frame) (first rest))
	(update-buttons)))))

(defun delete-all-pages (&optional button)
  (declare (ignore button))
  (with-result-inspector (frame)
    (setf (map-on-display-p frame) nil)
    (setf (pages frame) nil
	  (active-page frame) nil
	  (selected-page frame) nil)
    (update-buttons)))

(defun delete-page (&optional button)
  (declare (ignore button))
  (with-result-inspector (frame)
    (setf (map-on-display-p frame) nil)
    (when (selected-page frame)      
      (setf (pages frame) 
	    (delete (selected-page frame)
		    (pages frame)))
      (setf (selected-page frame)
	    (first (pages frame)))
      (update-buttons))))


(defun delete-selected (&optional button)
  (declare (ignore button))
  (with-result-inspector (frame)
    (setf (map-on-display-p frame) nil)
    (when (selected-page frame)
      (dolist (obj (remove-if-not #'selected (objects (selected-page frame))))
	(setf (objects (selected-page frame))
	      (delete obj (objects (selected-page frame)))))
      (if (objects (selected-page frame))
	  (update-buttons)
	(delete-page +button-delete-page+)))))

(defun delete-unselected (&optional button)
  (declare (ignore button))
  (with-result-inspector (frame)
    (setf (map-on-display-p frame) nil)
    (when (selected-page frame)
      (dolist (obj (remove-if #'selected (objects (selected-page frame))))
	(setf (objects (selected-page frame))
	      (delete obj (objects (selected-page frame)))))
      (if (objects (selected-page frame))
	  (update-buttons)
	(delete-page +button-delete-page+)))))

(defun unselect-all (&optional button)
  (declare (ignore button))
  (with-result-inspector (frame)
    (setf (map-on-display-p frame) nil)
    (when (selected-page frame)
      (dolist (obj (objects (selected-page frame)))
	(setf (selected obj) nil))
      (update-buttons))))

(defun button-draw-thumbnails (button value)
  (declare (ignore button))
  (setf *draw-thumbnails* value)
  (with-result-inspector (frame) 
    (redisplay-frame-pane frame (get-frame-pane frame 'query-results) :force-p t)))

(defun button-show-bindings (button value)
  (declare (ignore button))
  (setf *inspector-display-binding-names-mode* value)
  (with-result-inspector (frame) 
    (redisplay-frame-pane frame (get-frame-pane frame 'query-results) :force-p t)))

(defun button-solid-areas (button value)
  (declare (ignore button))
  (setf *inspector-solid-areas* value)
  (with-result-inspector (frame) 
    (redisplay-frame-pane frame (get-frame-pane frame 'query-results) :force-p t)))
  
(defun button-permutations (button value)
  (declare (ignore button))
  (setf *exclude-permutations* value))

(defun button-experimental (button value)
  (declare (ignore button))
  (setf *experimental* value))

  
;;;
;;;
;;;

(defun lock-buttons ()
  (dolist (button (list +button-previous+ +button-next+
			+button-delete-all-pages+ +button-delete-page+
			+button-delete-selected+ +button-delete-unselected+
			+button-unselect-all+))
    (deactivate-gadget button))
  (activate-gadget +button-abort+)
  (show-search-progress 100)
  (setf *searching-active* t))

(defun unlock-buttons ()
  (setf *searching-active* nil)
  (deactivate-gadget +button-abort+)
  (update-buttons) 
  (show-search-progress 0 :force-p t))

(defun update-buttons ()
  (with-result-inspector (frame)
    (unless *searching-active*
      (cond ((pages frame)
	     (let ((nr (get-page-nr)))
	       (if (= 1 nr)
		   (deactivate-gadget +button-previous+)
		 (activate-gadget +button-previous+))
	       (if (= nr (length (pages frame)))
		   (deactivate-gadget +button-next+)
		 (activate-gadget +button-next+))
	       (activate-gadget +button-delete-selected+)
	       (activate-gadget +button-delete-unselected+)	     
	       (activate-gadget +button-delete-page+)
	       (activate-gadget +button-delete-all-pages+)
	       (activate-gadget +button-unselect-all+)))
	    (t (deactivate-gadget +button-previous+)
	       (deactivate-gadget +button-next+)	     
	       (deactivate-gadget +button-delete-all-pages+)
	       (deactivate-gadget +button-delete-page+)
	       (deactivate-gadget +button-delete-selected+)
	       (deactivate-gadget +button-delete-unselected+)
	       (deactivate-gadget +button-unselect-all+))))
    (window-clear (get-frame-pane frame 'query-results))
    (redisplay-frame-pane frame (get-frame-pane frame 'page-nr) :force-p t)
    (draw-query-results frame (get-frame-pane frame 'query-results))))


(defmethod show-page-nr ((frame inspector) stream)  
  (format stream " Page No. ~A of ~A Pages." (get-page-nr) (length (pages frame))))

;;;
;;;
;;;


(defun make-query-result (vars binding)
  (with-result-inspector (inspector-frame)   
    (let ((binding            
           (mapcar #'(lambda (var object)
                       (list var
                             (etypecase object
                               (symbol 
                                (get-associated-substrate-node (get-current-map) object))
                               (map-object object))))
                   vars binding)))

      
      (when (or (not (active-page inspector-frame))
                (full (active-page inspector-frame)))
        
        (make-new-page))
        
      (let* ((agg (make-aggregate (mapcar #'second binding) :hierarchicly-p nil))
             (xcenter (x (pcenter agg)))
             (ycenter (y (pcenter agg)))
             (radius (max (+ +offset-radius+ (/ (- (x (pmax agg)) (x (pmin agg))) 2))
                          (+ +offset-radius+ (/ (- (y (pmax agg)) (y (pmin agg))) 2))))
             (res-object (make-instance 'query-result
                                        :bindings binding
                                        :map-radius radius
                                        :map-xcenter xcenter
                                        :map-ycenter ycenter)))

        (pushend res-object (objects (active-page inspector-frame)))
            
        (delete-object agg)
            
        (when (eq (selected-page inspector-frame)
                  (active-page inspector-frame))
          (draw-thumbnail res-object))))))

;;;
;;;
;;;

(defun draw-selected-query-result ()
  (with-result-inspector (frame)
    (let ((result (selected-query-result frame))
          (stream (get-frame-pane frame 'infos)))
      (window-clear stream)
      (multiple-value-bind (w h)
          (window-inside-size stream)
        (when result
          (let ((*draw-thumbnails* t)
                (*display-map-text-mode* t))
            (draw-result result stream (- w 2) (- h 2) :fast-p nil :text-p t :full-view t)))))))

(defmethod draw-query-results ((frame inspector) stream)
  (declare (ignore stream))
  (if (map-on-display-p frame)
      (com-inspector-show-map)
    (when (selected-page frame)
      (dolist (result (objects (selected-page frame)))
        (let ((*display-map-text-mode* nil))	 
          (draw-thumbnail result))))))

;;;
;;;
;;;

(define-gesture-name :select-query-result :pointer-button (:left :shift))

(define-gesture-name :inspect-query-result :pointer-button :left)

;;;
;;;
;;;


(define-inspector-command (com-inspector-query :name "Query")
    ()
  (com-inspector-answer-query))


(define-inspector-command (com-inspector-answer-query :name "Answer Query")
    ()
  (let ((query (accept 'form :prompt "Enter Query")))
    (terpri *standard-input*)
    (let ((vois (accept '((sequence symbol) :separator #\Space)           
                        :prompt "Enter Variables")))
      
      (inspector-answer-query query vois))))


(define-inspector-command (com-inspector-load-sqd-map :name "Load SQD Map")
    ()
  (let ((file (file-selector "Load SQD Map" "maps:" "sqd")))
    (when file	
      (window-clear *standard-output*)
      (format *standard-output* "Attempting to load SQD map ~A!" file)
      (install-as-current-mapviewer-map
       (load-and-install-map file)))))

(define-inspector-command (com-inspector-load-map :name "Load Map")
    ()
  (let ((file (file-selector "Load Map" "maps:" "map")))      
    (when file
      (install-as-current-mapviewer-map (load-map file)))))

(define-inspector-command (com-inspector-map :name "Map")
    ()
  (com-inspector-show-map))

(define-inspector-command (com-inspector-show-map :name "Show Map")
    ()
  (with-result-inspector (frame)
    (let ((stream (get-frame-pane frame 'query-results)))
      (setf (map-on-display-p frame) t)
      (unhighlight-all)
      (when (and *last-query* 
                 (is-map-query-p *last-query*))
        (dolist (binding (result-bindings *last-query*))
          (mapcar #'(lambda (var object)                
                      (let ((object (if (symbolp object)                             
                                        (get-associated-substrate-node (substrate *last-query*)
                                                                       object)
                                      object)))
                        (when object
                          (setf (geometry::bound-to object) var))))
                  (answer-pattern *last-query*) binding)))
      (window-clear stream)
      (draw-overview frame stream))))


(define-inspector-command (com-inspector-thumbnails :name "Thumbnails")
    ()
  (com-inspector-show-thumbnails))

(define-inspector-command (com-inspector-show-thumbnails :name "Show Thumbnails")
    ()
  (with-result-inspector (frame)
    (let ((stream (get-frame-pane frame 'query-results)))
      (setf (map-on-display-p frame) nil)
      (window-clear stream)    
      (draw-query-results frame stream))))


(define-inspector-command (com-reload-ontology :name "Reload Ontology")
    ()
  (load-geo-ontology))


(define-inspector-command (com-reload-queries :name "Reload Queries")
    ()
  (load "q-queries.lisp"))

(define-inspector-command (com-inspector-ontology :name "Ontology")
    ()
  (com-inspector-show-ontology))

#+:midelora
(define-inspector-command (com-inspector-show-ontology :name "Show Ontology")
    ()
  (if (is-midelora-map-p *cur-map*)
      (dag::visualize-dag (taxonomy (find-tbox 'prover::oejendorf :error-p t)))
    (let ((taxonomy (racer-dag::get-racer-taxonomy 'racer-user::oejendorf)))
      (dag:visualize-dag taxonomy))))

#-:midelora
(define-inspector-command (com-inspector-show-ontology :name "Show Ontology")
    ()
  (let ((taxonomy (racer-dag::get-racer-taxonomy 'racer-user::oejendorf)))
    (dag:visualize-dag taxonomy)))

(define-inspector-command (com-inspector-delete-page :name "Delete Page")
    ()
  (delete-page))

(define-inspector-command (com-inspector-delete-all-pages :name "Delete All Pages")
    ()
  (delete-all-pages))

(define-inspector-command (com-inspector-next-page :name "Next Page")
    ()
  (next-page))

(define-inspector-command (com-inspector-prev-page :name "Previous Page")
    ()
  (previous-page))
  

(define-inspector-command (com-inspector-show-query :name "Show Query")
    ()  
  (window-clear *standard-output*) 
  (when *last-query*
    (terpri *standard-output*)
    (format t "Query:  ") 
    (write (unparse-query *last-query*) :pretty t :escape nil :readably nil)
    (format t "~%~% Variables:  ")
    (write (answer-pattern *last-query*)  :pretty t :escape nil :readably nil)))
          


(define-inspector-command (com-inspector-show-code :name "Show Code")
    ()
  (window-clear *standard-output*)
  (when *last-query*
    (dolist (query (cons *last-query* 
                         (thematic-substrate::all-subqueries *last-query*)))
      (pprint (source query) *standard-output*))))


(define-inspector-command (com-inspector-answer :name "Answer")
    ()
  (com-inspector-show-answer))

(define-inspector-command (com-inspector-show-answer :name "Show Answer")
    ()
  (let ((stream *standard-output*) )
    (when *last-query*
      (run #'(lambda () 
               (window-clear stream) 
               (format stream "Last query returned ~A tuples: ~%" 
                       (length (result-bindings *last-query*)))
               (pprint (mapcar #'(lambda (binding) 
                                   (mapcar #'(lambda (var object) 
                                               (list var 
                                                     (if (symbolp object) 
                                                         object 
                                                       (name object))))
                                           (answer-pattern *last-query*) binding))
                               (result-bindings *last-query*) )
                       stream))))))

  
(define-inspector-command (com-inspector-clear :name "Clear")
    ()
  (window-clear *standard-output*)   
  (window-clear *query-io*)
  (unhighlight-all)
  (setf *last-query* nil)
  (setf (map-on-display-p *application-frame*) nil)
  (delete-all-pages)
  (update-buttons))


(define-inspector-command (com-inspector-show-repository :name "Show Repository")
    ()
  (show-qbox (get-current-map)))


(define-inspector-command (com-inspector-clear-repository :name "Clear Repository")
    ()
  (thematic-substrate::clear-repository (get-current-map)))


#|

  (define-inspector-command (com-inspector-kill :name "Kill")
      ()
    (kill)
    (unlock-buttons))

  |#


(define-inspector-command (com-inspector-restart-map-viewer :name "Restart Map Viewer")
    ()
  (map-viewer))


(define-inspector-command (com-inspector-quit :name "Quit")
    ()
  (with-result-inspector (frame)
    (let ((yes-or-no (notify-user frame
				  "Quit selected! Are you sure?"
				  :style :question)))
      (when yes-or-no
        (setf *result-inspector-frame* nil)
	(frame-exit frame)))))


;;;
;;;
;;;


(define-inspector-command (com-inspector-inspect-query-result)
    ((object 'query-result))
  (select-query-result object)
  (draw-selected-query-result))

(defmethod select-query-result ((object query-result))
  (with-result-inspector (frame)    
    (setf (selected-query-result frame) object)
    (unhighlight-all)
    (dolist (tuple (bindings object))
      (let ((object (second tuple))
            (var (first tuple)))
        (setf (geometry::bound-to 
               (if (symbolp object)                             
                   (get-associated-substrate-node (substrate *last-query*)
                                                  object)
                 object))
              var)))))

(define-presentation-to-command-translator inspect-query-result
    (query-result
     com-inspector-inspect-query-result inspector
     :tester (() (not *searching-active*))
     :echo t
     :maintain-history nil
     :gesture :inspect-query-result)
    (object)
  (list object))

;;;
;;;
;;;

(define-inspector-command (com-inspector-select-query-result)
    ((object 'query-result))
  
  (setf (selected object) (not (selected object)))
  (com-inspector-inspect-query-result object)
  (draw-thumbnail object))

(define-presentation-to-command-translator select-query-result
    (query-result
     com-inspector-select-query-result inspector
     :echo nil
     :tester (() (not *searching-active*))
     :maintain-history nil
     :gesture :select-query-result)
    (object)
  (list object))

;;;
;;;
;;;

(defmacro with-dlmaps-standard-settings (&body body)
  `(if *exclude-permutations*
       (with-nrql-settings (:abox-mirroring nil 
                            :query-optimization t
                            :two-phase-query-processing-mode nil
                            :told-information-querying nil
                            :tuple-computation-mode :set-at-a-time
                        
                            :exclude-permutations t
                            :query-repository nil

                            :report-inconsistent-queries nil
                            :report-tautological-queries  nil
                                    
                            :query-realization nil

                            :check-abox-consistency nil
                        
                            :rewrite-to-dnf t)
     
         (racer:with-unique-name-assumption
           (let ((ts::*runtime-evaluation-p* nil)
                 (ts::*compile-queries-p* nil))
             ,@body)))
     
     (with-nrql-settings (:abox-mirroring nil 
                          :query-optimization t
                        
                          :two-phase-query-processing-mode nil
                          :told-information-querying nil
                          :tuple-computation-mode :set-at-a-time
                        
                          :exclude-permutations nil
                          :query-repository nil

                          :report-inconsistent-queries nil
                          :report-tautological-queries  nil
                                    
                          :query-realization nil

                          :check-abox-consistency nil
                        
                          :rewrite-to-dnf t)
     
       (racer:with-unique-name-assumption
         (let ((ts::*runtime-evaluation-p* nil)
               (ts::*compile-queries-p* nil))
           ,@body)))))



(defmacro with-dlmaps-experimental-settings (&body body)
  `(if *exclude-permutations*
       (with-nrql-settings (:query-optimization t
                        
                            :two-phase-query-processing-mode nil
                            :tuple-computation-mode :set-at-a-time
                        
                            :exclude-permutations t
                            :query-repository t

                            :report-inconsistent-queries t

                            :query-realization nil)
     
         (racer:with-unique-name-assumption
           (let ((ts::*runtime-evaluation-p* nil)
                 (ts::*compile-queries-p* nil))
             ,@body)))
     
     (with-nrql-settings (:query-optimization t
                        
                          :two-phase-query-processing-mode t
                          :tuple-computation-mode :set-at-a-time
                        
                          :exclude-permutations nil
                          :query-repository t

                          :report-inconsistent-queries t

                          :query-realization nil)

       (racer:with-unique-name-assumption
         (let ((ts::*runtime-evaluation-p* nil)
               (ts::*compile-queries-p* nil))
           ,@body)))))

;;;
;;;
;;;

(defun inspector-answer-query (query vois &optional doc)
  (present-query query vois)
  (terpri *standard-input*)
  (terpri *standard-input*)    
  (in-substrate* (get-current-map))
  (with-result-inspector (frame)         
    (setf (map-on-display-p frame) nil)    
    (delete-all-pages)    
    (let ((*standard-output* (frame-standard-output frame))
          (*package* ;(racer-package *cur-substrate*)
           (find-package :racer-user)))

      (window-clear *standard-output*)
      (terpri *standard-output*)
      (when doc 
        (format t "Natürlichsprachlich:~%") 
        (format t doc)          
        (terpri *standard-output*)
        (terpri *standard-output*))
      (format t "Query:  ") 
      (write query :pretty t :escape nil :readably nil)
      (format t "~%~% Variables:  ")
      (write vois  :pretty t :escape nil :readably nil)
      (terpri *standard-output*)
      (terpri *standard-output*)
      (pprint 
       ;;; aus irgendwelchen Gründen (locking?)
       ;;; blockiert die Anwendung, wenn kein
       ;;; Prozess gestartet wird!
       (run #'(lambda ()
                (if *experimental* 
                    (with-dlmaps-experimental-settings
                      (answer-query query vois))
                  (with-dlmaps-standard-settings
                    (answer-query query vois)))))))))

(defun present-query (q v)
  (let ((stream *standard-input* )
        (*print-pretty* t))
    (format stream "Command: Answer Query~%")
    (format stream "Enter Query: ")
    (present q 'form :stream stream)
    (terpri stream) 
    (format stream "Enter Variables: ")     
    (present v '(sequence symbol) 
             :separator #\Space
             :stream stream)))

;;;
;;; Demo-Queries
;;;
          
(define-inspector-command (com-inspector-q1 :name "Q1")
    ()
  (q1))

(define-inspector-command (com-inspector-q2 :name "Q2")
    ()
  (q2))

(define-inspector-command (com-inspector-q3 :name "Q3")
    ()
  (q3))

(define-inspector-command (com-inspector-q4 :name "Q4")
    ()
  (q4))

(define-inspector-command (com-inspector-q5 :name "Q5")
    ()
  (q5))

(define-inspector-command (com-inspector-q6 :name "Q6")
    ()
  (q6))

(define-inspector-command (com-inspector-q7 :name "Q7")
    ()
  (q7))

(define-inspector-command (com-inspector-q8 :name "Q8")
    ()
  (q8))

(define-inspector-command (com-inspector-q9 :name "Q9")
    ()
  (q9))

(define-inspector-command (com-inspector-q10 :name "Q10")
    ()
  (q10))

(define-inspector-command (com-inspector-q11 :name "Q11")
    ()
  (q11))

(define-inspector-command (com-inspector-q12 :name "Q12")
    ()
  (q12))

(define-inspector-command (com-inspector-q13 :name "Q13")
    ()
  (q13))

(define-inspector-command (com-inspector-q14 :name "Q14")
    ()
  (q14))

(define-inspector-command (com-inspector-q15 :name "Q15")
    ()
  (q15))

(define-inspector-command (com-inspector-q16 :name "Q16")
    ()
  (q16))

(define-inspector-command (com-inspector-q17 :name "Q17")
    ()
  (q17))

(define-inspector-command (com-inspector-q18 :name "Q18")
    ()
  (q18))

(define-inspector-command (com-inspector-q19 :name "Q19")
    ()
  (q19))

(define-inspector-command (com-inspector-q20 :name "Q20")
    ()
  (q20))

