;;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GRAPH -*-

(in-package graph)

;;;
;;;

#|

(defmethod (setf bound-by) (val (object graph-item))
  (if val
      (mark object val)
    (unmark object)))

(defmethod bound-by ((object graph-item))
  (marked-p object))

|#


;;;
;;; Graph Visualizer
;;;

#|

(defconstant +text-style+
  (make-text-style :sans-serif
                   :bold
                   :normal))

|#

(defconstant +text-style+
  (make-text-style nil
                   :bold
                   :very-large))




(defconstant +button-text-style+
  (make-text-style :sans-serif
                   :roman
                   :very-small))

(defvar *node-kinds* nil)

(defvar *edge-kinds* nil)

(defvar *graph* nil)

(defvar *selected-object* nil)

;;;
;;;
;;;

(defconstant +node-body-color+ +blue+)

(defconstant +node-contur-color+ +blue+)

(defconstant +edge-color+ +black+)

(defconstant +node-text-color+ +black+)

(defconstant +edge-text-color+ +black+)

(defconstant +node-colors+
  (make-cyclic
   (mapcar #'(lambda (rgb)
               (make-rgb-color (first rgb)
                               (second rgb)
                               (third rgb)))
           '((0 0 0)
             (0 0 1) (0 1 0) (1 0 0)
             (0 1 1) (1 1 0) (1 0 1)
	      
             (0 0 .6) (0 .6 0) (.6 0 0)
             (0 .6 .6) (.6 .6 0) (.6 0 .6)
             (.6 .6 .6)

             (0 0 .5) (0 .5 0) (.5 0 0)
             (0 .5 .5) (.5 .5 0) (.5 0 .5)
             (.5 .5 .5)
	      
             (0 0 .4) (0 .4 0) (.4 0 0)
             (0 .4 .4) (.4 .4 0) (.4 0 .4)
             (.4 .4 .4)

             (0 0 .3) (0 .3 0) (.3 0 0)
             (0 .3 .3) (.3 .3 0) (.3 0 .3)
             (.3 .3 .3)
	      
             (0 0 .2) (0 .2 0) (.2 0 0)
             (0 .2 .2) (.2 .2 0) (.2 0 .2)
             (.2 .2 .2)

             (0 0 .1) (0 .1 0) (.1 0 0)
             (0 .1 .1) (.1 .1 0) (.1 0 .1)
             (.1 .1 .1)))))


(defconstant +edge-colors+ +node-colors+)

(defmethod color :around ((node node))
  (let ((kind (kind node)))
    (if (and (compute-color-from-kind-p node) kind)
        (if *node-kinds* 
            (let ((pos (position kind *node-kinds* :test #'equal)))
              (if pos 
                  (nth pos +node-colors+)
                (progn 
                  (pushend kind *node-kinds*)
                  (nth (1- (length *node-kinds*)) +node-colors+))))
          (progn 
            (setf *node-kinds* (list kind))
            (nth 0 +node-colors+)))
      (call-next-method))))


(defmethod color :around ((edge edge))
  (let ((kind (kind edge)))
    (if (and (compute-color-from-kind-p edge) kind)
        (if *edge-kinds* 
            (let ((pos (position kind *edge-kinds* :test #'equal)))
              (if pos 
                  (nth pos +edge-colors+)
                (progn 
                  (pushend kind *edge-kinds*)
                  (nth (1- (length *edge-kinds*)) +edge-colors+))))
          (progn 
            (setf *edge-kinds* (list kind))
            (nth 0 +edge-colors+)))
      (call-next-method))))

(defun reset-kinds ()
  (setf *node-kinds* nil
        *edge-kinds* nil))

;;;
;;;
;;;

(defvar *graph-visualizer* nil)

(defmacro with-graph-visualizer ((frame) &rest body)
  `(let ((,frame *graph-visualizer*))
     ,@body))



(define-application-frame graph-visualizer ()
  ((bar :accessor bar :initform nil)

   (resizer :accessor resizer :initform nil)
   
   (process :initform nil
	    :accessor process)

   (scale :initform 1 :reader scale)
   
   (graphs :initform nil :reader graphs :initarg :graphs)
   
   (graph :initform nil :reader graph :initarg :graph)

   (kind-of-graph :initform 'polygon
                  :accessor kind-of-graph)

   ;;; Darstellungsoptionen

   (show-nodes-p :initform t 
                 :accessor show-nodes-p)
   
   (show-edges-p :initform t
                 :accessor show-edges-p)


   (show-node-info-p :initform t
	             :accessor show-node-info-p)

   (show-edge-info-p :initform t
	             :accessor edge-info-p)
   

   (show-color-nodes-p :initform t
                       :accessor show-color-nodes-p)

   (show-color-edges-p :initform t
                       :accessor show-color-edges-p)
   

   (show-backward-edges-p :initform t
		          :accessor show-backward-edges-p)
   (show-forward-edges-p :initform t
                         :accessor show-forward-edges-p)
   (show-loops-p :initform t 
	         :accessor show-loops-p))
  
  (:panes   
   (refresh-button  (make-pane 'push-button
                               :label "Refresh"
			       :text-style +button-text-style+	     
                               :activate-callback #'(lambda (x)
                                                      (declare (ignore x))
                                                      (refresh))))

   (new-button  (make-pane 'push-button
                           :label "New"
			   :text-style +button-text-style+	                                    
                           :activate-callback #'(lambda (x)
                                                  (declare (ignore x))
                                                  (com-new) (refresh))))

   (delete-button (make-pane 'push-button
			     :label "Delete"
			     :text-style +button-text-style+	                                    
			     :activate-callback #'(lambda (x)
                                                    (declare (ignore x))
						    (com-delete) (refresh))))
   
   (delete-others-button  (make-pane 'push-button
				     :label "Delete Others"
				     :text-style +button-text-style+
				     :activate-callback #'(lambda (x)
                                                            (declare (ignore x))
							    (com-delete-others) (refresh))))

   (next-button  (make-pane 'push-button
                            :label ">>"
			    :text-style +button-text-style+	                                    
                            :activate-callback #'(lambda (x)
                                                   (declare (ignore x))
                                                   (com-next) (refresh))))

   (prev-button (make-pane 'push-button
                           :label "<<"
			   :text-style +button-text-style+	                                    
                           :activate-callback #'(lambda (x)
                                                  (declare (ignore x))
                                                  (com-prev) (refresh))))

   (quit-button (make-pane 'push-button
                           :label "Quit"
			   :text-style +button-text-style+	                                    
                           :activate-callback #'(lambda (x)
                                                  (declare (ignore x))
                                                  (com-quit) (refresh))))
   
   (display :application
	    :display-function #'draw-graph-visualizer
            :incremental-redisplay t
	    :text-cursor nil
            :borders t
            :scroll-bars t)

   (infopane :application
	     :text-style +text-style+	    
	     :text-cursor nil
	     :end-of-line-action :allow
	     :scroll-bars :both)

   (info :application
	 :text-style +text-style+
	 :display-function
	 #'show-infos
	 :height '(2 :line)
	 :min-height '(2 :line)
	 :max-height '(2 :line)
	 :text-cursor nil
         :end-of-line-action :allow
         :text-margin 30
	 :scroll-bars nil)

   (options :accept-values
	    :text-style +button-text-style+
	    :scroll-bars nil
	    :height '(2 :line)
	    :min-height '(2 :line)
	    :max-height '(2 :line)
	    :display-function
	    `(accept-values-pane-displayer
	      :displayer ,#'(lambda (frame stream)
			      (accept-options frame stream))))

   (bar (setf *bar*
              (make-pane 'slider 
                         :show-value-p t
                         :text-style +button-text-style+
                         :value-changed-callback #'(lambda (slider value)
                                                     (declare (ignore slider))
                                                     (with-graph-visualizer (frame)
                                                                            (with-slots (graph graphs) frame
                                                                              (setf graph
                                                                                    (nth (round (* (/ value 100)
                                                                                                   (1- (length graphs))))
                                                                                         graphs))
                                                                              (refresh))))
                         :min-value 0 :max-value 100
                         :client 'bar
                         :id 'bar)))

   (resizer (setf *resizer*
                  (make-pane 'slider 
                             :show-value-p t
                             :text-style +button-text-style+
                             :value-changed-callback #'(lambda (slider value)
                                                         (declare (ignore slider))
                                                         (with-graph-visualizer (frame)
                                                                                (with-slots (scale) frame
                                                                                  (setf scale value)
                                                                                  (refresh))))
                             :min-value 1 :max-value 10
                             :number-of-tick-marks 10
                             :number-of-quanta 20
                             :client 'resizer
                             :id 'resizer))))

  (:layouts
   (:default
    #+(and :lispworks :win32)
    (vertically ()
      (2/3
       (vertically ()
         options
         info	    	    
         display
         (horizontally ()
           (1/4 new-button)
           (1/4 delete-button)
           (1/4 delete-others-button)
           (1/4 quit-button)
           +fill+)
         (horizontally ()
           (1/3 prev-button)
           (1/3 refresh-button)
           (1/3 next-button)
           +fill+)
         (horizontally ()
           (1/2 bar)
           (1/2 resizer)
           +fill+)))
      (1/3 infopane))
    #-(and :lispworks :win32)    
    (vertically ()
      (2/3
       (vertically ()
         options
         info	    	    
         display
         (horizontally ()
           (1/4 new-button)
           (1/4 delete-button)
           (1/4 delete-others-button)
           (1/4 quit-button))
         (horizontally ()
           (1/9 prev-button)
           (1/9 next-button)
           (1/9 refresh-button)
           (3/9 bar)
           (3/9 resizer))))
      (1/3 infopane)))))

;;;
;;; Drawing Functions
;;;


(defmethod show-infos ((frame graph-visualizer) stream)
  (with-slots (graph graphs) frame
    (when (and graph graphs)
      (format stream "Number of Objects: ~A. Current Object: ~A. Info: ~A~%" 
              (length graphs)
              (position graph graphs)
              (info (graph frame))))))

(defun get-node-size ()
  (with-graph-visualizer (frame)
                         (with-slots (graph) frame
                           (let* ((size (no-of-nodes graph))
                                  (node-radius (/ 15 (max 1 size))))
                             node-radius))))

(defmethod draw-graph-visualizer ((frame graph-visualizer) stream &optional window-stream)
  (when (graph frame)
    (with-slots (graph scale
                       kind-of-graph 
                       show-nodes-p show-edges-p
                       show-node-info-p show-edge-info-p
                       show-color-nodes-p show-color-edges-p
                       show-backward-edges-p show-forward-edges-p show-loops-p) frame

      (multiple-value-bind (x y)
	  (bounding-rectangle-size (if window-stream
				       (bounding-rectangle (window-viewport window-stream))
				     (bounding-rectangle (window-viewport stream))))
	(let* ((s (* 1.5 (* x (/ (get-node-size) 100))))
               (x (- x (* 2 s)))
               (y (- y (* 2 s))))             

          (with-translation (stream s s)
            
            (with-translation (stream (if (eq kind-of-graph 'chain)
                                          0
                                        0)
                                      (if (eq kind-of-graph 'chain)
                                          (cond((and show-backward-edges-p                                            
                                                     show-forward-edges-p)                     
                                                (/ y 2))
                                               (show-forward-edges-p
                                                y)
                                               (show-backward-edges-p 
                                                0)
                                               (t y))
                                        y))
                                            
              (with-scaling (stream scale scale)
                (with-scaling (stream (/ x 100)
                                      (if (and show-backward-edges-p 
                                               show-forward-edges-p
                                               (eq kind-of-graph 'chain))
                                          (/ y 200)
                                        (/ y 100)))
                  (setf *graph* graph)
                  
                  (with-text-size (stream (if window-stream
                                              :very-large
                                            :small))
                  
                    (draw-graph graph stream kind-of-graph
                                :print-p window-stream
                                :show-nodes-p show-nodes-p
                                :show-edges-p show-edges-p
                                :show-node-info-p show-node-info-p
                                :show-edge-info-p show-edge-info-p
                                :show-color-nodes-p show-color-nodes-p
                                :show-color-edges-p show-color-edges-p
                                :show-backward-edges-p show-backward-edges-p
                                :show-forward-edges-p show-forward-edges-p
                                :show-loops-p show-loops-p)))))))))))


;;;
;;; External Interface
;;;

(defmethod (setf graph) (value (frame graph-visualizer))
  (with-slots (graphs graph bar) frame
    (setf graph value)
    (if (and graphs graph)
        (let ((l (length graphs)))
          (setf (gadget-value bar)
                (* 100 (/ (position graph graphs) (max 1 (1- l)))))
          (when bar (activate-gadget bar)))
      (when bar (deactivate-gadget bar)))))

(defmethod (setf graphs) (value (frame graph-visualizer))
  (with-slots (bar graphs graph) frame    
    (cond ((null value)
           (setf graphs nil)
           (setf graph nil)
           (when bar (deactivate-gadget bar)))
          ((or (not (consp value))
               (not (every #'(lambda (x)
                               (typep x 'graph))
                           value)))
           (error "Bad objects in list!"))
          (t
           (setf graphs value)
           (setf graph (first value))
           (when bar (activate-gadget bar))))))


(defun set-objects (objects)
  (with-graph-visualizer (frame)
                         (setf (graphs frame) objects)))

(defun prepend-objects (objects)
  (with-graph-visualizer (frame)
                         (setf (graphs frame) 
                               (append (graphs frame) objects))))

(defun append-objects (objects)
  (with-graph-visualizer (frame)
                         (setf (graphs frame) 
                               (append objects (graphs frame)))))

;;;
;;; Commands: Navigation
;;;


(define-graph-visualizer-command (com-delete :name "Delete" )
    ()
  (with-graph-visualizer (frame)
                         (with-slots (graph graphs) frame
                           (when (and graph graphs)
                             (let ((ograph1 graph))
                               (com-next)
                               (if (cdr graphs)
                                   (setf (graphs frame)
                                         (delete ograph1 graphs))
                                 (com-new)))))))

(define-graph-visualizer-command (com-next :name "Next" )
    ()
  (with-graph-visualizer (frame)
                         (with-slots (graphs graph) frame    
                           (let ((rest (member graph graphs)))
                             (setf (graph frame) ;; wichtig! accessor verwenden! 
                                   (if (cdr rest)
                                       (first (cdr rest))
                                     (first graphs)))))))


(define-graph-visualizer-command (com-prev :name "Prev" )
    ()
  (with-graph-visualizer (frame)
                         (with-slots (graph graphs) frame
                           (let* ((rev (reverse graphs))
                                  (rest (member graph rev)))
                             (setf (graph frame)
                                   (if (cdr rest)
                                       (first (cdr rest))
                                     (first rev)))))))


(define-graph-visualizer-command (com-quit :name "Quit" )
    ()
  (with-graph-visualizer (frame)
                         (with-slots (process) frame
                           (when process
                             #+:allegro 
                             (mp:process-kill process))))
  (with-graph-visualizer (frame)
                         (frame-exit frame)))


(define-graph-visualizer-command (com-new :name "New" )
    ()
  (with-graph-visualizer (frame)
                         (setf (graphs frame) nil)))

(define-graph-visualizer-command (com-delete-others :name "Delete Others" )
    ()
  (with-graph-visualizer (frame)
                         (setf (graphs frame)
                               (list (graph frame)))))


(define-graph-visualizer-command (com-refresh :name "Refresh" )
    ()
  (refresh))

(defun refresh ()
  (with-graph-visualizer (frame)
                         (dolist (pane '(display info))
                           (redisplay-frame-pane frame pane))))

;;;
;;; Graphik
;;;


(defun draw-marker (stream direction posx posy size)
  (draw-arrow* stream posx posy (+ posx (* direction 2)) posy
               :head-length (max 2 (/ 20 size))
               :head-width (max 2 (/ 20 size))))

(defmethod item-pos ((node node))
  (get-node-position (in-graph node) node))

(defmethod draw-graph ((graph graph) (stream stream) (kind-of-graph (eql 'chain))
                       &key
                       show-nodes-p
                       show-edges-p
                       show-node-info-p
                       show-edge-info-p
                       show-color-nodes-p
                       show-color-edges-p
                       show-backward-edges-p 
                       show-forward-edges-p
                       show-loops-p
                       print-p)

  (declare (ignore print-p))

  (let* ((nodes (get-nodes graph))
         (edges (get-edges graph))
         (size (length nodes)))
    (when nodes 
      (let* ((stepx (/ 100 (max 1 (1- size))))
             (stepy (/ 100 (max 1 (1- size))))
             (node-radius (get-node-size))
             (loop-radius (/ node-radius 2))
             (looptext-y loop-radius))
      
        (when show-edges-p
          (let ((edgeclusters nil))
        
            (dolist (edge edges)
              (with-slots (from to) edge
                (let* ((from (item-pos from))
                       (to (item-pos to))
                       (found (find-if #'(lambda (x) 
                                           (and (equal (first x) from)
                                                (equal (second x) to)))
                                       edgeclusters)))
                  (if found
                      (push edge (third found))
                    (push (list from to (list edge)) edgeclusters)))))
                    
          
            (dolist (edgecluster edgeclusters)                                      
              (let* ((count (length (third edgecluster)))
                     (finestepy (/ stepy count))
                     (num 0))

                (dolist (edge (third edgecluster))
                  (incf num)
                  (with-slots (from to) edge
                    (let ((from (item-pos from))
                          (to (item-pos to)))

                      (when (and from to)
                        (with-drawing-options (stream :ink (if show-color-edges-p 
                                                               (color edge)
                                                             +edge-color+))
                          (let* ((height (- to from))
                                 (y (+ (* (1- height) stepy)
                                       (* num finestepy)))
                                 (ym (+ (* (1- (abs height)) stepy)
                                        (* num finestepy))))
                            (with-output-as-presentation (stream edge 'edge)
                              (cond ((and (zerop height) show-loops-p)
                                     (draw-circle* stream 
                                                   (+ (* to stepx) (* num (/ node-radius count)))
                                                   0
                                                   node-radius
                                                   :filled nil
                                                   :line-thickness 2)
                                     (when show-edge-info-p
                                       (draw-text* stream 
                                                   (format nil "~A" (info edge 
                                                                          :for-graph-visualizer-p t))
                                                   (+ (* to stepx) node-radius
                                                      (* num (/ node-radius count)))
                                                   looptext-y
                                                   :text-style +text-style+
                                                   :ink +edge-text-color+)))
                                
                                    ((and (plusp height) show-forward-edges-p)
                                     (draw-ellipse* stream 
                                                    (* (/ (+ from to) 2) stepx)
                                                    0                      
                                                    (* (- to (/ (+ from to) 2)) stepx)
                                                    0
                                                    0
                                                    y
                                                    :line-thickness 2
                                                    :filled nil
                                                    :start-angle
                                                    #+(or mcl lispworks) pi #-(or mcl lispworks) 0
                                                    :end-angle
                                                    #+(or mcl lispworks) 0 #-(or mcl lispworks) pi)
                                 
                                     (draw-marker stream 1
                                                  (* (/ (+ from to) 2) stepx)
                                                  (- y)
                                                  size)
                                 
                                     (when show-edge-info-p
                                       (draw-text* stream 
                                                   (format nil "~A" (info edge :for-graph-visualizer-p t))
                                                   (* (/ (+ from to) 2) stepx)
                                                   (- y)
                                                   :text-style +text-style+                                               
                                                   :ink +edge-text-color+)))
                                
                                    ((and (minusp height) show-backward-edges-p)
                                     (draw-ellipse* stream 
                                                    (* (/ (+ from to) 2) stepx)
                                                    0                      
                                                    (* (- to (/ (+ from to) 2)) stepx)
                                                    0
                                                    0
                                                    ym
                                                    :line-thickness 2
                                                    :filled nil
                                                    :start-angle #+(or mcl lispworks) 0 #-(or mcl lispworks) pi
                                                    :end-angle #+(or mcl lispworks) pi #-(or mcl lispworks) 0)
                                 
                                     (draw-marker stream -1
                                                  (* (/ (+ from to) 2) stepx)
                                                  ym
                                                  size)
                                 
                                     (when show-edge-info-p
                                       (draw-text* stream 
                                                   (format nil "~A" (info edge :for-graph-visualizer-p t))
                                                   (* (/ (+ from to) 2) stepx)
                                                   ym
                                                   :text-style +text-style+                                               
                                                   :ink +edge-text-color+)))))))))))))))
            
        (when show-nodes-p
          (dolist (node nodes)
            (with-output-as-presentation (stream node 'node)
              (let ((pos (item-pos node)))
                (draw-circle* stream (* pos stepx) 0 node-radius
                              :filled t
                              :ink (if show-color-nodes-p
                                       (color node)
                                     +node-body-color+))
                (draw-circle* stream (* pos stepx) 0 node-radius
                              :filled nil
                              :line-thickness 2
                              :ink +node-contur-color+)
                (when show-node-info-p
                  (with-text-size (stream :large)
                    (draw-text* stream 
                                (format nil "~A" (info node :for-graph-visualizer-p t))
                                (- (* pos stepx) (* 1.6 node-radius))
                                (* 1.6 node-radius)
                                :text-style +text-style+
                                :ink +node-text-color+)))))))))))

#+:lispworks
(defmethod make-postscript-file ((frame graph-visualizer) filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede)

    #| (let ((*standard-output* stream))
         (when (dag-top dag)
           (cl-user::psgraph (dag-top dag) 
                             #'dag-node-children
                             #'(lambda (x) (list (dag-node-name x)))))) |#
    (clim:with-output-to-postscript-stream (ps-stream stream 
                                                      :orientation :portrait
                                                      :scale-to-fit t)
      (draw-graph-visualizer frame ps-stream
                             (get-frame-pane frame 'display)))))

(defmethod draw-graph ((graph graph) (stream stream) (kind-of-graph (eql 'polygon))
                       &key
                       show-nodes-p
                       show-edges-p
                       show-node-info-p
                       show-edge-info-p
                       show-color-nodes-p
                       show-color-edges-p
                       show-backward-edges-p 
                       show-forward-edges-p
                       show-loops-p
                       print-p)

  (declare (ignore print-p))

  
  (let* ((nodes (get-nodes graph))
         (edges (get-edges graph))
           
         (pi2 (/ pi 2))
         (2pi (* 2 pi))
                         
         (size (max 1 (length nodes)))
         (step (/ 2pi size))
           
         (node-radius (get-node-size))
         (node-radius2 (* 1.5 node-radius))
         (loop-radius (* 1.2 node-radius))
           
         (r 45)
         (text-radius-ratio .3)
         (node-label-offset (* 2 node-radius)))

      
    (labels ((node-pos (index &optional (offset 0))
               (values
                (+     50 (* (+ offset r) (cos (+ pi (* step index)))))
                (+ (- 50) (* (+ offset r) (sin (+ pi (* step index))))))))
        
      (when show-edges-p
        (let ((edgeclusters nil))
        
          (dolist (edge edges)
            (with-slots (from to) edge
              (let* ((from (item-pos from))
                     (to (item-pos to))
                     (found (find-if #'(lambda (x) 
                                         (and (equal (first x) from)
                                              (equal (second x) to)))
                                     edgeclusters)))
                (if found
                    (push edge (third found))
                  (push (list from to (list edge)) edgeclusters)))))
                    
          
          (dolist (edgecluster edgeclusters)
            (let* ((count (length (third edgecluster)))
                   (inc (unless (zerop (1- count))
                          (/ pi (1- count))))
                   (num 0))
                         
              (dolist (edge (third edgecluster))
                (incf num)
                (with-slots (from to) edge          
                  (let* ((from (item-pos from))
                         (to (item-pos to))
                         (height (when (and from to)
                                   (- to from))))

                    (when height
                      
                      (multiple-value-bind (fromcx fromcy) 
                          (node-pos from)
                        (multiple-value-bind (tocx tocy) 
                            (node-pos to)
                         
                          (let* ((orientation (if (eq from to) 
                                                  0
                                                (vector-orientation fromcx fromcy tocx tocy)))

                          
                                 (anglefrom (+ orientation (if (zerop (1- count))
                                                               0
                                                             (+ (- pi2) (* inc (1- num))))))
                                 (angleto   (- orientation (if (zerop (1- count))
                                                               0 
                                                             (+ pi2 (* inc (1- num))))))
                               
                                 (fromx (+ fromcx (* node-radius (cos anglefrom))))
                                 (fromy (+ fromcy (* node-radius (sin anglefrom))))
                               
                                 (tox (+ tocx (* node-radius (cos angleto))))
                                 (toy (+ tocy (* node-radius (sin angleto))))                         
                               
                                 (fromx1 (+ fromcx (* node-radius2 (cos anglefrom))))
                                 (fromy1 (+ fromcy (* node-radius2 (sin anglefrom))))
                               
                                 (textx (+ fromx (* text-radius-ratio (- tox fromx))))
                                 (texty (+ fromy (* text-radius-ratio (- toy fromy)))))
                          
                          
                            (with-drawing-options (stream :ink (if show-color-edges-p 
                                                                   (color edge)
                                                                 +edge-color+))
                            
                              (with-output-as-presentation (stream edge 'edge)
                                (cond ((and (zerop height) show-loops-p)
                                       (draw-circle* stream fromx fromy
                                                     loop-radius
                                                     :line-thickness 2
                                                     :filled nil)
                                       (when show-edge-info-p
                                         (draw-text* stream 
                                                     (format nil "~A" (info edge :for-graph-visualizer-p t))
                                                     fromx1 fromy1
                                                     :text-style +text-style+
                                                     :ink +edge-text-color+)))
                                      ((or (and (plusp height) show-forward-edges-p)
                                           (and (minusp height) show-backward-edges-p))
                                     
                                       (draw-line* stream 
                                                   fromx
                                                   fromy
                                                   tox 
                                                   toy)
                                       (draw-arrow* stream
                                                    fromx
                                                    fromy
                                                    (/ (+ fromx tox) 2)
                                                    (/ (+ fromy toy) 2)
                                                    :head-length (max 2 (/ 20 size))
                                                    :head-width (max 2 (/ 20 size)))
                                     
                                       (when show-edge-info-p
                                         (draw-text* stream 
                                                     (format nil "~A" (info edge :for-graph-visualizer-p t))
                                                     textx
                                                     texty
                                                     :text-style +text-style+
                                                     :ink +edge-text-color+)))))))))))))))))
            
             
      (when show-nodes-p
        (dolist (node nodes)
          (let ((pos (item-pos node)))
            (multiple-value-bind (x y)
                (node-pos pos)
              (with-output-as-presentation (stream node 'node)
                (draw-circle* stream x y node-radius 
                              :filled t
                              :ink (if show-color-nodes-p
                                       (color node)
                                     +node-body-color+))
                (draw-circle* stream x y node-radius
                              :filled nil
                              :ink  +node-contur-color+)
                (when show-node-info-p
                  (multiple-value-bind (x y)
                      (node-pos pos node-label-offset)
                    (with-text-size (stream :large)                  
                      (draw-text* stream 
                                  (format nil "~A" (info node :for-graph-visualizer-p t))
                                  x y
                                  :text-style +text-style+
                                  :ink +node-text-color+))))))))))))



;;;
;;; Start
;;;

(defmethod accept-options ((frame graph-visualizer) stream)
  
  (with-slots (kind-of-graph 
               show-nodes-p show-edges-p
               show-node-info-p show-edge-info-p
               show-color-nodes-p show-color-edges-p
               show-backward-edges-p show-forward-edges-p show-loops-p) frame

    
    (formatting-table (stream :multiple-columns t)
      (formatting-row (stream)
	(formatting-cell (stream)
	  (multiple-value-bind (object)
	      (accept 'completion
		      :query-identifier 'kind-of-graph
		      :prompt nil
		      :prompt-mode :raw
		      :stream stream 
		      :default kind-of-graph
		      :view `(option-pane-view :items (chain polygon)))
	    (setf kind-of-graph object)))

	
	(formatting-cell (stream)	  
	  (multiple-value-bind (object)
	      (accept 'boolean
		      :stream stream :default show-nodes-p
		      :prompt-mode :raw
		      :query-identifier 'show-nodes-p
		      :prompt 
		      "Nodes")
	    (setf show-nodes-p object)))

	(formatting-cell (stream)	  
	  (multiple-value-bind (object)
	      (accept 'boolean
		      :stream stream :default show-edges-p
		      :prompt-mode :raw
		      :query-identifier 'show-edges-p
		      :prompt 
		      "Edges")
	    (setf show-edges-p object)))

	(formatting-cell (stream)	  
	  (multiple-value-bind (object)
	      (accept 'boolean
		      :stream stream :default show-node-info-p
		      :prompt-mode :raw
		      :query-identifier 'show-node-info-p
		      :prompt 
		      "Node Info")
	    (setf show-node-info-p object)))


	(formatting-cell (stream)	  
	  (multiple-value-bind (object)
	      (accept 'boolean
		      :stream stream :default show-edge-info-p
		      :prompt-mode :raw
		      :query-identifier 'show-edge-info-p
		      :prompt 
		      "Edge Info")
	    (setf show-edge-info-p object)))

	(formatting-cell (stream)	  
	  (multiple-value-bind (object)
	      (accept 'boolean
		      :stream stream :default show-color-nodes-p
		      :prompt-mode :raw
		      :query-identifier 'show-color-nodes-p
		      :prompt 
		      "Coloured Nodes")
	    (setf show-color-nodes-p object)))

	(formatting-cell (stream)	  
	  (multiple-value-bind (object)
	      (accept 'boolean
		      :stream stream :default show-color-edges-p
		      :prompt-mode :raw
		      :query-identifier 'show-color-edges-p
		      :prompt 
		      "Coloured Edges")
	    (setf show-color-edges-p object)))

	(formatting-cell (stream)	  
	  (multiple-value-bind (object)
	      (accept 'boolean
		      :stream stream :default show-backward-edges-p
		      :prompt-mode :raw
		      :query-identifier 'show-backward-edges-p
		      :prompt 
		      "Backward Edges")
	    (setf show-backward-edges-p object)))

	(formatting-cell (stream)	  
	  (multiple-value-bind (object)
	      (accept 'boolean
		      :stream stream :default show-forward-edges-p
		      :prompt-mode :raw
		      :query-identifier 'show-forward-edges-p
		      :prompt 
		      "Forward Edges")
	    (setf show-forward-edges-p object)))
        
	(formatting-cell (stream)	  
	  (multiple-value-bind (object)
	      (accept 'boolean
		      :stream stream :default show-loops-p
		      :prompt-mode :raw
		      :query-identifier 'show-loops-p
		      :prompt 
		      "Loops")
	    (setf show-loops-p object)))))))

	
;;;
;;; Interaktion
;;;

#+:lispworks
(define-graph-visualizer-command (com-ps-file :menu "Make Postscript File") ()
  (with-graph-visualizer (frame)
                         (make-postscript-file frame
                                               "graph.ps")))


(define-graph-visualizer-command (com-show-info :name "Show Info")
    ((object 'edge))
  (with-graph-visualizer (frame)
                         (setf *selected-object* object)
                         (let ((stream (get-frame-pane frame 'infopane)))
                           (describe object stream))))

#+:allegro
(define-graph-visualizer-command (com-print :menu t)
    ()
  (with-graph-visualizer (frame)
                         (with-open-stream 
                             (pipe (excl:run-shell-command  (format nil "lpr -P~A -h" '|r135_hp|)
                                                            :input :stream :wait nil))
                           (with-output-to-postscript-stream (stream pipe 
                                                                     :orientation :landscape
                                                                     :scale-to-fit t)
                             (draw-graph-visualizer frame stream 
                                                    (get-frame-pane frame 'display))))))


#+:allegro
(define-graph-visualizer-command (com-save :menu t)
    ()
  (with-graph-visualizer (frame)
                         (with-open-file (file "~/rcc-visualizer-output.ps"
                                               :direction :output :if-exists :supersede
                                               :if-does-not-exist :create)
                           (with-output-to-postscript-stream (stream file 
                                                                     :orientation :portrait
                                                                     :scale-to-fit t)
                             (draw-graph-visualizer frame stream 
                                                    (get-frame-pane frame 'display))))))


;;;
;;; Translatoren
;;;

(define-presentation-to-command-translator object-to-command
    (graph-item com-show-info graph-visualizer
	        :gesture :select)         
    (object)
  (list object))

;;;
;;;
;;;

(defmethod visualize ((graphs list) &optional (kill-p t))
  #| (unless (every #'(lambda (graph) 
                        (<= (length (nodes graph)) 20))
                    graphs)
       (error "Doesn't make sense to visualize graphs with more than 20 nodes with this program!")) |#
  (with-graph-visualizer (frame)
                         (when (and frame kill-p)
                           (frame-exit frame)))
  (let ((port (find-port))
        (graphs (remove-duplicates graphs))
        (frame (make-application-frame 'graph-visualizer
                 :width 850
                 :height 860)))

    (setf *graph-visualizer* frame)
    (setf (bar frame) *bar*)
    (setf (resizer frame) *resizer*)
  
    (setf (clim:text-style-mapping port +text-style+)
          ;;"-*-fixed-medium-r-semicondensed-*-11-*-*-*-*-*-*-*"
          "fixed")

    
    #+:allegro
    (mp:process-run-function
     "GRAPH Visualizer"
     #'(lambda ()    
	 (run-frame-top-level *graph-visualizer*)))
    #+:lispworks
    (mp:process-run-function
     "GRAPH Visualizer"
     nil
     #'(lambda ()    
	 (run-frame-top-level *graph-visualizer*)))
    #+mcl
    (ccl:process-run-function
     "GRAPH Visualizer"
     #'(lambda ()
	 (run-frame-top-level *graph-visualizer*)))

    (setf (graphs frame) graphs)))

(defmethod visualize ((graph graph) &optional (kill-p t)) 
  (visualize (list graph) kill-p))

;;;
;;; Demo: Ableitung eines gelabelten "Simple Graphs"
;;; Verwaltung der Knoten und Kanten über einfache Listen
;;; Achtung: *nicht* von Simple Graph erben!!!
;;; 

#|

(defvar *all-graphs* nil)

(defpersistentclass simple-graph (graph)
  ((nodes :accessor nodes :initform nil)
   (edges :accessor edges :initform nil)))

(defpersistentclass simple-node (node)
  ((label :accessor label :initarg :label :initform nil)))

(defpersistentclass simple-edge (edge)
  ((label :accessor label :initarg :label :initform nil)))

;;;
;;;
;;;  

(defun make-simple-graph (name) 
  (make-instance 'simple-graph :name name))

(defmethod make-graph ((type (eql 'simple-graph)) &key name &allow-other-keys)
  (make-simple-graph name))

(defmethod initialize-instance :after ((graph simple-graph) &rest initargs)
  (push graph *all-graphs*))

(defmethod initialize-instance :after ((node simple-node) &rest initargs)
  (push node (nodes (in-graph node))))

(defmethod initialize-instance :after ((edge simple-edge) &rest initargs)
  (push edge (edges (in-graph edge))))

(defmethod find-graph ((graph-spec symbol) &rest args)
  (find graph-spec *all-graphs* :key #'name))

(defmethod find-graph ((graph-spec integer) &rest args)
  (find graph-spec *all-graphs* :key #'id :test #'=))

(defmethod find-node ((graph-spec simple-graph) (node-spec symbol) &rest args)
  (find node-spec (nodes graph-spec) :key #'name))

(defmethod find-node ((graph-spec simple-graph) (node-spec integer) &rest args)
  (find node-spec (nodes graph-spec) :key #'id :test #'=))

(defmethod find-node ((graph-spec simple-graph) (node-spec simple-node) &rest args)
  (find node-spec (nodes graph-spec)))

(defmethod find-edge ((graph-spec simple-graph) (edge-spec symbol) &rest args)
  (find edge-spec (edges graph-spec) :key #'name))

(defmethod find-edge ((graph-spec simple-graph) (edge-spec integer) &rest args)
  (find edge-spec (edges graph-spec) :key #'id :test #'=))

(defmethod find-edge ((graph-spec simple-graph) (edge-spec simple-edge) &rest args)
  (find edge-spec (edges graph-spec)))

(defmethod get-node-position ((graph-spec simple-graph) (node-spec simple-node) &rest args)
  (let* ((nodes (get-nodes graph-spec)))
    (position node-spec nodes)))

(defmethod no-of-nodes ((graph simple-graph) &rest args)
  (length (nodes graph)))

(defmethod no-of-edges ((graph simple-graph) &rest args)
  (length (edges graph)))
  

(defmethod delete-graph ((graph simple-graph) &rest args)
  (setf *all-graphs* 
        (delete graph *all-graphs*)))

(defmethod copy-graph ((graph simple-graph) &key &allow-other-keys)
  (let ((new-graph 
         (make-instance 'simple-graph 
                        :name (name graph)
                        :edge-counter (edge-counter graph)
                        :node-counter (node-counter graph)                        
                        :id (id graph))))
    (with-slots (nodes edges) new-graph
      (setf nodes (mapcar #'(lambda (node)
                              (copy-graph-item new-graph node))
                          (nodes graph))
            edges (mapcar #'(lambda (edge) 
                              (copy-graph-item new-graph edge))
                          (edges graph))))
    new-graph))

(defmethod make-node ((graph simple-graph) &key name label &allow-other-keys)
  (make-instance 'simple-node 
                 :in-graph graph 
                 :label label 
                 :name name))

(defmethod make-edge ((graph simple-graph) (from simple-node) (to simple-node) &key label &allow-other-keys)
  (make-instance 'simple-edge 
                 :in-graph graph 
                 :from from
                 :to to 
                 :label label
                 :name (intern (format nil "EDGE-~A-~A" (name from) (name to)))))

(defmethod delete-node ((graph simple-graph) (node simple-node) &key &allow-other-keys)
  (unless (eq (in-graph node) graph)
    (error "Node ~A not in graph ~A!" node graph))
  (setf (nodes graph)
        (delete node (nodes (in-graph node)))))

(defmethod delete-edge ((graph simple-graph) (edge simple-edge) &key &allow-other-keys)
  (unless (eq (in-graph edge) graph)
    (error "Edge ~A not in graph ~A!" edge graph))
  (setf (edges (in-graph edge))
        (delete edge (edges (in-graph edge)))))

(defmethod copy-node ((new-graph simple-graph) (node simple-node) &key &allow-other-keys)
  (make-instance 'simple-node 
                 :in-graph new-graph 
                 :id (id node)
                 :label (label node)
                 :name (name node)))

(defmethod copy-edge ((new-graph simple-graph) (edge simple-edge) &key &allow-other-keys)
  (make-instance 'simple-edge 
                 :in-graph new-graph 
                 :id (id edge)
                 :label (label edge)
                 :name (name edge)
                 :from (find-node new-graph (id (from edge)))
                 :to (find-node new-graph (id (to edge)))))

(defmethod get-nodes ((graph simple-graph) &rest args)
  (reverse (nodes graph)))

(defmethod get-edges ((graph simple-graph) &rest args)
  (reverse (edges graph)))

(defmethod get-edges-between ((graph simple-graph) (from simple-node) (to simple-node) &rest args)
  (remove-if-not #'(lambda (edge)
                     (and (eq (from edge) from)
                          (eq (to edge) to)))
                 (get-edges graph)))

;;;
;;;
;;;

(defmethod info ((graph simple-graph) &key)
  (format nil "Id: ~A Nodes: ~A, Edges: ~A"
          (id graph)
          (no-of-nodes graph)
          (no-of-edges graph)))

(defmethod info ((node simple-node) &key for-graph-visualizer-p)
  (if for-graph-visualizer-p 
      (print-object node nil)
    (format nil "Id: ~A Label: ~A" 
            (id node) (label node))))

(defmethod info ((edge simple-edge) &key for-graph-visualizer-p)
  (if for-graph-visualizer-p 
      (print-object edge nil)
    (format nil "(~A/~A,~A/~A):~A"
            (name (from edge))
            (id (from edge))
            (name (to edge))
            (id (to edge))
            (label edge))))

;;;
;;;
;;;

(defmethod kind ((graph simple-graph))
  'kind-simple-graph)

(defmethod kind ((node simple-node))
  (label node))

(defmethod kind ((edge simple-edge))
  (label edge))

;;;
;;; wird nur verwendet, wenn kein "kind", ansonsten
;;; "compute-color-from-kind-p t" als default!
;;; 

(defmethod color ((node simple-node))
  +yellow+)

(defmethod color ((edge simple-edge))
  +yellow+)

(let ((graph (make-graph 'simple-graph :name 'graph :delete-if-exists-p t)))
  (setf x graph)
  
  (setf a (make-node graph :name 'a :label 'label-a))
  (setf b (make-node graph :name 'b :label 'label-b))
  (setf c (make-node graph :name 'c :label 'label-c))
  (setf d (make-node graph :name 'd :label 'label-a))
  
  (delete-node 'graph 'd)

  (setf ab1 (make-edge 'graph a b :label 'r1))
  (setf ab2 (make-edge 'graph a b :label 'r2))
  
 
  (visualize graph))

(let ((graph (make-graph 'simple-graph :name 'graph))
      (graph (make-graph 'simple-graph :name 'graph)))
      
  (princ *all-graphs*)
  (delete-graph 'graphd :all-p nil :error-p nil)
  (princ *all-graphs*)
  (delete-graph 'graph :all-p t)
  (princ *all-graphs*))
  
|#

