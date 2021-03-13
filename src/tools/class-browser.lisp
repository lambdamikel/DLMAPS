(in-package :TOOLS)


(defun make-postscript-file (application-frame filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede)
    (clim:with-output-to-postscript-stream (ps-stream stream 
                                                      :scale-to-fit t)
      (draw-display application-frame ps-stream))))


(define-application-frame class-browser ()
  ((root-node :initform (find-class 'clim:design)
              :initarg :root-node
              :accessor root-node)
   (app-stream :initform nil :accessor app-stream))
  (:panes  (display :application
                    :display-function 'draw-display
                    :display-after-commands :no-clear))
  (:layouts
   (:defaults
    (horizontally () display))))
 
(defmethod draw-display ((frame class-browser) stream)
  (indenting-output (stream '(8 :character))
    (format-graph-from-roots (root-node *application-frame*)
                             #'draw-class-node
                             #'clos:class-direct-subclasses
                             :stream stream
                             :center-nodes nil
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
(defun draw-class-node (object stream)
  (with-output-as-presentation (stream object 'node)
                               (surrounding-output-with-border
                                (stream :shape :rectangle)
                                (format stream "~A"
                                        (class-name object)))))
  
(define-class-browser-command (exit :menu "Exit") ()
  (frame-exit *application-frame*))
  

(define-class-browser-command (com-ps-file :menu "Make Postscript File") ()
  (make-postscript-file *application-frame*
                        "dag.ps" ))


(defun class-browser (&optional (root-node (find-class 'basic-sheet))
                          (port (find-port)))
  (if (atom root-node) (setf root-node (list root-node))) 
  (let ((class-browser (make-application-frame 'class-browser
                                          :frame-manager
                                          (find-frame-manager
                                           :port port)
                                          :width 800
                                          :height 600
                                          :root-node root-node)))
    (run-frame-top-level class-browser)))


(defun show-subclasses (class)
  (class-browser (find-class class)))
