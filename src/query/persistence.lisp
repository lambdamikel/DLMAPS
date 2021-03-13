;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;;
;;;

#+:racer-server
(nrql-defun store-substrate-for-abox (filename &optional (for-abox (current-abox)) (type-of-substrate *type-of-substrate*))

  #+:racer-server 
  (unless *unsafe-mode* 
    (error "Store substrate is only allowed if RACER is started in unsafe mode (option -u)."))
    
  (let ((substrate 
         (find-racer-substrate for-abox type-of-substrate)))

    (if substrate

        (progn 
            
          (persistence-manager:make-object-persistent 
       
           (list (find-abox (abox substrate))
                 substrate)
             
           filename)

          (name substrate))
        
      :not-found)))

(nrql-defun store-all-substrates (filename)

  #+:racer-server 
  (unless *unsafe-mode* 
    (error "Store all substrates is only allowed if RACER is started in unsafe mode (option -u)."))
    
  (persistence-manager:make-object-persistent 
   
   (list (current-abox)
         *cur-substrate* 
         (mapcar #'find-abox (mapcar #'abox *all-substrates*))
         *all-substrates*)
             
   filename)

  (mapcar #'name *all-substrates*))


#+:racer-server
(nrql-defun restore-substrate (filename)
  (let* ((state 
          (racer::with-environment ()
            (persistence-manager:load-persistent-object filename)))
         (substrate (second state))
         (abox (first state)))
    
    (if state

        (progn 

          (setf racer::*current-abox* abox)
          (setf racer::*current-tbox* (racer::tbox racer::*current-abox*))

          (setf (find-abox (abox substrate)) racer::*current-abox*)
          (setf (find-tbox (tbox substrate)) racer::*current-tbox*)
          
          (let* ((old (find-racer-substrate (name substrate)
                                            (type-of substrate)))
                 (dbox (find-dbox (tbox substrate) :error-p nil)))

            (when old
              (delete-substrate old))

            (when dbox
              (setf *all-dboxes* (delete dbox *all-dboxes*))))
           
          (push substrate *all-substrates*)
          (when (dbox substrate)
            (push (dbox substrate) *all-dboxes*))

          (setf *cur-substrate* substrate
                *type-of-substrate* (type-of substrate))
          
          (name substrate))

      :failed)))

#+:racer-server
(nrql-defun restore-all-substrates (filename)
  (let* ((state 
          (racer::with-environment ()
            (persistence-manager:load-persistent-object filename)))
         (substrates (fourth state))
         (aboxes (third state))
         (cur-abox (first state))
         (cur-substrate (second state)))

    (declare (ignorable aboxes))
    
    (if state

        (progn 

          (setf *current-abox* cur-abox)
          (setf *cur-substrate* cur-substrate
                *type-of-substrate* (type-of cur-substrate))
          (setf *current-tbox* (racer::tbox *current-abox*))

          (setf (find-abox (racer::abox-name *current-abox*)) *current-abox*)
          (setf (find-tbox (racer::tbox-name *current-tbox*)) *current-tbox*)
          
          (delete-all-substrates)
          (delete-all-dboxes)
           
          (setf *all-dboxes*
                (mapcar #'dbox substrates))
          (setf *all-substrates* substrates)

          (mapcar #'name substrates))

      :failed)))

;;;
;;;
;;;


#-:racer-server
(nrql-defun store-substrate-for-abox (filename &optional (for-abox (current-abox)) (type-of-substrate *type-of-substrate*))
  (let ((substrate 
         (find-racer-substrate for-abox type-of-substrate)))

    (if substrate

        (progn 
            
          (persistence-manager:make-object-persistent 
           substrate
           (format nil "~A-SUBSTRATE.RQL" filename))

          (store-abox-image (format nil "~A-ABOX.RQL" filename)
                            (find-abox (abox substrate)))
            
          filename)
        
      :not-found)))

#-:racer-server
(nrql-defun restore-substrate (filename)
  (restore-abox-image (format nil "~A-ABOX.RQL" filename))
          
  (let ((substrate (persistence-manager:load-persistent-object 
                    (format nil "~A-SUBSTRATE.RQL" filename))))
    (push substrate *all-substrates*)
    (setf *cur-substrate* substrate
          *type-of-substrate* (type-of substrate))
    
    (name substrate)))

;;;
;;;
;;;

(nrql-defun store-substrate-for-current-abox (filename)
  (store-substrate-for-abox filename))

