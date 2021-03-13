;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(timed-defmethod mark-deleted :after ((abox abox) (edge abox-edge)) 
  t)

(timed-defmethod mark-deleted :after ((abox abox1) (edge abox-edge))
  (with-slots (all-edges)  abox
    (if (not *use-avl-trees-for-abox1-p*)
        (setf all-edges (delete edge all-edges))
      (delete-from-avl-tree edge all-edges :key #'id))))

;;;
;;;
;;;

(timed-defmethod delete-edge ((abox abox) (edge abox-edge) &rest args)
  (declare (ignorable args))
  (call-next-method))

(timed-defmethod delete-edge ((abox abox1) (edge abox-edge) &rest args)
  (declare (ignorable args))

  (if (old-p edge)
      (call-next-method)

    (let ((from (from edge))
          (to (to edge)))
      ;;; wird sonst von substrate7.lisp erledigt!
      (setf (slot-value from 'successors) (delete to (slot-value from 'successors)))
      (setf (slot-value to 'predecessors) (delete from (slot-value to 'predecessors)))
            
      (setf (slot-value  from 'outgoing) (delete edge (slot-value from 'outgoing)))
      (setf (slot-value to 'incoming) (delete edge (slot-value to 'incoming))))))

(timed-defmethod delete-edge :after ((abox abox) (edge abox-edge) &rest args)
  (declare (ignorable args))

  (mark-deleted abox edge))

;;;
;;;
;;;

(defmethod get-state-vector ((edge abox-edge))
  (with-slots (old-p 
               active-p 
               deleted-p
               multiplicity
               inverse-multiplicity) edge

    (list ;old-p
     active-p 
     deleted-p
     multiplicity
     inverse-multiplicity)))

(defmethod set-state-vector ((edge abox-edge) state &optional maintain-index-structures-p)
  (declare (ignorable maintain-index-structures-p))
  (with-slots (old-p 
               active-p 
               deleted-p
               multiplicity
               inverse-multiplicity) edge

    (setf ;old-p (pop state)
     active-p  (pop state)
     deleted-p (pop state)
     multiplicity (pop state)
     inverse-multiplicity (pop state))))
