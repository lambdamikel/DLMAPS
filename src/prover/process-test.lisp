
(defvar *count* 0)

(defun test ()
  (let ((*count* 0))
    (mp:process-run-function "TEST"
                             nil
                             #'(lambda ()
                                 (let ((*count* 'test))
                                   (princ *count*)
                                   (terpri)
                                   (let ((*count* 'test2))
                                     (test2))
                                   (princ *count*)
                                   (princ 'done)
                                   (terpri)
                                   (princ *count*) (terpri))))
    *count*))

(defun test2 ()
  (princ *count*) 
  (terpri) 
  (let ((*count* 'test3))
    (test3))
  (princ *count*) 
  (terpri))


(defun test3 ()
  (princ *count*) 
  (terpri)
  (setf *count* 0)
  (loop as i from 1 to 100 do (incf *count*))
  (princ *count*))
                                 
