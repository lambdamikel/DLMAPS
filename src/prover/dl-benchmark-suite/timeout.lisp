;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(in-package :cl-user)

(defmacro with-timeout ((seconds . timeout-forms) &body body)
  #+:ALLEGRO
  `(mp:with-timeout (,seconds . ,timeout-forms)
     ,@body)
  #+:MCL
  `(with-timeout-1 ,seconds #'(lambda () ,@timeout-forms) #'(lambda () ,@body)))

(defmacro with-timeout ((seconds . timeout-forms) &body body)
  #+:ALLEGRO
  `(let ((timeout-fn #'(lambda () ,@timeout-forms)))
     (unwind-protect
       (restart-case
         (mp:with-timeout (,seconds . (funcall timeout-fn))
           ,@body)
         (abort-execution ()
                          :report
                          (lambda (stream)
                            (format stream "Abort execution as timeout"))
                          (funcall timeout-fn)))))
  #+:MCL
  `(with-timeout-1 ,seconds #'(lambda () ,@timeout-forms) #'(lambda () ,@body)))

#+:MCL
(defun with-timeout-1 (seconds timeout-fn body-fn)
  (let* ((tag 'tag-1)
         (current-process *current-process*)
         (timeout-process 
          (process-run-function
           (format nil "Timeout ~D" (round seconds))
           #'(lambda ()
               (sleep (round seconds))
               (process-interrupt current-process
                                  #'(lambda ()
                                      (throw tag (funcall timeout-fn))))))))
    (catch tag
      (unwind-protect
        (restart-case
          (funcall body-fn)
          (abort-execution ()
                           :report
                           (lambda (stream)
                             (format stream "Abort execution as timeout"))
                           (funcall timeout-fn)))
        (process-kill timeout-process)))))

#+:ALLEGRO
(let ((elapsed-gc-time-user 0)
      (elapsed-gc-time-system 0)
      (elapsed-run-time-user 0)
      (elapsed-run-time-system 0)
      (elapsed-real-time 0)
      (used-cons-cells 0)
      (used-symbols 0)
      (used-other-bytes 0))

  (defun set-time-vars (pelapsed-gc-time-user
                        pelapsed-gc-time-system
                        pelapsed-run-time-user
                        pelapsed-run-time-system
                        pelapsed-real-time
                        pused-cons-cells
                        pused-symbols
                        pused-other-bytes)
    (setf elapsed-gc-time-user pelapsed-gc-time-user)
    (setf elapsed-gc-time-system pelapsed-gc-time-system)
    (setf elapsed-run-time-user pelapsed-run-time-user)
    (setf elapsed-run-time-system pelapsed-run-time-system)
    (setf elapsed-real-time pelapsed-real-time)
    (setf used-cons-cells pused-cons-cells)
    (setf used-symbols pused-symbols)
    (setf used-other-bytes pused-other-bytes))

  (defun gctime ()
    (+ elapsed-gc-time-user elapsed-gc-time-system))
  
  (defun total-bytes-allocated ()
    (+ (* 8 used-cons-cells) (* 64 used-symbols) used-other-bytes)))

#+:MCL
(defun total-bytes-allocated ()
  (ccl::total-bytes-allocated))

(let (#+:mcl initial-gc-time #+:mcl initial-consed
      initial-real-time initial-run-time)
  
  #+:mcl
  (defun with-timed-form (thunk)
    (setf initial-gc-time (gctime))
    (setf initial-consed (total-bytes-allocated))
    (setf initial-real-time (get-internal-real-time))
    (setf initial-run-time (get-internal-run-time))
    (let* ((result (funcall thunk))
           (elapsed-real-time (/ (- (get-internal-real-time) initial-real-time)
                                 internal-time-units-per-second))
           (elapsed-run-time (/ (- (get-internal-run-time) initial-run-time)
                                internal-time-units-per-second))
           (elapsed-mf-time (max 0 (- elapsed-real-time elapsed-run-time)))
           (elapsed-gc-time (/ (- (gctime) initial-gc-time)
                               internal-time-units-per-second))
           (bytes-consed (- (total-bytes-allocated) initial-consed
                            (if (fixnump initial-consed) 0 16))))
      (values result elapsed-run-time elapsed-real-time elapsed-mf-time
              elapsed-gc-time bytes-consed)))
  
  #+:ALLEGRO
  (defun with-timed-form (thunk)
    (setf initial-real-time (get-internal-real-time))
    (setf initial-run-time (get-internal-run-time))
    (let* ((result (excl::time-a-funcall thunk #'set-time-vars))
           (elapsed-real-time (/ (- (get-internal-real-time) initial-real-time)
                                 internal-time-units-per-second))
           (elapsed-run-time (/ (- (get-internal-run-time) initial-run-time)
                                internal-time-units-per-second))
           (elapsed-mf-time (max 0 (- elapsed-real-time elapsed-run-time)))
           (elapsed-gc-time (/ (gctime) internal-time-units-per-second))
           (bytes-consed (total-bytes-allocated)))
      (values result elapsed-run-time elapsed-real-time elapsed-mf-time
              elapsed-gc-time (abs bytes-consed))))
  
  #+:mcl
  (defun timed-out-handler ()
    (let* ((elapsed-real-time (/ (- (get-internal-real-time) initial-real-time)
                                 internal-time-units-per-second))
           (elapsed-run-time (/ (- (get-internal-run-time) initial-run-time)
                                internal-time-units-per-second))
           (elapsed-mf-time (max 0 (- elapsed-real-time elapsed-run-time)))
           (elapsed-gc-time (/ (- (gctime) initial-gc-time)
                               internal-time-units-per-second))
           (bytes-consed (- (total-bytes-allocated) initial-consed
                            (if (fixnump initial-consed) 0 16))))
      (values '(?) elapsed-run-time elapsed-real-time elapsed-mf-time
              elapsed-gc-time bytes-consed)))
  
  #+:ALLEGRO
  (defun timed-out-handler ()
    (let* ((elapsed-real-time (/ (- (get-internal-real-time) initial-real-time)
                                 internal-time-units-per-second))
           (elapsed-run-time (/ (- (get-internal-run-time) initial-run-time)
                                internal-time-units-per-second))
           (elapsed-mf-time (max 0 (- elapsed-real-time elapsed-run-time)))
           (elapsed-gc-time (/ (gctime) internal-time-units-per-second))
           (bytes-consed (total-bytes-allocated)))
      (values '(?) elapsed-run-time elapsed-real-time elapsed-mf-time
              elapsed-gc-time (abs bytes-consed)))))
  
  

