;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROVER -*-

(in-package :PROVER)

;;;
;;;
;;;

(defmacro hist-incoming (node)
  `(let* ((id (id ,node))
          (created-before
           (remove-if-not #'(lambda (in)
                              (< (id (from in))
                                 id))
                          (incoming ,node))))

     (if (some #'old-p created-before)
         nil
       (first created-before))))


(defmethod blocking-blocked-p ((language dl-with-subset-blocking) (blocking abox-node) (blocked abox-node))
  
  ;;; Subset Blocking f. ALCHF-RPLUS

  (loop-over-node-concepts (concept blocked)
    (unless (on-tableau-p blocking concept)
      (return-from blocking-blocked-p nil)))
  
  t)


(defmethod blocking-blocked-p ((language alchi-rplus) (blocking abox-node) (blocked abox-node))
  
  ;;; Equal Blocking f. ALCHI-RPLUS
  
  (loop-over-node-concepts (concept blocked)
    (unless (on-tableau-p blocking concept)
      (return-from blocking-blocked-p nil)))
  
  (loop-over-node-concepts (concept blocking)
    (unless (on-tableau-p blocked concept)
      (return-from blocking-blocked-p nil)))
  
  t)


(defmethod blocking-blocked-p ((language alchif-rplus) (blocking abox-node) (blocked abox-node))
  ; wird doppelt aufgerufen, pairwise blocking 
  (call-next-method))


(defmethod blocking-blocked-p ((language dl) (blocking abox-node) (blocked abox-node))
  nil)
  

;;;
;;;
;;;

(defmethod blocking-or-blocking-predecessor ((abox abox) (blocking node))
  (or (slot-value blocking 'blocking-for)
      (let ((incoming (hist-incoming blocking)))
        (and incoming 
             (blocking-or-blocking-predecessor abox (from incoming))))))


(defmacro loop-between-blocked-blocking ((var blocked blocking) &body body)
  `(let ((,var ,blocked)
         (in nil))
     
     (loop 
      
      ,@body
      
      (when (eq ,var ,blocking)
        (return))

      (setf in (hist-incoming ,var))
      (unless in (error "Node ~A is not a predecessor of ~A!" ,blocking ,blocked))
      (setf ,var (from in)))))

;;;
;;;
;;;


(defmethod find-blocking-node ((abox abox) (blocked node))

  (when (and (active-p blocked)
             (not (old-p blocked))
             (not (blocked-p blocked)))
    
    (let ((incoming
           (hist-incoming blocked))
          (path (list blocked))
          (blocking-node nil))
      
      (loop while incoming do
            (let ((blocking (from incoming)))
                  
              (push blocking path)
              
              (setf incoming (hist-incoming blocking))
              
              (when (blocking-blocked-p *language* blocking blocked)
                
                (announce "Found blocked node ~A: blocked by: ~A" blocked blocking)
                
                (register-blocking-blocked abox blocking blocked) ; deaktiviert den Knoten auch
                
                (setf blocking-node blocking
                      incoming nil))))

      blocking-node)))

(defrule block-nodes (dl-with-simple-blocking abox :args (blocked))
  
  (when *blocking-enabled-p* 
      (if blocked
          (find-blocking-node abox blocked)
        (loop-over-leaf-nodes (blocked abox)
                              (find-blocking-node abox blocked))))

  +insert-body-code+)


(defrule block-nodes (dl-with-pairwise-blocking abox :args (blocked))
  
  (labels ((do-it (blocked) 
             
             (when (and (active-p blocked)
                        (not (old-p blocked))
                        (not (blocked-p blocked)))
        
               (let* ((x blocked)
                      
                      (incoming
                       (hist-incoming x)))

                 (when (and incoming
                            (not (deleted-p incoming)))
                     
                   (let* ((role (role incoming))
                          (x1 (from incoming))
                          
                          (incoming (hist-incoming x1))

                          (path (list x1 x)))

                     (when (and x x1 incoming
                                (not (deleted-p incoming)))

                       (loop while incoming do
                         
                             (let* ((y (from incoming))
                                    (blocking y))

                               (setf incoming
                                     (hist-incoming y))

                               (push y path)

                               (when (and incoming 
                                          (not (deleted-p incoming)))
                                 
                                 (let ((y1 (from incoming)))
                                   
                                   (push y1 path)

                                   (if (and (eq (role incoming) role)
                                            (blocking-blocked-p *language* x y)
                                            (blocking-blocked-p *language* x1 y1))

                                       (progn 
                    
                                         (announce "Found pairwise blocked node ~A: blocked by: ~A" 
                                                   blocked blocking)
                                         
                                         (register-blocking-blocked abox blocking blocked) 
                                         
                                         (setf incoming nil))
                           
                                     (setf incoming
                                           (hist-incoming y1))))))))))))))

    (when *blocking-enabled-p* 
      (if blocked
          (do-it blocked)
        (loop-over-leaf-nodes (blocked abox)
          (do-it blocked)))))

  +insert-body-code+)

