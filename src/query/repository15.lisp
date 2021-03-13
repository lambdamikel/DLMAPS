;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

;;;
;;;
;;;

(defpersistentclass qbox (dag)
  ((time-stamp :reader time-stamp :initform 0)))

;;;
;;;
;;;

(defmethod clear-repository ((substrate substrate))
  (setf (slot-value substrate 'qbox) nil)
  'okay-repository-cleared)

(defmethod show-qbox ((substrate substrate) &optional definitions-p)
  (when (qbox substrate)
    (show-qbox (qbox substrate)
               definitions-p)))

(defmethod show-qbox ((qbox qbox) &optional definitions-p)
  (visualize-dag qbox
                 ;;; :view :textually
                 :printer 
                 (if definitions-p 
                     'dag-node-name
                   'show-node-name)))


(defmethod show-node-name ((x null))
  nil)

(defmethod show-node-name ((x query))
  (if (equivalents x)
      
      (format nil "~A:~A = ~A"
              (time-stamp x)
              (or (iterator-id x)
                  (format nil "SUBQUERY-~A-OF-~A"
                          (subquery-id x)
                          (iterator-id
                           (top-level-query x))))
              ;;; wichtig, nur so kann man sehen,
              ;;; ob eine Subsumptionsbeziehung dazu
              ;;; verwendet wurde, Kandidaten zu 
              ;;; enumerierten! Zeitliche Reihenfolge!
              (mapcar #'(lambda (x) 
                          (format nil "~A"
                                  (or (iterator-id x)
                                      (format nil "SUBQUERY-~A-OF-~A"
                                              (subquery-id x)
                                              (iterator-id
                                               (top-level-query x))))
                                  ;(time-stamp x)
                                  ))
                      (equivalents x)))

    (format nil "~A:~A"
            (time-stamp x)
            (or (iterator-id x)
                (format nil "SUBQUERY ~A OF ~A"
                        (subquery-id x)
                        (iterator-id
                         (top-level-query x)))))))
    

;;;
;;;
;;;

(defmethod make-qbox ((substrate substrate))
  (let* ((top (make-instance 'master-top-query 
                             :name 'master-top-query 
                             :iterator-id 'master-top-query
                             :time-stamp 0
                             :dont-initialize-p t))
         (bottom (make-instance 'master-bottom-query 
                                :name 'master-bottom-query 
                                :iterator-id 'master-bottom-query
                                :time-stamp 0
                                :dont-initialize-p t))
         (qbox (make-dag :type 'qbox
                         :name (format nil "QBox for Substrate ~A" (name substrate)))))
                   
    (insert-dag-node qbox top)   
    (setf (dag-node-parents bottom) (list top))
    (insert-dag-node qbox bottom)
    
    qbox))

;;;
;;;
;;;

(defmethod get-qbox ((query query))
  (qbox (substrate query)))

(defmethod get-qbox ((substrate substrate))
  (qbox substrate))

;;;
;;;
;;;

(defmethod delete-dag-node :before ((dag qbox) (query query))
  (dolist (equivalent-query (equivalents query))
    (setf (slot-value equivalent-query 'equivalents)
          (delete query (slot-value equivalent-query 'equivalents)))))

;;;
;;;
;;;


(defmethod remove-outdated-qbox-nodes ((substrate substrate))
  (remove-outdated-qbox-nodes (qbox substrate)))

(defmethod remove-outdated-qbox-nodes ((qbox qbox)) 
  (dolist (query (dag-nodes qbox))
    (when (or (not (valid-qbox-entry-p query))
              (not (query-accurate-p query)))
      (delete-dag-node qbox query))))

;;;
;;;
;;;

(defmethod classify ((query query))

  (let ((substrate (substrate query))
        (qbox (get-qbox query)))
          
    (unless qbox
      (setf qbox (make-qbox substrate))
      (setf (slot-value (substrate query) 'qbox) qbox))

    (unless (dag-node-parents query)

      (remove-outdated-qbox-nodes qbox)

      (setf (slot-value query 'time-stamp)
            (time-stamp qbox))

      (let ((*warnings-p* nil))

        (let ((parents (compute-node-parents query qbox))
              (children (compute-node-children query qbox)))

          (when *debug-p* 
            (format t "~%~% ~A -> Parents: ~A Children: ~A~%" query parents children))
          
          (if (and (set-equal parents children)
                   parents
                   (not (cdr parents))
                   (not (cdr children)))

              (let ((equi-node (car parents)))
                (unless (eq equi-node query)
                
                  (setf (slot-value query 'equivalents)
                        (list equi-node))
                  (pushnew query (slot-value equi-node 'equivalents))))
                                
            (setf (dag-node-parents query) parents 
                  (dag-node-children query) children))))))
    
  'classified)

;;;
;;;
;;;

(defmethod compute-node-parents ((query query) (qbox qbox) &rest args)
  (declare (ignorable args))

  (labels ((mark-all-descendants (node)
             (mark-dag-node node)
             (mapc #'mark-all-descendants (dag-node-children node)))               
           (do-it (nodes)
             (when nodes
               (let ((current (first nodes))
                     (nodes (rest nodes)))
                 (if (dag-node-marked-p current)
                     (do-it nodes)
                   (if (query-entails-p query current
					:enforce-same-arity-p t)
                       (progn
                         (do-it (append nodes (dag-node-children current))))
                     (progn 
                       (mark-all-descendants current)
                       (do-it nodes))))))))
    

    (if *syntactic-repository-p* 

        (or 
         (remove-if-not #'(lambda (q) 
                            ;;; syntaktischer Cache-Hit
                            (tree-equal (original-query q)
                                        (original-query query)))
                        (dag-nodes qbox))
         (list (dag-top qbox)))
        
      (when (dag-nodes qbox)    
        (unmark-all-dag-nodes qbox)      
        (do-it (list (dag-top qbox)))      
        (remove-duplicates 
         (remove-if-not #'(lambda (q) 
                            (and (not (dag-node-marked-p q))
                                 (every #'(lambda (child) 
                                            (dag-node-marked-p child))
                                        (dag-node-children q))))
                        (dag-nodes qbox)))))))
    
(defmethod compute-node-children ((query query) (qbox qbox) &rest args)
  (declare (ignorable args))

  (labels ((mark-all-ancestors (query)
             (mark-dag-node query)
             (mapc #'mark-all-ancestors (dag-node-parents query)))

           (unmark-relevant-dag-nodes (dag val)
             (dolist (query (dag-nodes dag))
               (if (and (dag-node-marked-p query)
                        (= (dag-node-marked-p query)
                           val))
                   (unmark-dag-node query)
                 (mark-dag-node query))))
           
           (mark-all-descendants (query val)
             (let ((node-val (dag-node-marked-p query)))
               (when (or (and (not node-val)
                              (= val 1))
                         (and node-val
                              (= node-val (1- val))))
                 (mark-dag-node query val))
               (mapc #'(lambda (x)
                         (mark-all-descendants x val))
                     (dag-node-children query))))
             
           (do-it (nodes)

             (when nodes
               (let ((current (first nodes))
                     (nodes (rest nodes)))

                 (if (dag-node-marked-p current)
                     ;;; richtig! s. unmark-relevant-... -> toggelt die markierung!
                     
                     (do-it nodes)
                   
                   (if (query-entails-p current query
					:enforce-same-arity-p t)
                       
                       (progn
                         (do-it (if (dag-node-parents current)
                                    (append nodes (dag-node-parents current))
                                  nodes)))
                     (progn 
                       (mark-all-ancestors current)
                       (do-it nodes))))))))

    
    (if *syntactic-repository-p* 
        (or (remove-if-not #'(lambda (q) 
                               ;;; syntaktischer Cache-Hit
                               (tree-equal (original-query q)
                                           (original-query query)))
                           (dag-nodes qbox))
            (list (dag-bottom qbox)))
            
      (when (dag-nodes qbox)
        (unmark-all-dag-nodes qbox)
      
        (let* ((parents (dag-node-parents query))
               (n (length parents)))
        
          (when parents          
                    
            (mark-all-descendants (first parents) 1)      
          
            (loop as parent in (rest parents) 
                  as i from 2 by 1 do
                  (mark-all-descendants parent i))

            (unmark-relevant-dag-nodes qbox n))

          (when (dag-node-marked-p (dag-bottom qbox))
            (nrql-error "Query repository: internal error"))
          
          (do-it (list (dag-bottom qbox))))
      
        (remove-duplicates 
         (remove-if-not #'(lambda (q) 
                            (and (not (dag-node-marked-p q))
                                 (every #'(lambda (parent) 
                                            (dag-node-marked-p parent))
                                        (dag-node-parents q))))
                        (dag-nodes qbox)))))))

;;;
;;;
;;;

(defmethod unregister-query ((query query))
  (if (in-dag query)
      (delete-dag-node (in-dag query) query)
    (dolist (equivalent-query (equivalents query))
      (setf (slot-value equivalent-query 'equivalents)
            (delete query (slot-value equivalent-query 'equivalents))))))

(defmethod register-query ((query query))
  (let* ((query-stamp
          (time-stamp query))
         (qbox-stamp 
          (when (get-qbox query)
            (time-stamp (get-qbox query)))))

    (labels ((register (query) 
               (let ((equivalents (equivalents query))
                     (parents (dag-node-parents query))
                     (children (dag-node-children query))
                     (qbox (get-qbox query)))

                 (incf (slot-value qbox 'time-stamp))
                 (setf (slot-value query 'time-stamp)
                       (time-stamp qbox))

                 (when *debug-p* (format t "REGISTER ~A: ~A ~A ~A~%" query equivalents parents children))
                 
                 (unless equivalents 
                   ;;; es wird immer nur der erste Knoten einer Aequivalenzklasse registriert!
                   (insert-dag-node qbox query)))))

      (cond ((or (not qbox-stamp)
                 (not query-stamp))

             (when *debug-p* (format t  "*** Classify ~A~%" query))

             (classify query)

             (register query))

            ((and qbox-stamp query-stamp
                  (not (= qbox-stamp query-stamp)))
             
             (let ((qbox (get-qbox query)))

               (delete-dag-node qbox query)
               
               (when *debug-p* (format t  "*** RE-Classify ~A~%" query))
               
               (classify query) 
               (register query)))

            (t 

             (when *debug-p* (format t "**** REGISTER ~A~%" query))
             
             (unless (in-dag query)
               (register query)))))))
