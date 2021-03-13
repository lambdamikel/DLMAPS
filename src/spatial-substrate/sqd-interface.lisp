;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SPATIAL-SUBSTRATE; Base: 10 -*-

(in-package spatial-substrate)

;;;
;;; Schnittstelle zum SQD-Reader: dieser ruft die 
;;; construct-map-object-for...-Routinen auf! 
;;; 

(defmethod construct-object-for ((type (eql :point)) os os2 &key os-num os2-num x y enum line-no)
  (declare (ignorable os2-num))
  (if (or os os2)
      (map-p x y :os os-num :line-no line-no :enum enum :map-color (get-mapcolor-for os type))
    (si-p x y)))

(defmethod construct-object-for ((type (eql :circle)) os os2 &key os-num os2-num x y r enum line-no)
  (declare (ignorable os2-num))
  (let ((points (mapcar #'(lambda (tuple)
                            (si-p (first tuple)
                                  (second tuple)))
                        (make-circle x y r :granularity 10))))
    (map-poly (mapcar #'(lambda (from to)
                         (si-l from to))
                     points 
                     (cdr points))
              ;; :check-p nil
             :os os-num :line-no line-no :enum enum
             :map-color (get-mapcolor-for os type))))

(defmethod construct-object-for ((type (eql :line)) os os2 &key os-num os2-num p1 p2 enum line-no)
  (declare (ignorable os2-num))
  (map-l p1 p2 :os os-num :line-no line-no :enum enum :map-color (get-mapcolor-for os type)))


(defmethod construct-object-for ((type (eql :spline)) os os2 &key os-num os2-num p1 p2 points enum line-no)
  (declare (ignorable os2-num))
  (if points      
      (map-chain
       (create-segment-list os-num p1 p2 points line-no enum)
       :os os-num :line-no line-no :enum enum :map-color (get-mapcolor-for os type))
    (map-l p1 p2 :os os-num :line-no line-no :enum enum :map-color (get-mapcolor-for os type))))


(defmethod construct-object-for ((type (eql :arc)) os os2 &key os-num os2-num x y r w p1 p2 enum line-no)
  (declare (ignorable os2-num r w))
  (let ((points (mapcar #'(lambda (tuple)
                            (si-p (first tuple)
                                  (second tuple)))
                        (make-arc p2 p1 x y :granularity 10)))
        (args (list :os os-num :line-no line-no :enum enum 
                    ;; :check-p nil
                    :map-color (get-mapcolor-for os type))))
    (apply #'map-chain (append (list (apply #'map-l p1 (first points) args))
                              (mapcar #'(lambda (from to)
                                          (apply #'map-l from to args))
                                      points                       
                                      (cdr points))
                              (list (apply #'map-l (first (last points)) p2 args)))
           ;; :check-p nil
           args)))


(defmethod construct-object-for ((type (eql :area)) os os2 &key os-num os2-num components enum line-no)
  (declare (ignorable os2-num))
  (let ((component-segments 
         ;; 
         ;; decompose the compontens into their segments, 
         ;; otherwise we can't create a polygon!
         ;; 
         (apply #'append (mapcar #'(lambda (x)
                                     (etypecase x
                                       (si-geom-chain                                        
                                        (segments x))
                                       (basic-map-line
                                        (list x))
                                       (cons x)))
                                 components))))

    (let* ((polygons
            (sort (create-polygons-from-segments os-num component-segments line-no enum)
                  #'(lambda (x y)
                      (contains-p x y))))
           (polygon (first polygons))
           (holes (rest polygons)))
      ;;
      ;; die "Löcher" werden zwar konstruiert und vermerkt, aber
      ;; nicht als Kartenobjekte zurückgegeben!
      ;; sind daher auch nicht im räumlichen Index etc. 
      ;;
      (setf (holes polygon) holes)
      (mapc #'remove-from-spatial-index holes)
      (mapc #'delete-object
            (remove-if-not #'(lambda (x) 
                               (typep x 'geom-chain))
                           components))
      (list polygon))))


(defmethod construct-object-for ((type (eql :symbol)) os os2 &key os-num os2-num x y name enum line-no)
  (declare (ignorable os2-num name))
  (map-sym x y :os os-num :line-no line-no :enum enum :map-color (get-mapcolor-for os type)))


(defmethod construct-object-for ((type (eql :text)) os os2 &key os-num os2-num x y h w text enum line-no)
  (declare (ignorable os-num os2-num enum line-no))
  (make-instance 'map-text
                 :tx x :ty y
                 :h h :w w
                 :text text))

;;;
;;; Hilfsfunktionen
;;;

(defun create-segment-list (os-num ps pe points line-no enum)
  (let* ((points (transform-xy-list points)))
    (append
     (list (map-l 
            ps 
            (si-p (first (first points))
                  (second (first points)))
            ;; :check-p nil
            :os os-num 
            :line-no line-no
            :enum enum
            :map-color (get-mapcolor-for os-num :line)))
     (mapcar #'(lambda (p1 p2)
                 (map-l (si-p (first p1) (second p1))                 
                       (si-p (first p2) (second p2))
                       ;; :check-p nil            
                       :os os-num 
                       :line-no line-no
                       :enum enum
                       :map-color (get-mapcolor-for os-num :line)))
             points (rest points))
     (list (map-l
            (si-p (first (first (last points)))
                  (second (first (last points))))
            pe
            ;; :check-p nil            
            :os os-num 
            :line-no line-no
            :enum enum
            :map-color (get-mapcolor-for os-num :line))))))

(defun create-polygons-from-segments (os-num components line-no enum)
  (labels ((do-it (components)
             (when components   
               (let ((segments (list (first components)))
                     (components (rest components))
                     (found t))
                 (loop while found do    
                       (setf found nil)
                       (let* ((current (first segments))
                              (next
                               (find-if #'(lambda (next) 
                                            (joins-p current next))
                                        components)))
                         (when next
                           (setf components (delete next components))
                           (setf found t)
                           (push next segments))))
                 (when segments
                   (if (and (cddr segments)
                            (joins-p (first segments)
                                     (first (last segments))))
                       (cons (map-poly segments 
                                      ;; :check-p nil
                                      :os os-num :line-no line-no :enum enum
                                      :map-color (get-mapcolor-for os-num :area))
                             (do-it components))
                     (progn 
                     ;(format t "Warning! Incomplete FL-component!~%")
                       (do-it components))))))))
    
    (do-it (apply #'append 
                  (mapcar #'(lambda (i)
                              (if (listp i)
                                  i
                                (list i)))
                          components)))))
