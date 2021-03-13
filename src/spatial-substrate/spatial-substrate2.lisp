;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SPATIAL-SUBSTRATE; Base: 10 -*-

(in-package spatial-substrate)

(defparameter *cur-map* nil)

;;;
;;;
;;;

(defmacro relevant-p (obj)
  `(is-map-node-p ,obj))

(defmacro spatially-relevant-p (obj)
  `(and (is-map-node-p ,obj)        
        (primary-p ,obj)))

;;;
;;;
;;;

(defvar *racer-cache* (make-hash-table :size 100))

(defun reset-racer-cache ()
  (clrhash *racer-cache*))

(defun concept-ancestors (concept tbox)
  (if concept
      (or (gethash concept *racer-cache*)
          (setf (gethash concept *racer-cache*)
                (cons (list concept)
                      (racer:atomic-concept-ancestors concept tbox))))
    (list (list 'top '*top*))))

;;;
;;; abstrakte Klassen
;;;

(defpersistentclass final-class)

(defpersistentclass map* ()
  ((spatial-index :reader spatial-index :initarg :spatial-index :initform nil)

   (qbox :initarg :qbox :reader qbox :initform nil)
   
   (name :reader name :initarg :name)
   (text-objects :accessor text-objects :initform nil)

   ;;; auch wenn einige Unterklassen diesen Slot nicht nuetzen werden, 
   ;;; aber es vereinfacht den Sourcecode... 
   
   (dc-edges-p :accessor dc-edges-p :initform nil)))

(defmethod racer-package ((map map*))
  (if (slot-exists-p map 'racer-package)
      (slot-value map 'racer-package)
    *package*))

;;;
;;;
;;;

(defmethod info ((map* map*) &key &allow-other-keys)
  (format nil "Map of type ~A, Nodes: ~A, Edges: ~A, DC-Edges: ~A"
          (type-of map*)
          (no-of-nodes map*)
          (no-of-edges map*)
          (dc-edges-p map*)))

;;;
;;;
;;;

(defmethod copy ((map* map*) &key &allow-other-keys)
  (error "Copy a map, really???"))

;;;
;;;
;;;

(defpersistentclass basic-map (map* substrate)) ; normales Substrate, simple-descriptions etc. 

(defpersistentclass racer-map (map* racer-substrate) ; assoziierte ABox, s. racer-substrate
  ((closed-roles-p :accessor closed-roles-p :initform nil)
   (racer-package :initform (find-package :racer-user))))


(defmethod info ((map* racer-map) &key &allow-other-keys)
  (format nil "Map of type ~A, Nodes: ~A, Edges: ~A, DC-Edges: ~A, Closed Roles: ~A"
          (type-of map*)
          (no-of-nodes map*)
          (no-of-edges map*)
          (dc-edges-p map*)
          (closed-roles-p map*)))

(defpersistentclass racer-descriptions-map (map* racer-descriptions-substrate) ; Substrate m. Racer-Descriptions, ohne ABox! 
  ((racer-package :initform (find-package :racer-user))))

#+:midelora
(defpersistentclass midelora-map (map* ts::midelora-substrate) ; f. eigenen Beweiser! 
  ((dl-package :initform (find-package :prover))
   (closed-roles-p :accessor closed-roles-p :initform nil)))

(defmethod graph::find-node ((substrate midelora-map) (name symbol) &key &allow-other-keys)
  (when (node-table substrate)
    (gethash name (node-table substrate))))
   

;;;
;;; "Mixin-Klassen"
;;;

(defpersistentclass explicit-relations-map (map*))

(defpersistentclass implicit-relations-map (map*))

;;;
;;; Klassen weiter aufschluesseln nach expliziten/impliziten Kanten 
;;; Implizit = anhand der Geometrie kann natürlich alles berechnet werden an rauemlichen Relationen / Kanten 
;;; Explizit = einige (alle) der impliziten Kanten sind materialisiert -> schnelle Navigation
;;; Basic-Map:
;;;

(defpersistentclass basic-explicit-relations-map (basic-map explicit-relations-map final-class))

(defpersistentclass basic-implicit-relations-map (basic-map implicit-relations-map final-class))

;;;
;;; diese sind *noch nicht* FINAL, s. unten!
;;;

(defpersistentclass racer-explicit-relations-map (racer-map explicit-relations-map))

(defpersistentclass racer-implicit-relations-map (racer-map implicit-relations-map))

;;;
;;; diese SIND final 
;;;

#+:midelora
(defpersistentclass midelora-explicit-relations-map (midelora-map explicit-relations-map final-class

                                                                  ))

#+:midelora
(defpersistentclass midelora-implicit-relations-map (midelora-map implicit-relations-map final-class
                                                                  )
  ((ts::needs-filler-reasoning-p :initform t)

   (created-succs-hash :reader created-succs-hash :initform (make-hash-table :size 1000 :test #'equal))))


#| 
#+:midelora
(defpersistentclass basic-explicit-midelora-implicit-relations-map (midelora-implicit-relations-map basic-explicit-relations-map final-class))

#+:midelora
(defpersistentclass basic-implicit-midelora-explicit-relations-map (midelora-explicit-relations-map basic-implicit-relations-map final-class))

#+:midelora
(defpersistentclass basic-explicit-midelora-explicit-relations-map (midelora-explicit-relations-map basic-explicit-relations-map final-class))

#+:midelora
(defpersistentclass basic-implicit-midelora-implicit-relations-map (midelora-implicit-relations-map basic-implicit-relations-map final-class))

|#

;;;
;;; 
;;;

(defpersistentclass racer-descriptions-explicit-relations-map (racer-descriptions-map explicit-relations-map final-class))

(defpersistentclass racer-descriptions-implicit-relations-map (racer-descriptions-map implicit-relations-map final-class))

;;;
;;; Kombinationen: Kanten explizit in ABox, implizit im Substrate; und andersrum
;;; macht natuerlich nur Sinn, wenn Unterklassen von racer-map, weil nur diese ABox-Assoziation bereitstellen!
;;; 

(defpersistentclass basic-explicit-racer-implicit-relations-map (racer-implicit-relations-map basic-explicit-relations-map final-class))

(defpersistentclass basic-implicit-racer-explicit-relations-map (racer-explicit-relations-map basic-implicit-relations-map final-class))

(defpersistentclass basic-explicit-racer-explicit-relations-map (racer-explicit-relations-map basic-explicit-relations-map final-class))

(defpersistentclass basic-implicit-racer-implicit-relations-map (racer-implicit-relations-map basic-implicit-relations-map final-class))

;;;
;;; Klassen fuer Objekte
;;;

(defpersistentclass map-object (substrate-object))

(defpersistentclass map-node (map-object substrate-node si-geom-thing) ; abstrakt
  ((map-color :accessor map-color :initarg :map-color :initform +black+)
   
   (os :accessor os :initarg :os :initform nil)
   (interpreted-os :reader interpreted-os :initform nil)

   (enum :accessor enum :initarg :enum :initform nil)
   (line-no :accessor line-no :initarg :line-no :initform nil)))

(defpersistentclass map-edge (map-object substrate-edge)) ; abstrakt

;;;
;;;
;;; 


(defmethod info ((node map-node) &key &allow-other-keys)
  (format nil "~A" (fourth (lookup-os (os node)))))

(defmethod info ((edge map-edge) &key &allow-other-keys)
  (format nil "~A"
          (get-textual-description (description edge))))

;;;
;;; Basic-Map-Objekte
;;;

(defpersistentclass basic-map-edge (map-edge))

(defpersistentclass basic-map-point (map-node si-geom-point))

(defpersistentclass basic-map-symbol (basic-map-point))

(defpersistentclass basic-map-line (map-node si-geom-line))

(defpersistentclass basic-map-chain (map-node si-geom-chain))

(defpersistentclass basic-map-polygon (map-node si-geom-polygon)
  ((holes :accessor holes :initform nil)

   (xy-list :accessor xy-list :initform nil)))


;;; 
;;; si-geom-aggregate ist noch nicht implementiert in spatial-index.lisp!
;;; 
;;; (defpersistentclass basic-map-aggregate (map-object si-geom-aggregate))
;;; 

;;;
;;; Racer-Map-Objekte (assoziierte ABox)
;;;

(defpersistentclass racer-map-edge (map-edge racer-substrate-edge))

(defpersistentclass racer-map-node (map-node racer-substrate-node))

(defpersistentclass racer-map-point (racer-map-node basic-map-point))

(defpersistentclass racer-map-symbol (racer-map-node basic-map-symbol))

(defpersistentclass racer-map-line (racer-map-node basic-map-line))

(defpersistentclass racer-map-chain (racer-map-node basic-map-chain))

(defpersistentclass racer-map-polygon (racer-map-node basic-map-polygon))

;;; (defpersistentclass racer-map-aggregate (racer-map-node basic-map-aggregate))


;;;
;;; "Racer-Descriptions"-Objekte (nicht unbedingt notwendig, diese Klassen, aber sei's drum)... 
;;;

(defpersistentclass racer-descriptions-map-edge (map-edge racer-descriptions-substrate-edge))

(defpersistentclass racer-descriptions-map-node (map-node racer-descriptions-substrate-node))

(defpersistentclass racer-descriptions-map-point (racer-descriptions-map-node basic-map-point))

(defpersistentclass racer-descriptions-map-symbol (racer-descriptions-map-node basic-map-symbol))

(defpersistentclass racer-descriptions-map-line (racer-descriptions-map-node basic-map-line))

(defpersistentclass racer-descriptions-map-chain (racer-descriptions-map-node basic-map-chain))

(defpersistentclass racer-descriptions-map-polygon (racer-descriptions-map-node basic-map-polygon))

;;; (defpersistentclass racer-descriptions-map-aggregate (racer-descriptions-map-node basic-map-aggregate))

;;;
;;; MIDELORA-Map-Objekte fuer eigene Beweiser
;;;

#+:midelora
(defpersistentclass midelora-map-edge (map-edge prover::abox-edge)
  ((materialized-virtual-p :accessor materialized-virtual-p :initform nil
                           :initarg :materialized-virtual-p)))

#+:midelora
(defpersistentclass midelora-map-node (map-node prover::abox-node))

#+:midelora
(defpersistentclass midelora-map-point (midelora-map-node basic-map-point))

#+:midelora
(defpersistentclass midelora-map-symbol (midelora-map-node basic-map-symbol))

#+:midelora
(defpersistentclass midelora-map-line (midelora-map-node basic-map-line))

#+:midelora
(defpersistentclass midelora-map-chain (midelora-map-node basic-map-chain))

#+:midelora
(defpersistentclass midelora-map-polygon (midelora-map-node basic-map-polygon))

;;; (defpersistentclass midelora-map-aggregate (abox-node basic-map-aggregate))

;;;
;;;
;;;

(defmethod get-current-edge-class ((map basic-map))
  'basic-map-edge)

(defmethod get-current-point-class ((map basic-map))
  'basic-map-point)

(defmethod get-current-line-class ((map basic-map))
  'basic-map-line)

(defmethod get-current-chain-class ((map basic-map))
  'basic-map-chain)

(defmethod get-current-polygon-class ((map basic-map))
  'basic-map-polygon)

(defmethod get-current-aggregate-class ((map basic-map))
  'basic-map-aggregate)

(defmethod get-current-symbol-class ((map basic-map))
  'basic-map-symbol)

;;;
;;;
;;;

(defmethod get-current-edge-class ((map racer-map))
  'racer-map-edge)

(defmethod get-current-point-class ((map racer-map))
  'racer-map-point)

(defmethod get-current-line-class ((map racer-map))
  'racer-map-line)

(defmethod get-current-chain-class ((map racer-map))
  'racer-map-chain)

(defmethod get-current-polygon-class ((map racer-map))
  'racer-map-polygon)

(defmethod get-current-aggregate-class ((map racer-map))
  'racer-map-aggregate)

(defmethod get-current-symbol-class ((map racer-map))
  'racer-map-symbol)

;;;
;;;
;;;

(defmethod get-current-edge-class ((map racer-descriptions-map))
  'racer-descriptions-map-edge)

(defmethod get-current-point-class ((map racer-descriptions-map))
  'racer-descriptions-map-point)

(defmethod get-current-line-class ((map racer-descriptions-map))
  'racer-descriptions-map-line)

(defmethod get-current-chain-class ((map racer-descriptions-map))
  'racer-descriptions-map-chain)

(defmethod get-current-polygon-class ((map racer-descriptions-map))
  'racer-descriptions-map-polygon)

(defmethod get-current-aggregate-class ((map racer-descriptions-map))
  'racer-descriptions-map-aggregate)

(defmethod get-current-symbol-class ((map racer-descriptions-map))
  'racer-descriptions-map-symbol)


;;;
;;;
;;;

#+:midelora
(defmethod get-current-edge-class ((map midelora-map))
  'midelora-map-edge)

#+:midelora
(defmethod get-current-point-class ((map midelora-map))
  'midelora-map-point)

#+:midelora
(defmethod get-current-line-class ((map midelora-map))
  'midelora-map-line)

#+:midelora
(defmethod get-current-chain-class ((map midelora-map))
  'midelora-map-chain)

#+:midelora
(defmethod get-current-polygon-class ((map midelora-map))
  'midelora-map-polygon)

#+:midelora
(defmethod get-current-aggregate-class ((map midelora-map))
  'midelora-map-aggregate)

#+:midelora
(defmethod get-current-symbol-class ((map midelora-map))
  'midelora-map-symbol)

;;;
;;; 
;;; 

(defmethod get-current-edge-class ((map basic-explicit-racer-implicit-relations-map))
  'basic-map-edge)

(defmethod get-current-edge-class ((map basic-explicit-racer-explicit-relations-map))
  'racer-map-edge)

;;;
;;;
;;;

(defmethod get-current-edge-class ((map basic-implicit-relations-map))
  (error "Since ~A is a ~A, we cannot create any edges!" map (type-of map)))

;;;
;;;
;;;

(defpersistentclass map-text ()
  ((matrix :accessor matrix :initarg :matrix :not-persistent)
   (text :accessor text :initarg :text)
   (bbox :accessor bbox)
   (tx :accessor tx :initarg :tx)
   (ty :accessor ty :initarg :ty)
   (h :accessor h :initarg :h)
   (w :accessor w :initarg :w)))

(defmethod initialize-instance :after ((obj map-text) &rest initargs)
  (push obj (slot-value (get-current-map) 'text-objects)))


(defmethod init ((obj map-text) stream)
  (with-slots (h tx ty w matrix) obj
    (let* ((sx (/ h 4))
	   (sy (/ h 4))
	   (r (deg-to-rad w))
	   (trans1 (make-translation-transformation tx ty))
	   (trans2 (make-scaling-transformation sx sy))
	   (trans3 (make-rotation-transformation r))
	   (transx (compose-transformations trans2 trans3)))
      (setf matrix
	    (compose-transformations trans1 transx))))
  (multiple-value-bind (xf yf xt yt)	
      (bounding-rectangle*
       (with-output-to-output-record (stream)
         (draw obj stream)))
    (setf (bbox obj)
          (bb xf yf xt yt)))
  obj)

;;;
;;;
;;;

(defmethod objects ((map map*))
  (get-nodes map))

;;;
;;;
;;;

#+:midelora
(defmethod get-standard-edge-class ((map midelora-map))
  'midelora-map-edge)

;;;
;;; Beschreibungen
;;; 

(defpersistentclass map-description (description))

(defpersistentclass map-node-description (map-description node-description))

(defpersistentclass map-edge-description (map-description edge-description))

;;;
;;;
;;;

(defpersistentclass basic-map-simple-node-description (map-node-description simple-node-description))

(defpersistentclass basic-map-simple-edge-description (map-edge-description simple-edge-description))


(defpersistentclass racer-map-node-description (basic-map-simple-node-description))

(defpersistentclass racer-map-edge-description (basic-map-simple-edge-description))


(defpersistentclass racer-description-map-node-description (map-node-description racer-node-description))

(defpersistentclass racer-description-map-edge-description (map-edge-description racer-edge-description))


#+:midelora
(defpersistentclass midelora-map-node-description (map-node-description prover::node-label))

#+:midelora
(defpersistentclass midelora-map-edge-description (map-edge-description prover::edge-label))

#|

#+:midelora
(defpersistentclass midelora-map-node-description (basic-map-simple-node-description prover::node-label ))

#+:midelora
(defpersistentclass midelora-map-edge-description (basic-map-edge-description prover::edge-label))

|# 

;;;
;;; Default-Beschreibungen setzen 
;;;

(defmethod get-standard-node-description-class ((map basic-map))
  'basic-map-simple-node-description)

(defmethod get-standard-edge-description-class ((map basic-map))
  'basic-map-simple-edge-description)



(defmethod get-standard-node-description-class ((map racer-map))
  'racer-map-node-description)

(defmethod get-standard-edge-description-class ((map racer-map))
  'racer-map-edge-description)


(defmethod get-standard-node-description-class ((map racer-descriptions-map))
  'racer-description-map-node-description)

(defmethod get-standard-edge-description-class ((map racer-descriptions-map))
  'racer-description-map-edge-description)


#+:midelora
(defmethod get-standard-node-description-class ((map midelora-map))
  'midelora-map-node-description)

#+:midelora
(defmethod get-standard-edge-description-class ((map midelora-map))
  'midelora-map-edge-description)

;;;
;;; s. racer-substrate5.lisp -> ruft diese Funktionen auf, um aus textual-description die
;;; Beschreibung f. ABox-Individuum / -Kante zu erzeugen
;;;

(defmethod convert-to-racer-description ((descr racer-map-node-description) &optional package)
  ;; (change-package-of-description `(and ,@(textual-description descr)) package t))
  (change-package-of-description (first (textual-description descr)) package t))

(defmethod convert-to-racer-description ((descr racer-map-edge-description) &optional package)
  (let ((descr (change-package-of-description (textual-description descr) package t)))
    (if (and (consp descr) (not (cdr descr)))
        (car descr)
      descr)))

;;;
;;; Beschreibungen initialisieren: OS-Num-Umwandlung in "Konzepte" / Namen etc. anstoßen! 
;;;

(defmethod initialize-description ((descr basic-map-simple-node-description))
  (setf (textual-description descr) 
        (let* ((os (lookup-os (textual-description descr)))
               (concept (fourth os)))
          (compute-os-closure descr concept))))

(defmethod initialize-description ((descr basic-map-simple-edge-description))
  (setf (textual-description descr) 
        (ensure-list (textual-description descr))))

;;;
;;;
;;;

(defmethod initialize-description ((descr racer-description-map-node-description))  
  (let* ((os (lookup-os (textual-description descr)))
         (concept (fourth os)))

    (with-slots (textual-description racer-package racer-tbox) descr

      (setf racer-package (racer-package (get-current-map)))

      (setf racer-tbox (tbox (get-current-map)))
      
      (setf textual-description
            `(and ,@(compute-os-closure descr concept))))))
        

(defmethod initialize-description ((descr racer-description-map-edge-description))
  (with-slots (racer-tbox racer-package textual-description) descr
    (setf racer-package (racer-package (get-current-map)))

    (setf racer-tbox
          (tbox (get-current-map)))

    (setf textual-description
          (change-package-of-description textual-description (racer-package descr) t nil))))

;;;
;;;
;;;
;;;

#+:midelora
(defmethod initialize-description :before ((descr midelora-map-node-description))
  (let* ((os (lookup-os (textual-description descr)))
         (concept (fourth os)))
    
    (setf (textual-description descr) 
          (change-package-of-description concept :prover t nil))))

#+:midelora
(defmethod initialize-description :before ((descr midelora-map-edge-description))
  (with-slots (racer-tbox textual-description) descr
    (setf textual-description           
          (change-package-of-description textual-description :prover t nil))))


;;;
;;; Load, Save, Install
;;;

(defmethod install-as-current-map ((obj map*))
  (setf *cur-map* obj)
  (when (spatial-index obj)
    (install-as-current-index 
     (spatial-index obj)))
  (in-substrate* obj)
  obj)

;;;
;;;
;;;

(defmethod save-map ((obj map*) &optional name)
  (unless (make-object-persistent obj
                                  (translate-logical-pathname
                                   (or name 
                                       (format nil "~A.map" (name obj)))))
    (error "Can't save ~A!" obj)))


(defmethod save-map :after ((obj racer-map) &optional name)
  (racer:store-kb-image (format nil "~A.kb" 
                                (translate-logical-pathname (or name (name obj))))
                        (abox obj)))

(defun load-map (fn)  
  (setf *cur-map* nil)
  (let ((map (load-persistent-object fn)))
    (reset-racer-cache)
    (unless map
      (error "Can't load ~A!" fn))
    (when (typep map 'racer-map)
      (racer:restore-kb-image (format nil "~A.kb" fn)))
    (install-as-current-map map)
    map))

;;;
;;;
;;;
		     
(defun make-map-from-sqd-file (fn &rest args &key (type 'explicit-relations-map) &allow-other-keys)
  (load-geo-ontology)

  (setf *id-counter* 0)

  (let* ((map 
          (apply #'ts::in-substrate-fn (intern fn)
                 :delete-if-exists-p t 
                 :filename fn
                 :new-abox-p t
                 :new-tbox-p nil
                 :type type 
                 :tbox (case type
                         #+:midelora
                         ((midelora-explicit-relations-map 
                           midelora-implicit-relations-map
                           basic-explicit-midelora-implicit-relations-map 
                           basic-implicit-midelora-explicit-relations-map
                           basic-explicit-midelora-explicit-relations-map
                           basic-implicit-midelora-implicit-relations-map 
                           )
                          (prover::current-tbox))
                         (otherwise nil))
                 args)))
    
    (with-substrate* (map :delete-if-exists-p nil)
      (install-as-current-map map)	    
      (read-sqd-file fn))

    (finalize map)

    map))

;;;
;;; wichtig: wenn die Substrate-Kanten implizit sind, kann natuerlich 
;;; auch nicht die entsp. "make-edge"-Methode aus racer-substrate.lisp
;;; die Role Assertion erzeugen, da ja keine explizite Kante erzeugt wird...  
;;; also muss "make-edge" ueberladen werden! 
;;; 

(defmethod make-edge ((substrate basic-implicit-racer-explicit-relations-map) 
                      (from map-node) (to map-node)
                      &rest args
                      &key description)
  (racer:add-role-assertion (abox substrate)
                            (racer-object from) (racer-object to)
                            (convert-to-racer-description description (racer-package substrate))))

;;;
;;; 
;;;

(defmethod finalize ((map explicit-relations-map))
  (labels ((do-it (a b)
             (when (and (not (get-edges-between map b a :error-p nil))
                        (not (get-edges-between map a b :error-p nil)))
               (let ((res (calculate-rcc-relation a b)))
                 (when (or (dc-edges-p map)
                           (not (eq res :dc)))
                   (create-edge map a b 
                                (make-standard-edge-description res)
                                :register-choice-points-p nil
                                :convert-to-racer-description-p (typep map 'racer-explicit-relations-map)
                                :type (get-current-edge-class map))
                   (create-edge map b a
                                (make-standard-edge-description (inverse-rcc-relation res))
                                :register-choice-points-p nil
                                :convert-to-racer-description-p (typep map 'racer-explicit-relations-map)
                                :type (get-current-edge-class map)))))))


    (dolist (a (get-nodes map))
      (when (spatially-relevant-p a)
        (if (dc-edges-p map)
            (dolist (b (get-nodes map))
              (when (spatially-relevant-p b)
                (do-it a b)))
          (with-relevant-objects (b a (and (spatially-relevant-p b)
                                           (not (eq a b))))
                                 (do-it a b)))))
    
    map))

(defmethod finalize :before ((map map*))  
  (sort-current-spatial-index))

(defmethod finalize  ((map map*))  
  map)

(defmethod finalize :after ((map racer-substrate))
  (racer:set-current-abox (abox map)))

#+:midelora
(defmethod finalize :after ((map prover::abox))
  (prover::set-current-abox map))

;;;
;;;
;;;

(defmethod close-roles ((map racer-explicit-relations-map))
  (unless (closed-roles-p map)
    (let* ((nodes            
            (remove-if-not #'(lambda (x) (spatially-relevant-p x))
                           (get-nodes map)))
           (no-of-other-nodes (length nodes)))
      (dolist (a nodes)
        (let ((m no-of-other-nodes)) ; wegen fehlender EQ-loop am Knoten selbst
          (dolist (rel (list :ec ; :dc
                             :eq 
                             :tpp :tppi 
                             :ntpp :ntppi :po
                             :pp :ppi
                             ))
            (let ((n 0))        
              (dolist (edge (outgoing a))
                (unless (spatially-relevant-p (to edge))
                  (error "!"))
                             
                (when (equal (textual-description (description edge)) 
                             (list rel))
                  (decf m)
                  (incf n)))

              (let ((rel (change-package-of-description rel :racer-user t)))
                
                (racer:add-concept-assertion (abox map)
                                             (racer-object a)
                                             `(racer-user::exactly ,n ,rel)))))))

      (setf (closed-roles-p map) t))))


#+:midelora
(defmethod close-roles ((map midelora-explicit-relations-map))
  (unless (closed-roles-p map)
    (let* ((nodes            
            (remove-if-not #'(lambda (x) (spatially-relevant-p x))
                           (get-nodes map)))
           (no-of-other-nodes (length nodes))
           (roles (mapcar #'(lambda (x) 
                              (make-description 'map-simple-rcc-or-edge-query (list :or x)
                                                :dont-initialize-p t))
                          (list :ec ; :dc
                                :eq 
                                :tpp :tppi 
                                :ntpp :ntppi :po
                                :pp :ppi
                                ))))

      (dolist (a nodes)
        (let ((m no-of-other-nodes)) ; wegen fehlender EQ-loop am Knoten selbst
          (dolist (rel roles)
            (let ((n 0))        
              (dolist (edge (outgoing a))
                (unless (spatially-relevant-p (to edge))
                  (error "!"))
                             
                (when (matches-p rel edge)
                  (decf m)
                  (incf n)))

              (let ((rel (change-package-of-description (second (textual-description rel)) :prover t)))
                (prover::add-concept-assertion map
                                               a
                                               `(prover::exactly ,n ,rel)))))))

      (setf (closed-roles-p map) t))))

;;;
;;;
;;;

(defun get-current-map ()
  *cur-map*)
		     
;;;
;;; MAP-Objekte
;;;

(defmethod all-os ((obj map-node))
  (when (os obj)
    (list (os obj))))


(defmethod delete-object progn ((obj map-node) &key)
  (delete-node (in-graph obj) obj))

;;;
;;;
;;;

(defvar *id-counter* 0)

(defun get-new-name (line-no)
  (declare (ignore line-no))
  (intern (format nil "IND-~A" (incf *id-counter*))
          (typecase (get-current-map)
            (racer-map :racer-user)
            #+:midelora (midelora-map :prover)
            (racer-descriptions-map :racer-user)
            (basic-map :spatial-substrate))))

;;;
;;;
;;;

(defun make-map-point (x y &rest initargs &key line-no os &allow-other-keys)
  (apply #'create-node (get-current-map) (get-new-name line-no)
         (make-standard-node-description os)
         :type (get-current-point-class (get-current-map))
         :x x :y y 
         initargs))

(defun map-p (x y &rest initargs)
  (apply #'make-map-point x y initargs))



(defun make-map-symbol (x y &rest initargs &key line-no os &allow-other-keys)
  (apply #'create-node (get-current-map) (get-new-name line-no) 
         (make-standard-node-description os)
         :type (get-current-symbol-class (get-current-map))
         :x x :y y 
         initargs))

(defun map-sym (x y &rest initargs)
  (apply #'make-map-symbol x y initargs))



(defun make-map-line (p1 p2 &rest initargs &key line-no os &allow-other-keys)
  (apply #'create-node (get-current-map) (get-new-name line-no) 
         (make-standard-node-description os)
         :type (get-current-line-class (get-current-map))
         :p1 p1 
         :p2 p2
         initargs))

(defun map-l (p1 p2 &rest initargs)
  (apply #'make-map-line p1 p2 initargs))


(defun make-map-chain (segments &rest initargs &key line-no os &allow-other-keys)
  (when (segment-list-ok-p segments nil t)
    (apply #'create-node (get-current-map) (get-new-name line-no) 
           (make-standard-node-description os)
           :type (get-current-chain-class (get-current-map))
           :segments segments
           initargs)))

(defun map-chain (segments &rest initargs)
  (apply #'make-map-chain segments initargs))


(defun make-map-polygon (segments &rest initargs &key line-no os &allow-other-keys)
  (when (segment-list-ok-p segments t t)
    (apply #'create-node (get-current-map) (get-new-name line-no) 
           (make-standard-node-description os)
           :type (get-current-polygon-class (get-current-map))
           :segments segments
           initargs)))

(defun map-poly (segments &rest initargs)
  (apply #'make-map-polygon segments initargs))


#|

(defun make-map-aggregate (objects &rest initargs &key line-no os &allow-other-keys)
  (apply #'create-node (get-current-map) (get-new-name line-no) (make-standard-node-description os)
         :type (get-current-aggregate-class (get-current-map))
         :has-parts objects 
         initargs))

(defun map-agg (objects &rest initargs)
  (apply #'make-map-aggregate objects initargs))

|#

         
;;;
;;;
;;;

(defun set-client-bb (xmin ymin xmax ymax) ; wird vom SQD-Reader aufgerufen => Bounding Box!  
  (when (get-current-map)
    (setf (slot-value (get-current-map) 'spatial-index)
          (init-spatial-index xmin ymin xmax ymax))))

;;;
;;;
;;;

(defmethod initialize-instance :after ((obj map-node) &rest initargs)
  (setf (slot-value obj 'interpreted-os)
        (lookup-os (os obj))))

(defmethod initialize-instance :after ((obj basic-map-polygon) &rest initargs)
  (with-selected-objects 
   (contained-object 
    obj 
    (typep contained-object '(or racer-map-polygon racer-map-polygon)) :inside)
   (incf (no-of-contained-polygons obj)))
  (setf (xy-list obj)
        (mapcan #'(lambda (p)
                    (list (x p) (y p)))
                (point-list obj))))


;;;
;;;
;;;

(defmethod compute-os-closure ((descr basic-map-simple-node-description) concept)
    #-:midelora 
    (mapcar #'(lambda (x) (change-package-of-description x :keyword nil))
            (apply #'append (concept-ancestors concept 
                                               (change-package-of-description
                                                (racer:current-tbox)
                                                :racer-user))))
    #+:midelora 
    (mapcar #'(lambda (x) (change-package-of-description x :keyword nil))
            (apply #'append (prover::concept-ancestors concept 
                                                       (prover::current-tbox)))))


(defmethod compute-os-closure ((descr racer-description-map-node-description) concept)
  (let* ((package (racer-package descr))
         (concept (change-package-of-description concept
                                                 package
                                                 t)))
    
    (mapcar #'(lambda (x) (change-package-of-description x :racer-user nil))
            (apply #'append (concept-ancestors concept (racer-tbox descr))))))

;;;
;;;
;;;

#+:midelora

(defmethod create-node ((abox midelora-map) (node symbol)
                        (description midelora-map-node-description)
                        &rest args
                        &key 
                        &allow-other-keys)
  (let ((node (call-next-method)))
    (prover::node-instance node (textual-description description))
    node))



#+:midelora
(defmethod prover::compute-virtual-successors ((abox midelora-implicit-relations-map) (node midelora-map-node) &optional role)

  (when (and (prover::old-p node)
             (spatially-relevant-p node))
      
    (let ((map (in-graph node)))

      (when (is-midelora-implicit-relations-map-p map)
        
        (let ((hash (created-succs-hash map)))

          (unless (gethash (list node role) hash)

            (let* ((nodes nil)
                   (a node)
                   (rcc 
                    (change-package-of-description
                     (textual-description role)
                     :keyword)))
                      
              (let* ((rcc
                      (if (consp rcc)
                          (inverse-rcc-relation 
                           (translate-spatial-vocabulary
                            (second rcc)))
                        (translate-spatial-vocabulary rcc)))
                     (rcc (finer (finer (finer rcc)))))

                (if (dc-edges-p map)
                    (dolist (b (get-nodes map))
                      (when (and (prover::old-p b)
                                 (spatially-relevant-p b))
                        (let ((res (calculate-rcc-relation a b)))
                          (when (member res rcc)
                            (push b nodes)))))
            
                  (with-relevant-objects (b a (and (spatially-relevant-p b)
                                                   (prover::old-p b)
                                                   (not (eq a b))))
                                         (let ((res (calculate-rcc-relation a b)))
                                           (when (member res rcc)
                                             (push b nodes)))))
    
                (setf (gethash (list node role) hash) nodes)

                nodes))))))))

#+:midelora
(defmethod prover::materialize-virtual-edge ((abox midelora-implicit-relations-map) (node midelora-map-node) (succ midelora-map-node) role)
  (prover::relate node succ role 
                  :description-type 'midelora-map-edge-description
                  :comment 'virtual-computed-midelora-rcc-edge
                  :old-p t
                  :materialized-virtual-p t))

#|

(defmethod prover::is-materialized-virtual-edge-p ((edge midelora-map-edge))
  (materialized-virtual-p edge))

|#

;;;
;;;
;;;

#+:midelora
(defmethod prover::prepare-abox-for-querying-and-sat-p :after ((abox midelora-implicit-relations-map) &rest args)
  (declare (ignorable args))
  (clrhash (created-succs-hash abox)))


#+:midelora
(defmethod prover::obvious-non-instance ((abox midelora-implicit-relations-map) node concept)
  (cond ((prover::is-bottom-concept-p concept)

         t)

        ((prover::is-top-concept-p concept)

         nil)

        ((member concept (prover::told-concepts node))
         
         nil)
	
        (t

         (let* ((negated (prover::negated-concept concept)))

           (or (member negated (prover::told-concepts node))
               
               (prover::obvious-instance abox node negated)
               

               ;;; erfordert das Retrieval-Konzeptmodell keine Nachfolger, dann 
               ;;; ist der Test wieder anwenbar! 
               
               (some #'(lambda (ma) 
                         (some #'(lambda (mb)
                                   (and (not (or (prover::node-model-successors mb)
                                                 (prover::node-model-attributes mb)
                                                 (prover::node-model-alls mb)
                                                 (prover::node-model-at-mosts mb)))
                                        (prover::models-mergeable-p prover::+alch+ ma mb)))
                               (prover::cached-models negated)))
                     (prover::ind-models node)))))))
