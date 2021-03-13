;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CL-USER -*-

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim
   (optimize
    (safety 0)		        ; Run time error checking level
    (speed 3)			; Speed of the compiled code
    (compilation-speed 0)         ; Speed of compilation
    (space 0)			; Space of both intermediate files and object
    (debug 0)
    )))

;;;
;;;
;;; 

(setf (logical-pathname-translations "base")
        (list '("**;*.*" "~mwessel/lispworks/work/dlmaps/**/*.*")))
  
(setf (logical-pathname-translations "dlmaps")
      (list '("fonts;*.*" "base:db;*.*")
            '("code;*.*" "base:*.*")
            '("**;*.*" "base:**;*.*")))

(setf (logical-pathname-translations "dl-test")
      (list '("**;*.*" "dlmaps:prover;dl-benchmark-suite;**;*.*")))
    
(setf (logical-pathname-translations "nrql-dev")
      (list '("**;*.*" "dlmaps:query;**;*.*")))

;;;
;;;
;;; 

(require "clim")

(load "dlmaps:define-system.lisp")

(setf system::*sg-default-size* (* 10 16000))

(setf *print-length* nil)

;;;
;;;
;;; 

(defun lracer (&optional (force-p nil))
  (declare (ignorable force-p))

  (setf (logical-pathname-translations "lracer")
        (list '("**;*.*" "dlmaps:lracer;**;*.*")))

  (push :lracer *features*)
  
  (load "lracer:lracer-sysdcl.lisp")

  (compile-load-lracer))

(defun dlmaps-dev (&optional (force-p nil))
  (push :debug *features*)
  (dlmaps force-p))

(defun dlmaps (&optional (force-p nil))
  (push :dlmaps *features*)
  (push :nrql-dev *features*)
  (push :midelora *features*)  
  (dlmaps-general force-p)
  (compile-system 'dlmaps :load t :force force-p)

  (terpri)
  (terpri)
  (princ "(dlmaps-demo)"))

(defun dlmaps-general (&optional (force-p nil))
  (lracer force-p)
  (load "dlmaps:query;queries-sysdcl.lisp")
  (load "dlmaps:dlmaps-sysdcl.lisp")
  (compile-system 'packages :load t :force-p t))


;;;
;;;
;;; 

(define-system packages
  (:default-pathname "dlmaps:")
  (:serial
   "dlmaps-packages4"))

(define-system persistence 
  (:default-pathname "dlmaps:persistence;")
  (:serial 
   packages
   "persistence3"))

(define-system logic-tools 
  (:default-pathname "dlmaps:tools;")
  (:serial 
   packages
   "tools"
   "logic-tools"))

(define-system tools 
  (:default-pathname "dlmaps:tools;")
  (:serial 
   packages
   "grapher"
   #+:clim "class-browser"
   "tools"
   logic-tools
   #+:clim "gui-tools"
   #+:clim "fonts"))

(define-system heap 
  (:default-pathname "dlmaps:tools;")
  (:serial 
   "heap"))

(define-system dag 
  (:default-pathname "dlmaps:tools;")
  (:serial 
   packages
   "dag3"))

(define-system racer-dag 
  (:default-pathname "dlmaps:tools;")
  (:serial 
   packages
   dag
   "racer-dag"))

#+:clim
(define-system sqd
  (:default-pathname "dlmaps:sqd;")
  (:serial 
   packages
   "sqd-reader2"
   "hex"
   "def1"
   "def2"
   "os3"
   "sqd-transformer"))

#+:clim
(define-system geometry
  (:default-pathname "dlmaps:geometry;")
  (:serial 
   packages
   "geometry"
   "polygons"
   "tree-structure"
   "box-relations"
   "predicates"
   "epsilon2"
   "rcc-predicates"   
   "relations"))

#+:clim
(define-system map-viewer
  (:default-pathname "dlmaps:map-viewer;")
  (:serial 
   packages
   sqd
   "map-viewer3"
   "result-inspector2"
   "q-queries"
   "interface"))

(define-system basic-graph
  (:default-pathname "dlmaps:basic-graph;")
  (:serial 
   packages
   persistence 
   tools
   "basic-graph"))

#+:clim
(define-system graph-visualizer
  (:default-pathname "dlmaps:graph-visualizer;")
  (:serial 
   basic-graph
   "graph-visualizer3"))

(define-system thematic-substrate
  (:default-pathname "dlmaps:thematic-substrate;")
  (:serial 
   packages
   #+:clim graph-visualizer
   #-:clim basic-graph
   ;; "process"
   "racer-conversions3"
   "descriptions5"
   "substrate7"
   "rolebox"
   "rolebox-substrate6"
   "jepd-substrate4"
   ))

(define-system racer-substrate
  ;;; nicht individuell compilierbar!
  (:default-pathname "dlmaps:thematic-substrate;")
  (:serial                
   "common15"
   "racer-substrate5"))

#+:clim
(define-system spatial-substrate
  (:default-pathname "dlmaps:spatial-substrate;")
  (:serial 
   packages
   thematic-substrate
   nrql
   "spatial-index-macros"   
   "spatial-index3"
   "rcc-selection"
   "sqd-interface"
   "map-colors"
   "spatial-substrate2"
   "query2"
   "syntax"
   "compiler6"
   "runtime"
   "optimizer2"
   "syntactic-rewriting"
   "semantic-rewriting"
   "reasoning"))

(define-system tables
  (:default-pathname "dlmaps:tables;")
  (:serial 
   packages
   "tables"))

(define-system ontology
  (:default-pathname "dlmaps:ontology;")
  (:serial 
   packages
   "load-ontology"))

(define-system dl-benchmark
  (:default-pathname "dl-test:")
  (:serial 
   "dlmaps-tests"
   "dl-tests"))

(define-system prover
  (:default-pathname "dlmaps:prover;")
  (:serial 
   packages
   heap
   dag
   racer-dag
   #+:clim graph-visualizer
   thematic-substrate
   tables
   "settings"
   "specials"  
   "languages"
   "macros"
   "syntax9"
   "abstractions"
   "strategy"
   "abox9"    
   "node-label2"
   "edge-label"
   "abox-nodes"
   "abox-edges"
   "tools"
   "kernel16"
   "node-management"
   "edge-management"
   ;"delete"
   "concept-selection13"
   "merging2"
   "creators"
   "abox-interface"
  
   ;;; Rules

   "abox-enumeration"
   "deterministic-expansion3"
   "or-expansion"
   "some-expansion2"
   "feature-expansion"
   "simple-number-restrictions-expansion"
   "meta-constraints"
   "blocking2"
   "model-merging4"
   "abox-saturation"
   "precompletion"
   "core-models"
   
   ;;; Prover

   "alch-prover"   
   "alchf-prover"
   
   "alchn-prover"   
   "alchf-rplus-prover"
   
   ;; "alchfn-rplus-prover"

   ;; "alchi-prover"
   ;; "alchif-prover"
   ;; "alchi-rplus-prover"
   ;; "alchif-rplus-prover"
   ;; "alchifn-rplus-prover"

   ;; "alci-ra-minus-prover"
   ;; "alci-ra-jepd-prover"
   
   ;;; Interface

   "interface4"
   "tbox9"   
   "roles"
   "models"
   
   "abox-queries3"
   "krss"   
   "file-interface"
   "statistics"

   ;; "ddl"
   
   #+:clsql "db-abox"
   
   dl-benchmark
   
   nrql
   "midelora-lubm"
   "tester"))


#+(and :dlmaps :clim)
(define-system dlmaps
  (:default-pathname "dlmaps:code;")
  (:serial
   packages
   racer-dag
   persistence
   tools 
   sqd
   geometry
   ontology
   graph-visualizer
   thematic-substrate
   tables
   #+:midelora prover
   nrql
   spatial-substrate
   map-viewer
   
   "demo"))

;;;
;;;
;;;

;; (dlmaps t)

;; (dlmaps-demo)

