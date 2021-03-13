;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CL-USER -*-
 
(in-package :cl-user)

;;;
;;;
;;;

;; (push :sequential-query-scheduling *features*)

(push :owl-datatype-support *features*)

#-:clisp
(push :multiprocess-queries *features*)

#-:dlmaps
(push :only-runtime-evaluation *features*)

#+(and (or :lispworks :allegro)
       (not :dlmaps)
       :multiprocess-queries)
(push :process-pooling *features*)

;;;
;;;
;;;


(eval-when (:compile-toplevel :load-toplevel :execute)

  (setf (logical-pathname-translations "nrql")
        #+:nrql-dev
        (logical-pathname-translations "nrql-dev")
        #-:nrql-dev
        '(("**;*.*" "racer:nrql;**;*.*"))))


(define-system nrql
  (:default-pathname "nrql:")

  (:serial

   "nrql-symbols"

   #-:dlmaps "nrql-packages2"
   #+:dlmaps packages
   #-:dlmaps "tools2"
   
   "specials20"
   "messages"
   "process18"
   "process-pool2"
   "macros7"
   
   "dag3"

   #-:dlmaps "common16"
   #-:dlmaps "basic-substrate"
   #-:dlmaps "basic-descriptions"
   #-:dlmaps "racer-conversions3"
   #-:dlmaps "rolebox"
   #-:dlmaps "tables2"
   
   #+:dlmaps logic-tools
   #+:dlmaps racer-substrate
   
   "query44"
   "nrql-queries5"
   "syntax25"
   "parser14"
   "dispatcher10"
   "syntactic-sugar12"
   "syntactic-rewriting23"   

   #+:dlmaps "query-clustering"

   "query-realizer3"
   "tuple-construction12"
   "compiler44"
   "caching-code6"

   "abox-queries-code3"

   #+:dlmaps "runtime3"

   "optimizer21"
   "reasoning29"
   "hooks9"
   "preparation20"
   "execution9"
   "get-next-tuple4"
   "interface45"

   #-:midelora
   "racer-critical-functions4"
   
   "api41"

   "xml-trafo"

   "repository15"
   "cache-references11"

   "dl-prover-interface"
   
   "defined-queries9"
   
   #+:clim "browser3"

   ;;; nur fuer Racer: 

   #+(and (not :dlmaps) (not :midelora)) "data-substrate7"
   #+(and (not :dlmaps) (not :midelora)) "mirror-data-substrate2"
   #+(and (not :dlmaps) (not :midelora)) "rcc-substrate7"
   #+:sql-substrate "sql-substrate2"

   "stub-generation"
   "nrql-version"

   #+(or :racer-server :dlmaps) "persistence"
   
   "test-queries"
   #+(and :lispworks :linux) "deliver-nrql"
   ))

