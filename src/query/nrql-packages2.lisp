;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: CL-USER; Base: 10 -*-

(in-package :CL-USER)


(DEFPACKAGE DAG
  (:USE
   #+:clim CLIM-LISP
   #+:clim CLIM
   #-:clim common-lisp)
  #+:clim 
  (:SHADOWING-IMPORT-FROM CLIM-LISP
   BOOLEAN)
   
  (:EXPORT 
   
   DAG
   DAG-NAME
   MAKE-DAG
   MAKE-DAG-NODE

   COPY-DAG-NODE

   COM-MAKE-SUBDAG-FROM-NODE
   COM-EXCHANGE-CHILD-NODE-POSITIONS
   
   DAG-BROWSER
   DAG-OPTIONS
   DAG-INFO
   DAG-DISPLAY

   DRAW-DAG-NODE
   DRAW-DAG-DISPLAY
   SHOW-TOP-P
   SHOW-BOTTOM-P
   ACCEPT-OPTIONS
   
   DAG-NODE
   DAG-NODES
   
   DAG-ROOTS
   DAG-LEAVES

   DAG-TOP
   DAG-BOTTOM

   ORIENTATION
   
   
   DAG-ROOT
   FIND-DAG-NODE   
   
   DAG-NODE-NAME
   DAG-NODE-MARKED-P
   DAG-NODE-PARENTS
   DAG-NODE-CHILDREN
   DAG-NODE-ANCESTORS
   DAG-NODE-DESCENDANTS
   IN-DAG

   MARK-DAG-NODE
   FN-MARK-DAG-NODE
   UNMARK-DAG-NODE
   MARK-ALL-DAG-NODES
   FN-MARK-ALL-DAG-NODES
   UNMARK-ALL-DAG-NODES
   
   DAG-ISOMORPHIC-P
   NODE-EQUIVALENT-P
   INSERT-DAG-NODE
   DELETE-DAG-NODE
   
   VISUALIZE-DAG   

   MAKE-POSTSCRIPT-FILE
   CREATE-CLOS-CLASSES   
   COMPUTE-TRANSITIVE-CLOSURE
   COMPUTE-HASSE-DIAGRAM))

#+:racer-server
(defpackage thematic-substrate
  (:nicknames ts)
  (:use
   #+:clim CLIM
   #+:clim CLIM-LISP
   #-:clim common-lisp
   dag
   racer
   nrql-symbols
   #+:sql-substrate clsql
   )
  (:shadow query)
  #+:clim
  (:SHADOWING-IMPORT-FROM CLIM-LISP
   BOOLEAN)   
  (:shadowing-import-from racer
   ATTRIBUTE-TYPE
   TOP BOTTOM
   INV SAME-AS))


#-:racer-server
(defpackage thematic-substrate
  (:nicknames ts)
  (:use
   #+:clim CLIM
   #+:clim CLIM-LISP
   #-:clim common-lisp
   dag
   racer
   nrql-symbols
   #+:sql-substrate clsql
   )
  (:shadow query)
  #+:clim
  (:SHADOWING-IMPORT-FROM CLIM-LISP
   BOOLEAN)   
  (:shadowing-import-from racer
   ATTRIBUTE-TYPE
  ))

  
