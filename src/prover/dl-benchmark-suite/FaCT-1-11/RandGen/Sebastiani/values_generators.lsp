;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%% GENERATOR'S VALUES                                                      %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; VARIABLE DEFINITIONS
(defvar *neg_prob*)      
(defvar *or_br*)        
(defvar *and_br*)        
(defvar *var_num*)       
(defvar *rule_num*)      
(defvar *prim_conc_prob*) 
(defvar *mod_degree*)    
(defvar *logic*)         
(defvar *sort-clauses*)
(defvar *execute_test*) 
(defvar *time_bound*) 
(defvar *stream_con*)
(defvar *stream_prefwff*)
(defvar *print_con*)
(defvar *print_prefwff*) 
(defvar *print_results*) 

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%%                                                                   %
;%%%        RANDOM GENERATOR'S VALUES                                  %
;%%%                                                                   %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%% SETTABLE VALUES                                                   %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;FLAGS
;(setq *sort-clauses* T)    ;[T] if T, wffs are generated sorted
(setq *sort-clauses* nil)    ;[T] if T, wffs are generated sorted
(setq *execute_test* T)    ;[T] if nil, no satisfiability test 
(setq *print_results* T)    ;[T] if T, print results to file results.text 

;TIMEOUT
(setq *timeout*  NIL)           ; do not change this value!!!!
#+ALLEGRO (setq *timeout* 1000) ; [1000] execution timeout (in seconds)
                                ; if NIL, no timeout

;INPUT/OUTPUT:
(setq *print_con* NIL)     ;[NIL] if true, prints the intermediate concept;
(setq *print_prefwff* NIL) ;[NIL] if true, prints the intermediate wff;
(setq *print-pretty* NIL)  ;[NIL] if true, prints wffs indented


;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%% DEFAULT VALUES                                                    %
;%%% (these values are modified by the testing functions)              %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(setq *var_num* 3)          ; variable number
(setq *rule_num* 1)         ; number of distinct modalities
(setq *and_br* 120)         ; and branching
(setq *mod_degree* 2)       ; modal degree

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%% FIXED VALUES                                                      %
;%%% (do not change them unless you are sure of what you are doing)    %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(setq *prim_conc_prob* 0.5) ; ratio primitive_concepts/concepts
(setq *neg_prob* .5)        ; negative literals ratio
(setq *or_br* 3)            ; or branching
(setq *logic* "MK")         ; [MK] logic - only K(m) - 

