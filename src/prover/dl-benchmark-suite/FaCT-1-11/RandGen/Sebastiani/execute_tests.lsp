;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%% RANDOM TESTS                                                      %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;SYNTAX:
;(execute_tab
;  logic              ;; logic (only multiple K "MK" available)
;  samples            ;; number of samples/point
;  start              ;; number of clauses : start
;  end                ;; number of clauses : end
;  step               ;; number of clauses : step
;  nvar               ;; number of variables
;  boxes              ;; number of distinct boxes (only for K(m))
;  depth              ;; modal depth
;)

;the test returns a list of lines in the form:
;<sat-value> <cpu-time> 
;where "sat values" is S (satisfiable) U (unsatisfiable) B (exceeded bound)

;example:
;S    307.67                               #(satisfiable)
;U    192.87                               #(unsatisfiable)
;B   1000.00  100000  1000000   1000.00    #(exceeded bound)

;varying variable number
;d=2,m=1,N=3,4,5
;(execute_tab "MK" 100 3 123 3 3 1 2)
;(execute_tab "MK" 100 4 164 4 4 1 2)
;(execute_tab "MK" 100 5 205 5 5 1 2)

;varying modality number
;N=4,d=2,m=2,5,10,20
;(execute_tab "MK" 100 4 164 4 4 2 2) 
;(execute_tab "MK" 100 4 164 4 4 5 2) 
;(execute_tab "MK" 100 4 164 4 4 10 2) 
;(execute_tab "MK" 100 4 164 4 4 20 2) 

;varying depth
;N=3,m=1,d=3,4,5
;(execute_tab "MK" 100 3 123 3 3 1 3)
;(execute_tab "MK" 100 3 123 3 3 1 4)
;(execute_tab "MK" 100 3 123 3 3 1 5)

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%% halpern's wffs test                                              %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;(test_tab 20)

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%% tests from Hustadt & Schmidt technical report                    %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; PS0:
(execute_tab  "MK" 100 5 200 5 5 1 2 :fname "Results/ps0.text")
; PS1:
(execute_tab  "MK" 100 3 120 3 3 1 5 :fname "Results/ps1.text")
; PS2:
(execute_tab  "MK" 100 3 120 3 3 1 4 :fname "Results/ps2.text")
; PS3:
(execute_tab  "MK" 100 3 120 3 3 1 3 :fname "Results/ps3.text")
; PS4:
(execute_tab  "MK" 100 3 120 3 3 1 2 :fname "Results/ps4.text")
; PS5:
(execute_tab  "MK" 100 4 160 4 4 1 2 :fname "Results/ps5.text")
; PS6:
(execute_tab  "MK" 100 4 160 4 4 2 2 :fname "Results/ps6.text")
; PS7:
(execute_tab  "MK" 100 4 160 4 4 5 2 :fname "Results/ps7.text")
; PS8:
(execute_tab  "MK" 100 4 160 4 4 10 2 :fname "Results/ps8.text")
; PS9:
(execute_tab  "MK" 100 4 160 4 4 20 2 :fname "Results/ps9.text")
; PS10:
(execute_tab  "MK" 100 8 320 8 8 1 2 :fname "Results/ps10.text")
; PS11:
(execute_tab  "MK" 100 10 400 10 10 1 2 :fname "Results/ps11.text")
