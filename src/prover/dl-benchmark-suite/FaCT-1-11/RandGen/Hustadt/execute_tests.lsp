;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%% tests from Hustadt & Schmidt technical report                    %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;; test parameters = N tests/time-point L-start L-end L-step
; PS12
(mpi-test 4 100 4 120 4 :fname "Results/ps12.text" :skip-tests T)
; PS13:
(mpi-test 6 100 6 180 6 :fname "Results/ps13.text" :skip-tests T)
