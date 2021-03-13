;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CL-USER -*-
 
(in-package :cl-user)

(ts::nrql-defun get-nrql-version ()
  '1.9.1)

;;;
;;; TO DO:
;;;
;;; (same-as $?x $?y) -> reasoning? qbox? ...
;;; forward checking (not (same-as $?x $?y)) ...
;;;
;;; (?x ?y (NOT equal)) -> to be implemented
;;; (?x ?y nrql-equal-role) -> Reasoning qbox etc.! 
;;; 
;;; - AVL-Baeume als Index fuer data-substrate-Nodes mit numerischen Werten etc.  
;;;



;;;
;;; fehler in aBox-queries-2 behoben -> abox-queries-3 -> RacerPro nachpflegen!
;;; mlisp -> same-as fehlerhaft, casesensititve Symbole!
;;;
;;; fehler in syntactic-rewriting22 -> 23 (neg (project-to in rewrite queries geht jetzt))
