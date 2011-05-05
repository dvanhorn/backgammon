#lang setup/infotab
(define name "Backgammon.")
(define scribblings '(("scribblings/backgammon.scrbl")))
(define categories '(misc))
(define required-core-version "5.1.1")
(define repositories (list "4.x"))
(define primary-file 
  '("main.rkt"))
(define blurb
  (list '(div "Rackgammon: Backgammon for Racket.")))
(define release-notes 
  (list
   '(div "Updated for 5.1.1.  Added command line launchers.")))