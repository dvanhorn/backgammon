#lang setup/infotab
(define name "Backgammon.")
(define scribblings '(("scribblings/backgammon.scrbl")))
(define categories '(misc))
(define required-core-version "5.0.1")
(define repositories (list "4.x"))
(define primary-file 
  '("main.ss"))
(define blurb
  (list '(div "Backgammon game and server.")))
(define release-notes 
  (list
   '(div "Updated for 5.0.1.")))