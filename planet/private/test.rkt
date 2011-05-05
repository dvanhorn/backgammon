#lang racket
(provide (all-defined-out))
(require (planet cce/scheme:7/planet)
         test-engine/scheme-tests
         (this-package-in backgammon))

;;======================================================================
;; Tests

(check-expect (points-add (list empty) B 0)
              (list (list B)))
(check-expect (points-add (list (list W) empty) B 1)
              (list (list W)
                    (list B)))

(check-expect (sexp->maybe-checker #f) #f)
(check-expect (sexp->maybe-checker B) B)
(check-expect (sexp->maybe-checker W) W)

(check-expect (sexp->board (board->sexp initial-board))
              initial-board)

;; For testing only.  Not a valid Board.
(define mt-board (make-board (list empty) empty (make-off empty empty)))

(check-expect (board-add mt-board W 'BAR)
              (make-board (list empty) (list W) (make-off empty empty)))
(check-expect (board-add mt-board W 'TOP)
              (make-board (list empty) empty (make-off (list W) empty)))
(check-expect (board-add mt-board W 'BOT)
              (make-board (list empty) empty (make-off empty (list W))))                          
(check-expect (board-add mt-board W 0)
              (make-board (list (list W)) empty (make-off empty empty)))

(check-expect (board-count white? initial-board) 15)
(check-expect (board-count black? initial-board) 15)
(check-expect (board-count white? packed-up-board) 15)
(check-expect (board-count black? packed-up-board) 15)
(check-expect (board-count white? some-board0) 15)
(check-expect (board-count black? some-board0) 15)
(check-expect (board-count white? some-board1) 15)
(check-expect (board-count black? some-board1) 15)
(check-expect (board-count white? some-board2) 15)
(check-expect (board-count black? some-board2) 15)

(check-expect (y->bar-index QUAD-HEIGHT true) 0)
(check-expect (y->bar-index (- QUAD-HEIGHT CRADIUS) true) -1)
(check-expect (y->bar-index (- QUAD-HEIGHT CRADIUS 1) true) -1)
(check-expect (y->bar-index (+ QUAD-HEIGHT CRADIUS) true) 0)
(check-expect (y->bar-index (+ QUAD-HEIGHT CRADIUS 1) true) 1)

(check-expect (y->bar-index QUAD-HEIGHT false) 0)
(check-expect (y->bar-index (- QUAD-HEIGHT CRADIUS) false) 0)

(test)