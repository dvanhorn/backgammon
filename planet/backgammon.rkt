#lang typed/racket
#|  , __              _                                                 
   /|/  \            | |                                                
    |___/  __,   __  | |   __,  __,   _  _  _    _  _  _    __   _  _   
    | \   /  |  /    |/_) /  | /  |  / |/ |/ |  / |/ |/ |  /  \_/ |/ |  
    |  \_/\_/|_/\___/| \_/\_/|/\_/|_/  |  |  |_/  |  |  |_/\__/   |  |_/
                            /|                                          
                            \|                                          
   Rackgammon: Backgammon for Racket.

   Copyright (c) 2010 David Van Horn
   Licensed under the Academic Free License version 3.0

   (at dvanhorn (dot ccs neu edu))

   For M.M.
|#
(provide (all-defined-out))
(require (planet cce/scheme:7:1/planet))
(require (planet dvanhorn/typed:1:5/util)
         (planet dvanhorn/typed:1:5/list)
         (planet dvanhorn/typed:1:5/2htdp/image))

;;======================================================================
;; Description

;; A Board consists of a list of lists of checkers, representing the 
;; points on the board; a list of checkers that are on the bar, and
;; an off bar component that contains a list of checkers off on the 
;; top of the board and a list of checkers off on the bottom of the 
;; board.

;; A Moment represents a moment in the play of the game.  At any given
;; moment there is a board, some number of dice, and possibly a selected
;; checker (the selected checker, if any, should be thought of as in
;; the hand of the player).

;; Moments are either active or passive.  An active moment is one that
;; the player "owns"; it is the player's moment to respond to.  A 
;; passive moment is one the opponent owns.  The player can only observe
;; the moment, but cannot respond to it.

;; An active or passive moment is a World and the game is played with
;; the following form:
;; pas_0 -> ... -> pas_n -> 
;; act_0 -> ... -> act_m -> 
;; pas_0 -> ... -> pas_p ->
;;          ...

;; When the moment is active, the player may:
;;   - defer to the opponent; pick up all dice to indicate the play is 
;;     complete and the player is ready for the opponent to make a play.
;;     Doing so causes the new moment to be passive and sends a message
;;     to the opponent to become active.
;;   - update the moment by selecting a checker, placing a checker, or
;;     rolling the dice.  The resulting moment remains active.

;; The player defers to the opponent by pressing "D".  
;; The player rolls the dice by pressing "R".
;; The player selects a checker by clicking on it.
;; The player places a checker (when one is selected) by clicking on a 
;; region of the board.

;; While passive, the player receives updates from the opponent during
;; its play.  Once passive, the moment only becomes active again when 
;; the opponent sends a message indicated it is now passive and the 
;; player should become active.


;;======================================================================
;; Idioms

;; This program uses the Racket idiom of non-false values counting as
;; true in conditionals.  This will cause problems in the student 
;; languages.  Otherwise it is written in the style of HtDP.

;; This program is written in Typed Racket.  Wherever reasoning is used
;; beyond what TR can handle, casts are inserted.  For each cast, there
;; is a comment on why the cast is sound (will never cause a runtime
;; error), unless the justification is trivial.


;;======================================================================
;; Known issues

;; - stacking too many checkers on a point will render incorrectly.
;;   (See fancy-point->img for proper rendering, but selection will
;;   be broken if you use this.)


;;======================================================================
;; Data definitions

(define-type-alias Checker (U 'black 'white))
(: checker? (Any -> Boolean : Checker))
(define (checker? x)
  ;; TR doesn't understand this `or':
  #;(or (eq? x 'black) (eq? x 'white))
  (cond [(eq? x 'black) true]
        [(eq? x 'white) true]
        [else false]))

(define B 'black)
(define W 'white)

(: white? (Checker -> Boolean : 'white))
(: black? (Checker -> Boolean : 'black))
(define (white? c) (symbol=? c W))
(define (black? c) (symbol=? c B))

(provide (struct-out board))
(define-struct: board 
  ([points : [Listof [Listof Checker]]] 
   [bar : [Listof Checker]] 
   [off : off]) 
  #:transparent)
(define-type-alias Board board)

(provide (struct-out off))
(define-struct: off 
  ([top : [Listof Checker]] 
   [bot : [Listof Checker]])
  #:transparent)

;; A Die is a (make-die [0,6) [0,360)).
(provide (struct-out die))
(define-struct: die 
  ([val : Nat] 
   [degree : Nat]) 
  #:transparent)
(define-type-alias Die die)

(define-type-alias Dice [Listof Die])

(provide (struct-out moment))
(define-struct: moment 
  ([board : Board] 
   [dice : Dice] 
   [selected : (Option Checker)])
  #:transparent)
(define-type-alias Moment moment)

;; Interp: this moment is "active" -- this computer owns it.
;; X and Y give the coordinates of the mouse.
(provide (struct-out board))
(define-struct: act 
  ([moment : Moment] 
   [x : (Option Nat)]
   [y : (Option Nat)]))

;; Interp: this moment is "passive" -- another computer owns it.
(provide (struct-out pas))
(define-struct: pas ([moment : Moment]))

(define-type-alias World (U act pas))

;; (Moment -> Moment) -> (World -> World)
;; Lift given function on moments to worlds.
;; The intersection is used to show that wrapping a function results
;; in a function that cannot change the mode of a world.
(: lift-wrapped (case-lambda ((Moment -> Moment) -> (act -> act))
                             ((Moment -> Moment) -> (pas -> pas))))
(define (lift-wrapped s2s)
  (λ (w)
    (cond [(act? w)
           (make-act (s2s (act-moment w))
                     (act-x w)
                     (act-y w))]
          [(pas? w)
           (make-pas (s2s (pas-moment w)))])))

;; A Destination is one of:
;; - [0,24)  ; one of the points
;; - 'TOP    ; off on the top
;; - 'BOT    ; off on the bottom
;; - 'BAR    ; the bar
;; Interp: indexes regions on the board.
(define-type-alias Dest (U Nat 'TOP 'BOT 'BAR))

;; Interp: indexes checkers on the board.
(provide (struct-out source))
(define-struct: source ([dest : Dest] [i : Nat]) #:transparent)

;; Count how many checkers satisfy p? on the board.
(: board-count ((Checker -> Boolean) Board -> Nat))
(define (board-count p? b)
  (+ (foldl (λ: ([p : [Listof Checker]] [s : Nat]) 
                (+ (count p? p)
                   s))
            0
            (board-points b))
     (cast exact-nonnegative-integer? (count p? (board-bar b)))
     (cast exact-nonnegative-integer? (count p? (off-top (board-off b))))
     (cast exact-nonnegative-integer? (count p? (off-bot (board-off b))))))

;; Points Checker [0,24) -> Points
(: points-add ([Listof [Listof Checker]] Checker Nat -> [Listof [Listof Checker]]))
(define (points-add pts c i)
  (replace-at pts (append (list-ref pts i) (list c)) i))


;;======================================================================
;; Message views

;; Conversions between structures and S-Expressions.

(define-type-alias Checker-SExp Checker)
(define-type-alias Die-SExp (List Nat Nat))
(define-type-alias Dice-SExp [Listof Die-SExp])
(define-type-alias Board-SExp
  (List (Listof (Listof Checker-SExp))
        (Listof Checker-SExp)
        (Listof Checker-SExp)
        (Listof Checker-SExp)))
(define-type-alias Moment-SExp
  (List Board-SExp Dice-SExp (Option Checker-SExp)))

(: moment->sexp (Moment -> Moment-SExp))
(define (moment->sexp m)
  (list (board->sexp (moment-board m))
        (dice->sexp (moment-dice m))
        (maybe-checker->sexp (moment-selected m))))

(: sexp->moment (Moment-SExp -> Moment))
(define (sexp->moment s)
  (make-moment (sexp->board (first s))
               (sexp->dice (second s))
               (sexp->maybe-checker (third s))))

(: board->sexp (Board -> Board-SExp))
(define (board->sexp b)
  (list (map (λ: ([p : [Listof Checker]]) 
                 (map checker->sexp p))
             (board-points b))
        (map checker->sexp (board-bar b))
        (map checker->sexp (off-top (board-off b)))
        (map checker->sexp (off-bot (board-off b)))))
   
;; TR: Uses (third (rest s)) because `fourth' doesn't have the right type.
(: sexp->board (Board-SExp -> Board))
(define (sexp->board s)
  (make-board (map (λ: ([p : [Listof Checker-SExp]])
                       (map sexp->checker p)) 
                   (first s))
              (map sexp->checker (second s))
              (make-off (map sexp->checker (third s))
                        (map sexp->checker (third (rest s))))))


(: checker->sexp (Checker -> Checker-SExp))
(define (checker->sexp c) c)
(: dice->sexp (Dice -> Dice-SExp))
(define (dice->sexp ds) 
  (map (λ: ([d : Die]) 
           (list (die-val d) (die-degree d))) 
       ds))

(: sexp->checker (Checker-SExp -> Checker))
(define (sexp->checker s) s)
(: sexp->dice (Dice-SExp -> Dice))
(define (sexp->dice s)
  (map (λ: ([s : Die-SExp]) 
           (make-die (first s) (second s))) 
       s))

(: sexp->maybe-checker ((Option Checker-SExp) -> (Option Checker)))
(define (sexp->maybe-checker s)
  (and s (sexp->checker s)))

(: maybe-checker->sexp ((Option Checker) -> (Option Checker-SExp)))
(define (maybe-checker->sexp c)
  (and c (checker->sexp c)))


;;======================================================================
;; Rendering

;; Radius and diameter of Checker image
(define CRADIUS 20)
(define CDIAM (* 2 CRADIUS))

(define DIE-SIZE (* 2 CRADIUS))
(define BRADIUS 5)

(define BORDER-CLR 'darkslateblue)
(define FLOOR-CLR 'slateblue)

(define QUAD-HEIGHT (* 7 CDIAM))
(define BOARD-HEIGHT (* 2 QUAD-HEIGHT))

(: checker->img (Checker -> Image))
(define (checker->img c)
  (cond [(white? c)
         (overlay (circle CRADIUS 'outline 'black)
                  (circle CRADIUS 'solid 'white))]
        [(black? c)
         (overlay (circle CRADIUS 'outline 'white)
                  (circle CRADIUS 'solid 'black))]))

(: point->img ([Listof Checker] -> Image))
(define (point->img p)
  (above (hspace CDIAM)
         (apply above0 (reverse (map checker->img p)))))

(: raw-points->img ([Listof [Listof Checker]] -> Image))
(define (raw-points->img ps)
  (apply beside/align0 (ann "bottom" Y-Place)
         ;; Eventually this should use fancy-point->img.
         (map point->img (reverse ps))))

;; Renders point with "stacking" of checkers when the point is large.
(: fancy-point->img ([Listof Checker] -> Image))
(define (fancy-point->img p)
    (cond [(> (length p) 6)
           (overlay/align 'center
                          'bottom
                          (above (point->img (drop p 6))
                                 (vspace CRADIUS))
                          (point->img (take p 6)))]
          [else
           (point->img p)]))

(: point-img (Checker -> Image))
(define (point-img c)
  (let ((theta (* 2 (cast real? (atan 1/10)))))
    (isosceles-triangle (/ CRADIUS (cast real? (sin (* 1/2 theta))))
                        (* theta (/ 180 pi)) 
                        'solid
                        c)))

(define die-sq-img
  (square DIE-SIZE 'outline 'black))

(define bullet (circle BRADIUS 'solid 'black))

(define die-face-one
  (overlay bullet die-sq-img))

(define die-face-two
  (overlay/xy bullet 
              (- (* 5/8 DIE-SIZE))
              (- (* 5/8 DIE-SIZE))
              (overlay/xy bullet 
                          (- (* 1/8 DIE-SIZE))
                          (- (* 1/8 DIE-SIZE))
                          die-sq-img)))

(define die-face-three
  (overlay bullet die-face-two))

(define die-face-four
  (overlay (rotate 90 die-face-two) die-face-two))

(define die-face-five
  (overlay bullet die-face-four))

(define black-point-img (point-img 'black))
(define white-point-img (point-img 'white))

(define quadrant-background-img   
  (rectangle (* 6 CDIAM)
             QUAD-HEIGHT
             'solid
             FLOOR-CLR))

(define quadrant-img
  (overlay/align
   'center 'bottom
   (apply beside0 
          (build-list 6
                      (λ: ([i : Nat])
                        (if (even? i)
                            black-point-img
                            white-point-img))));)
   quadrant-background-img))

(define bar-img
  (rectangle CDIAM QUAD-HEIGHT 'solid BORDER-CLR))

;; [Listof-6 [Listof Checker]] -> Image
(: quadrant->img ([Listof [Listof Checker]] -> Image))
(define (quadrant->img ps)
  (overlay/align 'center 'bottom
                 (raw-points->img ps)
                 quadrant-img))

;; Render the checker on its side (the way it looks when placed "off" the board).
(: checker-off->img (Checker -> Image))
(define (checker-off->img c)
  (overlay (rectangle CDIAM
                      (* 1/2 CRADIUS)
                      'outline
                      (if (white? c) 'black 'white))
           (rectangle CDIAM
                      (* 1/2 CRADIUS)
                      'solid
                      (if (white? c) 'white 'black))))

;; Render a list of checkers on their side.
(: off->img ([Listof Checker] -> Image))
(define (off->img o)
  (apply above0 (map checker-off->img o)))

;; Board [0,4) -> [Listof-6 [Listof Checker]]
(: board-quadrant (Board Nat -> [Listof [Listof Checker]]))
(define (board-quadrant b q)
  (take (drop (board-points b) (* q 6)) 6))

(: top-board->img (Board -> Image))
(define (top-board->img b)
  (rotate 180 
          (beside (overlay/align 'center 'bottom 
                                 (off->img (off-top (board-off b)))
                                 bar-img)
                  (quadrant->img (board-quadrant b 3))
                  bar-img
                  (quadrant->img (board-quadrant b 2))
                  bar-img)))

(: bot-board->img (Board -> Image))
(define (bot-board->img b)
  (beside bar-img
          (quadrant->img (board-quadrant b 1))
          bar-img
          (quadrant->img (board-quadrant b 0))
          (overlay/align 'center 'bottom 
                         (off->img (off-bot (board-off b)))
                         bar-img)))  

;; Render the board as an image.
(: board->img (Board -> Image))
(define (board->img b)
  (overlay (apply above0 (map checker->img (board-bar b)))
           (above (top-board->img b)
                  (bot-board->img b))))

(: act-moment->img (Moment -> Image))
(define (act-moment->img w)
  (overlay (beside (hspace (* 7 CDIAM))
                   (dice->img (moment-dice w)))
           (board->img (moment-board w))))

(: pas-moment->img (Moment -> Image))
(define (pas-moment->img w)
  (overlay (beside (rotate 180 (dice->img (moment-dice w)))
                   (hspace (* 7 CDIAM)))
           (board->img (moment-board w))))

;; Render the world as an image.
(: world->img (World -> Image))
(define (world->img w)
  (cond [(act? w)
         (let ((s (moment-selected (act-moment w))))
           (if (and (act-x w) s)
               (place-image (checker->img s)
                            (or (act-x w) 0)
                            (or (act-y w) 0)
                            (act-moment->img (act-moment w)))
               (act-moment->img (act-moment w))))]
        
        [(pas? w)
         (overlay (text "Passive" 40 'red)
                  (pas-moment->img (pas-moment w)))]))

;;======================================================================
;; Dice Rendering

(define die-half-six 
  (overlay/align 'left 'center
                 (beside (hspace (* 1/2 BRADIUS))
                         (apply above0 (interleave (vspace (* 1/2 BRADIUS))
                                                   (make-list 3 bullet))))                 
                 die-sq-img))

(define die-face-six
  (overlay die-half-six (rotate 180 die-half-six)))

(: die->img (Die -> Image))
(define (die->img d)
  (rotate (die-degree d)
          (overlay (list-ref (list die-face-one 
                                   die-face-two 
                                   die-face-three 
                                   die-face-four 
                                   die-face-five 
                                   die-face-six)
                             (die-val d))
                   (square DIE-SIZE 'solid 'white))))

(: dice->img (Dice -> Image))
(define (dice->img ds)
  (apply beside0 (interleave (hspace DIE-SIZE) 
                             (map die->img ds))))


;;======================================================================
;; Boards

(: board-remove-points (Board Nat Nat -> Board))
(define (board-remove-points b i j)
  (make-board (replace-at (board-points b)
                          (remove-at (list-ref (board-points b)
                                               i)
                                     j)
                          i)
              (board-bar b)
              (board-off b)))            

(: board-bar-fetch (Board Nat -> (List Checker Board)))
(define (board-bar-fetch b i)
  (list (list-ref (board-bar b) i)
        (make-board (board-points b)
                    (remove-at (board-bar b) i)
                    (board-off b))))

(: board-add (Board Checker Dest -> Board))
(define (board-add b c d)
  (cond [(eq? 'TOP d) 
         (make-board (board-points b)
                     (board-bar b)
                     (make-off (cons c (off-top (board-off b)))
                               (off-bot (board-off b))))]
        [(eq? 'BOT d) 
         (make-board (board-points b)
                     (board-bar b)
                     (make-off (off-top (board-off b))
                               (cons c (off-bot (board-off b)))))]
        [(eq? 'BAR d)
         (make-board (board-points b)
                     (cons c (board-bar b))
                     (board-off b))]
        [else
         (make-board (points-add (board-points b) c d)
                     (board-bar b)
                     (board-off b))]))

(: board-flip (Board -> Board))
(define (board-flip b)
  (make-board (reverse (board-points b))
              (reverse (board-bar b))
              (make-off (off-bot (board-off b))
                        (off-top (board-off b)))))

;; A board as you would find it packed up in a Backgammon set.
;; All checkers are off the board.
(define packed-up-board 
  (make-board (make-list 24 empty)
              empty
              (make-off (make-list 15 B)
                        (make-list 15 W))))

(define initial-board
  (make-board (list (list W W)
                    empty
                    empty
                    empty
                    empty
                    (list B B B B B)
                    empty
                    (list B B B)
                    empty
                    empty
                    empty
                    (list W W W W W)
                    (list B B B B B)
                    empty
                    empty
                    empty
                    (list W W W)
                    empty
                    (list W W W W W)
                    empty
                    empty
                    empty
                    empty
                    (list B B))
              empty
              (make-off empty empty)))

(define some-board0
  (make-board (list (list W W)
                    empty
                    empty
                    empty
                    empty
                    (list B B B B B)
                    empty
                    (list B B B)
                    empty
                    empty
                    empty
                    (list W W W W W)
                    (list B B B B B)
                    empty
                    empty
                    empty
                    (list W W W)
                    empty
                    (list W W W W)
                    empty
                    empty
                    empty
                    (list W)
                    empty)
              (list B B)
              (make-off empty empty)))

(define some-board1
  (make-board (list empty
                    empty
                    empty
                    empty
                    empty
                    (list B B B B B)
                    empty
                    (list B B B)
                    empty
                    empty
                    empty
                    empty
                    (list B B B B B)
                    empty
                    empty
                    empty
                    empty
                    empty
                    (list W W W W)
                    empty
                    empty
                    (list W)
                    (list W W W)
                    empty)
              (list B B)
              (make-off (list W W W W W W W)
                        empty)))

;; A roll off board
(define some-board2
  (make-board (list empty
                    (list B B B)
                    empty
                    empty
                    (list B)
                    (list B B B)
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    (list W W W W)
                    empty
                    empty
                    (list W)
                    (list W W W)
                    empty)
              empty
              (make-off (list W W W W W W W)
                        (list B B B B B B B B))))

;;======================================================================
;; Moments

(define moment0
  (make-moment initial-board
               empty
               #f))

;; Remove the dice from the given moment.
(: moment-no-dice (Moment -> Moment))
(define (moment-no-dice m)
  (make-moment (moment-board m)
               empty
               (moment-selected m)))

;; Flip the board within the moment.
(: moment-flip (Moment -> Moment))
(define (moment-flip m)
  (make-moment (board-flip (moment-board m))
               (moment-dice m)
               (moment-selected m)))

;; Roll the dice
(: roll-dice (Moment -> Moment))
(define (roll-dice m)
  (make-moment (moment-board m)
               (list (make-die (random 6) (random 360))
                     (make-die (random 6) (random 360)))
               (moment-selected m)))


;;======================================================================
;; Board interactions

;; Feature request: click on the dice to indicate being done.

;; Is y in the top half of the board?
(: top? (Nat -> Boolean))
(define (top? y)
  (<= 0 y QUAD-HEIGHT))

;; [0,16) Boolean -> [Maybe Destination]
(: click-dest (Nat Boolean -> (Option Dest)))
(define (click-dest col top?)
  (cond [(<= 1 col 6) 
         (if top? 
             (+ 11 col)
             (cast exact-nonnegative-integer?
                   (+ 6 (- 6 col))))]                  
        [(= 7 col) 'BAR]
        [(<= 8 col 13)
         (if top?
             (+ 10 col)
             (cast exact-nonnegative-integer?
                   (+ 6 (- 7 col))))]
        [(<= 14 col) (if top? 'TOP 'BOT)]
        [else #f]))

;; Tells you index of checker-size grid square on the points.
(: y->points-index (Nat -> Nat))
(define (y->points-index y)
  (let ((a (quotient y CDIAM)))
    ;; Needs justification.
    (cast exact-nonnegative-integer?
          (if (< a 7)
              a
              (- 13 a)))))
  
;; Interpetation: a negative index indicates being above the midpoint.
;; When odd?, zero means the middle element.
;; When not odd?, zero means the first element.
(: y->bar-index (Nat Boolean -> Integer))
(define (y->bar-index y odd?)
  (exact-ceiling (/ (- y QUAD-HEIGHT (if odd? CRADIUS 0)) CDIAM)))

(: maybe-dest (Nat Nat -> (Option Dest)))
(define (maybe-dest x y)
  (click-dest (cast exact-nonnegative-integer? (quotient x CDIAM))
              (top? y)))

(: maybe-select-bar (Board Nat Nat -> (Option (List Checker Board))))
(define (maybe-select-bar b x y)
  (let ((n (length (board-bar b))))
    (let ((o? (odd? n)))
      (let ((bi (y->bar-index y o?)))
        (if o?
            (and (<= (- (quotient (sub1 n) 2)) 
                     bi 
                     (quotient (sub1 n) 2))
                 (board-bar-fetch b 
                                  ;; We know n : Nat and (odd? n).
                                  ;; So (sub1 n) : Nat.
                                  ;; So (quotient (sub1 n) 2) : Nat.
                                  ;; We have bi : Integer
                                  ;; but also (>= bi (- (quotient (sub1 n) 2))).
                                  ;; So (+ (quotient (sub1 n) 2) bi) : Nat.
                                  (cast exact-nonnegative-integer?
                                        (+ (quotient (sub1 n) 2) bi))))
            
            (and (<= (- (sub1 (quotient n 2)))
                     bi 
                     (quotient n 2))
                 
                 (board-bar-fetch b 
                                  ;; By similar reasoning to above.
                                  (cast exact-nonnegative-integer?
                                        (sub1 (+ (quotient n 2) bi))))))))))

(: maybe-select (Board Nat Nat -> (Option (List Checker Board))))
(define (maybe-select b x y)
  (let ((d (maybe-dest x y)))
    (and d
         (cond [(eq? 'BAR d) 
                (maybe-select-bar b x y)]
               [(eq? 'TOP d)
                (and (cons? (off-top (board-off b)))
                     (list (first (off-top (board-off b)))
                           (make-board (board-points b)
                                       (board-bar b)
                                       (make-off (rest (off-top (board-off b)))
                                                 (off-bot (board-off b))))))]               
               [(eq? 'BOT d)
                (and (cons? (off-bot (board-off b)))
                     (list (first (off-bot (board-off b)))
                           (make-board (board-points b)
                                       (board-bar b)
                                       (make-off (off-top (board-off b))
                                                 (rest (off-bot (board-off b)))))))]                
               [else 
                (let ((pt (list-ref (board-points b) d)))
                  (and (< (y->points-index y)
                          (length pt))
                       
                       ;; Return the Checker and Update the board.
                       (list (list-ref pt (y->points-index y))
                             (board-remove-points b d (y->points-index y)))))]))))

(: moment-maybe-move (Moment Nat Nat -> Moment))
;; Assumes the checker to move is selected.
(define (moment-maybe-move w x y)
  (let ((d (maybe-dest x y)))
    (if d
        (make-moment (board-add (moment-board w) 
                                ;; By assumption.
                                (cast checker? (moment-selected w))
                                d)
                     (moment-dice w)
                     #f)
        w)))

(: moment-maybe-select (Moment Nat Nat -> Moment))
(define (moment-maybe-select w x y)
  (let ((ms (maybe-select (moment-board w) x y)))
    (if ms
        (make-moment (second ms)
                     (moment-dice w)
                     (first ms))
        w)))


;;======================================================================
;; Universe

(define-type-alias Message
  (U (List 'initial)
     (List 'active)
     (List 'passive)
     (List 'moment Moment-SExp)))

(require/typed 2htdp/universe
               [opaque Package package?]
               [opaque Mouse mouse-event?]
               [opaque Key key-event?]
               [make-package (World Message -> Package)]
               [mouse=? (Mouse Mouse -> Boolean)]
               [key=? (Key Key -> Boolean)])

(: squeek (World Nat Nat Mouse -> (U World Package)))
(define (squeek w x y m)
  (cond [(act? w)
         (let* ((s (act-moment w))
                (s* (cond [(mouse=? m (cast mouse-event? "button-down"))
                           (cond [(moment-selected s)
                                  (moment-maybe-move s x y)]
                                 [else
                                  (moment-maybe-select s x y)])]
                          [else s])))
           
           (if (eq? s s*)
               (make-act s x y)
               (make-package 
                (make-act s* x y) 
                `(moment ,(moment->sexp s*)))))]        
        [else w]))

;; This is a work-around for:
;; http://bugs.racket-lang.org/query/?cmd=view&pr=10922
(: clean-squeek (World Integer Integer Mouse -> (U World Package)))
(define (clean-squeek w x y m)
  (if (and (exact-nonnegative-integer? x)
           (exact-nonnegative-integer? y))
      (squeek w x y m)
      w))

(: peck (World Key -> (U World Package)))
(define (peck w k)
  (cond [(act? w)
         (cond [(and (key=? k (cast key-event? "D"))
                     (not (moment-selected (act-moment w))))
                (make-package (make-pas (moment-no-dice (act-moment w)))
                              '(passive))]
               [(key=? k (cast key-event? "R")) 
                (let ((w* ((lift-wrapped roll-dice) w)))
                  (make-package 
                   w*
                   `(moment ,(moment->sexp (act-moment w*)))))]
               [else w])]
        [else w]))

(: slurp (World Message -> World))
(define (slurp w msg)
  (cond [(eq? (first msg) 'initial)
         (make-act (moment-flip moment0) #f #f)]
        [(eq? (first msg) 'active)
         ;; By assumption, the world is passive.
         (make-act (moment-no-dice (pas-moment (cast pas? w))) #f #f)]
        [(eq? msg 'passive)
         ;; By assumption, the world is active.
         (make-pas (act-moment (cast act? w)))]
        [(and (eq? (first msg) 'moment)
              ;; Needed to convince TR (it can't reason about list structure yet).
              (not (empty? (rest msg))))
         ;; May not do the right thing with history
         (make-pas (moment-flip (sexp->moment (second msg))))]
        ;; We weren't able to convince TR that this case is impossible.
        [else
         (error "Unkown message" msg)]))

