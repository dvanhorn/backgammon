#lang racket
(provide play serve-couples serve-singles serve1 serve2)

(require planet/version
         (this-package-in backgammon)
         (this-package-in server)
         (this-package-in private/test))

(require (except-in 2htdp/universe 
                    make-package package? 
                    key-event? key=? 
                    mouse-event? mouse=?))


;;=================================================================
;; Play

;; String [#:IP String] -> World
(define (play player-name #:IP [ip LOCALHOST])
  (big-bang (make-pas moment0)
    (register ip)
    (name (format "Backgammon: ~a" player-name))
    (on-draw world->img)
    (on-mouse clean-squeek)
    (on-key peck)
    (on-receive slurp)))

