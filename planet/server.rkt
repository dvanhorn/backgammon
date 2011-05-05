#lang racket
(require 2htdp/universe)
;(require (file "private/list.ss"))
(provide (all-defined-out))

(define (remf f ls)
  (cond [(empty? ls) empty]
        [(f (first ls)) (rest ls)]
        [else 
         (cons (first ls)
               (remf f (rest ls)))]))

;; Serves single player (self-playing) games.
(define (serve-singles)
  (thread
   (λ ()
     (universe '*
               (on-new 
                (λ (is iw)
                  (make-bundle '* (list (make-mail iw '(initial))) empty)))
               (on-msg
                (λ (is iw msg)
                  (make-bundle '* 
                               (cond [(equal? msg '(passive))
                                      (list (make-mail iw '(active)))]
                                     [else empty])
                               empty)))))))

;; Serves two play games, pairing off incoming couples first come, first served.

;; UniverseState = (make-u [Maybe IWorld] [Listof (list IWorld IWorld)])
;; The first world is the active world, the second is the passive.
(define-struct u (loner pairs))

(define (serve-couples)
  (thread (λ ()
            (universe (make-u #f empty)
                      (on-new (λ (us iw)
                                (if (u-loner us)
                                    (make-bundle (make-u #f (cons (list (u-loner us) iw)
                                                                  (u-pairs us)))
                                                 (list (make-mail (u-loner us) '(initial)))
                                                 empty)
                                    (make-bundle (make-u iw (u-pairs us))
                                                 empty
                                                 empty))))
                      
                      
                      (on-msg (λ (us iw msg)
                                (cond [(equal? msg '(passive))
                                       (flip-play us iw)]
                                      [(equal? (first msg) 'moment)
                                       (notify-moment us iw (second msg))])))))))

;; IWorld [Listof (list IWorld IWorld)] -> IWorld
(define (pair-partner iw pairs)
  (cond [(iworld=? iw (first (first pairs)))
         (second (first pairs))]
        [else
         (pair-partner iw (rest pairs))]))

;; UniverseState IWorld -> Bundle
(define (flip-play us iw)
  (let ((iw* (pair-partner iw (u-pairs us))))
    (make-bundle
     (make-u (u-loner us)
             (cons (list iw* iw)
                   (remf (λ (p) (iworld=? iw (first p))) (u-pairs us))))
     (list (make-mail iw* '(active)))
     empty)))
  
(define (notify-moment us iw m)
  (make-bundle us
               (list (make-mail (pair-partner iw (u-pairs us))
                                `(moment ,m)))
               empty))
                            
                      
  

