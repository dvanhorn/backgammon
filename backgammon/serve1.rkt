#lang racket
(require racket/cmdline
	 planet/version
	 (this-package-in server))
(define ip (make-parameter #f))
(command-line #:program "backgammon-serve1"
	      #:args ()
	      (serve1))
