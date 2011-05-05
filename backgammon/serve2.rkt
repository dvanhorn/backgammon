#lang racket
(require racket/cmdline
	 planet/version
	 (this-package-in server))
(define ip (make-parameter #f))
(command-line #:program "backgammon-serve2"
	      #:args ()
	      (serve2))
