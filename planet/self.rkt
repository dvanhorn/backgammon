#lang racket
(require racket/cmdline
	 planet/version
	 (only-in 2htdp/universe launch-many-worlds)
	 (this-package-in main))
(define ip (make-parameter #f))
(command-line #:program "backgammon-self"
	      #:args ()
	      (launch-many-worlds (play "Self")
				  (serve1)))
