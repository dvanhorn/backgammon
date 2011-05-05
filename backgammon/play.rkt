#lang racket
(require racket/cmdline
	 planet/version
	 (this-package-in main))
(define ip (make-parameter #f))
(command-line #:program "backgammon-play"
	      #:once-each
	      [("--ip") 
	       addr 
	       "Play at IP address"
	       (ip addr)]
	      #:args (name)
	      (let ((addr (ip)))
		(if addr
		    (play name #:IP addr)
		    (play name))))
