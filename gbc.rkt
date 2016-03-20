#lang racket

(provide scheme->gba)

(require "compile-program.rkt")
(require racket/cmdline)

(define output-path (make-parameter "scheme.s"))

(define code-path
  (command-line 
   #:program "gbc.rkt"

   #:once-each
   [("-o") op "Output assembly to this file (default scheme.s)" (output-path op)]

   #:args (code-path)
   code-path))

(define (scheme->gba code-path output-path)

	(define prelude-port (open-input-file "prelude.s" #:mode 'text))
	(define prelude (port->string prelude-port))
	(close-input-port prelude-port)

	(define code-port (open-input-file code-path #:mode 'text))
	(define code (read code-port))
	(close-input-port code-port)

	(define out-port (open-output-file output-path #:exists 'truncate))

	(define (emit s . a)
		(if (null? a)
			(displayln s out-port)
			(if (= (length a) 1)
				(displayln (format s (car a)) out-port)
				(displayln (format s a) out-port))))

	(emit prelude)

	(compile-program emit code)

	(emit "	.ident	\"gbc.rkt dev\"")

	(close-output-port out-port))

(scheme->gba code-path (output-path))