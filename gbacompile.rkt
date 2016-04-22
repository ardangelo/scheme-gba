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

	;(define code-port (open-input-file code-path #:mode 'text))
	;(define code (file->list code-port))
	(define code (file->list code-path))
	;(close-input-port code-port)

	(define out-port (open-output-file output-path #:exists 'truncate))

	(define (emit s . a)
		(if (null? a)
			(displayln s out-port)
			(if (= (length a) 1)
				(displayln (format s (car a)) out-port)
				(displayln (apply format (cons s a)) out-port))))

	(define global-label (first (string-split output-path ".")))

	(emit prelude)

	(emit "	.ident	\"gbc.rkt dev\"")
	(emit "	.global	~a" global-label)
	(emit "	.type ~a, %function" global-label)
	(emit "~a:" global-label)

	(with-handlers
		([exn:fail:user? (lambda (e) (begin
			(close-output-port out-port)
			(delete-file output-path)
			(error (exn-message e))))])
		(compile-program emit code))

	(emit "	.ident	\"gbacompile.rkt dev\"")

	(close-output-port out-port))

(scheme->gba code-path (output-path))