#lang racket

(provide compile-program)

; @ Function supports interworking.
; @ args = 0, pretend = 0, frame = 0
; @ frame_needed = 0, uses_anonymous_args = 0
; @ link register save eliminated.

(define (compile-program emit x)
	(emit "	mov r0, #~a" x)
	(emit "	bx	lr"))