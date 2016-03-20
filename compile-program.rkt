#lang racket

(provide compile-program)

; @ Function supports interworking.
; @ args = 0, pretend = 0, frame = 0
; @ frame_needed = 0, uses_anonymous_args = 0
; @ link register save eliminated.
(define (compile-program emit x)
	(let (
		(fixnum-shift 2)
		(fixnum-tag 0)

		(char-shift 8)
		(char-tag 15)

		(boolean-shift 7)
		(boolean-tag 31)

		(empty-list-val 47))

	(define (immediate-rep x)
		(define (tag x shamt t) (bitwise-ior (arithmetic-shift x shamt) t))
		(cond
			((integer? x)	(tag x fixnum-shift fixnum-tag))
			((char? x)		(tag (char->integer x) char-shift char-tag))
			((boolean? x)	(tag (if x 1 0) boolean-shift boolean-tag))
			((null? x) empty-list-val)
			(#t x)))

	(define (eat-zeros b)
		(if (not (zero? (modulo b 2))) (eat-zeros (arithmetic-shift b -1)) b))

	(if (< (abs (eat-zeros (immediate-rep x))) 255) ; can't mov more than 8b + 4b shift
		(emit "	mov r0, #~a" (immediate-rep x))
		(emit "	ldr r0, =#~a" (immediate-rep x)))
	(emit "	bx	lr")))