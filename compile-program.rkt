#lang racket

(provide compile-program)

; @ Function supports interworking.
; @ args = 0, pretend = 0, frame = 0
; @ frame_needed = 0, uses_anonymous_args = 0
; @ link register save eliminated.
(define (compile-program emit x)
	(define (fits b)
		(< b (expt 2 32)))

	(define (can-mov-constant b)
		(define (helper x shamt)
			(if (> shamt 30) #f
				(if (> (modulo x 4) 0)
					(<= (abs x) 255)
					(helper (arithmetic-shift -2 x) (+ shamt 2)))))
		(helper b 0))

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

	(let ((target (immediate-rep x)))

		(if (fits target)
			(begin
				(if (can-mov-constant target) ; can't mov more than 8b + 4b even shift
					(emit "	mov r0, #~a" target)
					(emit "	ldr r0, =#~a" target))
				(emit "	bx	lr"))
			(raise-user-error (format "overflow on (immediate-rep ~a)" x))))))