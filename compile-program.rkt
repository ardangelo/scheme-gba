#lang racket

(provide compile-program)

; @ Function supports interworking.
; @ args = 0, pretend = 0, frame = 0
; @ frame_needed = 0, uses_anonymous_args = 0
; @ link register save eliminated.

(define fixnum-mask     #b00000011) 
(define fixnum-tag      #b00000000) 
(define fixnum-shift    2) 

(define char-mask       #b11111111) 
(define char-tag        #b00001111) 
(define char-shift      8) 

(define boolean-mask    #b01111111) 
(define boolean-tag     #b00111111) 
(define boolean-shift   7) 

(define empty-list-val  #b00101111)

(define (compile-program emit x)
	(define (fits b)
		(< b (expt 2 32)))

	; immediate values
	(define (can-mov-constant b)
		(define (helper x shamt)
			(if (> shamt 30) #f
				(if (> (modulo x 4) 0)
					(<= (abs x) 255)
					(helper (arithmetic-shift -2 x) (+ shamt 2)))))
		(helper b 0))

	(define (immediate-rep x)
		(define (tag x shamt t) (bitwise-ior (arithmetic-shift x shamt) t))
		(cond
			((integer? x)	(tag x fixnum-shift fixnum-tag))
			((char? x)		(tag (char->integer x) char-shift char-tag))
			((boolean? x)	(tag (if x 1 0) boolean-shift boolean-tag))
			((null? x) empty-list-val)
			(#t (raise-user-error (format "unknown type on (immediate-rep ~a)" x)))))

	(define (immediate? x) (or (integer? x) (char? x) (boolean? x) (null? x)))

	(define (emit-immediate x)
		(let ((target (immediate-rep x)))
		(if (fits target)
			(if (can-mov-constant target) ; can't mov more than 8b + 4b even shift
				(emit "	mov r0, #~a" target)
				(emit "	ldr r0, =#~a" target))
			(raise-user-error (format "overflow on (immediate-rep ~a)" x)))))

	; expressions
	(define (primcall? x) 
		(and (list? x) (member (car x) '(add1 sub1 integer->char char->integer null?))))
	(define (primcall-op x) (car x))
	(define (primcall-operand1 x) (cadr x))

	(define (emit-expr x)
		(cond
			((immediate? x) (emit-immediate x))
			((primcall? x)
				(case (primcall-op x)
					[(add1)
						(emit-expr (primcall-operand1 x))
						(emit "	add r0, r0, #~a" (immediate-rep 1))]
					[(sub1)
						(emit-expr (primcall-operand1 x))
						(emit "	sub r0, r0, #~a" (immediate-rep 1))]
					[(integer->char)
						(emit-expr (primcall-operand1 x))
						(let ((shamt (- char-shift fixnum-shift)))
							(if (> shamt 0)
								(emit "	lsl r0, r0, #~a" shamt)
								(emit "	asr r0, r0, #~a" (- shamt))))
						(emit "	and r0, r0, #~a" (arithmetic-shift -1 char-shift))
						(emit "	orr r0, r0, #~a" char-tag)]
					[(char->integer)
						(emit-expr (primcall-operand1 x))
						(let ((shamt (- fixnum-shift char-shift)))
							(if (> shamt 0)
								(emit "	lsl r0, r0, #~a" shamt)
								(emit "	asr r0, r0, #~a" (- shamt))))
						(emit "	and r0, r0, #~a" (arithmetic-shift -1 fixnum-shift))
						(emit "	orr r0, r0, #~a" fixnum-tag)]
					[else
						(raise-user-error (format "unknown expr to emit: ~a" x))]))
			(#t (raise-user-error (format "unknown expr type to emit: ~a" x)))))

	(begin 
		(emit-expr x)
		(emit "	bx	lr")
	)
)