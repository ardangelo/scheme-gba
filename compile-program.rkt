#lang racket

(provide compile-program)

(require "gba.rkt")

(define wordsize 4)

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

	(define (quote-empty-list? x) (equal? x (quote (quote ()))))
	(define (immediate-rep x)
		(define (tag x shamt t) (bitwise-ior (arithmetic-shift x shamt) t))
		(cond
			((integer? x)	(tag x fixnum-shift fixnum-tag))
			((char? x)		(tag (char->integer x) char-shift char-tag))
			((boolean? x)	(tag (if x 1 0) boolean-shift boolean-tag))
			((quote-empty-list? x) empty-list-val)
			(#t (raise-user-error (format "unknown type on (immediate-rep ~a)" x)))))
	(define (immediate? x) (or (integer? x) (char? x) (boolean? x) (quote-empty-list? x)))
	(define (emit-immediate x)
		(let ((target (immediate-rep x)))
		(if (fits target)
			(if (can-mov-constant target) ; can't mov more than 8b + 4b even shift
				(emit "	mov r0, #~a" target)
				(emit "	ldr r0, =#~a" target))
			(raise-user-error (format "overflow on (immediate-rep ~a)" x)))))

	; primitive calls
	(define (primcall? x) 
		(and (list? x) (member (car x) '(
			; unary primitives
			add1 sub1 integer->char char->integer null? zero? not integer? boolean?
			; binary primitives
			+ - * = <))))
	(define (primcall-op x) (car x))
	(define (primcall-operand1 x) (cadr x))
	(define (primcall-operand2 x) (caddr x))

	; let
	(define (bindings x) (cadr x))
	(define (body x) (caddr x))
	(define (let? x) (and (list? x) (eq? (car x) 'let)))
	(define (lookup x env)
		(hash-ref env (symbol->string x)))
	(define (extend-env var si env) (begin
		(hash-set! env (symbol->string var) si)
		env))
	(define (emit-let bindings body si env)
		(define (lhs b) (car b))
		(define (rhs b) (cadr b))
		(let f ((b* bindings) (new-env env) (si si))
			(cond
				((null? b*) (emit-expr body si new-env))
			(else
				(let ((b (car b*)))
					(emit-expr (rhs b) si env)
					(emit "	str r0, [sp, #~a]" si) ; using ld/stmfd cant keep track of si
					(f (cdr b*) (extend-env (lhs b) si new-env) (- si wordsize)))))))
	
	; emit expressions
	(define (emit-expr x si env)
		; shortcuts
		(define (cmp-and-set-boolean rand) (begin
			(emit-expr (primcall-operand1 x) si env)
			(emit "	cmp r0, #~a" rand)
			(emit "	moveq r0, #~a" (immediate-rep #t))
			(emit "	movne r0, #~a" (immediate-rep #f))))

		(define (emit-1operand x)
			(emit-expr (primcall-operand1 x) si env))
		(define (emit-2operands x)
			(emit-expr (primcall-operand2 x) si env)
			(emit "	str r0, [sp, #~a]" si)
			(emit-expr (primcall-operand1 x) (- si wordsize) env)
			(emit "	ldr r1, [sp, #~a]" si))

		(cond
			((immediate? x) (emit-immediate x))
			((symbol? x) (emit "	ldr r0, [sp, #~a]" (lookup x env)))
			((let? x) (emit-let (bindings x) (body x) si env))
			((primcall? x)
				(case (primcall-op x)

					; unary primitives
					[(add1)
						(emit-1operand x)
						(emit "	add r0, r0, #~a" (immediate-rep 1))]
					[(sub1)
						(emit-1operand x)
						(emit "	sub r0, r0, #~a" (immediate-rep 1))]
					[(integer->char)
						(emit-1operand x)
						(let ((shamt (- char-shift fixnum-shift)))
							(if (> shamt 0)
								(emit "	lsl r0, r0, #~a" shamt)
								(emit "	asr r0, r0, #~a" (- shamt))))
						(emit "	and r0, r0, #~a" (arithmetic-shift -1 char-shift))
						(emit "	orr r0, r0, #~a" char-tag)]
					[(char->integer)
						(emit-1operand x)
						(let ((shamt (- fixnum-shift char-shift)))
							(if (> shamt 0)
								(emit "	lsl r0, r0, #~a" shamt)
								(emit "	asr r0, r0, #~a" (- shamt))))
						(emit "	and r0, r0, #~a" (arithmetic-shift -1 fixnum-shift))
						(emit "	orr r0, r0, #~a" fixnum-tag)]
					[(null?)
						(emit-1operand x)
						(cmp-and-set-boolean empty-list-val)]
					[(zero?)
						(emit-1operand x)
						(cmp-and-set-boolean 0)]
					[(not)
						(emit-1operand x)
						(cmp-and-set-boolean (immediate-rep #f))]
					[(integer?)
						(emit-1operand x)
						(emit "	and r0, r0, #~a" fixnum-mask)
						(cmp-and-set-boolean fixnum-tag)]
					[(boolean?)
						(emit-1operand x)
						(emit "	and r0, r0, #~a" boolean-mask)
						(cmp-and-set-boolean boolean-tag)]

					; binary primtives
					[(+)
						(emit-2operands x)
						(emit "	add r0, r0, r1")]
					[(+)
						(emit-2operands x)
						(emit "	sub r0, r0, r1")]
					[(*)
						(emit-expr (primcall-operand2 x) si env)
						(emit "	asr r0, r0, #~a" fixnum-shift)
						(emit "	str r0, [sp, #~a]" si)
						(emit-expr (primcall-operand1 x) si env)
						(emit "	mov r2, r0, asr #~a" fixnum-shift)
						(emit "	ldr r1, [sp, #~a]" si)
						(emit "	mul r0, r1, r2")
						(emit "	lsl r0, r0, #~a" fixnum-shift)]
					[(= char=?)
						(emit-2operands x)
						(emit "	cmp r0, r1")
						(emit "	moveq r0, #~a" (immediate-rep #t))
						(emit "	movne r0, #~a" (immediate-rep #f))]
					[(<)
						(emit-2operands x)
						(emit "	cmp r0, r1")
						(emit "	movlt r0, #~a" (immediate-rep #t))
						(emit "	movge r0, #~a" (immediate-rep #f))]

					[else
						(raise-user-error (format "unknown expr to emit: ~a" x))]))
			(#t (raise-user-error (format "unknown expr type to emit: ~a" x)))))

	(begin 
		(emit-expr x 0 (make-hash))
		(emit "	bx lr")
	)
)