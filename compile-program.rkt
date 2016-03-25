#lang racket

(provide compile-program)

(require "mem-defs.rkt")
(require "gba.rkt")

; interpreter values
(define label-count 0)

(define (compile-program emit x)
	(emit "	mov ~a, #~a" (ptr-loc heapptr) loc-iwram) ; set up the heap

	(define (stash-reg reg offset)
		(emit "	str ~a, [sp, #~a]" (ptr-loc reg) offset))
	(define (restore-reg reg offset)
		(emit "	ldr ~a, [sp, #~a]" (ptr-loc reg) offset))

	(define (immediate? x) (or (integer? x) (char? x) (boolean? x) (quote-empty-list? x)))
	(define (move dst src
		#:si [si 0] #:env [env (make-hash)] #:cndl [cndl ""] #:shift [shift null])
		(if (not (or (ptr? src) (integer? src)))
			(raise-user-error (format "invalid move src: ~a" src))
			(if (not (ptr? dst))
				(raise-user-error (format "invalid move dst: ~a" dst))
				(if (not (eq? src dst))
					(cond
						[(direct-reg? dst)
							(cond
								[(integer? src)
									(if (null? shift)
										(if (can-mov-constant src)
											(emit "	mov~a ~a, #~a" cndl (ptr-loc dst) src)
											(emit "	ldr~a ~a, =#~a" cndl (ptr-loc dst) src))
										(if (can-mov-constant src)
											(emit "	mov~a ~a, #~a, ~a #~a" cndl (ptr-loc dst) src (car shift) (cadr shift))
											(raise-user-error (format "ldr immediate with barrel shift not supported: ~a" src))))]
								[(direct-reg? src)
									(if (null? shift)
										(emit "	mov~a ~a, ~a" cndl (ptr-loc dst) (ptr-loc src))
										(emit "	mov~a ~a, ~a, ~a #~a" cndl (ptr-loc dst) (ptr-loc src) (car shift) (cadr shift)))]
								[(offset-reg? src)
									(emit "	ldr~a ~a, [~a, #~a]" cndl (ptr-loc dst) (offset-base src) (offset-amt src))]
								[(direct-mem? src)
									(emit "	ldr~a ~a, ~a" cndl (ptr-loc dst) (ptr-loc src))]
								[#t
									(raise-user-error "internal move failure")])]
						[(offset-reg? dst)
							(if (not (null? shift))
								(raise-user-error (format "memory load with barrel shift not supported: ~a" src))
								(cond
									[(immediate? src)
										(if (can-mov-constant src)
											(emit "	str~a #~a, [~a, #~a]" cndl src (offset-base dst) (ptr-loc dst))
											(begin
												(emit "	ldr~a ~a, =#~a" cndl scratch src)
												(emit "	str~a ~a, [~a, #~a]" cndl scratch (offset-base dst) (offset-amt dst))))]
									[(direct-reg? src)
										(emit "	str~a ~a, [~a, #~a]" cndl (ptr-loc src) (offset-base dst) (offset-amt dst))]
									[(offset-reg? src)
										(emit "	ldr~a ~a, [~a, #~a]" cndl scratch (offset-base src) (offset-amt src))
										(emit "	str~a ~a, [~a, #~a]" cndl scratch (offset-base dst) (offset-amt dst))]
									[#t
										(raise-user-error (format "internal move failure"))]))]
						[(direct-mem? dst)
							(if (not (null? shift))
									(raise-user-error (format "memory load with barrel shift not supported: ~a" src))
								(cond
									[(immediate? src)
										(if (can-mov-constant src)
											(emit "	str~a #~a, ~a" cndl src (ptr-loc dst))
											(begin
												(emit "	ldr~a ~a, =#~a" cndl scratch src)
												(emit "	str~a ~a, ~a" cndl scratch (ptr-loc dst))))]
									[(direct-reg? src)
										(emit "	str~a ~a, ~a" cndl (ptr-loc src) (ptr-loc dst))]
									[(offset-reg? src)
										(emit "	ldr~a ~a, [~a, #~a]" cndl scratch (offset-base src) (offset-amt src))
										(emit "	str~a ~a, ~a" cndl scratch (ptr-loc dst))]
									[(direct-mem? src)
										(emit "	ldr~a ~a, ~a" cndl scratch (ptr-loc src))
										(emit "	str~a ~a, ~a" cndl scratch (ptr-loc dst))]
									[#t
										(raise-user-error (format "internal move failure"))]))]
						[#t
							(raise-user-error (format "unsupported pointer type ~a" (ptr-type dst)))])
					'()))))
	(define (push src si) (begin
		(move (stack-offset si) src)
		(- si wordsize)))
	(define (pop dst si) (begin
		(move dst (stack-offset si))
		(+ si wordsize)))
	(define (store-imm dst-addr src-imm #:cndl [cndl ""])
		(move (ptr 'mem dst-addr) src-imm #:cndl cndl))
	(define (load-ptr dst-ptr src-addr #:cndl [cndl ""])
		(move dst-ptr (ptr 'mem src-addr) #:cndl cndl))
	(define (store-ptr dst-addr src-ptr #:cndl [cndl ""])
		(move (ptr 'mem dst-addr) src-ptr #:cndl cndl))
	(define (load-reg free-reg ptr)
		(if (eq? (ptr-type ptr) 'reg)
			ptr
			(begin
				(move free-reg ptr)
				free-reg)))

	; immediate values
	(define (quote-empty-list? x) (equal? x (quote (quote ()))))
	(define (immediate-rep x)
		(define (tag x shamt t) (bitwise-ior (arithmetic-shift x shamt) t))
		(cond
			[(integer? x)	(tag x fixnum-shift fixnum-tag)]
			[(char? x)		(tag (char->integer x) char-shift char-tag)]
			[(boolean? x)	(tag (if x 1 0) boolean-shift boolean-tag)]
			[(quote-empty-list? x) empty-list-val]
			[#t (raise-user-error (format "unknown type on (immediate-rep ~a)" x))]))
	(define (emit-immediate x)
		(let ((target (immediate-rep x)))
		(if (< target (expt 2 32))
			(move r0 target)
			(raise-user-error (format "overflow on (immediate-rep ~a)" x)))))

	; primitive calls
	(define (primcall-op x) (car x))
	(define (primcall-operand1 x) (cadr x))
	(define (primcall-operand2 x) (caddr x))
	(define (primcall? x) 
		(and (list? x) (list? (member (primcall-op x) '(
			add1 sub1 integer->char char->integer
			null? zero? not integer? boolean?
			+ - * = <
			cons cons? car cdr
			make-vector vector?
			make-string string? string-ref string-set!)))))

	; let
	(define (bindings x) (cadr x))
	(define (body x) (caddr x))
	(define (let? x) (and (list? x) (eq? (car x) 'let)))
	(define (lookup x env)
		(hash-ref env x))
	(define (extend-env varname valptr env) (begin
		(hash-set! env varname valptr)
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
					(f (cdr b*) 
						(extend-env (lhs b) (stack-offset si) new-env) (push r0 si)))))))

	; conditionals
	(define (unique-label) (begin
		(set! label-count (+ label-count 1))
		(format "L~a" label-count)))
	(define (emit-label label) (emit "~a:" label))
	(define (if? x) (and (list? x) (eq? (car x) 'if)))
	(define (emit-if condition if-true if-false si env)
		(let ((L0 (unique-label)) (L1 (unique-label)))
			(emit-expr condition si env)
			(emit "	cmp ~a, #~a" (ptr-loc r0) (immediate-rep #f))
			(emit "	beq ~a" L0)
			(emit-expr if-true si env)
			(emit "	b ~a" L1)
			(emit-label L0)
			(emit-expr if-false si env)
			(emit-label L1)))

	; emit expressions
	(define (emit-expr x si env)
		; shortcuts
		(define (cmp-and-set-boolean rand) (begin
			(emit-expr (primcall-operand1 x) si env)
			(emit "	cmp ~a, #~a" (ptr-loc r0) rand)
			(move r0 (immediate-rep #t) #:cndl "eq")
			(move r0 (immediate-rep #f) #:cndl "ne")))
		(define (cmp-tag rand-ptr mask tag) (begin
			(move r0 rand-ptr)
			(emit-1operand x)
			(emit "	and ~a, ~a, #~a" (ptr-loc r0) (ptr-loc rand-ptr) mask)
			(cmp-and-set-boolean tag)))

		(define (emit-1operand x)
				(emit-expr (primcall-operand1 x) si env))
		(define (emit-2operands x)
			(emit-expr (primcall-operand2 x) si env)

			(let* ([saved (new-saved-reg)] [stash-reqd (reg-in-use? saved)])
				(if stash-reqd
					(stash-reg saved si)
					(mark-used saved))

				(move saved r0)
				(emit-expr (primcall-operand1 x) (if stash-reqd (- si wordsize) si) env)
				(move r1 saved)

				(if stash-reqd
					(restore-reg saved si)
					(mark-unused si))))

		(cond
			[(immediate? x) (emit-immediate x)]
			[(let? x) (emit-let (bindings x) (body x) si env)]
			[(if? x) (emit-if (cadr x) (caddr x) (cadddr x) si env)]
			[(primcall? x)
				(case (primcall-op x)

					; unary primitives
					[(add1)
						(emit-1operand x)
						(emit "	add ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0) (immediate-rep 1))]
					[(sub1)
						(emit-1operand x)
						(emit "	sub ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0) (immediate-rep 1))]
					[(integer->char)
						(emit-1operand x)
						(let ((shamt (- char-shift fixnum-shift)))
							(if (> shamt 0)
								(emit "	lsl ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0) shamt)
								(emit "	asr ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0) (- shamt))))
						(emit "	and ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0) (arithmetic-shift -1 char-shift))
						(emit "	orr ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0) char-tag)]
					[(char->integer)
						(emit-1operand x)
						(let ((shamt (- fixnum-shift char-shift)))
							(if (> shamt 0)
								(emit "	lsl ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0) shamt)
								(emit "	asr ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0) (- shamt))))
						(emit "	and ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0) (arithmetic-shift -1 fixnum-shift))
						(emit "	orr ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0) fixnum-tag)]
					[(null?)
						(emit-1operand x)
						(cmp-and-set-boolean empty-list-val)]
					[(zero?)
						(emit-1operand x)
						(cmp-and-set-boolean 0)]
					[(not)
						(emit-1operand x)
						(cmp-and-set-boolean (immediate-rep #f))]
					[(integer?) (cmp-tag r0 fixnum-mask fixnum-tag)]
					[(boolean?) (cmp-tag r0 boolean-mask boolean-tag)]

					; binary primtives
					[(+)
						(emit-2operands x)
						(emit "	add r0, r0, r1")]
					[(-)
						(emit-2operands x)
						(emit "	sub r0, r0, r1")]
					[(*)
						(emit-expr (primcall-operand2 x) si env)

						(let* ([saved (new-saved-reg)] [stash-reqd (reg-in-use? saved)])
							(if stash-reqd
								(stash-reg saved si)
								(mark-used saved))

							(emit "	asr ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0)  fixnum-shift)
							(move saved r0)
							(emit-expr (primcall-operand1 x) (if stash-reqd (- si wordsize) si) env)
							(move r1 r0 #:shift (list "asr" fixnum-shift))
							(emit "	mul r0, ~a, r1" (ptr-loc saved))
							(emit "	lsl ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0) fixnum-shift)

							(if stash-reqd
								(restore-reg saved si)
							(mark-unused si)))]
					[(= char=?)
						(emit-2operands x)
						(emit "	cmp ~a, ~a" (ptr-loc r0) (ptr-loc r1))
						(move r0 (immediate-rep #t) #:cndl "eq")
						(move r0 (immediate-rep #f) #:cndl "ne")]
					[(<)
						(emit-2operands x)
						(emit "	cmp ~a, ~a" (ptr-loc r0) (ptr-loc r1))
						(move r0 (immediate-rep #t) #:cndl "lt")
						(move r0 (immediate-rep #f) #:cndl "gt")]

						; heap primitives
					[(cons)
						(let ((car r0) (cdr r1))
							(emit-2operands x)
							(move (heap-offset 0) car)
							(move (heap-offset wordsize) cdr)
							(move r0 heapptr)
							(emit "	orr r0, r0, #1")
							(emit "	add ~a, ~a, #~a" (ptr-loc heapptr) (ptr-loc heapptr) (* 2 wordsize)))]
					[(cons?) (cmp-tag r0 heap-mask pair-tag)]

					[(car)
						(emit-1operand x)
						(move r0 (ptr 'offset (list r0 -1)))]
					[(cdr)
						(emit-1operand x)
						(move r0 (ptr 'offset (list r0 3)))]

					[(make-vector)
						(emit-1operand x)
						(let ([temp r1])
							(move (heap-offset 0) r0)
							(move temp r0)
							(move r0 heapptr)
							(emit "	orr ~a, ~a, #2" (ptr-loc r0) (ptr-loc r0))
							(emit "	add ~a, ~a, #11" (ptr-loc temp) (ptr-loc temp))
							(emit "	and ~a, ~a, #-8" (ptr-loc temp) (ptr-loc temp))
							(emit "	add ~a, ~a, ~a" (ptr-loc heapptr) (ptr-loc temp) (ptr-loc heapptr)))]
					[(vector?) (cmp-tag r0 heap-mask vector-tag)]

					[else
						(raise-user-error (format "unknown expr to emit: ~a" x))])]

			[(symbol? x) (move r0 (lookup x env))]
			[#t (raise-user-error (format "unknown expr type to emit: ~a" x))]))

	(begin 
		(emit-expr x 0 (make-hash))
		(emit "	bx lr")))