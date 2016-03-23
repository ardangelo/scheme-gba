#lang racket

(provide compile-program)

(require "gba.rkt")

; machine values
(define wordsize 4)

; immediate values
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

; interpreter values
(define label-count 0)

; memory defines

; memory management
(struct ptr (type loc)
	#:guard (lambda (type loc type-name)
		(cond
			[(not (member type '(reg stack)))
				(raise-user-error (format "invalid ptr type: ~a" type))]
			[else (values type loc)])))

(struct var (varname ptr)
	#:guard (lambda (varname ptr type-name)
		(cond
			[(not (ptr? ptr))
				(raise-user-error (format "invalid ptr: ~a" ptr))]
			[else (values varname ptr)])))

(define r0 (ptr 'reg "r0"))
(define r1 (ptr 'reg "r1"))
(define r2 (ptr 'reg "r2"))
(define r3 (ptr 'reg "r3"))
(define r4 (ptr 'reg "r4"))
(define r5 (ptr 'reg "r5"))
(define r6 (ptr 'reg "r6"))
(define r7 (ptr 'reg "r7"))
(define r8 (ptr 'reg "r8"))
(define r9 (ptr 'reg "r9"))
(define r10 (ptr 'reg "r10"))
(define r11 (ptr 'reg "r11"))
(define r12 (ptr 'reg "r12"))

(define scratch r8)
(define (alloc regs si)
	(define (helper regs-to-check)
		(cond
			((null? regs-to-check)
				(ptr 'stack si))
			((not (set-member? regs (car regs-to-check)))
				(car regs-to-check))
			(#t (helper (cdr regs-to-check)))))
	(helper (list r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12)))
(define (add-regs regs ptr)
	(if (eq? (ptr-type ptr) 'reg)
		(set-add regs ptr)
		regs))
(define (remove-regs regs ptr)
	(if (eq? (ptr-type ptr) 'reg)
		(set-remove regs ptr)
		regs))
(define (new-si si ptr)
	(if (eq? (ptr-type ptr) 'stack)
		(- si wordsize)
		si))

(define (can-mov-constant b) ; can't mov more than 8b + 4b even shift
		(define (helper x shamt)
			(if (> shamt 30) #f
				(if (> (modulo x 4) 0)
					(<= (abs x) 255)
					(helper (arithmetic-shift -2 x) (+ shamt 2)))))
		(helper b 0))

(define (fits b)
	(< b (expt 2 32)))

(define (compile-program emit x)
	(define (move dst src #:si [si 0] #:env [env (make-hash)] #:cndl [cndl ""] #:shift [shift null])
		(if (not (or (ptr? src) (var? src) (integer? src)))
			(raise-user-error (format "invalid move src: ~a" src))
			(if (not (ptr? dst))
				(raise-user-error (format "invalid move dst: ~a" dst))

				(if (not (eq? src dst))
					(case (ptr-type dst)
						[(reg)
							(cond
								((integer? src)
									(if (null? shift)
										(emit "	ldr~a ~a, =#~a" cndl (ptr-loc dst) src)
										(emit "	mov~a ~a, ~a, ~a ~a" cndl (ptr-loc dst) (ptr-loc src) (car shift) (cadr shift))))
								((eq? (ptr-type src) 'reg)
									(if (null? shift)
										(emit "	mov~a ~a, ~a" cndl (ptr-loc dst) (ptr-loc src))
										(emit "	mov~a ~a, ~a, ~a ~a" cndl (ptr-loc dst) (ptr-loc src) (car shift) (cadr shift))))
								((eq? (ptr-type src) 'stack)
									(emit "	ldr~a ~a, [sp, #~a]" cndl (ptr-loc dst) (ptr-loc src)))
								(else
									(raise-user-error "internal move failure")))]
						[(stack)
							(cond
								((integer? src)
									(if (can-mov-constant src)
										(emit "	str~a #~a, [sp, #~a]" cndl src (ptr-loc dst))
										(begin
											(emit "	ldr~a ~a, =#~a" cndl scratch src)
											(emit "	str~a ~a, [sp, #~a]" cndl scratch (ptr-loc dst)))))
								((eq? (ptr-type src) 'reg)
									(emit "	str~a ~a, [sp, #~a]" cndl (ptr-loc src) (ptr-loc dst)))

								((eq? (ptr-type src) 'reg)
									(emit "	str~a ~a, ~a" cndl (ptr-loc dst) (ptr-loc src)))
								((eq? (ptr-type src) 'stack)
									(emit "	ldr~a ~a, [sp, #~a]" cndl scratch (ptr-loc src))
									(emit "	str~a ~a, [sp, #~a]" cndl scratch (ptr-loc dst)))
								(else
									(raise-user-error (format "internal move failure"))))]
						[else
							(raise-user-error (format "unsupported pointer type ~a" (ptr-type dst)))])
					'()))))
	(define (push src si)
		(move (ptr 'stack si) src))
	(define (pop dst si)
		(move dst (ptr 'stack si)))

	; immediate values
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
			(move r0 target)
			(raise-user-error (format "overflow on (immediate-rep ~a)" x)))))

	; primitive calls
	(define (primcall-op x) (car x))
	(define (primcall-operand1 x) (cadr x))
	(define (primcall-operand2 x) (caddr x))
	(define (primcall? x) 
		(and (list? x) (list? (member (primcall-op x) '(
			add1 sub1 integer->char char->integer null? zero? not integer? boolean?
			+ - * = <)))))

	; let
	(define (bindings x) (cadr x))
	(define (body x) (caddr x))
	(define (let? x) (and (list? x) (eq? (car x) 'let)))
	(define (lookup x env)
		(hash-ref env x))
	(define (extend-env varname valptr env) (begin
		(hash-set! env varname valptr)
		env))
	(define (emit-let bindings body regs si env)
		(define (lhs b) (car b))
		(define (rhs b) (cadr b))
		(let f ((b* bindings) (new-env env) (regs regs) (si si))
			(cond
				((null? b*) (emit-expr body regs si new-env))
			(else
				(let ((b (car b*)))
					(emit-expr (rhs b) regs si env)
					(push r0 si)
					(f (cdr b*) 
						(extend-env (lhs b) (ptr 'stack si) new-env)
						regs
						(- si wordsize)))))))

	; conditionals
	(define (unique-label) (begin
		(set! label-count (+ label-count 1))
		(format "L~a" label-count)))
	(define (emit-label label) (emit "~a:" label))
	(define (if? x) (and (list? x) (eq? (car x) 'if)))
	(define (emit-if condition if-true if-false regs si env)
		(let ((L0 (unique-label)) (L1 (unique-label)))
			(emit-expr condition regs si env)
			(emit "	cmp ~a, #~a" (ptr-loc r0) (immediate-rep #f))
			(emit "	beq ~a" L0)
			(emit-expr if-true regs si env)
			(emit "	b ~a" L1)
			(emit-label L0)
			(emit-expr if-false regs si env)
			(emit-label L1)))

	; emit expressions
	(define (emit-expr x regs si env)
		; shortcuts
		(define (cmp-and-set-boolean rand) (begin
			(emit-expr (primcall-operand1 x) si env)
			(emit "	cmp ~a, #~a" (ptr-loc r0) rand)
			(move r0 (immediate-rep #t) #:cndl "eq")
			(move r0 (immediate-rep #f) #:cndl "ne")))

		(define (emit-1operand x)
			(emit-expr (primcall-operand1 x) si env))
		(define (emit-2operands x)
			(let ((op2-result (alloc regs si)))
				(emit-expr (primcall-operand2 x) regs si env)
				(move op2-result r0)
				(emit-expr (primcall-operand1 x) (add-regs regs op2-result) (new-si si op2-result) env)
				(move r1 op2-result)))

		(cond
			((immediate? x) (emit-immediate x))
			((let? x) (emit-let (bindings x) (body x) regs si env))
			((if? x) (emit-if (cadr x) (caddr x) (cadddr x) regs si env))
			((primcall? x)
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
					[(integer?)
						(emit-1operand x)
						(emit "	and ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0) fixnum-mask)
						(cmp-and-set-boolean fixnum-tag)]
					[(boolean?)
						(emit-1operand x)
						(emit "	and ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0) boolean-mask)
						(cmp-and-set-boolean boolean-tag)]

					; binary primtives
					[(+)
						(emit-2operands x)
						(emit "	add r0, r0, r1")]
					[(-)
						(emit-2operands x)
						(emit "	sub r0, r0, r1")]
					[(*)
						(let ((op2-result (alloc regs si)))
							(emit-expr (primcall-operand2 x) regs si env)
							(emit "	asr ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0)  fixnum-shift)
							(move op2-result r0)
							(emit-expr (primcall-operand1 x) (add-regs regs op2-result) (new-si si op2-result) env)
							(emit "	mov r2, r0, asr #~a" fixnum-shift)
							(move r1 op2-result)
							(emit "	mul r0, r1, r2")
							(emit "	lsl ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0) fixnum-shift))]
					[(= char=?)
						(emit-2operands x)
						(emit "	cmp ~a, #0" (ptr-loc r0))
						(move r0 (immediate-rep #t) #:cndl "eq")
						(move r0 (immediate-rep #f) #:cndl "ne")]
					[(<)
						(emit-2operands x)
						(emit "	cmp ~a, ~a" (ptr-loc r0) (ptr-loc r1))
						(move r0 (immediate-rep #t) #:cndl "lt")
						(move r0 (immediate-rep #f) #:cndl "gt")]

					[else
						(raise-user-error (format "unknown expr to emit: ~a" x))]))

			((symbol? x) (move r0 (lookup x env)))
			(#t (raise-user-error (format "unknown expr type to emit: ~a" x)))))

	(begin 
		(emit-expr x (set) 0 (make-hash))
		(emit "	bx lr")
	)
)