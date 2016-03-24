#lang racket

(provide compile-program)

(require "mem-defs.rkt")
(require "gba.rkt")

; interpreter values
(define label-count 0)

(define (compile-program emit x)
	(emit "	mov ~a, #~a" (ptr-loc heapptr) (ptr-loc ptr-iwram)) ; set up the heap

	(define (immediate? x) (or (integer? x) (char? x) (boolean? x) (quote-empty-list? x)))
	(define (move dst src
		#:si [si 0] #:env [env (make-hash)] #:cndl [cndl ""] #:shift [shift null])
		(define (offset-reg? ptr) (member (ptr-type ptr) '(stack heap)))
		(define (offset-base ptr)
			(if (eq? (ptr-type ptr) 'stack)
				(ptr-loc stackptr)
				(ptr-loc heapptr)))
		(define (direct-reg? ptr) (member (ptr-type ptr) '(reg)))
		(define (direct-mem? ptr) (member (ptr-type ptr) '(mem)))
		(if (not (or (ptr? src) (var? src) (integer? src)))
			(raise-user-error (format "invalid move src: ~a" src))
			(if (not (ptr? dst))
				(raise-user-error (format "invalid move dst: ~a" dst))
				(if (not (eq? src dst))
					(cond
						[(direct-reg? dst)
							(cond
								[(immediate? src)
									(if (null? shift)
										(emit "	ldr~a ~a, =#~a" cndl (ptr-loc dst) src)
										(emit "	mov~a ~a, ~a, ~a ~a" cndl (ptr-loc dst) (ptr-loc src) (car shift) (cadr shift)))]
								[(direct-reg? src)
									(if (null? shift)
										(emit "	mov~a ~a, ~a" cndl (ptr-loc dst) (ptr-loc src))
										(emit "	mov~a ~a, ~a, ~a ~a" cndl (ptr-loc dst) (ptr-loc src) (car shift) (cadr shift)))]
								[(offset-reg? src)
									(emit "	ldr~a ~a, [~a, #~a]" cndl (ptr-loc dst) (offset-base src) (ptr-loc src))]
								[(direct-mem? src)
									(emit "	ldr~a ~a, ~a" cndl (ptr-loc dst) (ptr-loc src))]
								[#t
									(raise-user-error "internal move failure")])]
						[(offset-reg? dst)
							(cond
								[(immediate? src)
									(if (can-mov-constant src)
										(emit "	str~a #~a, [~a, #~a]" cndl src (offset-base dst) (ptr-loc dst))
										(begin
											(emit "	ldr~a ~a, =#~a" cndl scratch src)
											(emit "	str~a ~a, [~a, #~a]" cndl scratch (offset-base dst) (ptr-loc dst))))]
								[(direct-reg? src)
									(emit "	str~a ~a, [~a, #~a]" cndl (ptr-loc src) (offset-base dst) (ptr-loc dst))]
								[(offset-reg? src)
									(emit "	ldr~a ~a, [~a, #~a]" cndl scratch (offset-base src) (ptr-loc src))
									(emit "	str~a ~a, [~a, #~a]" cndl scratch (offset-base dst) (ptr-loc dst))]
								[#t
									(raise-user-error (format "internal move failure"))])]
						[(direct-mem? dst)
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
									(emit "	ldr~a ~a, [~a, #~a]" cndl scratch (offset-base src) (ptr-loc src))
									(emit "	str~a ~a, ~a" cndl scratch (ptr-loc dst))]
								[(direct-mem? src)
									(emit "	ldr~a ~a, ~a" cndl scratch (ptr-loc src))
									(emit "	str~a ~a, ~a" cndl scratch (ptr-loc dst))]
								[#t
									(raise-user-error (format "internal move failure"))])]
						[#t
							(raise-user-error (format "unsupported pointer type ~a" (ptr-type dst)))])
					(emit "	@ src and dst same reg")))))
	(define (push src si) (begin
		(move (ptr 'stack si) src)
		(- si wordsize)))
	(define (pop dst si) (begin
		(move dst (ptr 'stack si))
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
			add1 sub1 integer->char char->integer null? zero? not integer? boolean?
			+ - * = <
			cons car cdr)))))

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
					(f (cdr b*) 
						(extend-env (lhs b) (ptr 'stack si) new-env)
						regs
						(push r0 si)))))))

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
			(let ((op2-result2 (alloc regs si)))
				(emit-expr (primcall-operand2 x) regs si env)
				(move op2-result2 r0)
				(emit-expr (primcall-operand1 x) (add-regs regs op2-result2) (new-si si op2-result2) env)
				(load-reg r1 op2-result2)))

		(cond
			[(immediate? x) (emit-immediate x)]
			[(let? x) (emit-let (bindings x) (body x) regs si env)]
			[(if? x) (emit-if (cadr x) (caddr x) (cadddr x) regs si env)]
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
						(emit "	add r0, r0, ~a" (ptr-loc (emit-2operands x)))]
					[(-)
						(emit "	sub r0, r0, ~a" (ptr-loc (emit-2operands x)))]
					[(*)
						(let ((op2-result (alloc regs si)))
							(emit-expr (primcall-operand2 x) regs si env)
							(emit "	asr ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0)  fixnum-shift)
							(move op2-result r0)
							(emit-expr (primcall-operand1 x) (add-regs regs op2-result) (new-si si op2-result) env)
							(emit "	mov r2, r0, asr #~a" fixnum-shift)
							(emit "	mul r0, ~a, r2" (load-reg r1 op2-result))
							(emit "	lsl ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0) fixnum-shift))]
					[(= char=?)
						(emit "	cmp ~a, ~a" (ptr-loc r0) (ptr-loc (emit-2operands x)))
						(move r0 (immediate-rep #t) #:cndl "eq")
						(move r0 (immediate-rep #f) #:cndl "ne")]
					[(<)
						(emit "	cmp ~a, ~a" (ptr-loc r0) (ptr-loc (emit-2operands x)))
						(move r0 (immediate-rep #t) #:cndl "lt")
						(move r0 (immediate-rep #f) #:cndl "gt")]

						; heap binary primitives
					[(cons)
						(let ((car r0) (cdr (emit-2operands x)))
							(move (ptr 'heap 0) car)
							(move (ptr 'heap wordsize) cdr)
							(move r0 heapptr)
							(emit "	orr r0, r0, #1")
							(emit "	add ~a, ~a, #~a" (ptr-loc heapptr) (ptr-loc heapptr) (* 2 wordsize)))]
					[else
						(raise-user-error (format "unknown expr to emit: ~a" x))])]

			[(symbol? x) (move r0 (lookup x env))]
			[#t (raise-user-error (format "unknown expr type to emit: ~a" x))]))

	(begin 
		(emit-expr x (set) 0 (make-hash))
		(emit "	bx lr")
	)
)