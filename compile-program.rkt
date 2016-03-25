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
	(define (load-addr dst src)
		(cond
			[(not (reg-ptr? dst))
				(raise-user-error (format "only supports loading addr into register: ~a" (ptr-type dst)))]
			[(mem-ptr? src)
				(move dst (ptr-loc src))]
			[(offset-ptr? src)
				(move dst (offset-base src))
				(emit "	add ~a, ~a, #~a" (ptr-loc dst) (ptr-loc dst) (offset-amt src))]
			[#t
				(raise-user-error (format "will not result in a mem addr: ~a" (ptr-type src)))]))
	(define (move dst src
		#:si [si 0] #:env [env (make-hash)] #:cndl [cndl ""] #:shift [shift null])
		(if (not (or (ptr? src) (integer? src)))
			(raise-user-error (format "invalid move src: ~a" src))
			(if (not (ptr? dst))
				(raise-user-error (format "invalid move dst: ~a" dst))
				(if (not (eq? src dst))
					(cond
						[(reg-ptr? dst)
							(cond
								[(integer? src)
									(if (null? shift)
										(if (can-mov-constant src)
											(emit "	mov~a ~a, #~a" cndl (ptr-loc dst) src)
											(begin
												(emit "	ldr~a ~a, =#~a" cndl (ptr-loc dst) src)
												; tonc says below is needed but I don't think it is?
												;(emit "	ldr~a ~a, ~a" cndl (ptr-loc dst) (ptr-loc dst))
												))
										(if (can-mov-constant src)
											(emit "	mov~a ~a, #~a, ~a #~a" cndl (ptr-loc dst) src (car shift) (cadr shift))
											(raise-user-error (format "ldr immediate with barrel shift not supported: ~a" src))))]
								[(reg-ptr? src)
									(if (null? shift)
										(emit "	mov~a ~a, ~a" cndl (ptr-loc dst) (ptr-loc src))
										(emit "	mov~a ~a, ~a, ~a #~a" cndl (ptr-loc dst) (ptr-loc src) (car shift) (cadr shift)))]
								[(offset-ptr? src)
									(emit "	ldr~a ~a, [~a, #~a]" cndl (ptr-loc dst) (ptr-loc (offset-base src)) (offset-amt src))]
								[(mem-ptr? src)
									(emit "	ldr~a ~a, ~a" cndl (ptr-loc dst) (ptr-loc src))]
								[#t
									(raise-user-error "internal move failure")])]
						[(offset-ptr? dst)
							(if (not (null? shift))
								(raise-user-error (format "memory load with barrel shift not supported: ~a" src))
								(cond
									[(immediate? src)
										(if (can-mov-constant src)
											(emit "	str~a #~a, [~a, #~a]" cndl src (ptr-loc (offset-base dst)) (offset-amt dst))
											(begin
												(emit "	ldr~a ~a, =#~a" cndl scratch src)
												(emit "	str~a ~a, [~a, #~a]" cndl scratch (ptr-loc (offset-base dst)) (offset-amt dst))))]
									[(reg-ptr? src)
										(emit "	str~a ~a, [~a, #~a]" cndl (ptr-loc src) (ptr-loc (offset-base dst)) (offset-amt dst))]
									[(offset-ptr? src)
										(emit "	ldr~a ~a, [~a, #~a]" cndl scratch (ptr-loc (offset-base src)) (offset-amt src))
										(emit "	str~a ~a, [~a, #~a]" cndl scratch (ptr-loc (offset-base dst)) (offset-amt dst))]
									[#t
										(raise-user-error (format "internal move failure"))]))]
						[(mem-ptr? dst)
							(if (not (null? shift))
									(raise-user-error (format "memory load with barrel shift not supported: ~a" src))
								(cond
									[(immediate? src)
										(if (can-mov-constant src)
											(emit "	str~a #~a, ~a" cndl src (ptr-loc dst))
											(begin
												(emit "	ldr~a ~a, =#~a" cndl scratch src)
												(emit "	str~a ~a, ~a" cndl scratch (ptr-loc dst))))]
									[(reg-ptr? src)
										(emit "	str~a ~a, ~a" cndl (ptr-loc src) (ptr-loc dst))]
									[(offset-ptr? src)
										(emit "	ldr~a ~a, [~a, #~a]" cndl scratch (ptr-loc (offset-base src)) (offset-amt src))
										(emit "	str~a ~a, ~a" cndl scratch (ptr-loc dst))]
									[(mem-ptr? src)
										(emit "	ldr~a ~a, ~a" cndl scratch (ptr-loc src))
										(emit "	str~a ~a, ~a" cndl scratch (ptr-loc dst))]
									[#t
										(raise-user-error (format "internal move failure"))]))]
						[#t
							(raise-user-error (format "unsupported pointer type ~a" (ptr-type dst)))])
					'()))))
	(define (push src si) (begin
		(move (stack-offset-ptr si) src)
		(- si wordsize)))
	(define (pop dst si) (begin
		(move dst (stack-offset-ptr si))
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
	(define (zero-fill start-ptr length)
		(let (
			[start-label (unique-label)]
			[end-label (unique-label)])

			(load-addr r0 start-ptr)
			(move r2 #x00)
			(emit "	add r1, r0, #~a" length)
			(emit-label start-label)
			(emit "	cmp r0, r1")
			(emit "	beq ~a" end-label)
			(emit "	strb r2, [r0], #1")
			(emit "	add r2, r2, #0x01")
			(emit "	b ~a" start-label)
			(emit-label end-label)))

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
	(define (primcall-operand n x)
		(list-ref x n))
	(define (primcall? x) 
		(and (list? x) (list? (member (primcall-op x) '(
			add1 sub1 integer->char char->integer
			null? zero? not integer? boolean?
			+ - * = <
			cons cons? car cdr
			make-vector vector? vector-ref vector-set!
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
						(extend-env (lhs b) (stack-offset-ptr si) new-env) (push r0 si)))))))

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
			(emit-expr (primcall-operand 1 x) si env)
			(emit "	cmp ~a, #~a" (ptr-loc r0) rand)
			(move r0 (immediate-rep #t) #:cndl "eq")
			(move r0 (immediate-rep #f) #:cndl "ne")))
		(define (cmp-tag rand-ptr mask tag) (begin
			(move r0 rand-ptr)
			(emit-operands 1 x si)
			(emit "	and ~a, ~a, #~a" (ptr-loc r0) (ptr-loc rand-ptr) mask)
			(cmp-and-set-boolean tag)))
		(define (immediate-convert ptr src-shift dst-shift dst-tag)
			(let ((shamt (- dst-shift src-shift)))
				(if (> shamt 0)
					(emit "	lsl ~a, ~a, #~a" (ptr-loc ptr) (ptr-loc ptr) shamt)
					(emit "	asr ~a, ~a, #~a" (ptr-loc ptr) (ptr-loc ptr) (- shamt))))
			(emit "	and ~a, ~a, #~a" (ptr-loc ptr) (ptr-loc ptr) (arithmetic-shift -1 dst-shift))
			(emit "	orr ~a, ~a, #~a" (ptr-loc ptr) (ptr-loc ptr) dst-tag))

		(define (emit-operands n x si) ;make this into a macro
			(if (= n 1)
				(emit-expr (primcall-operand 1 x) si env)
				(begin
					(emit-expr (primcall-operand n x) si env)
					(let* (
						[saved (new-saved-reg)]
						[stash-reqd (reg-in-use? saved)]
						[new-si (if stash-reqd (- si wordsize) si)])

						(if stash-reqd
							(stash-reg saved si)
							(mark-used saved))

						(move saved r0)

						(emit-operands (- n 1) x new-si)

						(move (case n [(3) r2] [(2) r1] [else (raise-user-error (format "too many operands to emit: ~a" n))]) saved) ; macrooo

						(if stash-reqd
							(restore-reg saved si)
							(mark-unused saved))))))

		(cond
			[(immediate? x) (emit-immediate x)]
			[(let? x) (emit-let (bindings x) (body x) si env)]
			[(if? x) (emit-if (cadr x) (caddr x) (cadddr x) si env)]
			[(primcall? x)
				(case (primcall-op x)

					; unary primitives
					[(add1)
						(emit-operands 1 x si)
						(emit "	add ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0) (immediate-rep 1))]
					[(sub1)
						(emit-operands 1 x si)
						(emit "	sub ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0) (immediate-rep 1))]
					[(integer->char)
						(emit-operands 1 x si)
						(immediate-convert r0 fixnum-shift char-shift char-tag)]
					[(char->integer)
						(emit-operands 1 x si)
						(immediate-convert r0 char-shift fixnum-shift fixnum-tag)]
					[(null?)
						(emit-operands 1 x si)
						(cmp-and-set-boolean empty-list-val)]
					[(zero?)
						(emit-operands 1 x si)
						(cmp-and-set-boolean 0)]
					[(not)
						(emit-operands 1 x si)
						(cmp-and-set-boolean (immediate-rep #f))]
					[(integer?) (cmp-tag r0 fixnum-mask fixnum-tag)]
					[(boolean?) (cmp-tag r0 boolean-mask boolean-tag)]

					; binary primtives
					[(+)
						(emit-operands 2 x si)
						(emit "	add r0, r0, r1")]
					[(-)
						(emit-operands 2 x si)
						(emit "	sub r0, r0, r1")]
					[(*)
						(emit-expr (primcall-operand 2 x) si env)

						(let* ([saved (new-saved-reg)] [stash-reqd (reg-in-use? saved)])
							(if stash-reqd
								(stash-reg saved si)
								(mark-used saved))

							(emit "	asr ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0)  fixnum-shift)
							(move saved r0)
							(emit-expr (primcall-operand 1 x) (if stash-reqd (- si wordsize) si) env)
							(move r1 r0 #:shift (list "asr" fixnum-shift))
							(emit "	mul r0, ~a, r1" (ptr-loc saved))
							(emit "	lsl ~a, ~a, #~a" (ptr-loc r0) (ptr-loc r0) fixnum-shift)

							(if stash-reqd
								(restore-reg saved si)
							(mark-unused si)))]
					[(= char=?)
						(emit-operands 2 x si)
						(emit "	cmp ~a, ~a" (ptr-loc r0) (ptr-loc r1))
						(move r0 (immediate-rep #t) #:cndl "eq")
						(move r0 (immediate-rep #f) #:cndl "ne")]
					[(<)
						(emit-operands 2 x si)
						(emit "	cmp ~a, ~a" (ptr-loc r0) (ptr-loc r1))
						(move r0 (immediate-rep #t) #:cndl "lt")
						(move r0 (immediate-rep #f) #:cndl "gt")]

						; heap primitives
					[(cons)
						(let ((car r0) (cdr r1))
							(emit-operands 2 x si)
							(move (heap-offset-ptr 0) car)
							(move (heap-offset-ptr wordsize) cdr)
							(move r0 heapptr)
							(emit "	orr r0, r0, #1")
							(emit "	add ~a, ~a, #~a" (ptr-loc heapptr) (ptr-loc heapptr) (* 2 wordsize)))]
					[(cons?) (cmp-tag r0 heap-mask pair-tag)]

					[(car)
						(emit-operands 1 x si)
						(move r0 (ptr 'offset (list r0 -1)))]
					[(cdr)
						(emit-operands 1 x si)
						(move r0 (ptr 'offset (list r0 3)))]

					[(make-vector)
						(zero-fill (heap-offset-ptr wordsize) (* wordsize (primcall-operand 1 x)))
						(emit-operands 1 x si)
						(move (heap-offset-ptr 0) r0)
						(move r1 r0)
						(move r0 heapptr)
						; do some checking to confirm that we want to return heap or 2
						(emit "	orr ~a, ~a, #2" (ptr-loc r0) (ptr-loc r0))
						(emit "	add ~a, ~a, #11" (ptr-loc r1) (ptr-loc r1))
						(emit "	and ~a, ~a, #-8" (ptr-loc r1) (ptr-loc r1))
						(emit "	add ~a, ~a, ~a" (ptr-loc heapptr) (ptr-loc r1) (ptr-loc heapptr))]
					[(vector?) (cmp-tag r0 heap-mask vector-tag)]
					[(vector-ref)
						(emit-operands 2 x si)
						(emit "	add ~a, ~a, ~a" (ptr-loc r0) (ptr-loc r0) (ptr-loc r1))
						(move r0 (reg-offset-ptr r0 wordsize))]
					[(vector-set!)
						(emit-operands 3 x si)
						(emit "	add ~a, ~a, ~a" (ptr-loc r0) (ptr-loc r0) (ptr-loc r1))
						(move (reg-offset-ptr r0 wordsize) r2)
						(move r0 empty-list-val)]

					[(make-string)
						(zero-fill (heap-offset-ptr wordsize) (primcall-operand 1 x))
						(emit-operands 1 x si)
						(move (heap-offset-ptr 0) r0)
						(move r1 r0)
						(move r0 heapptr)
						;(emit "	orr ~a, ~a, #2" (ptr-loc r0) (ptr-loc r0))
						(emit "	add ~a, ~a, #3" (ptr-loc r1) (ptr-loc r1))
						(emit "	and ~a, ~a, #-3" (ptr-loc r1) (ptr-loc r1))
						(emit "	add ~a, ~a, ~a" (ptr-loc heapptr) (ptr-loc r1) (ptr-loc heapptr))]
					[(string?) (cmp-tag r0 heap-mask string-tag)]
					[(string-ref)
						(emit-operands 2 x si)
						(emit "	add ~a, ~a, ~a" (ptr-loc r0) (ptr-loc r0) (ptr-loc r1))
						(move r0 (reg-offset-ptr r0 1) #:cndl "b")
						(immediate-convert r0 byte-shift char-shift char-tag)]
					[(string-set!)
						(emit-operands 3 x si)
						(immediate-convert r2 char-shift byte-shift byte-tag)
						(emit "	add ~a, ~a, ~a" (ptr-loc r0) (ptr-loc r0) (ptr-loc r1))
						(move (reg-offset-ptr r0 1) r2 #:cndl "b")
						(move r0 empty-list-val)]

					[else
						(raise-user-error (format "unknown expr to emit: ~a" x))])]

			[(symbol? x) (move r0 (lookup x env))]
			[#t (raise-user-error (format "unknown expr type to emit: ~a" x))]))

	(begin 
		(emit-expr x 0 (make-hash))
		(emit "	bx lr")))