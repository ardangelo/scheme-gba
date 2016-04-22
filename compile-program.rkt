#lang racket

(provide compile-program)

(require "mem-defs.rkt")
(require "gba.rkt")

; interpreter values
(define label-count 0)

(define output-stack '())

; arm syntax macros
(define-syntax label
	(syntax-rules ()
		[(label lbl) (set! output-stack (append output-stack (list lbl)))]))
(define-syntax imm
	(syntax-rules ()
		[(imm x) (format "#~a" x)]))
(define-syntax regsh
	(syntax-rules ()
		[(regsh reg amt) (format "[~a, #~a]" reg amt)]))
(define-syntax barsh
	(syntax-rules ()
		[(barsh shtgt shamt)
			(format "~a, ~a #~a" shtgt (if (> shamt 0) "lsl" "asr") (abs shamt))]))

(define (compile-unoptimized x)
	(define (inst instr . args)
		(set! output-stack (append output-stack (list (list instr args)))))
	(define (symbol-append instr cndl)
		(string->symbol (string-append (symbol->string instr) (symbol->string cndl))))

	(define (move dst src #:cndl [cndl null])
		(define movc (if (not (null? cndl)) (symbol-append 'mov cndl) 'mov))
		(define ldrc (if (not (null? cndl)) (symbol-append 'ldr cndl) 'ldr))
		(define strc (if (not (null? cndl)) (symbol-append 'str cndl) 'str))
		(cond
			[(reg-ptr? dst) (cond
				[(integer? src)
					(if (can-mov-constant src)
						(inst movc dst src)
						(inst ldrc dst (format "=~a" src)))]
				[(reg-ptr? src) (inst movc dst src)]
				[(or (offset-ptr? src) (mem-ptr? src))
					(inst ldrc dst src)])]
			[(or (offset-ptr? dst) (mem-ptr? dst)) (cond
				[(or (integer? src) (reg-ptr? src)) (inst strc src dst)]
				[(or (offset-ptr? src) (mem-ptr? src))
					(inst ldrc scratch src)
					(inst strc scratch dst)])]))

	(define (stash-reg reg offset)
		(move (stack-offset-ptr offset) reg))
	(define (restore-reg reg offset)
		(move reg (stack-offset-ptr offset)))

	(define (immediate? x) (or (integer? x) (char? x) (boolean? x) (quote-empty-list? x)))
	(define (load-addr dst src)
		(cond
			[(not (reg-ptr? dst))
				(raise-user-error (format "only supports loading addr into register: ~a" (ptr-type dst)))]
			[(mem-ptr? src)
				(move dst (ptr-loc src))]
			[(offset-ptr? src)
				(move dst (offset-base src))
				(inst 'add (ptr-loc dst) (ptr-loc dst) (offset-amt src))]
			[#t
				(raise-user-error (format "will not result in a mem addr: ~a" (ptr-type src)))]))
	(define (push src si)
		(begin
			(move (stack-offset-ptr si) src)
			(- si wordsize)))
	(define (pop dst si)
		(begin
			(move dst (stack-offset-ptr si))
			(+ si wordsize)))
	(define (zero-fill start-ptr length)
		(let (
			[start-label (unique-label)]
			[end-label (unique-label)])

			(load-addr r0 start-ptr)
			(move r2 0)
			(inst 'add r1 r0 length)
			(emit-label start-label)
			(inst 'cmp r0 r1)
			(inst 'beq end-label)
			(inst 'strb r2 "[r0]" 1)
			(inst 'add r2 r2 1)
			(inst 'b start-label)
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
		(and
			(list? x)
			(list? (member (primcall-op x) '(
				add1 sub1 integer->char char->integer null? zero? not integer? boolean?
				+ - * = <
				cons cons? car cdr
				make-vector vector? vector-ref vector-set!
				make-string string? string-ref string-set!
				primcall funcall)))))

	; let
	(define (bindings x) (cadr x))
	(define (body x) (caddr x))
	(define (let? x) (and (list? x) (eq? (car x) 'let)))
	(define (lookup x env)
		(hash-ref env x))
	(define (extend-env varname valptr env)
		(begin
			(hash-set! env varname valptr)
			env))
	(define (emit-let bindings body si env)
		(define (lhs b) (car b))
		(define (rhs b) (cadr b))
		(let f ([b* bindings] [new-env env] [si si])
			(cond
				[(null? b*)
					(emit-expr body si new-env)]
				[#t
					(let ((b (car b*)))
						(emit-expr (rhs b) si env)
						(f (cdr b*) 
							(extend-env (lhs b) (stack-offset-ptr si) new-env) (push r0 si)))])))

	; conditionals
	(define (unique-label)
		(begin
			(set! label-count (+ label-count 1))
			(format ".L~a" label-count)))
	(define (emit-label lbl)
		(label lbl))

	(define (if? x) (and (list? x) (eq? (car x) 'if)))
	(define (emit-if condition if-true if-false si env)
		(let ((L0 (unique-label)) (L1 (unique-label)))
			(emit-expr condition si env)
			(inst 'cmp r0 (immediate-rep #f))
			(inst 'beq L0)
			(emit-expr if-true si env)
			(inst 'b L1)
			(emit-label L0)
			(emit-expr if-false si env)
			(emit-label L1)))

	; procedures
	(define (lvars x) (cadr x))
	(define (vars x) (cadr x))
	(define (expr x) (caddr x))

	(define (lexpr? x)
		(and (list? x) (eq? (car x) 'code) (list? (cadr x))))
	(define (labels? x)
		(and (list? x) (eq? (car x) 'labels)))
	(define (labelcall? x)
		(and (list? x) (eq? (car x) 'labelcall)))
	(define (closure? x)
		(and (list? x) (eq? (car x) 'closure)))

	(define (emit-labels lvars expr si env)
		(define (env-gen lvars new-env)
			(if (null? lvars)
				new-env
				(env-gen
					(cdr lvars)
					(extend-env (car (car lvars)) (unique-label) new-env))))
		(let ([new-env (env-gen lvars env)])
			(for ([label-def lvars])
				(emit-lexpr (car label-def) (cadr label-def) si new-env))))
	(define (emit-lexpr label-name label-code si env)
		(define (placehold-args vars argc si env)
			(if (null? vars) env
				(if (<= argc 4)
					(placehold-args (cdr vars) (+ argc 1) si
						(extend-env
							(car vars)
							(case argc [(4) r3] [(3) r2] [(2) r1] [(1) r0]) env))
					(placehold-args (cdr vars) (+ argc 1) (- si wordsize)
						(extend-env
							(car vars)
							(stack-offset-ptr si) env)))))
		(if (not (lexpr? label-code))
			(raise-user-error (format "not a lexpr (code (var ... ) <Expr>): ~a" label-code))
			(let* (
				[new-env (placehold-args (vars label-code) 1 (- si wordsize) env)]
				[argc (length (vars label-code))]
				[new-si (if (<= argc 4)
					(- si wordsize)
					(+ (- si (* wordsize (- argc 4))) wordsize))]) ; leave space for lr

				(begin
					(emit-label (lookup label-name new-env))
					(move (stack-offset-ptr si) lr)
					(emit-expr (expr label-code) new-si new-env)
					(move lr (stack-offset-ptr si))
					(inst 'bx (ptr-loc lr))))))
	(define (emit-labelcall label-name args si env)
		(define (bind-args args argc si env)
			(if (null? vars) si
				(begin
					(emit-expr (car args) si env)
					(let (
						[target (if (<= argc 4)
							(case argc [(4) r3] [(3) r2] [(2) r1] [(1) r0])
							(stack-offset-ptr si))]
						[new-si (if (<= argc 4) si (- si wordsize))])
						(move target r0)
						(bind-args (cdr args) (+ 1 argc) new-si env)))))
		(begin
			(bind-args args 1 si env)
			(inst 'b (lookup label-name env))))
	; (define (emit-closure label-name values si env)
	; 	; store address of (lookup label-name env) in r0
	; 	(move
	; 		(heap-offset-ptr 0)
	; 		(bitwise-ior (arithmetic-shift (string->integer (substring (lookup label-name env) 2)) heap-shift) closure-tag))
	; 	(move (heap-offset-ptr wordsize) r0)
	; 	(for ([value values] [i (in-range 2 (+ 2 (length values)))])
	; 		(emit-expr value si env)
	; 		(move (heap-offset-ptr (* i wordsize)) r0))
	; 	(move r0 heapptr)
	; 	(emit "	add ~a, ~a, #~a"
	; 		(ptr-loc heapptr)
	; 		(ptr-loc heapptr)
	; 		(* (+ (length values) 2))))

	; emit expressions
	(define (emit-expr x si env)
		; shortcuts
		(define (cmp-and-set-boolean rand)
			(begin
				(emit-expr (primcall-operand 1 x) si env)
				(inst 'cmp r0 rand)
				(move r0 (immediate-rep #t) #:cndl 'eq)
				(move r0 (immediate-rep #f) #:cndl 'ne)))
		(define (cmp-tag rand-ptr mask tag)
			(begin
				(move r0 rand-ptr)
				(emit-operands 1 x si)
				(inst 'and r0 rand-ptr mask)
				(cmp-and-set-boolean tag)))
		(define (immediate-convert ptr src-shift dst-shift dst-tag)
			(let ((shamt (- dst-shift src-shift)))
				(if (> shamt 0)
					(inst 'lsl ptr ptr shamt)
					(inst 'asr ptr ptr (- shamt))))
			(inst 'and ptr ptr (arithmetic-shift -1 dst-shift))
			(inst 'or ptr ptr dst-tag))

		(define (emit-operands n x si) ;make this into a macro
			(if (= n 1)
				(emit-expr (primcall-operand 1 x) si env)
				(if (> n 3)
					(begin ; save onto the stack
						(emit-expr (primcall-operand n x) si env)
						(stash-reg r0 si)
						(emit-operands (- n 1) x (- si wordsize)))
					(begin ; save in r1 - r3
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
								(mark-unused saved)))))))

		(cond
			[(immediate? x) (emit-immediate x)]
			[(let? x) (emit-let (bindings x) (body x) si env)]
			[(if? x) (emit-if (cadr x) (caddr x) (cadddr x) si env)]
			[(labels? x) (emit-labels (lvars x) (expr x) si env)]
			; [(funcall? x) (emit-funcall (cadr x) (cddr x) si env)]
			; [(closure? x) (emit-closure (cadr x) (cddr x) si env)]
			[(primcall? x)
				(case (primcall-op x)

					; unary primitives
					[(add1)
						(emit-operands 1 x si)
						(inst 'add r0 r0 (immediate-rep 1))]
					[(sub1)
						(emit-operands 1 x si)
						(inst 'sub r0 r0 (immediate-rep 1))]
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
						(inst 'add r0 r0 r1)]
					[(-)
						(emit-operands 2 x si)
						(inst 'sub r0 r0 r1)]
					[(*)
						(emit-expr (primcall-operand 2 x) si env)

						(let* ([saved (new-saved-reg)] [stash-reqd (reg-in-use? saved)])
							(if stash-reqd
								(stash-reg saved si)
								(mark-used saved))

							(inst 'asr r0 r0 fixnum-shift)
							(move saved r0)
							(emit-expr (primcall-operand 1 x) (if stash-reqd (- si wordsize) si) env)
							(inst 'asr r1 r0 fixnum-shift)
							(inst 'mul r0 saved r1)
							(inst 'lsl r0 r0 fixnum-shift)

							(if stash-reqd
								(restore-reg saved si)
							(mark-unused si)))]
					[(= char=?)
						(emit-operands 2 x si)
						(inst 'cmp r0 r1)
						(move r0 (immediate-rep #t) #:cndl 'eq)
						(move r0 (immediate-rep #f) #:cndl 'ne)]
					[(<)
						(emit-operands 2 x si)
						(inst 'cmp r0 r1)
						(move r0 (immediate-rep #t) #:cndl 'lt)
						(move r0 (immediate-rep #f) #:cndl 'gt)]

						; heap primitives
					[(cons)
						(let ((car r0) (cdr r1))
							(emit-operands 2 x si)
							(move (heap-offset-ptr 0) car)
							(move (heap-offset-ptr wordsize) cdr)
							(move r0 heapptr)
							(inst 'orr r0 r0 1)
							(inst 'add heapptr heapptr (* 2 wordsize)))]
					[(cons?) (cmp-tag r0 heap-mask pair-tag)]

					[(car)
						(emit-operands 1 x si)
						(move r0 (reg-offset-ptr r0 -1))]
					[(cdr)
						(emit-operands 1 x si)
						(move r0 (reg-offset-ptr r0 3))]

					[(make-vector)
						(zero-fill (heap-offset-ptr wordsize) (* wordsize (primcall-operand 1 x)))
						(emit-operands 1 x si)
						(move (heap-offset-ptr 0) r0)
						(move r1 r0)
						(move r0 heapptr)
						; do some checking to confirm that we want to return heap or 2
						(inst 'orr r0 r0 2)
						(inst 'add r1 r1 11)
						(inst 'and r1 r1 -8)
						(inst 'add heapptr r1 heapptr)]
					[(vector?) (cmp-tag r0 heap-mask vector-tag)]
					[(vector-ref)
						(emit-operands 2 x si)
						(inst 'add r0 r0 r1)
						(move r0 (reg-offset-ptr r0 wordsize))]
					[(vector-set!)
						(emit-operands 3 x si)
						(inst 'add r0 r0 r1)
						(move (reg-offset-ptr r0 wordsize) r2)
						(move r0 empty-list-val)]

					[(make-string)
						(zero-fill (heap-offset-ptr wordsize) (primcall-operand 1 x))
						(emit-operands 1 x si)
						(move (heap-offset-ptr 0) r0)
						(move r1 r0)
						(move r0 heapptr)
						;(emit "	orr ~a, ~a, #2" (ptr-loc r0) (ptr-loc r0))
						(inst 'add r1 r1 3)
						(inst 'and r1 r1 -3)
						(inst 'add heapptr r1 heapptr)]
					[(string?) (cmp-tag r0 heap-mask string-tag)]
					[(string-ref)
						(emit-operands 2 x si)
						(inst 'add r0 r0 r1)
						(move r0 (reg-offset-ptr r0 1) #:cndl 'b)
						(immediate-convert r0 byte-shift char-shift char-tag)]
					[(string-set!)
						(emit-operands 3 x si)
						(immediate-convert r2 char-shift byte-shift byte-tag)
						(inst 'add r0 r0 r1)
						(move (reg-offset-ptr r0 1) r2 #:cndl 'b)
						(move r0 empty-list-val)]

					[else
						(raise-user-error (format "unknown expr to emit: ~a" x))])]

			[(symbol? x) (move r0 (lookup x env))]
			[#t (raise-user-error (format "unknown expr type to emit: ~a" x))]))

	(begin 
		(load-addr heapptr (mem-ptr loc-iwram)) ; set up the heap
		(emit-expr x 0 (make-hash))
		(inst 'bx lr)
		output-stack))

(define (optimize stack)
	(let ([regs-clean (make-hash)])
		(for-each (lambda (reg) (hash-set! regs-clean reg #f)) all-regs)
		(for/list ([line stack])
			line)))

(define (compile-program emit x)
	(define (op->string op)
		(cond
			[(string? op) op]
			[(integer? op) (format "#~a" op)]
			[(ptr? op)
				(cond
					[(or (reg-ptr? op) (mem-ptr? op)) (ptr-loc op)]
					[(offset-ptr? op) (regsh (ptr-loc (offset-base op)) (offset-amt op))])]))
	(let ([reduced-stack (optimize (compile-unoptimized x))])

		(for ([line reduced-stack])
			(case (length (second line))
				[(0) (emit "	~a" (first line))]
				[(1) (emit
					"	~a ~a"
					(symbol->string (first line))
					(op->string (first (cadr line))))]
				[(2) (emit
					"	~a ~a, ~a"
					(symbol->string (first line))
					(op->string (first (cadr line)))
					(op->string (second (cadr line))))]
				[(3) (emit
					"	~a ~a, ~a, ~a"
					(symbol->string (first line))
					(op->string (first (cadr line)))
					(op->string (second (cadr line)))
					(op->string (third (cadr line))))]))))