#lang racket

(provide (all-defined-out))

; machine values
(define wordsize 4)

; immediate values
(define fixnum-mask #b00000011)
(define fixnum-tag #b00000000)
(define fixnum-shift 2)

(define char-mask #b11111111)
(define char-tag #b00001111)
(define char-shift 8)

(define boolean-mask #b01111111)
(define boolean-tag #b00111111)
(define boolean-shift 7)

(define pair-tag #b001)
(define vector-tag #b010)
(define string-tag #b011)
(define symbol-tag #b101)
(define closure-tag #b110)

(define empty-list-val  #b00101111)

; memory defines

; memory management
(struct ptr (type loc)
	#:guard (lambda (type loc type-name)
		(cond
			[(not (member type '(reg stack heap mem)))
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
(define sp (ptr 'reg "sp"))

(define scratch r7)
(define stackptr sp)
(define heapptr r8)

(define (alloc regs si) ; eventually make this a queue that auto moves to stack
	(define gp-regs (list r1 r2 r3 r4 r5 r6 r9 r10 r11 r12)) ; r7 scratch, r8 heap

	(define (helper regs-to-check)
		(cond
			[(null? regs-to-check)
				(ptr 'stack si)]
			[(not (set-member? regs (car regs-to-check)))
				(car regs-to-check)]
			[#t (helper (cdr regs-to-check))]))
	(helper gp-regs))
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