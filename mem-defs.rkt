#lang racket

(provide (all-defined-out))

; machine values
(define wordsize 4)

; immediate values
(define byte-mask #xFF)
(define byte-tag 0)
(define byte-shift 0)

(define fixnum-mask #b00000011)
(define fixnum-tag #b00)
(define fixnum-shift 2)

(define char-mask #b11111111)
(define char-tag #b00001111)
(define char-shift 8)

(define boolean-mask #b01111111)
(define boolean-tag #b00111111)
(define boolean-shift 7)

(define heap-mask #b111)
(define heap-shift 3)
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
              [(not (member type '(reg offset mem)))
               (raise-user-error (format "invalid ptr type: ~a" type))]
              [else (values type loc)])))

(define (offset-ptr? ptr) (eq? (ptr-type ptr) 'offset))
(define (offset-base ptr)
  (if (offset-ptr? ptr)
      (car (ptr-loc ptr))
      (raise-user-error (format "not an offset ptr: ~a" ptr))))
(define (offset-amt ptr)
  (if (offset-ptr? ptr)
      (cadr (ptr-loc ptr))
      (raise-user-error (format "not an offset ptr: ~a" ptr))))
(define (reg-ptr? ptr) (member (ptr-type ptr) '(reg)))
(define (mem-ptr? ptr) (member (ptr-type ptr) '(mem)))

(define (reg-offset-ptr reg si)
  (ptr 'offset (list reg si)))
(define (stack-offset-ptr si)
  (reg-offset-ptr stackptr si))
(define (heap-offset-ptr si)
  (reg-offset-ptr heapptr si))
(define (mem-ptr loc)
  (ptr 'mem loc))

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
(define lr (ptr 'reg "lr"))
(define pc (ptr 'reg "pc"))

(define scratch r7)
(define stackptr sp)
(define heapptr r8)

; register mgmt and alloc

(define all-regs (list r0 r1 r2 r3 r4 r5 r6 r9 r10 r11 r12))
(define callee-saved-regs (list r4 r5 r6 r9 r10 r11 r12))
(define reg-alloc-map (make-hash))
(for ([reg callee-saved-regs])
  (hash-set! reg-alloc-map reg #f))

(define (reg-in-use? reg)
  (hash-ref reg-alloc-map reg))
(define (mark-used reg)
  (hash-set! reg-alloc-map reg #t))
(define (mark-unused reg)
  (hash-set! reg-alloc-map reg #f))

(define (new-saved-reg)
  (define select-random
    (lambda (ls)
      (let ((len (length ls)))
        (list-ref ls (random len)))))
  (define (helper regs-to-check)
    (cond
      [(null? regs-to-check)
       (select-random callee-saved-regs)]
      [(not (reg-in-use? (car regs-to-check)))
       (car regs-to-check)]
      [#t (helper (cdr regs-to-check))]))
  (helper callee-saved-regs))

(define (can-mov-constant b) ; can't mov more than 8b + 4b even shift
  (define (helper x shamt)
    (if (> shamt 30) #f
        (if (> (modulo x 4) 0)
            (<= (abs x) 255)
            (helper (arithmetic-shift -2 x) (+ shamt 2)))))
  (helper b 0))

; memory mgmt and alloc
