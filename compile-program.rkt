#lang racket

(provide compile-program)

(require "mem-defs.rkt")
(require "gba.rkt")

; interpreter values
(define label-count 0)

(define output-stack '())

(define (compile-unoptimized global-label x)
  (define (inst instr . args)
    (set! output-stack (append output-stack (list (list instr args)))))
  (define (symbol-append instr cndl)
    (string->symbol (string-append (symbol->string instr) (symbol->string cndl))))

  (define (move dst src #:cndl [cndl null])
    (define movc (if (not (null? cndl)) (symbol-append 'mov cndl) 'mov))
    (define ldrc (if (not (null? cndl)) (symbol-append 'ldr cndl) 'ldr))
    (define strc (if (not (null? cndl)) (symbol-append 'str cndl) 'str))
    (if (eq? dst src)
        (inst (format "    @ move ~a <- ~a skipped" (ptr-loc dst) (ptr-loc src)))
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
                                                    (inst strc scratch dst)])])))

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
      (inst 'strb r2 (reg-offset-ptr r0 0) 1) ; post-indexing mode; (++r0)* = r2
      ;(inst 'add r2 r2 1)
      (inst 'b start-label)
      (emit-label end-label)))

  ; immediate values
  (define (quote-empty-list? x) (equal? x (quote (quote ()))))
  (define (immediate-rep x)
    (define (tag x shamt t) (bitwise-ior (arithmetic-shift x shamt) t))
    (cond
      [(integer? x)   (tag x fixnum-shift fixnum-tag)]
      [(char? x)      (tag (char->integer x) char-shift char-tag)]
      [(boolean? x)   (tag (if x 1 0) boolean-shift boolean-tag)]
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
     (list? (member (primcall-op x) '(asm
                                      add1 sub1 integer->char char->integer null? zero? not integer? boolean?
                                      + - * = <
                                      cons cons? car cdr
                                      make-vector vector? vector-ref vector-set!
                                      make-string string? string-ref string-set!
                                      eq? constant-ref constant-init)))))

  ; let
  (define (bindings x) (cadr x))
  (define (body x) (caddr x))
  (define (let? x) (and (list? x) (eq? (car x) 'let)))
  (define (lookup x env)
    (hash-ref env x))
  (define (extend-env varname valptr env)
    (begin
      ;(displayln (format "setting ~a to ~a" varname valptr))
      (hash-set env varname valptr)))
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
      (set! label-count (add1 label-count))
      (format ".L~a" label-count)))
  (define (emit-label lbl)
    (inst (format "~a:" lbl)))

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
  (define (lexpr? x)
    (or
     (and (list? x)
          (and (eq? (car x) 'code) (list? (cadr x)) (list? (caddr x))))
     (eq? x '(datum))))
  (define (labels? x)
    (and (list? x) (eq? (car x) 'labels)))
  ;(define (labelcall? x)
  ;   (and (list? x) (eq? (car x) 'labelcall)))
  (define (funcall? x)
    (and (list? x) (eq? (car x) 'funcall)))
  (define (closure? x)
    (and (list? x) (eq? (car x) 'closure)))
  (define (lambda? x)
    (and (list? x) (eq? (car x) 'lambda)))
  (define (tailcall? x)
    (and (list? x) (eq? (car x) 'tailcall)))
  (define (apply? x env)
    #f)
  ;  (and (list? x) 
  ;       (or (eq? (car x) 'apply)
  ;           (and (hash-has-key? env (car x))
  ;                (bitwise-ior (ptr-loc (lookup (car x) env)))))))

  (define (emit-labels lvars expr si env)
    (define (env-gen lvars new-env)
      (if (null? lvars)
          new-env
          (env-gen
           (cdr lvars)
           (extend-env (car (car lvars)) (unique-label) new-env))))
    (let ([new-env (env-gen lvars env)])
      (for ([label-def lvars])
        (emit-lexpr (car label-def) (cadr label-def) si new-env))
      (emit-expr expr si env)))
  (define (emit-lexpr label-name label-code si env)
    (define (placehold-params params argc si env)
      (if (null? params) env
          (placehold-params (cdr params) (add1 argc) (- si wordsize)
                            (extend-env
                             (car params)
                             (stack-offset-ptr si) env))))
    (define (placehold-free-vars vars argc si env)
      (if (null? vars) env
          (begin
            (move r2 (reg-offset-ptr r0 (* argc wordsize)))
            (placehold-free-vars (cdr vars) (add1 argc)
                                 (push r2 si)
                                 (extend-env
                                  (car vars)
                                  (stack-offset-ptr si) env)))))
    (if (not (lexpr? label-code))
        (raise-user-error (format "not a lexpr (datum)/(code (formal-param ... ) (free-var ... ) <Expr>): ~a" label-code))
        
        (let*
                (
                 [argc (length (cadr label-code))]
                 [stack-used (+ (* argc wordsize) wordsize)]
                 [new-si (- 0 (+ (* argc wordsize) wordsize))]
                 [params-env (placehold-params (cadr label-code) 0 (- wordsize) env)]);(- si (- stack-used wordsize)) env)]) ; space for lr
              
              (define save-stack output-stack)
              (set! output-stack '()) ; bad :(
              (emit-label (lookup label-name env))
              (let ([new-env (placehold-free-vars (caddr label-code) 1 new-si params-env)])
                (inst "    @ setup done, emitting label code")
                (emit-expr (cadddr label-code) new-si new-env)
                (inst 'bx lr)
                (set! output-stack (append output-stack save-stack))))))
  ; (define (emit-labelcall label-name args si env)
  ;   (define (bind-args args argc si env) ; put em all on the stack
  ;       (if (null? args) (push lr si)
  ;           (begin
  ;               (emit-expr (car args) si env)
  ;               (bind-args (cdr args) (+ 1 argc) (push r0 si) env))))
  ;   (begin
  ;       (let ([stack-used (+ (* (length args) wordsize) wordsize)]) ; space for lr
  ;           (inst 'sub sp sp stack-used)
  ;           (bind-args args 0 (+ si (- stack-used wordsize)) env)
  ;           (inst 'bl (lookup label-name env))
  ;           (move lr (stack-offset-ptr si))
  ;           (inst 'add sp sp stack-used))))

  ; closures
  (define (emit-closure lvar values si env)
    (inst (format "    @ lvar ~a values ~a" lvar values))
    (inst 'ldr r0 (format "=~a" (lookup lvar env)))
    (inst 'lsl r0 r0 heap-shift)
    (inst 'orr r0 r0 closure-tag)
    (move (heap-offset-ptr 0) r0)
    (for ([value values] [i (in-range 1 (add1 (length values)))])
      (inst (format "    @ value ~a" value))
      (emit-expr value si env)
      (move (heap-offset-ptr (* i wordsize)) r0))
    (move r0 heapptr)
    (inst 'add heapptr heapptr (* (add1 (length values)) wordsize)))

  (define (emit-funcall operator args si env)
    (define (bind-args args argc si env) ; put em all on the stack
      (if (null? args)
          (begin
            (inst "    @ evaluating operator")
            (emit-expr operator si env)
            si)
          (begin
            (inst (format "    @ funcall arg ~a" (car args)))
            (emit-expr (car args) si env)
            (bind-args (cdr args) (add1 argc) (push r0 si) env))))
    (let ([new-si (bind-args args 0 (push lr si) env)]) ; space for closure ptr
      (inst "    @ get proc addr from closure pointer")
      (inst 'ldr r1 "[r0]") ; r0: closure pointer, r1: func address
      (inst 'asr r1 r1 heap-shift)
      (inst "    @ shift stack to prepare for jump")
      (inst 'add sp sp si)
      (move lr pc)
      (inst "    @ jump to closure")
      (inst 'bx r1)
      (inst 'sub sp sp si)
      (pop lr si)))

  (define (emit-tailcall operator args si env)
    (define (bind-args args argc si env) ; put em all on the stack
      (if (null? args)
          (begin
            (inst "    @ evaluating tailcall operator")
            (emit-expr operator si env)
            si)
          (begin
            (inst (format "    @ tailcall arg ~a" (car args)))
            (emit-expr (car args) si env)
            (bind-args (cdr args) (add1 argc) (push r0 si) env))))
    (define (overwrite-args args argc new-si si env)
      (if (null? args) '()
          (begin
            (inst (format "    @ copying arg ~a" argc))
            (move
             (stack-offset-ptr (- si (* (- argc 1) 4)))
             (stack-offset-ptr (- new-si (* (- argc 1) 4))))
            (overwrite-args (cdr args) (- argc 1) new-si si env))))
    ;(let ([new-si (bind-args args 0 (push lr si) env)]) ; space for closure ptr
    (let ([new-si (bind-args args 0 si env)]) ; don't need to save LR for a tailcall
      (inst "    @ get proc addr from closure pointer")
      (inst 'ldr r1 "[r0]") ; r0: closure pointer, r1: func address
      (inst 'asr r1 r1 heap-shift)
      (inst "    @ overwrite caller's stack args with tailcall args")
      (overwrite-args args 0 new-si si env)
      ;(move lr pc) ; we shouldn't need this b/c nothing to return to
      (inst "    @ jump to tailcall closure")
      (inst 'bx r1)
      ;(inst 'sub sp sp si)
      (pop lr si)))
  
  (define (emit-apply expr si env tailcall-flag)
    (let ([op+args
           (if (and (list? expr) (eq? (car expr) 'apply))
               ((cadr expr) . (cadr (caddr expr)))
               (if (and (hash-has-key? env (car x))
                        (bitwise-ior (ptr-loc (lookup (car x) env))))
                   ((car expr) . (cdr expr))
                   (error (format "error: expression not an apply form: ~a" expr))))])
      (emit-funcall (first op+args) (second op+args) si env)))

  ; lambdas
  (define (emit-lambda expr si env)
    (define (find-free-vars params body env)
      (define (is-free? var)
        (and (not (member var params)) (hash-has-key? env var)))
      (filter is-free? (flatten body)))
    (let* (
           [params (cadr expr)]
           [body (caddr expr)]
           [free-vars (find-free-vars (cadr expr) (caddr expr) env)]
           [label-name (unique-label)]
           [lvar (string->symbol label-name)]
           [new-env (extend-env lvar label-name env)])
      ;special case where body is a complex constant
      (if (and
           (null? params)
           (null? free-vars)
           (not (immediate? (car body))))
          (emit-complex-constant body lvar si new-env)
          (begin
            (emit-lexpr lvar (list 'code params free-vars body) si new-env)
            (emit-closure lvar free-vars si new-env)))))
  (define (emit-complex-constant expr ref-lvar si env)
    (inst (format "    @ emitting complex const ~a" expr))
    ; create space for word on heap
    ; generate store-lvar label
    ; let si' = push heap ptr to stack
    ; let env' = extend-env store-lvar (stack-offset-ptr si) env
    ; constant-init store-lvar expr
    ; emit-closure ref-lvar '() si' env', returned to emit-let and bound to original lhs
    (let*
      (
        [store-lvar (string->symbol (unique-label))]
        [ref-env (extend-env ref-lvar (symbol->string ref-lvar) env)]
        [store-env (extend-env store-lvar (heap-offset-ptr 0) ref-env)])
      ; constant-init
      (inst (format "    @ cc lexpr"))
      (emit-lexpr ref-lvar (list 'code '() '() (list 'constant-ref store-lvar)) si store-env)
      (inst (format "    @ cc expr"))
      (emit-expr expr si env)
      (inst (format "    @ storing result"))
      (move (heap-offset-ptr 0) r0)
      (let ([new-si si]);(push r0 si)]);(push heapptr si)])
        (inst 'add heapptr heapptr wordsize)
        (inst (format "    @ emitting closure"))
        (emit-closure ref-lvar (list (list 'constant-ref store-lvar)) new-si store-env))))

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
      [(labels? x) (emit-labels (cadr x) (caddr x) si env)]
      ;[(labelcall? x) (emit-labelcall (cadr x) (cddr x) si env)]
      [(funcall? x) (emit-funcall (cadr x) (cddr x) si env)]
      [(tailcall? x) (emit-tailcall (cadr x) (cddr x) si env)]
      [(closure? x) (emit-closure (cadr x) (cddr x) si env)]
      [(lambda? x) (emit-lambda x si env)]
      [(apply? x env) (emit-apply x si env)]
      [(primcall? x)
       (case (primcall-op x)

         ; unary primitives
         [(asm)
          (inst (cadr x))]
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
         [(= char=? eq?) ; TODO: error on comparing immediate/nonimmediate or using wrong eq
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
          ;(emit "    orr ~a, ~a, #2" (ptr-loc r0) (ptr-loc r0))
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
         [(constant-init)
          (inst (format "    @ init constant ~a" x))
          (emit-operands 1 x si)
          (move (heap-offset-ptr 0) r0)
          (move r0 heapptr)
          (inst 'add heapptr heapptr wordsize)]
         [(constant-ref)
          (inst (format "    @ ref constant ~a" x))
          (move r0 (lookup (cadr x) env))]

         [else
          (raise-user-error (format "unknown expr to emit: ~a" x))])]

      [(symbol? x) (move r0 (lookup x env))]
      [#t (raise-user-error (format "unknown expr type to emit: ~a" x))]))

  (begin 
    (define env (make-immutable-hash))
    ;(inst ".thumb_func")
    (emit-label global-label)
    (load-addr heapptr (mem-ptr loc-iwram)) ; set up the heap
    (for ([line x])
      (emit-expr line 0 env))
    (inst 'bx lr)
    output-stack))

(define (optimize stack)
  (let ([regs-clean (make-hash)])
    (for-each (lambda (reg) (hash-set! regs-clean reg #f)) all-regs)
    (for/list ([line stack])
      line)))

(define (compile-program emit global-label x)
  (define (op->string op)
    (cond
      [(string? op) op]
      [(integer? op) (format "#~a" op)]
      [(ptr? op)
       (cond
         [(or (reg-ptr? op) (mem-ptr? op)) (ptr-loc op)]
         [(offset-ptr? op)
          (if (zero? (offset-amt op))
              (format "[~a]" (ptr-loc (offset-base op)))
              (format "[~a, #~a]" (ptr-loc (offset-base op)) (offset-amt op)))])]))

  (let ([reduced-stack (optimize (compile-unoptimized global-label x))])

    (for ([line reduced-stack])
      (if (string? line)
          (emit "~a" line)
          (case (length (cadr line))
            [(0) (emit "~a" (first line))]
            [(1) (emit
                  "    ~a ~a"
                  (symbol->string (first line))
                  (op->string (first (cadr line))))]
            [(2) (emit
                  "    ~a ~a, ~a"
                  (symbol->string (first line))
                  (op->string (first (cadr line)))
                  (op->string (second (cadr line))))]
            [(3) (emit
                  "    ~a ~a, ~a, ~a"
                  (symbol->string (first line))
                  (op->string (first (cadr line)))
                  (op->string (second (cadr line)))
                  (op->string (third (cadr line))))])))))
