
(select-module (scythe internal compile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (map proc lst)
  (if (null? lst)
      lst
      (cons (proc (car lst)) (map proc (cdr lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (new-rdepth)
  (cons (cons -1 '()) '()))

(define (rdepth-ref rd)
  (car (car rd)))

(define (rdepth-expand! rd)
  (set-car! rd (cons -1 (car rd))))

(define (rdepth-contract! rd)
  (let* ((stk1 (car rd))
         (stk2 (cdr stk1))
         (n1 (car stk1))
         (n2 (car stk2)))
    (when (> n1 n2) (set-car! stk2 n1))
    (set-car! rd stk2)))

(define (rdepth-set! rd n)
  (let ((x (car (car rd))))
    (when (> n x)
      (set-car! (car rd) n))))

(define (rdepth-add! rd adder)
  (let ((n (+ (car (car rd)) adder)))
    (when (>= n -1)
      (set-car! (car rd) n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (push-inst-nop asmb)
  (assembler-push! asmb +asm-inst-nop+))
(define (push-inst-cframe typ lbl asmb)
  (assembler-push! asmb +asm-inst-cframe+ #t lbl))
(define (push-inst-eframe narg asmb)
  (assembler-push! asmb +asm-inst-eframe+ narg))
(define (push-inst-epop asmb)
  (assembler-push! asmb +asm-inst-epop+))
(define (push-inst-eshift ns asmb)
  (assembler-push! asmb +asm-inst-eshift+ ns))
(define (push-inst-immval val asmb)
  (assembler-push! asmb +asm-inst-immval+ val))
(define (push-inst-push asmb)
  (assembler-push! asmb +asm-inst-push+))
(define (push-inst-mvpush asmb)
  (assembler-push! asmb +asm-inst-mvpush+))
(define (push-inst-return asmb)
  (assembler-push! asmb +asm-inst-return+))
(define (push-inst-call narg mrv lbl asmb)
  (assembler-push! asmb +asm-inst-call+ narg)
  (if (= mrv 1)
      (push-inst-mrve asmb)
      (push-inst-nop asmb))
  (push-inst-label lbl asmb))
(define (push-inst-tcall narg asmb)
  (assembler-push! asmb +asm-inst-tcall+ narg))
(define (push-inst-gref sym mod asmb)
  (assembler-push! asmb +asm-inst-gref+ sym mod))
(define (push-inst-gdef sym mod asmb)
  (assembler-push! asmb +asm-inst-gdef+ sym mod))
(define (push-inst-gset sym mod asmb)
  (assembler-push! asmb +asm-inst-gset+ sym mod))
(define (push-inst-sref idx layer asmb)
  (assembler-push! asmb +asm-inst-sref+ idx layer))
(define (push-inst-sset idx layer asmb)
  (assembler-push! asmb +asm-inst-sset+ idx layer))
(define (push-inst-jmp typ lbl asmb)
  (assembler-push! asmb +asm-inst-jmp+ #t lbl))
(define (push-inst-jmpt typ lbl asmb)
  (assembler-push! asmb +asm-inst-jmpt+ #t lbl))
(define (push-inst-jmpf typ lbl asmb)
  (assembler-push! asmb +asm-inst-jmpf+ #t lbl))
(define (push-inst-box idx layer asmb)
  (assembler-push! asmb +asm-inst-box+ idx layer))
(define (push-inst-close nr-free arity code asmb)
  (assembler-push! asmb +asm-inst-close+ nr-free arity code))
(define (push-inst-demine idx layer asmb)
  (assembler-push! asmb +asm-inst-demine+ idx layer))
(define (push-inst-emine narg asmb)
  (assembler-push! asmb +asm-inst-emine+ narg))
(define (push-inst-edemine narg layer asmb)
  (assembler-push! asmb +asm-inst-edemine+ narg layer))
(define (push-inst-mrvc arity asmb)
  (assembler-push! asmb +asm-inst-mrvc+ arity))
(define (push-inst-mrve asmb)
  (assembler-push! asmb +asm-inst-mrve+))
(define (push-inst-module mod asmb)
  (assembler-push! asmb +asm-inst-module+ mod))
(define (push-inst-label lbl asmb)
  (assembler-push! asmb +asm-inst-label+ lbl))
(define (push-inst-undef asmb)
  (assembler-push! asmb +asm-inst-immval+ (begin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export (current-module) 'make-env)
(define (make-env module)
  module)

(module-export (current-module) 'env-outmost?)
(define (env-outmost? env)
  (module? env))

(module-export (current-module) 'env-extend)
(define (env-extend vars varg env)
  (let* ((vars (if (vector? vars) vars (list->vector vars)))
         (len (vector-length vars))
         (assigned (make-vector len #f)))
    (cons (vector 'variable vars assigned varg) env)))

(module-export (current-module) 'env-extend-syntax)
(define (env-extend-syntax keys syxs env)
  (let* ((keys (if (vector? keys) keys (list->vector keys)))
         (syxs (cond ((vector? syxs) syxs)
                     ((pair? syxs) (list->vector syxs))
                     (else (make-vector (vector-length keys) #f)))))
    (cons (vector 'keyword keys syxs) env)))

(module-export (current-module) 'env-variable-layer?)
(define (env-variable-layer? env)
  (eq? (vector-ref (car env) 0) 'variable))

(module-export (current-module) 'env-keyword-layer?)
(define (env-keyword-layer? env)
  (eq? (vector-ref (car env) 0) 'keyword))

(module-export (current-module) 'env-outer)
(define (env-outer env)
  (if (env-outmost? env) env (cdr env)))

(module-export (current-module) 'env-module)
(define (env-module env)
  (if (module? env)
      env
      (env-module (env-outer env))))

(module-export (current-module) 'env-assigned-variable?)
(define (env-assigned-variable? env idx)
  (unless (env-variable-layer? env)
    (error "failed to access compiler environment"))
  (vector-ref (vector-ref (car env) 2) idx))

(module-export (current-module) 'env-set-assigned!)
(define (env-set-assigned! env idx)
  (unless (env-variable-layer? env)
    (error "failed to access compiler environment"))
  (vector-set! (vector-ref (car env) 2) idx #t))

(module-export (current-module) 'env-syntax)
(define (env-syntax env idx)
  (unless (env-keyword-layer? env)
    (error "failed to access compiler environment"))
  (let ((syx (vector-ref (vector-ref (car env) 2) idx)))
    (unless syx
      (error "reference to uninitialized keyword"))
    syx))

(module-export (current-module) 'env-set-syntax!)
(define (env-set-syntax! env idx syx)
  (unless (env-keyword-layer? env)
    (error "failed to access compiler envrionment"))
  (vector-set! (vector-ref (car env) 2) idx syx))

(module-export (current-module) 'env-var-idx)
(define (env-var-idx env var)
  (let* ((vars (vector-ref (car env) 1))
         (len (vector-length vars)))
    (let rec ((idx 0))
      (if (>= idx len)
          #f
          (if (eq? var (vector-ref vars idx))
              idx
              (rec (+ idx 1)))))))

(module-export (current-module) 'env-copy-keyword)
(define (env-copy-keyword env)
  (cond ((env-outmost? env)
         env)
        ((env-keyword-layer? env)
         (cons (car env) (env-copy-keyword (env-outer env))))
        (else
         (env-copy-keyword (env-outer env)))))

(define (env-search-internal env ident limit layer vlayer find-proc)
  (if (or (env-outmost? env) (eq? env limit))
      (values ident #f layer vlayer env)
      (let ((idx (find-proc env ident)))
        (if idx
            (values ident idx layer vlayer env)
            (env-search-internal (env-outer env) ident limit
                                 (+ layer 1)
                                 (if (env-variable-layer? env)
                                     (+ vlayer 1)
                                     vlayer)
                                 find-proc)))))

(define (env-search env ident find-proc)
  (let ((ienv (if (identifier? ident) (identifier-env ident) #f)))
    (let-values (((v i l y e) (env-search-internal env ident ienv
                                                   0 0 find-proc)))
      (if (identifier? v)
          (cond ((env-outmost? e)
                 (values (identifier-name v) #f l y (env-module ienv)))
                ((eq? e ienv)
                 (env-search-internal ienv (identifier-name v) #f l y find-proc))
                (else
                 (values v i l y e)))
          (values v i l y e)))))

(define (env-rvr-find-proc env ident)
  (if (env-keyword-layer? env)
      #f
      (env-var-idx env ident)))

(module-export (current-module) 'env-resolve-variable-reference!)
(define (env-resolve-variable-reference! env ident assigned rdepth)
  (let-values (((v i l y e) (env-search env ident env-rvr-find-proc)))
    (unless (env-outmost? e)
      (when rdepth (rdepth-set! rdepth y))
      (when assigned (env-set-assigned! e i)))
    (values v i y e)))

(module-export (current-module) 'env-find-keyword)
(define (env-find-keyword env ident)
  (let-values (((v i l y e) (env-search env ident env-var-idx)))
    (cond ((env-outmost? e)
           (global-syntax-ref e v #f))
          ((env-keyword-layer? e)
           (env-syntax e i))
          (else
           #f))))

(module-export (current-module) 'env-find-identifier)
(define (env-find-identifier env ident)
  (let-values (((v i l y e) (env-search env ident env-var-idx)))
    (values v i l e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define precompile? (make-parameter #f))

(define (compile-error cmpl msg)
  (error (format "failed to compile: ~a" msg) (compiler-current-expr cmpl)))

(define (get-compiler-from-arg arg)
  (cond
   ((compiler? arg) arg)
   (else (make-compiler arg))))

(define (compile-exp cmpl exp env toplevel-p rdepth arity tail-p asmb)
  (p2-compile-exp cmpl
                  (p1-compile-exp cmpl exp env toplevel-p rdepth)
                  arity tail-p asmb))


(define (compile-internal exp arg precompile-p)
  (let ((cmpl (get-compiler-from-arg arg))
        (asmb (make-assembler)))
    (let ((env (compiler-base-env cmpl)))
      (parameterize ((precompile? precompile-p))
        (compile-exp cmpl exp env (env-outmost? env) (new-rdepth) -1 #f asmb)))
    (assembler-commit! asmb)
    asmb))

(module-export (current-module) 'compile)
(define (compile exp arg)
  (compile-internal exp arg #f))

(define (precompile exp arg)
  (compile-internal exp arg #t))

(define (compile-file file arg)
  (let ((cmpl (get-compiler-from-arg arg))
        (port (open-input-file file))
        (asmb (make-assembler)))
    (parameterize ((precompile? #t))
      (let loop ((exp (read port)))
        (unless (eof-object? exp)
          (compile-exp cmpl exp (compiler-base-env cmpl) #t (new-rdepth)
                       -1 #f asmb)
          (loop (read port)))))
    (assembler-commit! asmb)
    asmb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export (current-module) 'p2-syntax-id-lref)
(module-export (current-module) 'p2-syntax-id-gref)
(module-export (current-module) 'p2-syntax-id-self)
(module-export (current-module) 'p2-syntax-id-call)
(module-export (current-module) 'p2-syntax-id-gdef)
(module-export (current-module) 'p2-syntax-id-begin)
(module-export (current-module) 'p2-syntax-id-body)
(module-export (current-module) 'p2-syntax-id-lambda)
(module-export (current-module) 'p2-syntax-id-lset)
(module-export (current-module) 'p2-syntax-id-gset)
(module-export (current-module) 'p2-syntax-id-if)
(module-export (current-module) 'p2-syntax-id-cond)
(module-export (current-module) 'p2-syntax-id-and)
(module-export (current-module) 'p2-syntax-id-or)
(module-export (current-module) 'p2-syntax-id-let)
(module-export (current-module) 'p2-syntax-id-letrec)
(module-export (current-module) 'p2-syntax-id-letrec*)
(module-export (current-module) 'p2-syntax-id-do)
(module-export (current-module) 'p2-syntax-id-let-values)

(define p2-syntax-id-lref 0)
(define p2-syntax-id-gref 1)
(define p2-syntax-id-self 2)
(define p2-syntax-id-call 3)
(define p2-syntax-id-gdef 4)
(define p2-syntax-id-begin 5)
(define p2-syntax-id-body 6)
(define p2-syntax-id-lambda 7)
(define p2-syntax-id-lset 8)
(define p2-syntax-id-gset 9)
(define p2-syntax-id-if 10)
(define p2-syntax-id-cond 11)
(define p2-syntax-id-and 12)
(define p2-syntax-id-or 13)
(define p2-syntax-id-let 14)
(define p2-syntax-id-letrec 15)
(define p2-syntax-id-letrec* 16)
(define p2-syntax-id-do 17)
(define p2-syntax-id-let-values 18)

(define p2-nr-syntax-id 19)

(define p2-syntax-handler-table (make-vector p2-nr-syntax-id))

(define (p2-register-syntax id handler)
  (vector-set! p2-syntax-handler-table id handler))

(define (p2-get-syntax exp)
  (vector-ref p2-syntax-handler-table (vector-ref exp 0)))

(define (p2-compile-exp cmpl exp arity tail-p asmb)
  (let ((ce (compiler-current-expr cmpl)))
    (compiler-select-expr! cmpl exp)
    ((p2-get-syntax exp) cmpl exp arity tail-p asmb)
    (compiler-select-expr! cmpl ce)))

;;; #(lref <symbol> <idx> <layer>)
;;;    <idx>   : <integer>
;;;    <layer> : <integer>
(define (p2-syntax-handler-lref cmpl exp arity tail-p asmb)
  (push-inst-sref (vector-ref exp 2) (vector-ref exp 3) asmb)
  (when tail-p (push-inst-return asmb)))

;;; #(gref <symbol> <module>)
;;;    <module> : <list of symbols>
(define (p2-syntax-handler-gref cmpl exp arity tail-p asmb)
  (push-inst-gref (vector-ref exp 1) (vector-ref exp 2) asmb)
  (when tail-p (push-inst-return asmb)))

;;; #(self <object>)
(define (p2-syntax-handler-self cmpl exp arity tail-p asmb)
  (let ((o (vector-ref exp 1)))
    (if (module? o)
        (push-inst-module o asmb)
        (push-inst-immval o asmb)))
  (when tail-p (push-inst-return asmb)))

;;; #(call <procedure> <arg> ...)
;;;    <procedure> : <expr>
;;;    <arg>       : <expr>
(define (p2-syntax-handler-call cmpl exp arity tail-p asmb)
  (let ((len (vector-length exp))
        (lbl #f))
    (unless tail-p
      (set! lbl (assembler-assign-label-id! asmb))
      (push-inst-cframe 'label lbl asmb))
    (let loop ((idx 2))
      (when (< idx len)
        (p2-compile-exp cmpl (vector-ref exp idx) 1 #f asmb)
        (push-inst-push asmb)
        (loop (+ idx 1))))
    (p2-compile-exp cmpl (vector-ref exp 1) 1 #f asmb)
    (if tail-p
        (push-inst-tcall (- len 2) asmb)
        (push-inst-call (- len 2) arity lbl asmb))))

;;; #(gdef <symbol> <module> <expr>)
;;;    <module> : <list of symbols>
(define (p2-syntax-handler-gdef cmpl exp arity tail-p asmb)
  (p2-compile-exp cmpl (vector-ref exp 3) 1 #f asmb)
  (push-inst-gdef (vector-ref exp 1) (vector-ref exp 2) asmb)
  (when tail-p (push-inst-return asmb)))

;;; #(begin <expr> ...)
(define (p2-syntax-handler-begin cmpl exp arity tail-p asmb)
  (let ((nr (- (vector-length exp) 1)))
    (let loop ((idx 1))
      (when (< idx nr)
        (p2-compile-exp cmpl (vector-ref exp idx) -1 #f asmb)
        (loop (+ idx 1))))
    (if (= nr 0)
        (begin (push-inst-undef asmb)
               (when tail-p (push-inst-return asmb)))
        (p2-compile-exp cmpl (vector-ref exp nr) arity tail-p asmb))))

;;; #(body <vars> <inits> <expr>)
;;;    <vars>  : #(<symbol> ...)
;;;    <inits> : #(<expr> ...)
(define (p2-syntax-handler-body cmpl exp arity tail-p asmb)
  (let ((vars (vector-ref exp 1))
        (inits (vector-ref exp 2)))
    (let ((len (vector-length vars)))
      (when (> len 0)
        (push-inst-emine len asmb))
      (let loop ((idx 0))
        (when (< idx len)
          (p2-compile-exp cmpl (vector-ref inits idx) 1 #f asmb)
          (push-inst-demine idx 0 asmb)
          (loop (+ idx 1))))
      (p2-compile-exp cmpl (vector-ref exp 3) arity tail-p asmb)
      (when (and (not tail-p) (> len 0))
        (push-inst-epop asmb)))))

;;; #(lambda <params> <vp> <env> <body>)
;;;    <params> : #((<symbol> . <flg>) ...)
;;;    <vp>     : <boolean>
;;;    <env>    : <integer>
;;;    <body>   : <expr>
(define (p2-syntax-handler-lambda cmpl exp arity tail-p asmb)
  (let* ((params (vector-ref exp 1))
         (len (vector-length params))
         (basmb (make-assembler)))
    (let loop ((idx 0))
      (when (< idx len)
        (when (cdr (vector-ref params idx))
          (push-inst-box idx 0 basmb))
        (loop (+ idx 1))))
    (p2-compile-exp cmpl (vector-ref exp 4) -1 #t basmb)
    (assembler-commit! basmb)
    (push-inst-close (vector-ref exp 3)
                     (if (vector-ref exp 2) (- len) len)
                     basmb
                     asmb)
    (when tail-p
      (push-inst-return asmb))))

;;; #(lset <symbol> <idx> <layer> <expr>)
;;;    <idx>   : <integer>
;;;    <layer> : <integer>
(define (p2-syntax-handler-lset cmpl exp arity tail-p asmb)
  (p2-compile-exp cmpl (vector-ref exp 4) 1 #f asmb)
  (push-inst-sset (vector-ref exp 2) (vector-ref exp 3) asmb)
  (when tail-p
    (push-inst-return asmb)))

;;; #(gset <symbol> <module> <expr>)
;;;    <module> : <list of symbols>
(define (p2-syntax-handler-gset cmpl exp arity tail-p asmb)
  (p2-compile-exp cmpl (vector-ref exp 3) 1 #f asmb)
  (push-inst-gset (vector-ref exp 1) (vector-ref exp 2) asmb)
  (when tail-p
    (push-inst-return asmb)))

;;; #(if <condi> <conse> <alter>)
;;;    <condi> : <expr>
;;;    <conse> : <expr>
;;;    <alter> : <expr>
(define (p2-syntax-handler-if cmpl exp arity tail-p asmb)
  (let ((lbl-j (if tail-p #f (assembler-assign-label-id! asmb)))
        (lbl-a (assembler-assign-label-id! asmb)))
    (p2-compile-exp cmpl (vector-ref exp 1) 1 #f asmb)
    (push-inst-jmpf 'label lbl-a asmb)
    (p2-compile-exp cmpl (vector-ref exp 2) arity tail-p asmb)
    (unless tail-p
      (push-inst-jmp 'label lbl-j asmb))
    (push-inst-label lbl-a asmb)
    (p2-compile-exp cmpl (vector-ref exp 3) arity tail-p asmb)
    (unless tail-p
      (push-inst-label lbl-j asmb))))

;;; #(cond <clauses>)
;;;    <clauses> : #(<clause> ... <else>)
;;;    <clause>  : #(<test> >> <expr>)
;;;                | #(<test> => <expr>)
;;;                | #(<test> <>)
;;;    <else>    : #(else -- <expr>)
(define (p2-syntax-handler-cond cmpl exp arity tail-p asmb)
  (let* ((clauses (vector-ref exp 1))
         (nr-clauses (vector-length clauses))
         (nr-tests (- nr-clauses 1))
         (lbl-c (make-vector nr-clauses #f))
         (lbl-j (if tail-p #f (assembler-assign-label-id! asmb))))
    (let loop ((idx 0))
      (when (< idx nr-tests)
        (let ((cls (vector-ref clauses idx))
              (lbl lbl-j))
          (p2-compile-exp cmpl (vector-ref cls 0) 1 #f asmb)
          (when (or (not (eq? (vector-ref cls 1) '<>)) tail-p)
            (set! lbl (assembler-assign-label-id! asmb))
            (vector-set! lbl-c idx lbl))
          (push-inst-jmpt 'label lbl asmb))
        (loop (+ idx 1))))
    (let loop ((idx (- nr-clauses 1)) (lbl #f))
      (when (>= idx 0)
        (let* ((cls (vector-ref clauses idx))
               (typ (vector-ref cls 1)))
          (cond ((eq? typ '>>)
                 (when lbl (push-inst-jmp 'label lbl asmb))
                 (push-inst-label (vector-ref lbl-c idx) asmb)
                 (p2-compile-exp cmpl (vector-ref cls 2) arity tail-p asmb)
                 (loop (- idx 1) lbl-j))

                ((eq? typ '=>)
                 (let ((lbl-call #f))
                   (when lbl (push-inst-jmp 'label lbl asmb))
                   (push-inst-label (vector-ref lbl-c idx) asmb)
                   (unless tail-p
                     (set! lbl-call (assembler-assign-label-id! asmb))
                     (push-inst-cframe 'label lbl-call asmb))
                   (push-inst-push asmb)
                   (p2-compile-exp cmpl (vector-ref cls 2) 1 #f asmb)
                   (if tail-p
                       (begin
                         (push-inst-tcall 1 asmb)
                         (loop (- idx 1) #f))
                       (begin
                         (push-inst-call 1 arity lbl-call asmb)
                         (loop (- idx 1) lbl-j)))))

                ((eq? typ '<>)
                 (when tail-p
                   (push-inst-label (vector-ref lbl-c idx) asmb)
                   (push-inst-return asmb))
                 (loop (- idx 1) lbl))

                ((eq? typ '--)
                 (when lbl (push-inst-jmp 'label lbl asmb))
                 (p2-compile-exp cmpl (vector-ref cls 2) arity tail-p asmb)
                 (loop (- idx 1) lbl-j))))))
    (when lbl-j
      (push-inst-label lbl-j asmb))))

;;; #(and <expr> ...)
(define (p2-syntax-handler-and cmpl exp arity tail-p asmb)
  (let ((nr-tests (- (vector-length exp) 1))
        (lbl-j (assembler-assign-label-id! asmb)))
    (cond ((= nr-tests 0)
           (push-inst-immval #t asmb)
           (when tail-p
             (push-inst-return asmb)))
          (else
           (let loop ((idx 1))
             (when (< idx nr-tests)
               (p2-compile-exp cmpl (vector-ref exp idx) 1 #f asmb)
               (push-inst-jmpf 'label lbl-j asmb)
               (loop (+ idx 1))))
           (p2-compile-exp cmpl (vector-ref exp nr-tests) 1 tail-p asmb)
           (when (> nr-tests 1)
             (push-inst-label lbl-j asmb)
             (when tail-p
               (push-inst-return asmb)))))))

;;; #(or <expr> ...)
(define (p2-syntax-handler-or cmpl exp arity tail-p asmb)
  (let ((nr-tests (- (vector-length exp) 1))
        (lbl-j (assembler-assign-label-id! asmb)))
    (cond ((= nr-tests 0)
           (push-inst-immval #f asmb)
           (when tail-p
             (push-inst-return asmb)))
          (else
           (let loop ((idx 1))
             (when (< idx nr-tests)
               (p2-compile-exp cmpl (vector-ref exp idx) 1 #f asmb)
               (push-inst-jmpt 'label lbl-j asmb)
               (loop (+ idx 1))))
           (p2-compile-exp cmpl (vector-ref exp nr-tests) 1 tail-p asmb)
           (when (> nr-tests 1)
             (push-inst-label lbl-j asmb)
             (when tail-p
               (push-inst-return asmb)))))))

;;; #(let <vars> <inits> <body>)
;;;    <vars>  : #((<symbol> . <flg>) ...)
;;;    <inits> : #(<expr> ...)
;;;    <body>  : <expr>
(define (p2-syntax-handler-let cmpl exp arity tail-p asmb)
  (let ((vars (vector-ref exp 1))
        (inits (vector-ref exp 2)))
    (let ((nr-vars (vector-length vars)))
      (when (> nr-vars 0)
        (let loop ((idx 0))
          (when (< idx nr-vars)
            (p2-compile-exp cmpl (vector-ref inits idx) 1 #f asmb)
            (push-inst-push asmb)
            (loop (+ idx 1))))
        (push-inst-eframe nr-vars asmb)
        (let loop ((idx 0))
          (when (< idx nr-vars)
            (when (cdr (vector-ref vars idx))
              (push-inst-box idx 0 asmb))
            (loop (+ idx 1)))))
      (p2-compile-exp cmpl (vector-ref exp 3) arity tail-p asmb)
      (when (and (> nr-vars 0) (not tail-p))
        (push-inst-epop asmb)))))

;;; #(letrec <vars> <inits> <body>)
;;;    <vars>  : #((<symbol> . <flg>) ...)
;;;    <inits> : #(<expr> ...)
;;;    <body>  : <expr>
(define (p2-syntax-handler-letrec cmpl exp arity tail-p asmb)
  (let ((vars (vector-ref exp 1))
        (inits (vector-ref exp 2)))
    (let ((nr-vars (vector-length vars)))
      (when (> nr-vars 0)
        (push-inst-emine nr-vars asmb)
        (let loop ((idx 0))
          (when (< idx nr-vars)
            (p2-compile-exp cmpl (vector-ref inits idx) 1 #f asmb)
            (push-inst-push asmb)
            (loop (+ idx 1))))
        (push-inst-edemine nr-vars 0 asmb))
      (p2-compile-exp cmpl (vector-ref exp 3) arity tail-p asmb)
      (when (and (> nr-vars 0) (not tail-p))
        (push-inst-epop asmb)))))

;;; #(letrec* <vars> <inits> <body>)
;;;    <vars>  : #((<symbol> . <flg>) ...)
;;;    <inits> : #(<expr> ...)
;;;    <body>  : <expr>
(define (p2-syntax-handler-letrec* cmpl exp arity tail-p asmb)
  (let ((vars (vector-ref exp 1))
        (inits (vector-ref exp 2)))
    (let ((nr-vars (vector-length vars)))
      (when (> nr-vars 0)
        (push-inst-emine nr-vars asmb)
        (let loop ((idx 0))
          (when (< idx nr-vars)
            (p2-compile-exp cmpl (vector-ref inits idx) 1 #f asmb)
            (push-inst-demine idx 0 asmb)
            (loop (+ idx 1)))))
      (p2-compile-exp cmpl (vector-ref exp 3) arity tail-p asmb)
      (when (and (> nr-vars 0) (not tail-p))
        (push-inst-epop asmb)))))

;;; #(do <vars> <inits> <steps> <test> <expr> <cmds>)
;;;    <vars> : #((<symbol> . <flg>) ...)
;;;    <inits> : #(<expr> ...)
;;;    <steps> : #(<expr> ...)
;;;    <test>  : <expr>
;;;    <cmds>  : <expr> | '()
(define (p2-syntax-handler-do cmpl exp arity tail-p asmb)
  (let ((vars (vector-ref exp 1))
        (inits (vector-ref exp 2))
        (steps (vector-ref exp 3))
        (cmds (vector-ref exp 6)))
    (let ((nr-vars (vector-length vars))
          (lbl-e (assembler-assign-label-id! asmb))
          (lbl-s (assembler-assign-label-id! asmb)))
      (when (> nr-vars 0)
        (let loop ((idx 0))
          (when (< idx nr-vars)
            (p2-compile-exp cmpl (vector-ref inits idx) 1 #f asmb)
            (push-inst-push asmb)
            (loop (+ idx 1))))
        (push-inst-eframe nr-vars asmb))
      (push-inst-label lbl-s asmb)
      (let loop ((idx 0))
        (when (< idx nr-vars)
          (when (cdr (vector-ref vars idx))
            (push-inst-box idx 0 asmb))
          (loop (+ idx 1))))
      (p2-compile-exp cmpl (vector-ref exp 4) 1 #f asmb)
      (push-inst-jmpt 'label lbl-e asmb)
      (unless (null? cmds)
        (p2-compile-exp cmpl cmds -1 #f asmb))
      (when (> nr-vars 0)
        (let loop ((idx 0))
          (when (< idx nr-vars)
            (p2-compile-exp cmpl (vector-ref steps idx) 1 #f asmb)
            (push-inst-push asmb)
            (loop (+ idx 1))))
        (push-inst-eframe nr-vars asmb)
        (push-inst-eshift 1 asmb))
      (push-inst-jmp 'label lbl-s asmb)
      (push-inst-label lbl-e asmb)
      (p2-compile-exp cmpl (vector-ref exp 5) arity tail-p asmb)
      (when (and (> nr-vars 0) (not tail-p))
        (push-inst-epop asmb)))))

;;; #(let-values <mvars> <vvar> <inits> <expr> <nr>)
;;;    <mvars> : #((<symbol> . <flg>) ...)  ...)
;;;    <vvar>  : #(<boolean> ...)
;;;    <inits> : #(<expr> ...)
;;;    <nr>    : <integer>
(define (p2-syntax-handler-let-values cmpl exp arity tail-p asmb)
  (let ((mvars (vector-ref exp 1))
        (vvar (vector-ref exp 2))
        (inits (vector-ref exp 3))
        (nr-vars (vector-ref exp 5)))
    (let ((nr-mbs (vector-length mvars)))
      (let loop ((idx 0))
        (when (< idx nr-mbs)
          (let* ((vars (vector-ref mvars idx))
                 (vv (vector-ref vvar idx))
                 (n (vector-length vars))
                 (a (if vv (- n) n)))
            (p2-compile-exp cmpl (vector-ref inits idx) a #f asmb)
            (unless (= a 1)
              (push-inst-mrvc a asmb))
            (when (> n 0)
              (push-inst-mvpush asmb)))
          (loop (+ idx 1))))
      (when (> nr-vars 0)
        (push-inst-eframe nr-vars asmb))
      (let loop ((idx 0) (cnt 0))
        (when (< idx nr-mbs)
          (let* ((vars (vector-ref mvars idx))
                 (n (vector-length vars)))
            (let loop2 ((jdx 0))
              (when (< jdx n)
                (when (cdr (vector-ref vars jdx))
                  (push-inst-box cnt 0 asmb))
                (set! cnt (+ cnt 1))
                (loop2 (+ jdx 1)))))
          (loop (+ idx 1) cnt)))
      (p2-compile-exp cmpl (vector-ref exp 4) arity tail-p asmb)
      (when (and (> nr-vars 0) (not tail-p))
        (push-inst-epop asmb)))))

(p2-register-syntax p2-syntax-id-lref p2-syntax-handler-lref)
(p2-register-syntax p2-syntax-id-gref p2-syntax-handler-gref)
(p2-register-syntax p2-syntax-id-self p2-syntax-handler-self)
(p2-register-syntax p2-syntax-id-call p2-syntax-handler-call)
(p2-register-syntax p2-syntax-id-gdef p2-syntax-handler-gdef)
(p2-register-syntax p2-syntax-id-begin p2-syntax-handler-begin)
(p2-register-syntax p2-syntax-id-body p2-syntax-handler-body)
(p2-register-syntax p2-syntax-id-lambda p2-syntax-handler-lambda)
(p2-register-syntax p2-syntax-id-lset p2-syntax-handler-lset)
(p2-register-syntax p2-syntax-id-gset p2-syntax-handler-gset)
(p2-register-syntax p2-syntax-id-if p2-syntax-handler-if)
(p2-register-syntax p2-syntax-id-cond p2-syntax-handler-cond)
(p2-register-syntax p2-syntax-id-and p2-syntax-handler-and)
(p2-register-syntax p2-syntax-id-or p2-syntax-handler-or)
(p2-register-syntax p2-syntax-id-let p2-syntax-handler-let)
(p2-register-syntax p2-syntax-id-letrec p2-syntax-handler-letrec)
(p2-register-syntax p2-syntax-id-letrec* p2-syntax-handler-letrec*)
(p2-register-syntax p2-syntax-id-do p2-syntax-handler-do)
(p2-register-syntax p2-syntax-id-let-values p2-syntax-handler-let-values)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (p1-syntax-handler-reference cmpl exp env toplevel-p rdepth)
  (let-values (((v i l e) (env-resolve-variable-reference! env exp #f rdepth)))
    (if (env-outmost? e)
        (vector p2-syntax-id-gref v e)
        (vector p2-syntax-id-lref v i l))))

(define (p1-syntax-handler-self-eval cmpl exp env toplevel-p rdepth)
  (vector p2-syntax-id-self exp))


(define (p1-syntax-handler-application cmpl exp env toplevel-p rdepth)
  (list->vector (cons p2-syntax-id-call
                      (map (lambda (e)
                             (p1-compile-exp cmpl e env toplevel-p rdepth))
                           exp))))


(define p1-compiler-syntax-reference
  (make-syntax 'reference p1-syntax-handler-reference))

(define p1-compiler-syntax-self-eval
  (make-syntax 'self-eval p1-syntax-handler-self-eval))

(define p1-compiler-syntax-application
  (make-syntax 'application p1-syntax-handler-application))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (p1-register-syntax mod syx exp)
  (global-syntax-bind mod (syntax-keyword syx) syx exp))

(define (p1-search-syntax sym)
  (let loop ((lst syntaxes))
    (if (null? lst)
        #f
        (if (eq? sym (syntax-keyword (car lst)))
            (car lst)
            (loop (cdr lst))))))

(define (p1-get-syntax cmpl exp env toplevel-p)
  (cond ((or (symbol? exp) (identifier? exp))
         p1-compiler-syntax-reference)
        ((pair? exp)
         (let ((key (car exp)))
           (if (or (symbol? key) (identifier? key))
               (let ((syx (env-find-keyword env key)))
                 (if syx syx p1-compiler-syntax-application))
               p1-compiler-syntax-application)))
        (else
         p1-compiler-syntax-self-eval)))

(define (p1-compile-exp cmpl exp env toplevel-p rdepth)
  (let ((ce (compiler-current-expr cmpl)))
    (compiler-select-expr! cmpl exp)
    (rdepth-expand! rdepth)
    (let* ((syx (p1-get-syntax cmpl exp env toplevel-p))
           (x (if (macro? syx)
                  (p1-compile-exp cmpl (macro-exec-expansion syx exp env)
                                  env toplevel-p rdepth)
                  ((syntax-handler syx) cmpl exp env toplevel-p rdepth))))
      (rdepth-contract! rdepth)
      (compiler-select-expr! cmpl ce)
      x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (p1-normalize-definition cmpl exp)
  (let ((key (car exp))
        (rest (cdr exp)))
    (unless (pair? rest)
      (compile-error cmpl "malformed define"))
    (let loop ((x (car rest))
               (y (cdr rest)))
      (if (pair? x)
          (let ((name (car x))
                (formals (cdr x)))
            (loop name (cons (cons 'lambda (cons formals y)) '())))
          (cons key (cons x y))))))

(define (p1-decons-definition cmpl exp)
  (unless (= (length exp) 3)
    (compile-error cmpl "malformed define"))
  (let ((var (car (cdr exp)))
        (val (car (cdr (cdr exp)))))
    (unless (or (symbol? var) (identifier? var))
      (compile-error cmpl "malformed define"))
    (values var val)))

(define (p1-syntax-handler-definition cmpl exp env toplevel-p rdepth)
  (unless toplevel-p
    (compile-error cmpl "definition can apper in the toplevel or beginning of a <body>"))
  (let-values (((var val)
                (p1-decons-definition cmpl
                                      (p1-normalize-definition cmpl exp))))
    (let-values (((v i l e) (env-resolve-variable-reference! env var #f #f)))
      (unless (env-outmost? e)
        (compile-error cmpl "malformed define"))
      (vector p2-syntax-id-gdef v e
              (p1-compile-exp cmpl val env toplevel-p rdepth)))))

(define (p1-syntax-handler-begin cmpl exp env toplevel-p rdepth)
  (list->vector (cons p2-syntax-id-begin
                      (map (lambda (e)
                             (p1-compile-exp cmpl e env toplevel-p rdepth))
                           (cdr exp)))))

(define (p1-decons-body-internal cmpl body env toplevel-p vars inits exps)
  (cond ((null? body)
         (values vars inits exps))
        ((not (pair? body))
         (compile-error cmpl "malformed <body>"))
        (else
         (let* ((exp (car body))
                (syx (p1-get-syntax cmpl exp env toplevel-p)))
           (cond
            ((macro? syx)
             (p1-decons-body-internal cmpl
                                      (cons (macro-exec-expansion syx exp env)
                                            (cdr body))
                                      env toplevel-p vars inits exps))
            ((eq? syx compiler-syntax-definition)
             (let-values (((v i) (p1-decons-definition cmpl exp)))
               (p1-decons-body-internal cmpl (cdr body) env toplevel-p
                                        (cons v vars) (cons i inits) exps)))
            ((eq? syx compiler-syntax-begin)
             (let-values (((v i e) (p1-decons-body-internal cmpl (cdr exp) env
                                                            toplevel-p
                                                            vars inits exps)))
               (if (eq? e exps)
                   (p1-decons-body-internal cmpl (cdr body) env toplevel-p
                                            v i exps)
                   (values v i (if (null? (cdr body))
                                   e
                                   (cons (cdr body) e))))))
            (else
             (values vars inits (cons body exps))))))))


(define (p1-decons-body cmpl body env toplevel-p)
  (let-values (((v i e) (p1-decons-body-internal cmpl body env toplevel-p
                                            '() '() '())))
    (values (reverse v)
            (reverse i)
            (if (null? e) '(()) (reverse e)))))

(define (p1-cmpl-body cmpl body env toplevel-p rdepth)
  (let-values (((vars inits exps) (p1-decons-body cmpl body env toplevel-p)))
    (let* ((vv (list->vector vars))
           (env (if (null? vars)
                    env
                    (env-extend vv #f env)))
           (vi (list->vector (map
                              (lambda (e)
                                (p1-compile-exp cmpl e env toplevel-p rdepth))
                              inits)))
           (ve (list->vector (cons p2-syntax-id-begin
                                   (map(lambda (e)
                                         (p1-compile-exp cmpl e env
                                                         toplevel-p rdepth))
                                       (let rec ((exps exps))
                                         (if (null? exps) '()
                                             (append (car exps)
                                                     (rec (cdr exps)))))))))
           (rslt (vector p2-syntax-id-body vv vi ve)))
      (unless (null? vars)
        (rdepth-add! rdepth -1))
      rslt)))

(define (p1-decons-quote cmpl exp)
  (let ((lst (cdr exp)))
    (unless (pair? lst)
      (compile-error cmpl "malformed quote"))
    (unless (null? (cdr lst))
      (error "faild to compile: malformed quote" exp))
    (car lst)))

(define (p1-syntax-handler-quote cmpl exp env toplevel-p rdepth)
  (vector p2-syntax-id-self (p1-decons-quote cmpl exp)))

(define (p1-decons-formals cmpl formals)
  (let loop ((fm formals)
             (params '())
             (vparam #f))
    (cond
     ((symbol? fm)
      (when (memq fm params)
        (compile-error cmpl "malformed formals"))
      (loop '() (cons fm params) #t))
     ((pair? fm)
      (let ((f (car fm)))
        (when (or (not (symbol? f)) (memq f params))
          (compile-error cmpl "malformed formals"))
        (loop (cdr fm) (cons f params) vparam)))
     ((null? fm)
      (values (reverse params) vparam))
     (else
      (compile-error cmpl "malformed formals")))))

(define (p1-decons-lambda cmpl exp)
  (unless (pair? (cdr exp))
    (compile-error cmpl "malformed lambda"))
  (let-values (((params vparam) (p1-decons-formals cmpl (car (cdr exp)))))
    (values params vparam (cdr (cdr exp)))))

(define (p1-insert-assign-flg params env)
  (let rec ((idx 0) (par params))
    (if (null? par)
        '()
        (cons (cons (car par)
                    (env-assigned-variable? env idx))
              (rec (+ idx 1) (cdr par))))))

(define (p1-cmpl-lambda cmpl params vparam body env toplevel-p rdepth)
  (rdepth-expand! rdepth)
  (let* ((env (if (null? params)
                  env
                  (env-extend (list->vector params) vparam env)))
         (bo (p1-cmpl-body cmpl body env #f rdepth)))
    (unless (null? params)
      (rdepth-add! rdepth -1))
    (let ((rd (rdepth-ref rdepth)))
      (rdepth-contract! rdepth)
      (vector p2-syntax-id-lambda
              (list->vector (p1-insert-assign-flg params env))
              vparam
              (if (>= rd 0) (+ rd 1) 0)
              bo))))

(define (p1-syntax-handler-lambda cmpl exp env toplevel-p rdepth)
  (let-values (((params vparam body) (p1-decons-lambda cmpl exp)))
    (p1-cmpl-lambda cmpl params vparam body env toplevel-p rdepth)))

(define (p1-decons-assignment cmpl exp)
  (unless (list? exp)
    (compile-error cmpl "malformed if"))
  (unless (= (length exp) 3)
    (compile-error cmpl "malformed assignment"))
  (let ((var (list-ref exp 1))
        (val (list-ref exp 2)))
    (unless (or (symbol? var) (identifier? var))
      (compile-error cmpl "malformed assignment"))
    (values var val)))

(define (p1-syntax-handler-assignment cmpl exp env toplevel-p rdepth)
  (let-values (((var val) (p1-decons-assignment cmpl exp)))
    (let-values (((v i l e) (env-resolve-variable-reference! env var #t rdepth)))
      (let ((x (p1-compile-exp cmpl val env toplevel-p rdepth)))
        (if (env-outmost? e)
            (vector p2-syntax-id-gset v e x)
            (vector p2-syntax-id-lset v i l x))))))

(define (p1-decons-if cmpl exp)
  (unless (list? exp)
    (compile-error cmpl "malformed if"))
  (let ((len (length exp)))
    (cond ((= len 3)
           (values (list-ref exp 1) (list-ref exp 2) '() #f))
          ((= len 4)
           (values (list-ref exp 1) (list-ref exp 2) (list-ref exp 3) #t))
          (else
           (compile-error cmpl "malformed if")))))

(define (p1-syntax-handler-if cmpl exp env toplevel-p rdepth)
  (let-values (((condi conse alter exist) (p1-decons-if cmpl exp)))
    (vector p2-syntax-id-if
            (p1-compile-exp cmpl condi env toplevel-p rdepth)
            (p1-compile-exp cmpl conse env toplevel-p rdepth)
            (if exist
                (p1-compile-exp cmpl alter env toplevel-p rdepth)
                (vector p2-syntax-id-begin)))))

(define (p1-decons-cond cmpl exp)
  (let loop ((clauses (cdr exp))
             (test-lst '())
             (type-lst '())
             (expr-lst '())
             (else-exist #f))
    (if (null? clauses)
        (begin (unless else-exist
                 (set! test-lst (cons 'else test-lst))
                 (set! type-lst (cons '-- type-lst))
                 (set! expr-lst (cons '() expr-lst)))
               (values test-lst type-lst expr-lst))
        (let* ((cls (car clauses))
               (tst (car cls))
               (rst (cdr cls)))
          (when else-exist
            (compile-error cmpl "malformed cond"))
          (cond ((eq? tst 'else)
                 (loop (cdr clauses)
                       (cons tst test-lst)
                       (cons '-- type-lst)
                       (cons rst expr-lst)
                       #t))
                ((null? rst)
                 (loop (cdr clauses)
                       (cons tst test-lst)
                       (cons '<> type-lst)
                       (cons '() expr-lst)
                       else-exist))
                ((pair? rst)
                 (if (eq? (car rst) '=>)
                     (begin
                       (unless (= (length rst) 2)
                         (compile-error cmpl "malformed cond"))
                       (loop (cdr clauses)
                             (cons tst test-lst)
                             (cons '=> type-lst)
                             (cons (car (cdr rst)) expr-lst)
                             else-exist))
                     (loop (cdr clauses)
                           (cons tst test-lst)
                           (cons '>> type-lst)
                           (cons rst expr-lst)
                           else-exist)))
                (else
                 (compile-error cmpl "malformed cond")))))))

(define (p1-syntax-handler-cond cmpl exp env toplevel-p rdepth)
  (let-values (((test-lst type-lst expr-lst) (p1-decons-cond cmpl exp)))
    (let loop ((test-lst test-lst) (type-lst type-lst) (expr-lst expr-lst)
               (rslt '()))
      (if (null? test-lst)
          (vector p2-syntax-id-cond (list->vector rslt))
          (let ((tst (car test-lst))
                (typ (car type-lst))
                (exp (car expr-lst)))
            (cond ((eq? typ '>>)
                   (loop (cdr test-lst) (cdr type-lst) (cdr expr-lst)
                         (let ((ct (p1-compile-exp cmpl tst env
                                                   toplevel-p rdepth))
                               (ce (list->vector
                                    (cons p2-syntax-id-begin
                                          (map (lambda (e)
                                                 (p1-compile-exp cmpl e env
                                                                 toplevel-p
                                                                 rdepth))
                                               exp)))))
                           (cons (vector ct '>> ce) rslt))))
                  ((eq? typ '=>)
                   (loop (cdr test-lst) (cdr type-lst) (cdr expr-lst)
                         (let ((ct (p1-compile-exp cmpl tst env
                                                   toplevel-p rdepth))
                               (ce (p1-compile-exp cmpl exp env
                                                   toplevel-p rdepth)))
                           (cons (vector ct '=> ce) rslt))))
                  ((eq? typ '<>)
                   (loop (cdr test-lst) (cdr type-lst) (cdr expr-lst)
                         (cons (vector (p1-compile-exp cmpl tst env
                                                       toplevel-p rdepth)
                                       '<>)
                               rslt)))
                  ((eq? typ '--)
                   (loop (cdr test-lst) (cdr type-lst) (cdr expr-lst)
                         (let ((ce (list->vector
                                    (cons p2-syntax-id-begin
                                          (map (lambda (e)
                                                 (p1-compile-exp cmpl e env
                                                                 toplevel-p
                                                                 rdepth))
                                               exp)))))
                           (cons (vector 'else '-- ce) rslt))))))))))

(define (p1-decons-and cmpl exp)
  (unless (list? exp)
    (compile-error cmpl "malformed and"))
  (cdr exp))

(define (p1-syntax-handler-and cmpl exp env toplevel-p rdepth)
  (let ((tests (p1-decons-and cmpl exp)))
    (list->vector (cons p2-syntax-id-and
                        (map (lambda (e)
                               (p1-compile-exp cmpl e env toplevel-p rdepth))
                             tests)))))

(define (p1-decons-or cmpl exp)
  (unless (list? exp)
    (compile-error cmpl "malformed or"))
  (cdr exp))

(define (p1-syntax-handler-or cmpl exp env toplevel-p rdepth)
  (let* ((tests (p1-decons-or cmpl exp)))
    (list->vector (cons p2-syntax-id-or
                        (map (lambda (e)
                               (p1-compile-exp cmpl e env toplevel-p rdepth))
                             tests)))))

(define (p1-decons-when cmpl exp)
  (let ((x (cdr exp)))
    (unless (pair? x)
      (compile-error cmpl "malformed when"))
    (values (car x) (cdr x))))

(define (p1-syntax-handler-when cmpl exp env toplevel-p rdepth)
  (let-values (((test exps) (p1-decons-when cmpl exp)))
    (vector p2-syntax-id-if
            (p1-compile-exp cmpl test env toplevel-p rdepth)
            (list->vector (cons p2-syntax-id-begin
                                (map (lambda (e)
                                       (p1-compile-exp cmpl e env
                                                       toplevel-p rdepth))
                                     exps)))
            (vector p2-syntax-id-begin))))

(define (p1-decons-unless cmpl exp)
  (let ((x (cdr exp)))
    (unless (pair? x)
      (compile-error cmpl "malformed unless"))
    (values (car x) (cdr x))))

(define (p1-syntax-handler-unless cmpl exp env toplevel-p rdepth)
  (let-values (((test exps) (p1-decons-unless cmpl exp)))
    (vector p2-syntax-id-if
            (p1-compile-exp cmpl test env toplevel-p rdepth)
            (vector p2-syntax-id-begin)
            (list->vector (cons p2-syntax-id-begin
                                (map (lambda (e)
                                       (p1-compile-exp cmpl e env
                                                       toplevel-p rdepth))
                                     exps))))))

(define (p1-decons-let-like-syntax cmpl exp name-p keyword)
  (let ((x (cdr exp))
        (name #f)
        (body #f))
    (unless (pair? x)
      (compile-error cmpl (format "malformed ~a" keyword)))
    (when (and name-p (symbol? (car x)))
      (set! name (car x))
      (set! x (cdr x))
      (unless (pair? x)
        (compile-error cmpl (format "malformed ~a" keyword))))
    (set! body (cdr x))
    (let loop ((binds (car x))
               (idents '())
               (inits '()))
      (if (null? binds)
          (values name (reverse idents) (reverse inits) body)
          (begin
            (unless (pair? binds)
              (compile-error cmpl (format "malformed ~a" keyword)))
            (let ((x (car binds)) (ide #f) (ini #f))
              (unless (pair? x)
                (compile-error cmpl (format "malformed ~a" keyword)))
              (set! ide (car x))
              (set! x (cdr x))
              (unless (pair? x)
                (compile-error cmpl (format "malformed ~a" keyword)))
              (set! ini (car x))
              (unless (null? (cdr x))
                (compile-error cmpl (format "malformed ~a" keyword)))
              (loop (cdr binds) (cons ide idents) (cons ini inits))))))))

(define (p1-decons-let cmpl exp)
  (p1-decons-let-like-syntax cmpl exp #t 'let))

;;; (let <name> ((<v> <i>) ...) <expr> ...)
;;; -> ((letrec ((<name> (lambda (<v> ...) <expr> ...)))
;;;     <i> ...))

(define (p1-cmpl-named-let cmpl name vars inits body env toplevel-p rdepth)
  (let* ((new-env (env-extend (vector name) #f env))
         (lmd (p1-cmpl-lambda cmpl vars #f body new-env #f rdepth)))
    (rdepth-add! rdepth -1)
    `#(,p2-syntax-id-call
       ,`#(,p2-syntax-id-letrec
           ,`#(,(cons name (env-assigned-variable? new-env 0)))
           ,`#(,lmd)
           ,`#(,p2-syntax-id-lref name 0 0))
       ,@(map (lambda (i) (p1-compile-exp cmpl i env #f rdepth))
              inits))))

(define (p1-cmpl-let cmpl vars inits body env toplevel-p rdepth)
  (let* ((new-env (if (null? vars) env (env-extend vars #f env)))
         (cb (p1-cmpl-body cmpl body new-env #f rdepth)))
    (unless (null? vars)
      (rdepth-add! rdepth -1))
    `#(,p2-syntax-id-let
       ,(list->vector (p1-insert-assign-flg vars new-env))
       ,(list->vector (map (lambda (i) (p1-compile-exp cmpl i env #f rdepth))
                           inits))
       ,cb)))

(define (p1-syntax-handler-let cmpl exp env toplevel-p rdepth)
  (let-values (((name vars inits body) (p1-decons-let cmpl exp)))
    (if name
        (p1-cmpl-named-let cmpl name vars inits body env toplevel-p rdepth)
        (p1-cmpl-let cmpl vars inits body env toplevel-p rdepth))))

(define (p1-decons-let* cmpl exp)
  (let-values (((name vars inits body)
                (p1-decons-let-like-syntax cmpl exp #f 'let*)))
    (values vars inits body)))

(define (p1-syntax-handler-let* cmpl exp env toplevel-p rdepth)
  (let-values (((vars inits body) (p1-decons-let* cmpl exp)))
    (let rec ((v vars) (i inits) (e env))
      (if (null? v)
          (p1-cmpl-body cmpl body e #f rdepth)
          (let* ((new-env (env-extend (vector (car v)) #f e))
                 (b (rec (cdr v) (cdr i) new-env)))
            (rdepth-add! rdepth -1)
            (vector p2-syntax-id-let
                    (vector (cons (car v) (env-assigned-variable? new-env 0)))
                    (vector (p1-compile-exp cmpl (car i) e #f rdepth))
                    b))))))


(define (p1-decons-letrec cmpl exp)
  (let-values (((name vars inits body)
                (p1-decons-let-like-syntax cmpl exp #f 'letrec)))
    (values vars inits body)))

(define (p1-syntax-handler-letrec cmpl exp env toplevel-p rdepth)
  (let-values (((vars inits body) (p1-decons-letrec cmpl exp)))
    (let* ((new-env (if (null? vars)
                        env
                        (env-extend (list->vector vars) #f env)))
           (cb (p1-cmpl-body cmpl body new-env #f rdepth))
           (ci (list->vector (map (lambda (e)
                                    (p1-compile-exp cmpl e new-env #f rdepth))
                                  inits))))
      (unless (null? vars)
        (rdepth-add! rdepth -1))
      (vector p2-syntax-id-letrec
              (list->vector (p1-insert-assign-flg vars new-env))
              ci
              cb))))

(define (p1-decons-letrec* cmpl exp)
  (let-values (((name vars inits body)
                (p1-decons-let-like-syntax cmpl exp #f 'letrec*)))
    (values vars inits body)))

(define (p1-syntax-handler-letrec* cmpl exp env toplevel-p rdepth)
  (let-values (((vars inits body) (p1-decons-letrec* cmpl exp)))
    (let* ((new-env (if (null? vars)
                        env
                        (env-extend (list->vector vars) #f env)))
           (cb (p1-cmpl-body cmpl body new-env #f rdepth))
           (ci (list->vector (map (lambda (e)
                                    (p1-compile-exp cmpl e new-env #f rdepth))
                                  inits))))
      (unless (null? vars)
        (rdepth-add! rdepth -1))
      (vector p2-syntax-id-letrec*
              (list->vector (p1-insert-assign-flg vars new-env))
              ci
              cb))))

(define (p1-decons-do cmpl exp)
  (let ((x (cdr exp))
        (var-cls #f)
        (tst-cls #f)
        (cmd-cls #f)
        (test #f)
        (exps #f)
        (vars #f)
        (inits #f)
        (steps #f))
    (unless (pair? x)
      (compiler-error cmpl "malformed do"))
    (set! var-cls (car x))
    (set! x (cdr x))
    (unless (pair? x)
      (compile-error cmpl "malformed do"))
    (set! tst-cls (car x))
    (set! cmd-cls (cdr x))
    (unless (pair? tst-cls)
      (compile-error cmpl "malformed do"))
    (set! test (car tst-cls))
    (set! exps (cdr tst-cls))
    (let loop ((vis var-cls)
               (vs '()) (is '()) (ss '()))
      (cond ((null? vis)
             (set! vars (reverse vs))
             (set! inits (reverse is))
             (set! steps (reverse ss)))
            ((pair? vis)
             (let ((x (car vis))
                   (v #f) (i #f) (s #f))
               (unless (pair? x)
                 (compile-error cmpl "malformed do"))
               (set! v (car x))
               (set! x (cdr x))
               (unless (pair? x)
                 (compile-error cmpl "malformed do"))
               (set! i (car x))
               (set! x (cdr x))
               (if (pair? x)
                   (begin
                     (set! s (car x))
                     (set! x (cdr x)))
                   (set! s v))
               (when (pair? x)
                 (compile-error cmpl "malformed do"))
               (loop (cdr vis) (cons v vs) (cons i is) (cons s ss))))
            (else
             (compile-error cmpl "malformed do"))))
    (values vars inits steps test exps cmd-cls)))

(define (p1-syntax-handler-do cmpl exp env toplevel-p rdepth)
  (let-values (((vars inits steps test exps cmds) (p1-decons-do cmpl exp)))
    (let* ((new-env (if (null? vars)
                        env
                        (env-extend (list->vector vars) #f env)))
           (cmpl/ne (lambda (e)
                      (p1-compile-exp cmpl e new-env #f rdepth))))
      (let ((ce (list->vector (cons p2-syntax-id-begin (map cmpl/ne exps))))
            (cs (list->vector (map cmpl/ne steps)))
            (cc (if (null? cmds)
                    '()
                    (list->vector (cons p2-syntax-id-begin
                                        (map cmpl/ne cmds)))))
            (ct (cmpl/ne test)))
        (unless (null? vars)
          (rdepth-add! rdepth -1))
        (vector p2-syntax-id-do
                (list->vector (p1-insert-assign-flg vars new-env))
                (list->vector (map (lambda (e)
                                     (p1-compile-exp cmpl e env #f rdepth))
                                   inits))
                cs
                ct
                ce
                cc)))))

(define (p1-decons-mv-binding-spec cmpl mbs)
  (let loop ((mvbindings mbs)
             (formals '())
             (init '()))
    (if (null? mvbindings)
        (values (reverse formals) (reverse init))
        (begin
          (unless (pair? mvbindings)
            (compile-error cmpl "malfromed Mv binding spec"))
          (let ((x (car mvbindings)) (f #f) (i #f))
            (unless (pair? x)
              (compile-error cmpl "malfromed Mv binding spec"))
            (set! f (car x))
            (set! x (cdr x))
            (unless (pair? x)
              (compile-error cmpl "malfromed Mv binding spec"))
            (set! i (car x))
            (unless (null? (cdr x))
              (compile-error cmpl "malfromed Mv binding spec"))
            (loop (cdr mvbindings)
                  (let-values (((p v) (p1-decons-formals cmpl f)))
                    (cons (cons p v) formals))
                  (cons i init)))))))

(define (p1-decons-let-values cmpl exp)
  (let ((x (cdr exp)))
    (unless (pair? x)
      (compile-error cmpl (format "malformed ~a" (car exp))))
    (let-values (((formals init) (p1-decons-mv-binding-spec cmpl (car x))))
      (values formals init (cdr x)))))

(define (p1-syntax-handler-let-values cmpl exp env toplevel-p rdepth)
  (let-values (((formals inits body) (p1-decons-let-values cmpl exp)))
    (let* ((all-vars (let rec ((f formals))
                       (if (null? f)
                           '()
                           (append (car (car f)) (rec (cdr f))))))
           (all-vars-vec (list->vector all-vars))
           (new-env (if (null? all-vars)
                        env
                        (env-extend all-vars-vec #f env)))
           (cb (p1-cmpl-body cmpl body new-env #f rdepth))
           (mv (let ((cnt 0))
                 (map (lambda (f)
                        (list->vector
                         (let rec ((vars (car f)))
                           (if (null? vars)
                               '()
                               (let ((flg (env-assigned-variable? new-env cnt)))
                                 (set! cnt (+ cnt 1))
                                 (cons (cons (car vars) flg)
                                       (rec (cdr vars))))))))
                      formals)))
           (vv (map (lambda (f) (cdr f))
                    formals)))
      (unless (null? all-vars)
        (rdepth-add! rdepth -1))
      (vector p2-syntax-id-let-values
              (list->vector mv)
              (list->vector vv)
              (list->vector (map (lambda (e)
                                   (p1-compile-exp cmpl e env #f rdepth))
                                 inits))
              cb
              (vector-length all-vars-vec)))))

(define (p1-syntax-handler-let*-values cmpl exp env toplevel-p rdepth)
  (let-values (((formals inits body) (p1-decons-let-values cmpl exp)))
    (let rec ((fo formals) (in inits) (e env))
      (if (null? fo)
          (p1-cmpl-body cmpl body e #f rdepth)
          (let ((vars (car (car fo)))
                (vv (cdr (car fo))))
            (let* ((vars-vec (list->vector vars))
                   (new-env (if (null? vars)
                                e
                                (env-extend vars-vec vv e)))
                   (b (rec (cdr fo) (cdr in) new-env)))
              (unless (null? vars)
                (rdepth-add! rdepth -1))
              (vector p2-syntax-id-let-values
                      (vector
                       (list->vector
                        (let rec ((idx 0) (v vars))
                          (if (null? v)
                              '()
                              (cons (cons (car v)
                                          (env-assigned-variable? new-env idx))
                                    (rec (+ idx 1) (cdr v)))))))
                      (vector vv)
                      (vector (p1-compile-exp cmpl (car in) e #f rdepth))
                      b
                      (vector-length vars-vec))))))))

(define (with-dynamic-bindings bindings thunk)
  (apply push-dynamic-bindings bindings)
  (let ((val (thunk)))
    (pop-dynamic-bindings)
    val))

(define (p1-decons-parameterize cmpl exp)
  (let-values (((name params vals body)
                (p1-decons-let-like-syntax cmpl exp #f 'parameterize)))
    (values params vals (length params) body)))

(define (p1-syntax-handler-parameterize cmpl exp env toplevel-p rdepth)
  (let-values (((params vals nr body) (p1-decons-parameterize cmpl exp)))
    (vector p2-syntax-id-call
            (vector p2-syntax-id-gref
                    'with-dynamic-bindings
                    '(scythe internal compile))
            (let ((vec (make-vector (+ nr 2))))
              (vector-set! vec 0 p2-syntax-id-call)
              (vector-set! vec 1 (vector p2-syntax-id-gref
                                         'list
                                         '(scheme base)))
              (let loop ((idx 2)
                         (params params)
                         (vals vals))
                (if (null? params) vec
                    (let ((p (car params)) (v (car vals)))
                      (vector-set! vec idx
                                   (vector p2-syntax-id-call
                                           (p1-compile-exp cmpl p env #f rdepth)
                                           (p1-compile-exp cmpl v env #f rdepth)
                                           (vector p2-syntax-id-self #f)))
                      (loop (+ idx 1) (cdr params) (cdr vals))))))
            (p1-cmpl-lambda cmpl () () body env toplevel-p rdepth))))

(define (p1-decons-quasiquote cmpl exp)
  (let ((qq (cdr exp)))
    (unless (null? (cdr qq))
      (compile-error cmpl "malformed quasiquote"))
    (car qq)))

(define (p1-syntax-handler-quasiquote cmpl exp env toplevel-p rdepth)
  (let* ((tmpl (p1-decons-quasiquote cmpl exp))
         (qq (compile-qq-template tmpl))
         (n (qq-template-num-of-unquoted qq)))
    (if (= n 0)
        (vector p2-syntax-id-self tmpl)
        (let ((vec (make-vector (+ n 3))))
          (vector-set! vec 0 p2-syntax-id-call)
          (vector-set! vec 1 (vector p2-syntax-id-gref
                                     'substitute-qq-template
                                     '(scythe internal compile)))
          (vector-set! vec 2 (vector p2-syntax-id-self qq))
          (let loop ((i 0))
            (when (< i n)
              (vector-set! vec (+ i 3)
                           (p1-compile-exp cmpl
                                           (qq-template-unquoted qq i)
                                           env toplevel-p rdepth))
              (loop (+ i 1))))
          vec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export (current-module) 'current-macro-env-def)
(module-export (current-module) 'current-macro-env-use)

(define current-macro-env-def (make-parameter ()))
(define current-macro-env-use (make-parameter ()))

(define (macro-exec-expansion macro form use-env)
  (parameterize ((current-macro-env-def (macro-env macro))
                 (current-macro-env-use use-env))
    (macro-yield-transformer macro form)))

(define (p1-decons-syntax-definition cmpl exp)
  (let ((x (cdr exp)) (key #f) (trans #f))
    (unless (pair? x)
      (compile-error cmpl "malformed define-syntax"))
    (set! key (car x))
    (set! x (cdr x))
    (unless (symbol? key)
      (compile-error cmpl "malformed define-syntax"))
    (unless (pair? x)
      (compile-error cmpl "malformed define-syntax"))
    (set! trans (car x))
    (unless (null? (cdr x))
      (compile-error cmpl "malformed define-syntax"))
    (values key trans)))

(define (p1-syntax-handler-syntax-definition cmpl exp env toplevel-p rdepth)
  (when (not toplevel-p)
    (compile-error cmpl "syntax definition can apper in the toplevel or beginning of a <body>"))
  (let-values (((key trans) (p1-decons-syntax-definition cmpl exp)))
    (let-values (((v i l e) (env-find-identifier env key)))
      (unless (env-outmost? e)
        (compile-error cmpl "malformed define-syntax"))
      (let ((tenv (compiler-base-env cmpl)))
        (let ((texp (p1-compile-exp cmpl trans tenv #f (new-rdepth)))
              (mac (make-macro (eval trans tenv) env)))
          (global-syntax-bind e v mac #f)
          (if (precompile?)
              (vector p2-syntax-id-call
                      (vector p2-syntax-id-gref
                              'global-syntax-bind '(scythe internal compile))
                      (vector p2-syntax-id-self e)
                      (vector p2-syntax-id-self v)
                      (vector p2-syntax-id-call
                              (vector p2-syntax-id-gref
                                      'make-macro '(scythe internal compile))
                              texp
                              (vector p2-syntax-id-self env))
                      (vector p2-syntax-id-self #f))
              (vector p2-syntax-id-begin)))))))

(define (p1-decons-let-syntax cmpl exp)
  (let-values (((name keys transs body)
                (p1-decons-let-like-syntax cmpl exp #f 'let-syntax)))
    (values keys transs body)))

(define (p1-syntax-handler-let-syntax cmpl exp env toplevel-p rdepth)
  (let-values (((keywords transformers body) (p1-decons-let-syntax cmpl exp)))
    (let* ((tenv (env-copy-keyword env))
           (macros (map (lambda (t)
                          (make-macro (eval t tenv) env))
                        transformers)))
      (p1-cmpl-body cmpl body (env-extend-syntax keywords macros env)
                    #f rdepth))))

(define (p1-decons-letrec-syntax cmpl exp)
  (let-values (((name keys transs body)
                (p1-decons-let-like-syntax cmpl exp #f 'letrec-syntax)))
    (values keys transs body)))

(define (p1-syntax-handler-letrec-syntax cmpl exp env toplevel-p rdepth)
  (let-values (((keywords transformers body)
                (p1-decons-letrec-syntax cmpl exp)))
    (let* ((tenv (env-copy-keyword env))
           (benv (env-extend-syntax keywords #f env))
           (macros (map (lambda (t)
                          (make-macro (eval t tenv) benv))
                        transformers)))
      (let loop ((mac macros) (idx 0))
        (unless (null? mac)
          (env-set-syntax! benv idx (car mac))
          (loop (cdr mac) (+ idx 1))))
      (p1-cmpl-body cmpl body benv #f rdepth))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (p1-decons-with-module cmpl exp)
  (let ((x (cdr exp)))
    (unless (pair? x)
      (compile-error cmpl "malformed with-module"))
    x))

(define (p1-syntax-handler-with-module cmpl exp env toplevel-p rdepth)
  (let* ((x (p1-decons-with-module cmpl exp))
         (name (car x))
         (exps (cdr x))
         (benv (compiler-base-env cmpl)))
    (compiler-select-module! cmpl name)
    (let ((x (list->vector (cons p2-syntax-id-begin
                                 (map (lambda (e)
                                        (p1-compile-exp cmpl e
                                                        (compiler-base-env cmpl)
                                                        toplevel-p rdepth))
                                      exps)))))
      (compiler-select-base-env! cmpl benv)
      x)))

(define (p1-decons-select-module cmpl exp)
  (let ((x (cdr exp)))
    (unless (and (pair? x) (null? (cdr x)))
      (compile-error cmpl "malformed select-module"))
    (car x)))

(define (p1-syntax-handler-select-module cmpl exp env toplevel-p rdepth)
  (compiler-select-module! cmpl (p1-decons-select-module cmpl exp))
  (vector p2-syntax-id-begin))

(define (p1-syntax-handler-current-module cmpl exp env toplevel-p rdepth)
  (unless (= (length exp) 1)
    (compile-error cmpl "malformed current-module"))
  (vector p2-syntax-id-self (env-module (compiler-base-env cmpl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define compiler-syntax-definition
  (make-syntax 'define p1-syntax-handler-definition))

(define compiler-syntax-begin
  (make-syntax 'begin p1-syntax-handler-begin))

(define compiler-syntax-quote
  (make-syntax 'quote p1-syntax-handler-quote))

(define compiler-syntax-lambda
  (make-syntax 'lambda p1-syntax-handler-lambda))

(define compiler-syntax-assignment
  (make-syntax 'set! p1-syntax-handler-assignment))

(define compiler-syntax-if
  (make-syntax 'if p1-syntax-handler-if))

(define compiler-syntax-cond
  (make-syntax 'cond p1-syntax-handler-cond))

(define compiler-syntax-and
  (make-syntax 'and p1-syntax-handler-and))

(define compiler-syntax-or
  (make-syntax 'or p1-syntax-handler-or))

(define compiler-syntax-when
  (make-syntax 'when p1-syntax-handler-when))

(define compiler-syntax-unless
  (make-syntax 'unless p1-syntax-handler-unless))

(define compiler-syntax-let
  (make-syntax 'let p1-syntax-handler-let))

(define compiler-syntax-let*
  (make-syntax 'let* p1-syntax-handler-let*))

(define compiler-syntax-letrec
  (make-syntax 'letrec p1-syntax-handler-letrec))

(define compiler-syntax-letrec*
  (make-syntax 'letrec* p1-syntax-handler-letrec*))

(define compiler-syntax-do
  (make-syntax 'do p1-syntax-handler-do))

(define compiler-syntax-let-values
  (make-syntax 'let-values p1-syntax-handler-let-values))

(define compiler-syntax-let*-values
  (make-syntax 'let*-values p1-syntax-handler-let*-values))

(define compiler-syntax-parameterize
  (make-syntax 'parameterize p1-syntax-handler-parameterize))

(define compiler-syntax-quasiquote
  (make-syntax 'quasiquote p1-syntax-handler-quasiquote))

(define compiler-syntax-syntax-definition
  (make-syntax 'define-syntax p1-syntax-handler-syntax-definition))

(define compiler-syntax-let-syntax
  (make-syntax 'let-syntax p1-syntax-handler-let-syntax))

(define compiler-syntax-letrec-syntax
  (make-syntax 'letrec-syntax p1-syntax-handler-letrec-syntax))

(define compiler-syntax-with-module
  (make-syntax 'with-module p1-syntax-handler-with-module))

(define compiler-syntax-select-module
  (make-syntax 'select-module p1-syntax-handler-select-module))

(define compiler-syntax-current-module
  (make-syntax 'current-module p1-syntax-handler-current-module))

(let ((name '(scheme base)))
  (p1-register-syntax name compiler-syntax-definition #t)
  (p1-register-syntax name compiler-syntax-begin #t)
  (p1-register-syntax name compiler-syntax-quote #t)
  (p1-register-syntax name compiler-syntax-lambda #t)
  (p1-register-syntax name compiler-syntax-assignment #t)
  (p1-register-syntax name compiler-syntax-if #t)
  (p1-register-syntax name compiler-syntax-cond #t)
  (p1-register-syntax name compiler-syntax-and #t)
  (p1-register-syntax name compiler-syntax-or #t)
  (p1-register-syntax name compiler-syntax-when #t)
  (p1-register-syntax name compiler-syntax-unless #t)
  (p1-register-syntax name compiler-syntax-let #t)
  (p1-register-syntax name compiler-syntax-let* #t)
  (p1-register-syntax name compiler-syntax-letrec #t)
  (p1-register-syntax name compiler-syntax-letrec* #t)
  (p1-register-syntax name compiler-syntax-do #t)
  (p1-register-syntax name compiler-syntax-let-values #t)
  (p1-register-syntax name compiler-syntax-let*-values #t)
  (p1-register-syntax name compiler-syntax-parameterize #t)
  (p1-register-syntax name compiler-syntax-quasiquote #t)
  (p1-register-syntax name compiler-syntax-syntax-definition #t)
  (p1-register-syntax name compiler-syntax-let-syntax #t)
  (p1-register-syntax name compiler-syntax-letrec-syntax #t))

(let ((name '(scythe internal misc)))
  (p1-register-syntax name compiler-syntax-with-module #t)
  (p1-register-syntax name compiler-syntax-select-module #t)
  (p1-register-syntax name compiler-syntax-current-module #t))


;; (define *test-nr-test-total* 0)
;; (define *test-nr-test-passed* 0)
;; (define *test-nr-test-failed* 0)
;; (define *test-failed-list* '())

;; (define (test-compile exp expected)
;;   (let* ((actual (compile exp '(main)))
;;          (result (equal? expected actual)))
;;     (set! *test-nr-test-total* (+ *test-nr-test-total* 1))
;;     (if result
;;         (set! *test-nr-test-passed* (+ *test-nr-test-passed* 1))
;;         (set! *test-nr-test-failed* (+ *test-nr-test-failed* 1)))
;;     (display (if result #\. #\f))
;;     (unless result
;;       (let ((head     (format "Test: ~s: Failed" exp))
;;             (expected (format "  Expected: ~s" expected))
;;             (actual   (format "  Actual  : ~s" actual)))
;;         (set! *test-failed-list*
;;               (cons (format "~a\n~a\n~a\n" head expected actual)
;;                     *test-failed-list*))))))


;; (define (test-print-summary)
;;   (newline)
;;   (display (format "~a total  ~a passed  ~a failed\n"
;;                    *test-nr-test-total*
;;                    *test-nr-test-passed*
;;                    *test-nr-test-failed*))
;;   (let loop ((lst (reverse *test-failed-list*)))
;;     (unless (null? lst)
;;       (display (car lst))
;;       (loop (cdr lst)))))
