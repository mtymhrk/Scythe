
(select-module (scythe internal compile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (map proc lst)
  (if (null? lst)
      lst
      (cons (proc (car lst)) (map proc (cdr lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (new-cseq)
  (cons '() '()))

(define (cseq-code cseq)
  (car cseq))

(define (cseq-last cseq)
  (cdr cseq))

(define (cseq-unshift cseq elm)
  (let ((x (cons elm (cseq-code cseq))))
    (when (null? (cseq-last cseq))
      (set-cdr! cseq x))
    (set-car! cseq x)))

(define (cseq-push cseq elm)
  (let ((x (cons elm '())))
    (when (null? (cseq-code cseq))
      (set-car! cseq x))
    (unless (null? (cseq-last cseq))
      (set-cdr! (cseq-last cseq) x))
    (set-cdr! cseq x)))

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
(define (cons-inst op . arg) (cons op arg))
(define (push-inst inst cseq) (cseq-push cseq inst))

(define (push-inst-nop cseq)
  (push-inst (cons-inst 'nop) cseq))
(define (push-inst-undef cseq)
  (push-inst (cons-inst 'undef) cseq))
(define (push-inst-cframe lbl cseq)
  (push-inst (cons-inst 'cframe lbl) cseq))
(define (push-inst-eframe narg cseq)
  (push-inst (cons-inst 'eframe narg) cseq))
(define (push-inst-epop cseq)
  (push-inst (cons-inst 'epop) cseq))
(define (push-inst-eshift ns cseq)
  (push-inst (cons-inst 'eshift ns) cseq))
(define (push-inst-immval val cseq)
  (push-inst (cons-inst 'immval val) cseq))
(define (push-inst-push cseq)
  (push-inst (cons-inst 'push) cseq))
(define (push-inst-mvpush cseq)
  (push-inst (cons-inst 'mvpush) cseq))
(define (push-inst-return cseq)
  (push-inst (cons-inst 'return) cseq))
(define (push-inst-call narg mrv lbl cseq)
  (push-inst (cons-inst 'call narg) cseq)
  (if (= mrv 1)
      (push-inst-mrve cseq)
      (push-inst-nop cseq))
  (push-inst-label lbl cseq))
(define (push-inst-tcall narg cseq)
  (push-inst (cons-inst 'tcall narg) cseq))
(define (push-inst-gref sym mod cseq)
  (push-inst (cons-inst 'gref sym mod) cseq))
(define (push-inst-gdef sym mod cseq)
  (push-inst (cons-inst 'gdef sym mod) cseq))
(define (push-inst-gset sym mod cseq)
  (push-inst (cons-inst 'gset sym mod) cseq))
(define (push-inst-sref idx layer cseq)
  (push-inst (cons-inst 'sref idx layer) cseq))
(define (push-inst-sset idx layer cseq)
  (push-inst (cons-inst 'sset idx layer) cseq))
(define (push-inst-jmp lbl cseq)
  (push-inst (cons-inst 'jmp lbl) cseq))
(define (push-inst-jmpt lbl cseq)
  (push-inst (cons-inst 'jmpt lbl) cseq))
(define (push-inst-jmpf lbl cseq)
  (push-inst (cons-inst 'jmpf lbl) cseq))
(define (push-inst-box idx layer cseq)
  (push-inst (cons-inst 'box idx layer) cseq))
(define (push-inst-demine idx layer cseq)
  (push-inst (cons-inst 'demine idx layer) cseq))
(define (push-inst-emine narg cseq)
  (push-inst (cons-inst 'emine narg) cseq))
(define (push-inst-edemine narg layer cseq)
  (push-inst (cons-inst 'edemine narg layer) cseq))
(define (push-inst-mrvc arity cseq)
  (push-inst (cons-inst 'mrvc arity) cseq))
(define (push-inst-mrve cseq)
  (push-inst (cons-inst 'mrve) cseq))
(define (push-inst-label lbl cseq)
  (push-inst (cons-inst 'label lbl) cseq))
(define (push-inst-asm-close nr-free arity code cseq)
  (push-inst (cons-inst 'asm-close nr-free arity code) cseq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (new-env)
  '())

(define (empty-env? env)
  (null? env))

(define (cons-env vars varg env)
  (let* ((vars (if (vector? vars) vars (list->vector vars)))
         (len (vector-length vars))
         (assigned (make-vector len #f)))
    (cons (vector vars assigned varg) env)))

(define (outer-env env)
  (if (null? env) env (cdr env)))

(define (variable-length-argument? env layer)
  (if (<= layer 0)
      (vector-ref (car env) 2)
      (variable-length-argument? (outer-env env) (- layer 1))))

(define (assigned-variable? env idx layer)
  (if (<= layer 0)
      (vector-ref (vector-ref (car env) 1) idx)
      (assigned-variable? (outer-env env) idx (- layer 1))))

(define (resolve-reference-aux env sym assigned rdepth layer)
  (if (empty-env? env)
      #f
      (let* ((vars (vector-ref (car env) 0))
             (assi (vector-ref (car env) 1))
             (len (vector-length vars)))
        (let rec ((idx 0))
          (if (>= idx len)
              (resolve-reference-aux (outer-env env)
                                     sym
                                     assigned
                                     rdepth
                                     (+ layer 1))
              (if (eq? sym (vector-ref vars idx))
                  (begin
                    (when assigned
                      (vector-set! assi idx assigned))
                    (rdepth-set! rdepth layer)
                    (cons idx layer))
                  (rec (+ idx 1))))))))

(define (resolve-reference env sym assigned rdepth)
  (resolve-reference-aux env sym assigned rdepth 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (generate-label cmpl prefix)
  (let ((id  (compiler-assign-label-id! cmpl)))
    (string->symbol (if prefix
                        (format "lbl_~a_~a" prefix id)
                        (format "lbl_~a" id)))))

(define (current-module-name cmpl)
  (module-name (compiler-current-module cmpl)))

(define (compile-error cmpl msg)
  (error (format "failed to compile: ~a" msg) (compiler-current-expr cmpl)))

(define (get-compiler-from-arg arg)
  (cond
   ((compiler? arg) arg)
   (else (make-compiler arg))))

(define (compile exp arg)
  (let ((cmpl (get-compiler-from-arg arg))
        (cseq (new-cseq)))
    (p2-compile-exp cmpl
                    (p1-compile-exp cmpl exp (new-env) #t (new-rdepth))
                    -1 #f cseq)
    (cseq-code cseq)))

(define (compile-file file arg)
  (let ((cmpl (get-compiler-from-arg arg))
        (port (open-input-file file))
        (cseq (new-cseq)))
    (let loop ((exp (read port)))
      (unless (eof-object? exp)
        (p2-compile-exp cmpl
                        (p1-compile-exp cmpl exp (new-env) #t (new-rdepth))
                        -1 #f cseq)
        (loop (read port))))
    (cseq-code cseq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (p2-compile-exp cmpl exp arity tail-p cseq)
  (let ((ce (compiler-current-expr cmpl)))
    (compiler-select-expr! cmpl exp)
    ((p2-get-syntax exp) cmpl exp arity tail-p cseq)
    (compiler-select-expr! cmpl ce)))

;;; #(lref <symbol> <idx> <layer>)
;;;    <idx>   : <integer>
;;;    <layer> : <integer>
(define (p2-syntax-handler-lref cmpl exp arity tail-p cseq)
  (push-inst-sref (vector-ref exp 2) (vector-ref exp 3) cseq)
  (when tail-p (push-inst-return cseq)))

;;; #(gref <symbol> <module>)
;;;    <module> : <list of symbols>
(define (p2-syntax-handler-gref cmpl exp arity tail-p cseq)
  (push-inst-gref (vector-ref exp 1) (vector-ref exp 2) cseq)
  (when tail-p (push-inst-return cseq)))

;;; #(self <object>)
(define (p2-syntax-handler-self cmpl exp arity tail-p cseq)
  (push-inst-immval (vector-ref exp 1) cseq)
  (when tail-p (push-inst-return cseq)))

;;; #(call <procedure> <arg> ...)
;;;    <procedure> : <expr>
;;;    <arg>       : <expr>
(define (p2-syntax-handler-call cmpl exp arity tail-p cseq)
  (let ((len (vector-length exp))
        (lbl #f))
    (unless tail-p
      (set! lbl (generate-label cmpl "call-cont"))
      (push-inst-cframe lbl cseq))
    (let loop ((idx 2))
      (when (< idx len)
        (p2-compile-exp cmpl (vector-ref exp idx) 1 #f cseq)
        (push-inst-push cseq)
        (loop (+ idx 1))))
    (p2-compile-exp cmpl (vector-ref exp 1) 1 #f cseq)
    (if tail-p
        (push-inst-tcall (- len 2) cseq)
        (push-inst-call (- len 2) arity lbl cseq))))

;;; #(gdef <symbol> <module> <expr>)
;;;    <module> : <list of symbols>
(define (p2-syntax-handler-gdef cmpl exp arity tail-p cseq)
  (p2-compile-exp cmpl (vector-ref exp 3) 1 #f cseq)
  (push-inst-gdef (vector-ref exp 1) (vector-ref exp 2) cseq)
  (when tail-p (push-inst-return cseq)))

;;; #(begin <expr> ...)
(define (p2-syntax-handler-begin cmpl exp arity tail-p cseq)
  (let ((nr (- (vector-length exp) 1)))
    (let loop ((idx 1))
      (when (< idx nr)
        (p2-compile-exp cmpl (vector-ref exp idx) -1 #f cseq)
        (loop (+ idx 1))))
    (if (= nr 0)
        (begin (push-inst-undef cseq)
               (when tail-p (push-inst-return cseq)))
        (p2-compile-exp cmpl (vector-ref exp nr) arity tail-p cseq))))

;;; #(body <vars> <inits> <expr>)
;;;    <vars>  : #(<symbol> ...)
;;;    <inits> : #(<expr> ...)
(define (p2-syntax-handler-body cmpl exp arity tail-p cseq)
  (let ((vars (vector-ref exp 1))
        (inits (vector-ref exp 2)))
    (let ((len (vector-length vars)))
      (when (> len 0)
        (push-inst-emine len cseq))
      (let loop ((idx 0))
        (when (< idx len)
          (p2-compile-exp cmpl (vector-ref inits idx) 1 #f cseq)
          (push-inst-demine idx 0 cseq)
          (loop (+ idx 1))))
      (p2-compile-exp cmpl (vector-ref exp 3) arity tail-p cseq)
      (when (and (not tail-p) (> len 0))
        (push-inst-epop cseq)))))

;;; #(lambda <params> <vp> <env> <body>)
;;;    <params> : #((<symbol> . <flg>) ...)
;;;    <vp>     : <boolean>
;;;    <env>    : <integer>
;;;    <body>   : <expr>
(define (p2-syntax-handler-lambda cmpl exp arity tail-p cseq)
  (let* ((params (vector-ref exp 1))
         (len (vector-length params))
         (bcseq (new-cseq)))
    (let loop ((idx 0))
      (when (< idx len)
        (when (cdr (vector-ref params idx))
          (push-inst-box idx 0 bcseq))
        (loop (+ idx 1))))
    (p2-compile-exp cmpl (vector-ref exp 4) -1 #t bcseq)
    (push-inst-asm-close (vector-ref exp 3)
                         (if (vector-ref exp 2) (- len) len)
                         (cseq-code bcseq)
                         cseq)
    (when tail-p
      (push-inst-return cseq))))

;;; #(lset <symbol> <idx> <layer> <expr>)
;;;    <idx>   : <integer>
;;;    <layer> : <integer>
(define (p2-syntax-handler-lset cmpl exp arity tail-p cseq)
  (p2-compile-exp cmpl (vector-ref exp 4) 1 #f cseq)
  (push-inst-sset (vector-ref exp 2) (vector-ref exp 3) cseq)
  (when tail-p
    (push-inst-return cseq)))

;;; #(gset <symbol> <module> <expr>)
;;;    <module> : <list of symbols>
(define (p2-syntax-handler-gset cmpl exp arity tail-p cseq)
  (p2-compile-exp cmpl (vector-ref exp 3) 1 #f cseq)
  (push-inst-gset (vector-ref exp 1) (vector-ref exp 2) cseq)
  (when tail-p
    (push-inst-return cseq)))

;;; #(if <condi> <conse> <alter>)
;;;    <condi> : <expr>
;;;    <conse> : <expr>
;;;    <alter> : <expr>
(define (p2-syntax-handler-if cmpl exp arity tail-p cseq)
  (let ((lbl-j (if tail-p #f (generate-label cmpl "if-j")))
        (lbl-a (generate-label cmpl "if-a")))
    (p2-compile-exp cmpl (vector-ref exp 1) 1 #f cseq)
    (push-inst-jmpf lbl-a cseq)
    (p2-compile-exp cmpl (vector-ref exp 2) arity tail-p cseq)
    (unless tail-p
      (push-inst-jmp lbl-j cseq))
    (push-inst-label lbl-a cseq)
    (p2-compile-exp cmpl (vector-ref exp 3) arity tail-p cseq)
    (unless tail-p
      (push-inst-label lbl-j cseq))))

;;; #(cond <clauses>)
;;;    <clauses> : #(<clause> ... <else>)
;;;    <clause>  : #(<test> >> <expr>)
;;;                | #(<test> => <expr>)
;;;                | #(<test> <>)
;;;    <else>    : #(else -- <expr>)
(define (p2-syntax-handler-cond cmpl exp arity tail-p cseq)
  (let* ((clauses (vector-ref exp 1))
         (nr-clauses (vector-length clauses))
         (nr-tests (- nr-clauses 1))
         (lbl-c (make-vector nr-clauses #f))
         (lbl-j (if tail-p #f (generate-label cmpl "cond-j"))))
    (let loop ((idx 0))
      (when (< idx nr-tests)
        (let ((cls (vector-ref clauses idx))
              (lbl lbl-j))
          (p2-compile-exp cmpl (vector-ref cls 0) 1 #f cseq)
          (when (or (not (eq? (vector-ref cls 1) '<>)) tail-p)
            (set! lbl (generate-label cmpl "cond-c"))
            (vector-set! lbl-c idx lbl))
          (push-inst-jmpt lbl cseq))
        (loop (+ idx 1))))
    (let loop ((idx (- nr-clauses 1)) (lbl #f))
      (when (>= idx 0)
        (let* ((cls (vector-ref clauses idx))
               (typ (vector-ref cls 1)))
          (cond ((eq? typ '>>)
                 (when lbl (push-inst-jmp lbl cseq))
                 (push-inst-label (vector-ref lbl-c idx) cseq)
                 (p2-compile-exp cmpl (vector-ref cls 2) arity tail-p cseq)
                 (loop (- idx 1) lbl-j))

                ((eq? typ '=>)
                 (let ((lbl-call #f))
                   (when lbl (push-inst-jmp lbl cseq))
                   (push-inst-label (vector-ref lbl-c idx) cseq)
                   (unless tail-p
                     (set! lbl-call (generate-label cmpl "call-cont"))
                     (push-inst-cframe lbl-call cseq))
                   (push-inst-push cseq)
                   (p2-compile-exp cmpl (vector-ref cls 2) 1 #f cseq)
                   (if tail-p
                       (begin
                         (push-inst-tcall 1 cseq)
                         (loop (- idx 1) #f))
                       (begin
                         (push-inst-call 1 arity lbl-call cseq)
                         (loop (- idx 1) lbl-j)))))

                ((eq? typ '<>)
                 (when tail-p
                   (push-inst-label (vector-ref lbl-c idx) cseq)
                   (push-inst-return cseq))
                 (loop (- idx 1) lbl))

                ((eq? typ '--)
                 (when lbl (push-inst-jmp lbl cseq))
                 (p2-compile-exp cmpl (vector-ref cls 2) arity tail-p cseq)
                 (loop (- idx 1) lbl-j))))))
    (when lbl-j
      (push-inst-label lbl-j cseq))))

;;; #(and <expr> ...)
(define (p2-syntax-handler-and cmpl exp arity tail-p cseq)
  (let ((nr-tests (- (vector-length exp) 1))
        (lbl-j (generate-label cmpl "and-j")))
    (cond ((= nr-tests 0)
           (push-inst-immval #t cseq)
           (when tail-p
             (push-inst-return cseq)))
          (else
           (let loop ((idx 1))
             (when (< idx nr-tests)
               (p2-compile-exp cmpl (vector-ref exp idx) 1 #f cseq)
               (push-inst-jmpf lbl-j cseq)
               (loop (+ idx 1))))
           (p2-compile-exp cmpl (vector-ref exp nr-tests) 1 tail-p cseq)
           (when (> nr-tests 1)
             (push-inst-label lbl-j cseq)
             (when tail-p
               (push-inst-return cseq)))))))

;;; #(or <expr> ...)
(define (p2-syntax-handler-or cmpl exp arity tail-p cseq)
  (let ((nr-tests (- (vector-length exp) 1))
        (lbl-j (generate-label cmpl "or-j")))
    (cond ((= nr-tests 0)
           (push-inst-immval #f cseq)
           (when tail-p
             (push-inst-return cseq)))
          (else
           (let loop ((idx 1))
             (when (< idx nr-tests)
               (p2-compile-exp cmpl (vector-ref exp idx) 1 #f cseq)
               (push-inst-jmpt lbl-j cseq)
               (loop (+ idx 1))))
           (p2-compile-exp cmpl (vector-ref exp nr-tests) 1 tail-p cseq)
           (when (> nr-tests 1)
             (push-inst-label lbl-j cseq)
             (when tail-p
               (push-inst-return cseq)))))))

;;; #(let <vars> <inits> <body>)
;;;    <vars>  : #((<symbol> . <flg>) ...)
;;;    <inits> : #(<expr> ...)
;;;    <body>  : <expr>
(define (p2-syntax-handler-let cmpl exp arity tail-p cseq)
  (let ((vars (vector-ref exp 1))
        (inits (vector-ref exp 2)))
    (let ((nr-vars (vector-length vars)))
      (when (> nr-vars 0)
        (let loop ((idx 0))
          (when (< idx nr-vars)
            (p2-compile-exp cmpl (vector-ref inits idx) 1 #f cseq)
            (push-inst-push cseq)
            (loop (+ idx 1))))
        (push-inst-eframe nr-vars cseq)
        (let loop ((idx 0))
          (when (< idx nr-vars)
            (when (cdr (vector-ref vars idx))
              (push-inst-box idx 0 cseq))
            (loop (+ idx 1)))))
      (p2-compile-exp cmpl (vector-ref exp 3) arity tail-p cseq)
      (when (and (> nr-vars 0) (not tail-p))
        (push-inst-epop cseq)))))

;;; #(letrec <vars> <inits> <body>)
;;;    <vars>  : #((<symbol> . <flg>) ...)
;;;    <inits> : #(<expr> ...)
;;;    <body>  : <expr>
(define (p2-syntax-handler-letrec cmpl exp arity tail-p cseq)
  (let ((vars (vector-ref exp 1))
        (inits (vector-ref exp 2)))
    (let ((nr-vars (vector-length vars)))
      (when (> nr-vars 0)
        (push-inst-emine nr-vars cseq)
        (let loop ((idx 0))
          (when (< idx nr-vars)
            (p2-compile-exp cmpl (vector-ref inits idx) 1 #f cseq)
            (push-inst-push cseq)
            (loop (+ idx 1))))
        (push-inst-edemine nr-vars 0 cseq))
      (p2-compile-exp cmpl (vector-ref exp 3) arity tail-p cseq)
      (when (and (> nr-vars 0) (not tail-p))
        (push-inst-epop cseq)))))

;;; #(letrec* <vars> <inits> <body>)
;;;    <vars>  : #((<symbol> . <flg>) ...)
;;;    <inits> : #(<expr> ...)
;;;    <body>  : <expr>
(define (p2-syntax-handler-letrec* cmpl exp arity tail-p cseq)
  (let ((vars (vector-ref exp 1))
        (inits (vector-ref exp 2)))
    (let ((nr-vars (vector-length vars)))
      (when (> nr-vars 0)
        (push-inst-emine nr-vars cseq)
        (let loop ((idx 0))
          (when (< idx nr-vars)
            (p2-compile-exp cmpl (vector-ref inits idx) 1 #f cseq)
            (push-inst-demine idx 0 cseq)
            (loop (+ idx 1)))))
      (p2-compile-exp cmpl (vector-ref exp 3) arity tail-p cseq)
      (when (and (> nr-vars 0) (not tail-p))
        (push-inst-epop cseq)))))

;;; #(do <vars> <inits> <steps> <test> <expr> <cmds>)
;;;    <vars> : #((<symbol> . <flg>) ...)
;;;    <inits> : #(<expr> ...)
;;;    <steps> : #(<expr> ...)
;;;    <test>  : <expr>
;;;    <cmds>  : <expr> | '()
(define (p2-syntax-handler-do cmpl exp arity tail-p cseq)
  (let ((vars (vector-ref exp 1))
        (inits (vector-ref exp 2))
        (steps (vector-ref exp 3))
        (cmds (vector-ref exp 6)))
    (let ((nr-vars (vector-length vars))
          (lbl-e (generate-label cmpl "do-e"))
          (lbl-s (generate-label cmpl "do-s")))
      (when (> nr-vars 0)
        (let loop ((idx 0))
          (when (< idx nr-vars)
            (p2-compile-exp cmpl (vector-ref inits idx) 1 #f cseq)
            (push-inst-push cseq)
            (loop (+ idx 1))))
        (push-inst-eframe nr-vars cseq))
      (push-inst-label lbl-s cseq)
      (let loop ((idx 0))
        (when (< idx nr-vars)
          (when (cdr (vector-ref vars idx))
            (push-inst-box idx 0 cseq))
          (loop (+ idx 1))))
      (p2-compile-exp cmpl (vector-ref exp 4) 1 #f cseq)
      (push-inst-jmpt lbl-e cseq)
      (unless (null? cmds)
        (p2-compile-exp cmpl cmds -1 #f cseq))
      (when (> nr-vars 0)
        (let loop ((idx 0))
          (when (< idx nr-vars)
            (p2-compile-exp cmpl (vector-ref steps idx) 1 #f cseq)
            (push-inst-push cseq)
            (loop (+ idx 1))))
        (push-inst-eframe nr-vars cseq)
        (push-inst-eshift 1 cseq))
      (push-inst-jmp lbl-s cseq)
      (push-inst-label lbl-e cseq)
      (p2-compile-exp cmpl (vector-ref exp 5) arity tail-p cseq)
      (when (and (> nr-vars 0) (not tail-p))
        (push-inst-epop cseq)))))

;;; #(let-values <mvars> <vvar> <inits> <expr> <nr>)
;;;    <mvars> : #((<symbol> . <flg>) ...)  ...)
;;;    <vvar>  : #(<boolean> ...)
;;;    <inits> : #(<expr> ...)
;;;    <nr>    : <integer>
(define (p2-syntax-handler-let-values cmpl exp arity tail-p cseq)
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
            (p2-compile-exp cmpl (vector-ref inits idx) a #f cseq)
            (unless (= a 1)
              (push-inst-mrvc a cseq))
            (when (> n 0)
              (push-inst-mvpush cseq)))
          (loop (+ idx 1))))
      (when (> nr-vars 0)
        (push-inst-eframe nr-vars cseq))
      (let loop ((idx 0) (cnt 0))
        (when (< idx nr-mbs)
          (let* ((vars (vector-ref mvars idx))
                 (n (vector-length vars)))
            (let loop2 ((jdx 0))
              (when (< jdx n)
                (when (cdr (vector-ref vars jdx))
                  (push-inst-box cnt 0 cseq))
                (set! cnt (+ cnt 1))
                (loop2 (+ jdx 1)))))
          (loop (+ idx 1) cnt)))
      (p2-compile-exp cmpl (vector-ref exp 4) arity tail-p cseq)
      (when (and (> nr-vars 0) (not tail-p))
        (push-inst-epop cseq)))))

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
  (let ((rslv (resolve-reference env exp #f rdepth)))
    (if rslv
        (vector p2-syntax-id-lref exp (car rslv) (cdr rslv))
        (vector p2-syntax-id-gref exp (current-module-name cmpl)))))

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
  (cond ((symbol? exp)
         p1-compiler-syntax-reference)
        ((pair? exp)
         (let ((key (car exp)))
           (if (symbol? key)
               (let ((syx (global-syntax-ref (compiler-current-module cmpl)
                                             key
                                             #f)))
                 (if syx
                     syx
                     p1-compiler-syntax-application))
               p1-compiler-syntax-application)))
        (else
         p1-compiler-syntax-self-eval)))

(define (p1-compile-exp cmpl exp env toplevel-p rdepth)
  (let ((ce (compiler-current-expr cmpl)))
    (compiler-select-expr! cmpl exp)
    (rdepth-expand! rdepth)
    (let ((x ((syntax-handler (p1-get-syntax cmpl exp env toplevel-p))
              cmpl exp env toplevel-p rdepth)))
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
  (when (not (= (length exp) 3))
    (compile-error cmpl "malformed define"))
  (let ((var (car (cdr exp)))
        (val (car (cdr (cdr exp)))))
    (when (not (symbol? var))
      (compile-error cmpl "malformed define"))
    (values var val)))

(define (p1-syntax-handler-definition cmpl exp env toplevel-p rdepth)
  (when (not toplevel-p)
    (compile-error cmpl "definition can apper in the toplevel or beginning of a <body>"))
  (let-values (((var val)
                (p1-decons-definition cmpl
                                      (p1-normalize-definition cmpl exp))))
    (vector p2-syntax-id-gdef
            var
            (current-module-name cmpl)
            (p1-compile-exp cmpl val env toplevel-p rdepth))))

(define (p1-syntax-handler-begin cmpl exp env toplevel-p rdepth)
  (list->vector (cons p2-syntax-id-begin
                      (map (lambda (e)
                             (p1-compile-exp cmpl e env toplevel-p rdepth))
                           (cdr exp)))))

(define (p1-decons-body-aux cmpl body env toplevel-p vars inits exps)
  (cond ((null? body)
         (values vars inits exps))
        ((not (pair? body))
         (compile-error cmpl "malformed <body>"))
        (else
         (let* ((exp (car body))
                (syx (p1-get-syntax cmpl exp env toplevel-p)))
           (cond
            ((eq? syx compiler-syntax-definition)
             (let-values (((v i) (p1-decons-definition cmpl exp)))
               (p1-decons-body-aux cmpl (cdr body) env toplevel-p
                                   (cons v vars) (cons i inits) exps)))
            ((eq? syx compiler-syntax-begin)
             (let-values (((v i e) (p1-decons-body-aux cmpl (cdr exp) env
                                                       toplevel-p
                                                       vars inits exps)))
               (if (eq? e exps)
                   (p1-decons-body-aux cmpl (cdr body) env toplevel-p v i exps)
                   (values v i (if (null? (cdr body))
                                   e
                                   (cons (cdr body) e))))))
            (else
             (values vars inits (cons body exps))))))))


(define (p1-decons-body cmpl body env toplevel-p)
  (let-values (((v i e) (p1-decons-body-aux cmpl body env toplevel-p
                                            '() '() '())))
    (values (reverse v)
            (reverse i)
            (if (null? e) '(()) (reverse e)))))

(define (p1-cmpl-body cmpl body env toplevel-p rdepth)
  (let-values (((vars inits exps) (p1-decons-body cmpl body env toplevel-p)))
    (let* ((vv (list->vector vars))
           (env (if (null? vars)
                    env
                    (cons-env vv #f env)))
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

(define (p1-insert-assign-flg params env layer)
  (let rec ((idx 0) (par params))
    (if (null? par)
        '()
        (cons (cons (car par)
                    (assigned-variable? env idx layer))
              (rec (+ idx 1) (cdr par))))))

(define (p1-cmpl-lambda cmpl params vparam body env toplevel-p rdepth)
  (rdepth-expand! rdepth)
  (let* ((env (if (null? params)
                  env
                  (cons-env (list->vector params) vparam env)))
         (bo (p1-cmpl-body cmpl body env #f rdepth)))
    (unless (null? params)
      (rdepth-add! rdepth -1))
    (let ((rd (rdepth-ref rdepth)))
      (rdepth-contract! rdepth)
      (vector p2-syntax-id-lambda
              (list->vector (p1-insert-assign-flg params env 0))
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
    (unless (symbol? var)
      (compile-error cmpl "malformed assignment"))
    (values var val)))

(define (p1-syntax-handler-assignment cmpl exp env toplevel-p rdepth)
  (let-values (((var val) (p1-decons-assignment cmpl exp)))
    (let ((rslv (resolve-reference env var #t rdepth))
          (x (p1-compile-exp cmpl val env toplevel-p rdepth)))
      (if rslv
          (vector p2-syntax-id-lset var (car rslv) (cdr rslv) x)
          (vector p2-syntax-id-gset var (current-module-name cmpl) x)))))

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

(define (p1-decons-let cmpl exp)
  (let ((x (cdr exp))
        (name #f)
        (bind #f)
        (body #f))
    (unless (pair? x)
      (compile-error cmpl "malformed let"))
    (when (symbol? (car x))
      (set! name (car x))
      (set! x (cdr x))
      (unless (pair? x)
        (compile-error cmpl "malformed let")))
    (set! bind (car x))
    (set! body (cdr x))
    (let loop ((bind bind)
               (vars '())
               (inits '()))
      (if (null? bind)
          (values name (reverse vars) (reverse inits) body)
          (begin
            (unless (pair? bind)
              (compile-error cmpl "malformed let"))
            (let ((x (car bind)) (v #f) (i #f))
              (unless (pair? x)
                (compile-error cmpl "malformed let"))
              (set! v (car x))
              (set! x (cdr x))
              (unless (pair? x)
                (compile-error cmpl "malformed let"))
              (set! i (car x))
              (unless (null? (cdr x))
                (compile-error cmpl "malformed let"))
              (loop (cdr bind) (cons v vars) (cons i inits))))))))

;;; (let <name> ((<v> <i>) ...) <expr> ...)
;;; -> (let ((<v> <i>) ...)
;;;      (letrec* ((<name> (lambda (<v> ...) <expr> ...)))
;;;        (<name> <v> ...)))

(define (p1-cmpl-named-let-body cmpl name vars body env toplevel-p rdepth)
  (let ((new-env (cons-env (vector name) #f env)))
    (let ((lmd (p1-cmpl-lambda cmpl vars #f body new-env #f rdepth))
          (cal (cons (vector p2-syntax-id-lref name 0 0)
                     (let rec ((idx 0) (v vars))
                       (if (null? v)
                           '()
                           (cons (vector p2-syntax-id-lref (car v) idx 1)
                                 (rec (+ idx 1) (cdr v))))))))
      (rdepth-add! rdepth -1)
      (vector p2-syntax-id-letrec*
              (vector (cons name (assigned-variable? new-env 0 0)))
              (vector lmd)
              (list->vector (cons p2-syntax-id-call cal))))))

(define (p1-syntax-handler-let cmpl exp env toplevel-p rdepth)
  (let-values (((name vars inits body) (p1-decons-let cmpl exp)))
    (let* ((new-env (if (null? vars)
                        env
                        (cons-env (list->vector vars) #f env)))
           (cb (if name
                   (p1-cmpl-named-let-body cmpl name vars body new-env
                                           #f rdepth)
                   (p1-cmpl-body cmpl body new-env #f rdepth))))
      (unless (null? vars)
        (rdepth-add! rdepth -1))
      (vector p2-syntax-id-let
              (list->vector (p1-insert-assign-flg vars new-env 0))
              (list->vector (map (lambda (e)
                                   (p1-compile-exp cmpl e env #f rdepth))
                                 inits))
              cb))))

(define (p1-syntax-handler-let* cmpl exp env toplevel-p rdepth)
  (let-values (((name vars inits body) (p1-decons-let cmpl exp)))
    (when name
      (compile-error cmpl "malformed let*"))
    (let rec ((v vars) (i inits) (e env))
      (if (null? v)
          (p1-cmpl-body cmpl body e #f rdepth)
          (let* ((new-env (cons-env (vector (car v)) #f e))
                 (b (rec (cdr v) (cdr i) new-env)))
            (rdepth-add! rdepth -1)
            (vector p2-syntax-id-let
                    (vector (cons (car v) (assigned-variable? new-env 0 0)))
                    (vector (p1-compile-exp cmpl (car i) e #f rdepth))
                    b))))))

(define (p1-syntax-handler-letrec cmpl exp env toplevel-p rdepth)
  (let-values (((name vars inits body) (p1-decons-let cmpl exp)))
    (when name
      (error "malformed letrec" exp))
    (let* ((new-env (if (null? vars)
                        env
                        (cons-env (list->vector vars) #f env)))
           (cb (p1-cmpl-body cmpl body new-env #f rdepth))
           (ci (list->vector (map (lambda (e)
                                    (p1-compile-exp cmpl e new-env #f rdepth))
                                  inits))))
      (unless (null? vars)
        (rdepth-add! rdepth -1))
      (vector p2-syntax-id-letrec
              (list->vector (p1-insert-assign-flg vars new-env 0))
              ci
              cb))))

(define (p1-syntax-handler-letrec* cmpl exp env toplevel-p rdepth)
  (let-values (((name vars inits body) (p1-decons-let cmpl exp)))
    (when name
      (error "malformed letrec*" exp))
    (let* ((new-env (if (null? vars)
                        env
                        (cons-env (list->vector vars) #f env)))
           (cb (p1-cmpl-body cmpl body new-env #f rdepth))
           (ci (list->vector (map (lambda (e)
                                    (p1-compile-exp cmpl e new-env #f rdepth))
                                  inits))))
      (unless (null? vars)
        (rdepth-add! rdepth -1))
      (vector p2-syntax-id-letrec*
              (list->vector (p1-insert-assign-flg vars new-env 0))
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
                        (cons-env (list->vector vars) #f env)))
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
                (list->vector (p1-insert-assign-flg vars new-env 0))
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
                        (cons-env all-vars-vec #f env)))
           (cb (p1-cmpl-body cmpl body new-env #f rdepth))
           (mv (let ((cnt 0))
                 (map (lambda (f)
                        (list->vector
                         (let rec ((vars (car f)))
                           (if (null? vars)
                               '()
                               (let ((flg (assigned-variable? new-env
                                                              cnt 0)))
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
                                (cons-env vars-vec vv e)))
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
                                          (assigned-variable? new-env idx 0))
                                    (rec (+ idx 1) (cdr v)))))))
                      (vector vv)
                      (vector (p1-compile-exp cmpl (car in) e #f rdepth))
                      b
                      (vector-length vars-vec))))))))

(define (p1-decons-with-module cmpl exp)
  (let ((x (cdr exp)))
    (unless (pair? x)
      (compile-error cmpl "malformed with-module"))
    x))

(define (p1-syntax-handler-with-module cmpl exp env toplevel-p rdepth)
  (let* ((x (p1-decons-with-module cmpl exp))
         (name (car x))
         (exps (cdr x))
         (mod (current-module-name cmpl)))
    (compiler-select-module! cmpl name)
    (list->vector (cons p2-syntax-id-begin
                        (map (lambda (e)
                               (p1-compile-exp cmpl e env toplevel-p rdepth)))))
    (compiler-select-module! cmpl mod)))

(define (p1-decons-select-module cmpl exp)
  (let ((x (cdr exp)))
    (unless (and (pair? x) (null? (cdr x)))
      (compile-error cmpl "malformed select-module"))
    (car x)))

(define (p1-syntax-handler-select-module cmpl exp env toplevel-p rdepth)
  (compiler-select-module! cmpl (p1-decons-select-module cmpl exp))
  (vector p2-syntax-id-begin))

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

(define compiler-syntax-with-module
  (make-syntax 'with-module p1-syntax-handler-with-module))

(define compiler-syntax-select-module
  (make-syntax 'select-module p1-syntax-handler-select-module))

(p1-register-syntax '(scheme base) compiler-syntax-definition #t)
(p1-register-syntax '(scheme base) compiler-syntax-begin #t)
(p1-register-syntax '(scheme base) compiler-syntax-quote #t)
(p1-register-syntax '(scheme base) compiler-syntax-lambda #t)
(p1-register-syntax '(scheme base) compiler-syntax-assignment #t)
(p1-register-syntax '(scheme base) compiler-syntax-if #t)
(p1-register-syntax '(scheme base) compiler-syntax-cond #t)
(p1-register-syntax '(scheme base) compiler-syntax-and #t)
(p1-register-syntax '(scheme base) compiler-syntax-or #t)
(p1-register-syntax '(scheme base) compiler-syntax-when #t)
(p1-register-syntax '(scheme base) compiler-syntax-unless #t)
(p1-register-syntax '(scheme base) compiler-syntax-let #t)
(p1-register-syntax '(scheme base) compiler-syntax-let* #t)
(p1-register-syntax '(scheme base) compiler-syntax-letrec #t)
(p1-register-syntax '(scheme base) compiler-syntax-letrec* #t)
(p1-register-syntax '(scheme base) compiler-syntax-do #t)
(p1-register-syntax '(scheme base) compiler-syntax-let-values #t)
(p1-register-syntax '(scheme base) compiler-syntax-let*-values #t)
(p1-register-syntax '(scheme base) compiler-syntax-with-module #t)
(p1-register-syntax '(scheme base) compiler-syntax-select-module #t)


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
