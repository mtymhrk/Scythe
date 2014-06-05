
(with-module (scythe internal compile)

  ;; TODO 以下の定数は C 側で定義する
  ;; (define +number-of-padding-arity-check+ 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (new-cseq)
    (cons '() '()))

  (define (cseq-code cseq)
    (car cseq))

  (define (cseq-unshift cseq elm)
    (set-car! cseq (cons elm (cseq-code cseq))))

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
  (define (unshift-inst inst cseq) (cseq-unshift cseq inst))

  (define (unshift-inst-nop cseq)
    (unshift-inst (cons-inst 'nop) cseq))
  (define (unshift-inst-undef cseq)
    (unshift-inst (cons-inst 'undef) cseq))
  (define (unshift-inst-call narg cseq)
    (unshift-inst (cons-inst 'call narg) cseq))
  (define (unshift-inst-tcall narg cseq)
    (unshift-inst (cons-inst 'tcall narg) cseq))
  (define (unshift-inst-return cseq)
    (unshift-inst (cons-inst 'return) cseq))
  (define (unshift-inst-frame cseq)
    (unshift-inst (cons-inst 'frame) cseq))
  (define (unshift-inst-cframe cseq)
    (unshift-inst (cons-inst 'cframe) cseq))
  (define (unshift-inst-eframe cseq)
    (unshift-inst (cons-inst 'eframe) cseq))
  (define (unshift-inst-ecommit narg cseq)
    (unshift-inst (cons-inst 'ecommit narg) cseq))
  (define (unshift-inst-epop cseq)
    (unshift-inst (cons-inst 'epop) cseq))
  (define (unshift-inst-erebind narg cseq)
    (unshift-inst (cons-inst 'erebind narg) cseq))
  (define (unshift-inst-immval val cseq)
    (unshift-inst (cons-inst 'immval val) cseq))
  (define (unshift-inst-push cseq)
    (unshift-inst (cons-inst 'push) cseq))
  (define (unshift-inst-gref sym mod cseq)
    (unshift-inst (cons-inst 'gref sym mod) cseq))
  (define (unshift-inst-gdef sym mod cseq)
    (unshift-inst (cons-inst 'gdef sym mod) cseq))
  (define (unshift-inst-gset sym mod cseq)
    (unshift-inst (cons-inst 'gset sym mod) cseq))
  (define (unshift-inst-sref idx layer cseq)
    (unshift-inst (cons-inst 'sref idx layer) cseq))
  (define (unshift-inst-sset idx layer cseq)
    (unshift-inst (cons-inst 'sset idx layer) cseq))
  (define (unshift-inst-jmp lbl cseq)
    (unshift-inst (cons-inst 'jmp lbl) cseq))
  (define (unshift-inst-jmpt lbl cseq)
    (unshift-inst (cons-inst 'jmpt lbl) cseq))
  (define (unshift-inst-jmpf lbl cseq)
    (unshift-inst (cons-inst 'jmpf lbl) cseq))
  (define (unshift-inst-box idx layer cseq)
    (unshift-inst (cons-inst 'box idx layer) cseq))
  (define (unshift-inst-demine idx layer cseq)
    (unshift-inst (cons-inst 'demine idx layer) cseq))
  (define (unshift-inst-emine narg cseq)
    (unshift-inst (cons-inst 'emine narg) cseq))
  (define (unshift-inst-edemine narg layer cseq)
    (unshift-inst (cons-inst 'edemine narg layer) cseq))
  (define (unshift-inst-arity arity cseq)
    (unshift-inst (cons-inst 'arity arity) cseq))
  (define (unshift-inst-label lbl cseq)
    (unshift-inst (cons-inst 'label lbl) cseq))
  (define (unshift-inst-asm-close nr-free arity code cseq)
    (unshift-inst (cons-inst 'asm-close nr-free arity code) cseq))
  (define (unshift-arity-check arity cseq)
    (if (<= arity 1)
        (unshift-inst-arity arity cseq)
        (let rec ((i 0) (cseq cseq))
          (if (>= i number-of-padding-arity-check)
              cseq
              (rec (+ i 1) (unshift-inst-nop cseq))))))

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
     ((null? arg) (make-compiler))
     ((compiler? (car arg)) (car arg))
     (else (make-compiler (car arg)))))

  (define (compile exp . arg)
    (let ((cmpl (get-compiler-from-arg arg))
          (cseq (new-cseq)))
      (compile-exp cmpl exp (new-env) 1 #f #t (new-rdepth) cseq)
      (cseq-code cseq)))

  (define (compile-file file . arg)
    (let ((cmpl (get-compiler-from-arg arg))
          (port (open-input-file file)))
      (let rec ((exp (read port)))
        (if (eof-object? exp)
            '()
            (let ((cseq (new-cseq)))
              (compile-exp cmpl exp (new-env) 1 #f #t (new-rdepth) cseq)
              (append (cseq-code cseq) (rec (read port))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (compile-syntax-reference cmpl exp env arity tail-p toplevel-p
                                    rdepth cseq)
    (when tail-p (unshift-inst-return cseq))
    (let ((rslv (resolve-reference env exp #f rdepth)))
      (if rslv
          (unshift-inst-sref (car rslv) (cdr rslv) cseq)
          (unshift-inst-gref exp (current-module-name cmpl) cseq))))

  (define (compile-syntax-self-eval cmpl exp env arity tail-p toplevel-p
                                    rdepth cseq)
    (when tail-p (unshift-inst-return cseq))
    (unshift-inst-immval exp cseq))

  (define (decons-exp-application cmpl exp)
    (cons (car exp) (list->vector (cdr exp))))

  (define (cmpl-application cmpl proc args env arity tail-p toplevel-p
                            rdepth cseq)
    (let ((nr-args (vector-length args)))
      (if tail-p
          (unshift-inst-tcall nr-args cseq)
          (begin
            (unshift-arity-check arity cseq)
            (unshift-inst-call nr-args cseq)))
      (compile-exp cmpl proc env 1 #f toplevel-p rdepth cseq)
      (let loop ((idx (- nr-args 1)))
        (when (>= idx 0)
          (unshift-inst-push cseq)
          (compile-exp cmpl (vector-ref args idx) env 1 #f toplevel-p
                       rdepth cseq)
          (loop (- idx 1))))
      (if tail-p
          (when (> nr-args 0)
            (unshift-inst-eframe cseq))
          (if (> nr-args 0)
              (unshift-inst-frame cseq)
              (unshift-inst-cframe cseq)))))

  (define (compile-syntax-application cmpl exp env arity tail-p toplevel-p
                                      rdepth cseq)
    (let ((x (decons-exp-application cmpl exp)))
      (cmpl-application cmpl (car x) (cdr x) env arity tail-p toplevel-p
                        rdepth cseq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define syntaxes '())

  (define (register-syntax sym handler)
    (set! syntaxes (cons (cons sym handler) syntaxes)))

  (define (search-syntax sym)
    (let loop ((lst syntaxes))
      (if (null? lst)
          #f
          (if (eq? sym (car (car lst)))
              (cdr (car lst))
              (loop (cdr lst))))))

  (define (syntax cmpl exp env tail-p toplevel-p)
    (cond ((symbol? exp)
           compile-syntax-reference)
          ((pair? exp)
           (let ((key (car exp)))
             (if (symbol? key)
                 (if (resolve-reference env key #f (new-rdepth))
                     compile-syntax-application
                     (let ((f (search-syntax key)))
                       (if f
                           f
                           compile-syntax-application)))
                 compile-syntax-application)))
          (else
           compile-syntax-self-eval)))

  (define (compile-exp cmpl exp env arity tail-p toplevel-p rdepth cseq)
    (let ((ce (compiler-current-expr cmpl)))
      (compiler-select-expr! cmpl exp)
      (rdepth-expand! rdepth)
      ((syntax cmpl exp env tail-p toplevel-p)
       cmpl exp env arity tail-p toplevel-p rdepth cseq)
      (rdepth-contract! rdepth)
      (compiler-select-expr! cmpl ce)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (cmpl-empty cmpl exp env arity tail-p toplevel-p rdepth cseq)
    (when tail-p
      (unshift-inst-return cseq))
    (unshift-inst-undef cseq))

  (define (cmpl-exp-list cmpl explst env arity tail-p toplevel-p
                         rdepth cseq)
    (unless (list? explst)
      (compile-error cmpl "malformed expression"))
    (let* ((expvec (list->vector explst))
           (len (vector-length expvec)))
      (if (= len 0)
          (cmpl-empty cmpl '() env arity tail-p toplevel-p rdepth cseq)
          (compile-exp cmpl (vector-ref expvec (- len 1)) env
                       arity tail-p toplevel-p rdepth cseq))
      (let loop ((idx (- len 2)))
        (when (>= idx 0)
          (compile-exp cmpl (vector-ref expvec idx)
                       env -1 #f toplevel-p rdepth cseq)
          (loop (- idx 1))))))

  (define (normalize-definition cmpl exp)
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

  (define (decons-definition cmpl exp)
    (when (not (= (length exp) 3))
      (compile-error cmpl "malformed define"))
    (let ((var (car (cdr exp)))
          (val (car (cdr (cdr exp)))))
      (when (not (symbol? var))
        (compile-error cmpl "malformed define"))
      (cons var val)))

  (define (compile-syntax-definition cmpl exp env arity tail-p toplevel-p
                                     rdepth cseq)
    (when (not toplevel-p)
      (compile-error cmpl "definition can apper in the toplevel or beginning of a <body>"))
    (let ((var-val (decons-definition cmpl (normalize-definition cmpl exp))))
      (when tail-p
        (unshift-inst-return cseq))
      (unshift-inst-gdef (car var-val) (current-module-name cmpl) cseq)
      (compile-exp cmpl (cdr var-val) env 1 #f toplevel-p rdepth cseq)))

  (define (decons-begin cmpl exp)
    (cdr exp))

  (define (compile-syntax-begin cmpl exp env arity tail-p toplevel-p
                                rdepth cseq)
    (cmpl-exp-list cmpl (decons-begin cmpl exp) env
                   arity tail-p toplevel-p
                   rdepth cseq))

  (define (decons-body-aux cmpl body env tail-p toplevel-p
                           vars inits exps)
    (cond ((null? body)
           (vector vars inits exps))
          ((not (pair? body))
           (compile-error cmpl "malformed <body>"))
          (else
           (let* ((exp (car body))
                  (syn (syntax cmpl exp env tail-p toplevel-p)))
             (cond
              ((eq? syn compile-syntax-definition)
               (let ((x (decons-definition cmpl exp)))
                 (decons-body-aux cmpl (cdr body) env tail-p toplevel-p
                                  (cons (car x) vars)
                                  (cons (cdr x) inits)
                                  exps)))
              ((eq? syn compile-syntax-begin)
               (let ((x (decons-body-aux cmpl (decons-begin cmpl exp) env
                                         tail-p toplevel-p
                                         vars inits exps)))
                 (if (eq? (vector-ref x 2) exps)
                     (decons-body-aux cmpl (cdr body) env tail-p toplevel-p
                                      (cons (vector-ref x 0) vars)
                                      (cons (vector-ref x 1) inits)
                                      exps)
                     (begin
                       (unless (null? (cdr body))
                         (vector-set! x 2 (cons (cdr body) (vector-ref x 2))))
                       x))))
              (else
               (vector vars inits (cons body exps))))))))


  (define (decons-body cmpl body env tail-p toplevel-p)
    (let* ((x (decons-body-aux cmpl body env tail-p toplevel-p '() '() '()))
           (y (vector-ref x 2)))
      (vector-set! x 0 (list->vector (reverse (vector-ref x 0))))
      (vector-set! x 1 (list->vector (reverse (vector-ref x 1))))
      (vector-set! x 2 (if (null? y)
                           '(())
                           (reverse y)))
      x))

  (define (cmpl-body cmpl body env arity tail-p toplevel-p rdepth cseq)
    (let* ((x (decons-body cmpl body env tail-p toplevel-p))
           (vars (vector-ref x 0))
           (inits (vector-ref x 1))
           (exps (vector-ref x 2))
           (nr-vars (vector-length vars))
           (env (if (> nr-vars 0)
                    (cons-env vars #f env)
                    env)))
      (when (and (not tail-p) (> nr-vars 0))
        (unshift-inst-epop cseq))
      (let loop ((exps exps))
        (unless (null? exps)
          (cmpl-exp-list cmpl (car exps) env
                         arity tail-p toplevel-p rdepth cseq)
          (loop (cdr exps))))
      (when (> nr-vars 0)
        (let loop ((idx (- nr-vars 1)))
          (when (>= idx 0)
            (unshift-inst-demine idx 0 cseq)
            (compile-exp cmpl (vector-ref inits idx) env 1 #f toplevel-p
                         rdepth cseq)
            (loop (- idx 1))))
        (unshift-inst-emine nr-vars cseq)
        (rdepth-add! rdepth -1))))

  (define (decons-quote cmpl exp)
    (let ((lst (cdr exp)))
      (unless (pair? lst)
        (compile-error cmpl "malformed quote"))
      (unless (null? (cdr lst))
        (error "faild to compile: malformed quote" exp))
      (car lst)))

  (define (compile-syntax-quote cmpl exp env arity tail-p toplevel-p rdepth cseq)
    (compile-syntax-self-eval cmpl (decons-quote cmpl exp) env
                              arity tail-p toplevel-p
                              rdepth cseq))

  (define (decons-lambda cmpl exp)
    (unless (pair? (cdr exp))
      (compile-error cmpl "malformed lambda"))
    (let ((formals (car (cdr exp)))
          (body (cdr (cdr exp))))
      (let loop ((fm formals)
                 (params '())
                 (vparam #f))
        (cond
         ((symbol? fm)
          (loop '() (cons fm params) #t))
         ((pair? fm)
          (unless (symbol? (car fm))
            (compile-error cmpl "malformed lambda"))
          (loop (cdr fm) (cons (car fm) params) vparam))
         ((null? fm)
          (vector (list->vector (reverse params)) vparam body))
         (else
          (compile-error cmpl "malformed lambda"))))
      ))

  (define (cmpl-closure-body cmpl body nparam env arity tail-p toplevel-p
                             rdepth cseq)
    (cmpl-body cmpl body env arity tail-p toplevel-p rdepth cseq)
    (let loop ((idx 0))
      (when (< idx nparam)
        (when (assigned-variable? env idx 0)
          (unshift-inst-box idx 0 cseq))
        (loop (+ idx 1)))))

  (define (cmpl-lambda cmpl params vparam body env arity tail-p toplevel-p
                       rdepth cseq)
    (rdepth-expand! rdepth)
    (let* ((nparam (vector-length params))
           (env (if (> nparam 0)
                    (cons-env params vparam env)
                    env))
           (body-cseq (new-cseq)))

      (cmpl-closure-body cmpl body nparam env -1 #t #f rdepth body-cseq)
      (when (> nparam 0)
        (rdepth-add! rdepth -1))
      (when tail-p
        (unshift-inst-return cseq))
      (unshift-inst-asm-close (if (>= (rdepth-ref rdepth) 0)
                                  (+ (rdepth-ref rdepth) 1)
                                  0)
                              (if vparam
                                  (- 0 nparam)
                                  nparam)
                              (cseq-code body-cseq)
                              cseq))
    (rdepth-contract! rdepth))

  (define (compile-syntax-lambda cmpl exp env arity tail-p toplevel-p rdepth cseq)
    (let ((x (decons-lambda cmpl exp)))
      (cmpl-lambda cmpl (vector-ref x 0) (vector-ref x 1) (vector-ref x 2)
                   env arity tail-p toplevel-p rdepth cseq)))

  (define (decons-assignment cmpl exp)
    (unless (list? exp)
      (compile-error cmpl "malformed if"))
    (unless (= (length exp) 3)
      (compile-error cmpl "malformed assignment"))
    (let ((var (list-ref exp 1))
          (val (list-ref exp 2)))
      (unless (symbol? var)
        (compile-error cmpl "malformed assignment"))
      (cons var val)))

  (define (compile-syntax-assignment cmpl exp env arity tail-p toplevel-p
                                     rdepth cseq)
    (let* ((x (decons-assignment cmpl exp))
           (var (car x))
           (val (cdr x))
           (rslv (resolve-reference env var #t rdepth)))
      (when tail-p
        (unshift-inst-return cseq))
      (if rslv
          (unshift-inst-sset (car rslv) (cdr rslv) cseq)
          (unshift-inst-gset var (current-module-name cmpl) cseq))
      (compile-exp cmpl val env 1 #f toplevel-p rdepth cseq)))

  (define (decons-if cmpl exp)
    (unless (list? exp)
      (compile-error cmpl "malformed if"))
    (let ((len (length exp)))
      (cond ((= len 3)
             (vector (list-ref exp 1) (list-ref exp 2) '() #f))
            ((= len 4)
             (vector (list-ref exp 1) (list-ref exp 2) (list-ref exp 3) #t))
            (else
             (compile-error cmpl "malformed if")))))

  (define (compile-syntax-if cmpl exp env arity tail-p toplevel-p rdepth cseq)
    (let* ((x (decons-if cmpl exp))
           (condi (vector-ref x 0))
           (conse (vector-ref x 1))
           (alter (vector-ref x 2))
           (exist (vector-ref x 3))
           (lbl-j '())
           (lbl-a '()))
      (unless tail-p
        (set! lbl-j (generate-label cmpl "if-j"))
        (unshift-inst-label lbl-j cseq))
      (if exist
          (compile-exp cmpl alter env arity tail-p toplevel-p rdepth cseq)
          (cmpl-empty cmpl '()  env arity tail-p toplevel-p rdepth cseq))
      (set! lbl-a (generate-label cmpl "if-a"))
      (unshift-inst-label lbl-a cseq)
      (unless tail-p
        (unshift-inst-jmp lbl-j cseq))
      (compile-exp cmpl conse env arity tail-p toplevel-p rdepth cseq)
      (unshift-inst-jmpf lbl-a cseq)
      (compile-exp cmpl condi env 1 #f toplevel-p rdepth cseq)))

  (define (decons-cond cmpl exp)
    (let ((else-exist #f))
      (let loop ((clauses (cdr exp))
                 (test-lst '())
                 (expr-lst '()))
        (if (null? clauses)
            (begin (unless else-exist
                     (set! test-lst (cons 'else test-lst))
                     (set! expr-lst (cons '(else) expr-lst)))
                   (cons (list->vector (reverse test-lst))
                         (list->vector (reverse expr-lst))))
            (let* ((cls (car clauses))
                   (tst (car cls))
                   (rst (cdr cls)))
              (when (eq? tst 'else)
                (when else-exist
                  (compile-error cmpl "malformed cond"))
                (set! else-exist #t))
              (if (null? rst)
                  (set! rst (cons (if else-exist 'else '>>) rst))
                  (if (pair? rst)
                      (if (eq? (car rst) '=>)
                          (when (or else-exist (not (= (length rst) 2)))
                            (compile-error cmpl "malformed cond"))
                          (set! rst (cons (if else-exist 'else '>>) rst)))
                      (compile-error cmpl "malformed cond")))
              (loop (cdr clauses)
                    (cons tst test-lst)
                    (cons rst expr-lst)))))))

  (define (cmpl-cond-clause cmpl explst junc env arity tail-p toplevel-p
                            rdepth cseq)
    (let ((type (car explst))
          (exps (cdr explst))
          (lbl #f))
      (cond ((eq? type 'else)
             (when junc
               (unshift-inst-jmp junc cseq))
             (cmpl-exp-list cmpl exps env arity tail-p toplevel-p
                            rdepth cseq))
            ((eq? type '>>)
             (when (or tail-p (not (null? exps)))
               (set! lbl (generate-label cmpl "cond-c"))
               (when junc
                 (unshift-inst-jmp junc cseq))
               (if (null? exps)
                   (unshift-inst-return cseq)
                   (cmpl-exp-list cmpl exps env arity tail-p toplevel-p
                                  rdepth cseq))))
            ((eq? type '=>)
             (set! lbl (generate-label cmpl "cond-c"))
             (when junc
               (unshift-inst-jmp junc cseq))
             (if tail-p
                 (unshift-inst-tcall 1 cseq)
                 (begin (unshift-arity-check arity cseq)
                        (unshift-inst-call 1 cseq)))
             (compile-exp cmpl (car exps) env 1 #f toplevel-p rdepth cseq)
             (unshift-inst-push cseq)
             (if tail-p
                 (unshift-inst-eframe cseq)
                 (unshift-inst-frame cseq))))
      (when lbl
        (unshift-inst-label lbl cseq))
      lbl))

  (define (compile-syntax-cond cmpl exp env arity tail-p toplevel-p rdepth cseq)
    (let* ((x (decons-cond cmpl exp))
           (test-vec (car x))
           (expr-vec (cdr x))
           (nr-clauses (vector-length test-vec))
           (lbl-j #f)
           (lbl-c (make-vector nr-clauses #f)))
      (unless tail-p
        (set! lbl-j (generate-label cmpl "cond-j"))
        (unshift-inst-label lbl-j cseq))
      (let loop ((idx 0) (nc 0))
        (when (< idx nr-clauses)
          (let ((lbl (cmpl-cond-clause cmpl
                                       (vector-ref expr-vec idx)
                                       (if (= nc 0) #f lbl-j)
                                       env arity tail-p toplevel-p
                                       rdepth cseq)))
            (vector-set! lbl-c idx lbl)
            (loop (+ idx 1) (if lbl (+ nc 1) nc)))))
      (let loop ((idx (- nr-clauses 2 )))
        (when (>= idx 0)
          (let ((exp (vector-ref test-vec idx))
                (lbl (vector-ref lbl-c idx)))
            (cond (lbl   (unshift-inst-jmpt lbl cseq))
                  (lbl-j (unshift-inst-jmpt lbl-j cseq)))
            (compile-exp cmpl exp env 1 #f toplevel-p rdepth cseq))
          (loop (- idx 1))))))

  (define (decons-and cmpl exp)
    (unless (list? exp)
      (compile-error cmpl "malformed and"))
    (list->vector (cdr exp)))

  (define (compile-syntax-and cmpl exp env arity tail-p toplevel-p rdepth cseq)
    (let* ((tests (decons-and cmpl exp))
           (nr-tests (vector-length tests))
           (lbl-j (generate-label cmpl "and-j")))
      (when (= nr-tests 0)
        (when tail-p
          (unshift-inst-return cseq))
        (unshift-inst-immval #t cseq))
      (when (> nr-tests 1)
        (when tail-p
          (unshift-inst-return cseq))
        (unshift-inst-label lbl-j cseq))
      (when (> nr-tests 0)
        (compile-exp cmpl (vector-ref tests (- nr-tests 1)) env
                     1 tail-p toplevel-p rdepth cseq))
      (let loop ((idx (- nr-tests 2)))
        (when (>= idx 0)
          (unshift-inst-jmpf lbl-j cseq)
          (compile-exp cmpl (vector-ref tests idx) env 1 #f toplevel-p
                       rdepth cseq)
          (loop (- idx 1))))))

  (define (decons-or cmpl exp)
    (unless (list? exp)
      (compile-error cmpl "malformed or"))
    (list->vector (cdr exp)))

  (define (compile-syntax-or cmpl exp env arity tail-p toplevel-p rdepth cseq)
    (let* ((tests (decons-or cmpl exp))
           (nr-tests (vector-length tests))
           (lbl-j (generate-label cmpl "or-j")))
      (when (= nr-tests 0)
        (when tail-p
          (unshift-inst-return cseq))
        (unshift-inst-immval #f cseq))
      (when (> nr-tests 1)
        (when tail-p
          (unshift-inst-return cseq))
        (unshift-inst-label lbl-j cseq))
      (when (> nr-tests 0)
        (compile-exp cmpl (vector-ref tests (- nr-tests 1)) env
                     1 tail-p toplevel-p rdepth cseq))
      (let loop ((idx (- nr-tests 2)))
        (when (>= idx 0)
          (unshift-inst-jmpt lbl-j cseq)
          (compile-exp cmpl (vector-ref tests idx) env 1 #f toplevel-p
                       rdepth cseq)
          (loop (- idx 1))))))

  (define (decons-when cmpl exp)
    (let ((x (cdr exp)))
      (unless (pair? x)
        (compile-error cmpl "malformed when"))
      x))

  (define (compile-syntax-when cmpl exp env arity tail-p toplevel-p rdepth cseq)
    (let* ((x (decons-when cmpl exp))
           (test (car x))
           (exps (cdr x))
           (lbl-j (generate-label cmpl "when-j"))
           (lbl-a (generate-label cmpl "when-a")))
      (if tail-p
          (unshift-inst-return cseq)
          (unshift-inst-label lbl-j cseq))
      (unshift-inst-undef cseq)
      (unshift-inst-label lbl-a cseq)
      (unless tail-p
        (unshift-inst-jmp lbl-j cseq))
      (cmpl-exp-list cmpl exps env arity tail-p toplevel-p rdepth cseq)
      (unshift-inst-jmpf lbl-a cseq)
      (compile-exp cmpl test env 1 #f toplevel-p rdepth cseq)))

  (define (decons-unless cmpl exp)
    (let ((x (cdr exp)))
      (unless (pair? x)
        (compile-error cmpl "malformed unless"))
      x))

  (define (compile-syntax-unless cmpl exp env arity tail-p toplevel-p rdepth cseq)
    (let* ((x (decons-unless cmpl exp))
           (test (car x))
           (exps (cdr x))
           (lbl-j (generate-label cmpl "unless-j"))
           (lbl-a (generate-label cmpl "unless-a")))
      (if tail-p
          (unshift-inst-return cseq)
          (unshift-inst-label lbl-j cseq))
      (unshift-inst-undef cseq)
      (unshift-inst-label lbl-a cseq)
      (unless tail-p
        (unshift-inst-jmp lbl-j cseq))
      (cmpl-exp-list cmpl exps env arity tail-p toplevel-p rdepth cseq)
      (unshift-inst-jmpt lbl-a cseq)
      (compile-exp cmpl test env 1 #f toplevel-p rdepth cseq)))

  (define (decons-let cmpl exp)
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
            (vector name
                    (list->vector (reverse vars))
                    (list->vector (reverse inits))
                    body)
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

  (define (cmpl-named-let-body cmpl name vars inits body env
                               arity tail-p toplevel-p rdepth cseq)
    (let ((new-env (cons-env (vector name) #f env)))
      (unless tail-p
        (unshift-inst-epop cseq))
      (cmpl-application cmpl name vars new-env arity tail-p #f rdepth cseq)
      (unshift-inst-demine 0 0 cseq)
      (cmpl-lambda cmpl vars #f body new-env 1 #f #f rdepth cseq)
      (unshift-inst-emine 1 cseq)
      (rdepth-add! rdepth -1)))

  (define (compile-syntax-let cmpl exp env arity tail-p toplevel-p rdepth cseq)
    (let* ((x (decons-let cmpl exp))
           (name (vector-ref x 0))
           (vars (vector-ref x 1))
           (inits (vector-ref x 2))
           (body (vector-ref x 3))
           (nr-vars (vector-length vars))
           (new-env #f))
      (if (> nr-vars 0)
          (begin
            (set! new-env (cons-env vars #f env))
            (unless tail-p
              (unshift-inst-epop cseq)))
          (set! new-env env))
      (if name
          (cmpl-named-let-body cmpl name vars inits body new-env
                               arity tail-p #f rdepth cseq)
          (cmpl-closure-body cmpl body nr-vars new-env
                             arity tail-p #f rdepth cseq))
      (when (> nr-vars 0)
        (rdepth-add! rdepth -1)
        (unshift-inst-ecommit nr-vars cseq)
        (let loop ((idx (- nr-vars 1)))
          (when (>= idx 0)
            (unshift-inst-push cseq)
            (compile-exp cmpl (vector-ref inits idx) env
                         1 #f #f rdepth cseq)
            (loop (- idx 1))))
        (unshift-inst-eframe cseq))))

  (define (compile-syntax-let* cmpl exp env arity tail-p toplevel-p rdepth cseq)
    (let* ((x (decons-let cmpl exp))
           (name (vector-ref x 0))
           (vars (vector-ref x 1))
           (inits (vector-ref x 2))
           (body (vector-ref x 3))
           (nr-vars (vector-length vars))
           (new-env env))
      (when name
        (compile-error cmpl "malformed let*"))
      (let loop ((idx 0))
        (when (< idx nr-vars)
          (set! new-env (cons-env (vector (vector-ref vars idx))
                                  #f
                                  new-env))
          (unless tail-p
            (unshift-inst-epop cseq))
          (loop (+ idx 1))))
      (cmpl-body cmpl body new-env arity tail-p #f rdepth cseq)
      (let loop ((idx (- nr-vars 1)))
        (when (>= idx 0)
          (when (assigned-variable? new-env 0 0)
            (unshift-inst-box 0 0 cseq))
          (unshift-inst-ecommit 1 cseq)
          (unshift-inst-push cseq)
          (set! new-env (outer-env new-env))
          (rdepth-add! rdepth -1)
          (compile-exp cmpl (vector-ref inits idx) new-env
                       1 #f #f rdepth cseq)
          (unshift-inst-eframe cseq)
          (loop (- idx 1))))))

  (define (compile-syntax-letrec cmpl exp env arity tail-p toplevel-p rdepth cseq)
    (let* ((x (decons-let cmpl exp))
           (name (vector-ref x 0))
           (vars (vector-ref x 1))
           (inits (vector-ref x 2))
           (body (vector-ref x 3))
           (nr-vars (vector-length vars))
           (new-env env))
      (when name
        (error "failed to compile: malformed letrec" exp))
      (when (> nr-vars 0)
        (set! new-env (cons-env vars #f env))
        (unless tail-p
          (unshift-inst-epop cseq)))
      (cmpl-body cmpl body new-env arity tail-p #f rdepth cseq)
      (when (> nr-vars 0)
        (unshift-inst-edemine nr-vars 0 cseq)
        (let loop ((idx (- nr-vars 1)))
          (when (>= idx 0)
            (unshift-inst-push cseq)
            (compile-exp cmpl (vector-ref inits idx) new-env
                         1 #f #f rdepth cseq)
            (loop (- idx 1))))
        (unshift-inst-eframe cseq)
        (unshift-inst-emine nr-vars cseq)
        (rdepth-add! rdepth -1))))

  (define (compile-syntax-letrec* cmpl exp env arity tail-p toplevel-p
                                  rdepth cseq)
    (let* ((x (decons-let cmpl exp))
           (name (vector-ref x 0))
           (vars (vector-ref x 1))
           (inits (vector-ref x 2))
           (body (vector-ref x 3))
           (nr-vars (vector-length vars))
           (new-env env))
      (when name
        (compile-error cmpl "malformed letrec*"))
      (when (> nr-vars 0)
        (set! new-env (cons-env vars #f env))
        (unless tail-p
          (unshift-inst-epop cseq)))
      (cmpl-body cmpl body new-env arity tail-p #f rdepth cseq)
      (when (> nr-vars 0)
        (let loop ((idx (- nr-vars 1)))
          (when (>= idx 0)
            (unshift-inst-demine idx 0 cseq)
            (compile-exp cmpl (vector-ref inits idx) new-env
                         1 #f #f rdepth cseq)
            (loop (- idx 1))))
        (unshift-inst-emine nr-vars cseq)
        (rdepth-add! rdepth -1))))

  (define (decons-do cmpl exp)
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
      (let ((n (length var-cls)))
        (set! vars (make-vector n #f))
        (set! inits (make-vector n #f))
        (set! steps (make-vector n #f)))
      (let loop ((vis var-cls) (idx 0))
        (unless (null? vis)
          (unless (pair? vis)
            (compile-error cmpl "malformed do"))
          (let ((x (car vis)))
            (unless (pair? x)
              (compile-error cmpl "malformed do"))
            (vector-set! vars idx (car x))
            (set! x (cdr x))
            (unless (pair? x)
              (compile-error cmpl "malformed do"))
            (vector-set! inits idx (car x))
            (set! x (cdr x))
            (if (pair? x)
                (begin
                  (vector-set! steps idx (car x))
                  (set! x (cdr x)))
                (vector-set! steps idx (vector-ref vars idx)))
            (unless (null? x)
              (compile-error cmpl "malformed do"))
            (loop (cdr vis) (+ idx 1)))))
      (vector vars inits steps test exps cmd-cls)))

  (define (compile-syntax-do cmpl exp env arity tail-p toplevel-p rdepth cseq)
    (let* ((x (decons-do cmpl exp))
           (vars (vector-ref x 0))
           (inits (vector-ref x 1))
           (steps (vector-ref x 2))
           (test (vector-ref x 3))
           (exps (vector-ref x 4))
           (cmds (vector-ref x 5))
           (nr-vars (vector-length vars))
           (new-env env)
           (lbl-e (generate-label cmpl "do-e"))
           (lbl-s (generate-label cmpl "do-s")))
      (when (> nr-vars 0)
        (set! new-env (cons-env vars #f env))
        (unless tail-p
          (unshift-inst-epop cseq)))
      (cmpl-exp-list cmpl exps new-env arity tail-p #f rdepth cseq)
      (unshift-inst-label lbl-e cseq)
      (unshift-inst-jmp lbl-s cseq)
      (when (> nr-vars 0)
        (unshift-inst-erebind nr-vars cseq)
        (let loop ((idx (- nr-vars 1)))
          (when (>= idx 0)
            (unshift-inst-push cseq)
            (compile-exp cmpl (vector-ref steps idx) new-env
                         1 #f #f rdepth cseq)
            (loop (- idx 1))))
        (unshift-inst-eframe cseq))
      (unless (null? cmds)
        (cmpl-exp-list cmpl cmds new-env -1 #f #f rdepth cseq))
      (unshift-inst-jmpt lbl-e cseq)
      (compile-exp cmpl test new-env 1 #f #f rdepth cseq)
      (let loop ((idx (- nr-vars 1)))
        (when (>= idx 0)
          (when (assigned-variable? new-env idx 0)
            (unshift-inst-box idx 0 cseq))
          (loop (- idx 1))))
      (unshift-inst-label lbl-s cseq)
      (when (> nr-vars 0)
        (rdepth-add! rdepth -1)
        (unshift-inst-ecommit nr-vars cseq)
        (let loop ((idx (- nr-vars 1)))
          (when (>= idx 0)
            (unshift-inst-push cseq)
            (compile-exp cmpl (vector-ref inits idx) env 1 #f #f rdepth cseq)
            (loop (- idx 1))))
        (unshift-inst-eframe cseq))))

  (define (decons-with-module cmpl exp)
    (let ((x (cdr exp)))
      (unless (pair? x)
        (compile-error cmpl "malformed with-module"))
      x))

  (define (compile-syntax-with-module cmpl exp env arity tail-p toplevel-p
                                      rdepth cseq)
    (let* ((x (decons-with-module cmpl exp))
           (name (car x))
           (exps (cdr x))
           (mod (current-module-name cmpl)))
      (compiler-select-module! cmpl name)
      (cmpl-exp-list cmpl exps env arity tail-p toplevel-p rdepth cseq)
      (compiler-select-module! cmpl mod)))

  (register-syntax 'define       compile-syntax-definition)
  (register-syntax 'begin        compile-syntax-begin)
  (register-syntax 'quote        compile-syntax-quote)
  (register-syntax 'lambda       compile-syntax-lambda)
  (register-syntax 'set!         compile-syntax-assignment)
  (register-syntax 'if           compile-syntax-if)
  (register-syntax 'cond         compile-syntax-cond)
  (register-syntax 'and          compile-syntax-and)
  (register-syntax 'or           compile-syntax-or)
  (register-syntax 'when         compile-syntax-when)
  (register-syntax 'unless       compile-syntax-unless)
  (register-syntax 'let          compile-syntax-let)
  (register-syntax 'let*         compile-syntax-let*)
  (register-syntax 'letrec       compile-syntax-letrec)
  (register-syntax 'letrec*      compile-syntax-letrec*)
  (register-syntax 'do           compile-syntax-do)
  (register-syntax 'with-module  compile-syntax-with-module)

)


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


;; ;;; self eval 1
;; (test-compile 1
;;               '((immval 1)))

;; ;;; define global variable 1
;; (test-compile '(define global_var 1)
;;               '((immval 1)(gdef global_var (main))))

;; ;;; define global variable 2
;; (test-compile '(define (func x) x)
;;               '((asm-close 0 1
;;                            ((sref 0 0)(return)))(gdef func (main))))
;; ;;; refer global variable 1
;; (test-compile 'global_var
;;               '((gref global_var (main))))

;; ;;; refer global variable 2
;; (test-compile '(lambda (f1 f2) (lambda (b1 b2) global_var))
;;               '((asm-close 0 2
;;                            ((asm-close 0 2
;;                                        ((gref global_var (main))(return)))
;;                             (return)))))

;; ;;; set global variable 1
;; (test-compile '(set! global_var 'a)
;;               '((immval a)(gset global_var (main))))

;; ;;; quote
;; (test-compile ''(a b c)
;;               '((immval (a b c))))

;; ;;; application 1
;; (test-compile '(func)
;;               '((cframe)(gref func (main))(call 0)(arity 1)))

;; ;;; application 2
;; (test-compile '(func 'a 'b)
;;               '((frame)
;;                 (immval a) (push)
;;                 (immval b) (push)
;;                 (gref func (main))
;;                 (call 2)
;;                 (arity 1)))

;; ;;; application 3
;; (test-compile '((lambda (x) x) 1)
;;               '((frame)
;;                 (immval 1) (push)
;;                 (asm-close 0 1
;;                            ((sref 0 0)(return)))
;;                 (call 1)
;;                 (arity 1)))

;; ;;; application 4
;; (test-compile '((lambda (x) (lambda (y) (cons x y))) 1)
;;               '((frame)
;;                 (immval 1)(push)
;;                 (asm-close 0 1
;;                            ((asm-close 1 1
;;                                        ((eframe)
;;                                         (sref 0 1)(push)
;;                                         (sref 0 0)(push)
;;                                         (gref cons (main))
;;                                         (tcall 2)))
;;                             (return)))
;;                 (call 1)
;;                 (arity 1)))

;; ;;; application 5
;; (test-compile '(lambda () ((lambda () 1)))
;;               '((asm-close 0 0
;;                            ((asm-close 0 0 ((immval 1)(return)))
;;                             (tcall 0)))))

;; ;;; lambda 1
;; (test-compile '(lambda () 'a)
;;               '((asm-close 0 0 ((immval a)(return)))))

;; ;;; lambda 2
;; (test-compile '(lambda (v1 v2) 'a)
;;               '((asm-close 0 2 ((immval a)(return)))))

;; ;;; labmda 3
;; (test-compile '(lambda (v1 v2 . v3) 'a)
;;               '((asm-close 0 -3 ((immval a)(return)))))

;; ;;; lambda 4
;; (test-compile '(lambda v 'a)
;;               '((asm-close 0 -1 ((immval a)(return)))))

;; ;;; lambda 5
;; (test-compile '(lambda ())
;;               '((asm-close 0 0 ((undef)(return)))))

;; ;;; lambda 6
;; (test-compile '(lambda () (cons 'a 'b) (cons 'c 'd))
;;               '((asm-close 0 0
;;                            ((frame)
;;                             (immval a)(push)
;;                             (immval b)(push)
;;                             (gref cons (main))
;;                             (call 2)
;;                             (arity -1)
;;                             (eframe)
;;                             (immval c)(push)
;;                             (immval d)(push)
;;                             (gref cons (main))
;;                             (tcall 2)))))

;; ;;; let 1
;; (test-compile '(let ((x 1)(y 2)) x)
;;               '((eframe)
;;                 (immval 1)(push)
;;                 (immval 2)(push)
;;                 (ecommit 2)
;;                 (sref 0 0)
;;                 (epop)))

;; ;;; let 2
;; (test-compile '(let ())
;;               '((undef)))

;; ;;; let 3
;; (test-compile '(let ((x 1)(y 2)) (lambda () (cons x y)))
;;               '((eframe)
;;                 (immval 1)(push)
;;                 (immval 2)(push)
;;                 (ecommit 2)
;;                 (asm-close 1 0
;;                            ((eframe)
;;                             (sref 0 0)(push)
;;                             (sref 1 0)(push)
;;                             (gref cons (main))
;;                             (tcall 2)))
;;                 (epop)))

;; ;;; let 4
;; (test-compile '(let ((x 1)(y 2)) (set! y 100))
;;               '((eframe)
;;                 (immval 1)(push)
;;                 (immval 2)(push)
;;                 (ecommit 2)
;;                 (box 1 0)
;;                 (immval 100)
;;                 (sset 1 0)
;;                 (epop)))

;; ;;; let 5
;; (test-compile '(lambda () (let ((x 1)(y 2)) x))
;;               '((asm-close 0 0
;;                            ((eframe)
;;                             (immval 1)(push)
;;                             (immval 2)(push)
;;                             (ecommit 2)
;;                             (sref 0 0)
;;                             (return)))))

;; ;;; let 6
;; (test-compile '(let ((x 1)) (cons 'a 'b) (cons 'c 'd))
;;               '((eframe)
;;                 (immval 1)(push)
;;                 (ecommit 1)
;;                 (frame)
;;                 (immval a)(push)
;;                 (immval b)(push)
;;                 (gref cons (main))
;;                 (call 2)
;;                 (arity -1)
;;                 (frame)
;;                 (immval c)(push)
;;                 (immval d)(push)
;;                 (gref cons (main))
;;                 (call 2)
;;                 (arity 1)
;;                 (epop)))

;; ;;; named let 1
;; (test-compile '(let loop ((x 1)(y 2)) (loop x y))
;;               '((eframe)
;;                 (immval 1)(push)
;;                 (immval 2)(push)
;;                 (ecommit 2)
;;                 (emine 1)
;;                 (asm-close 1 2
;;                            ((eframe)
;;                             (sref 0 0)(push)
;;                             (sref 1 0)(push)
;;                             (sref 0 1)
;;                             (tcall 2)))
;;                 (demine 0 0)
;;                 (frame)
;;                 (sref 0 1)(push)
;;                 (sref 1 1)(push)
;;                 (sref 0 0)
;;                 (call 2)
;;                 (arity 1)
;;                 (epop)
;;                 (epop)))

;; ;;; named let 2
;; (test-compile '(let loop ())
;;               '((emine 1)
;;                 (asm-close 0 0 ((undef)(return)))
;;                 (demine 0 0)
;;                 (cframe)
;;                 (sref 0 0)
;;                 (call 0)
;;                 (arity 1)
;;                 (epop)))

;; ;;; named let 3
;; (test-compile '(lambda () (let loop ((x 1)(y 2)) (loop 3 4)))
;;               '((asm-close 0 0
;;                            ((eframe)
;;                             (immval 1)(push)
;;                             (immval 2)(push)
;;                             (ecommit 2)
;;                             (emine 1)
;;                             (asm-close 1 2
;;                                        ((eframe)
;;                                         (immval 3)(push)
;;                                         (immval 4)(push)
;;                                         (sref 0 1)
;;                                         (tcall 2)))
;;                             (demine 0 0)
;;                             (eframe)
;;                             (sref 0 1)(push)
;;                             (sref 1 1)(push)
;;                             (sref 0 0)
;;                             (tcall 2)))))

;; ;;; let* 1
;; (test-compile '(let* ((x 1)(y 2)) x)
;;               '((eframe)
;;                 (immval 1)(push)
;;                 (ecommit 1)
;;                 (eframe)
;;                 (immval 2)(push)
;;                 (ecommit 1)
;;                 (sref 0 1)
;;                 (epop)
;;                 (epop)))

;; ;;; let* 2
;; (test-compile '(let* ())
;;               '((undef)))

;; ;;; let* 3
;; (test-compile '(let* ((x 1)(y 2)) (lambda () (cons x y)))
;;               '((eframe)
;;                 (immval 1)(push)
;;                 (ecommit 1)
;;                 (eframe)
;;                 (immval 2)(push)
;;                 (ecommit 1)
;;                 (asm-close 2 0
;;                            ((eframe)
;;                             (sref 0 1)(push)
;;                             (sref 0 0)(push)
;;                             (gref cons (main))
;;                             (tcall 2)))
;;                 (epop)
;;                 (epop)))

;; ;;; let* 4
;; (test-compile '(let* ((x 1)(y 2)) (set! x 100))
;;               '((eframe)
;;                 (immval 1)(push)
;;                 (ecommit 1)
;;                 (box 0 0)
;;                 (eframe)
;;                 (immval 2)(push)
;;                 (ecommit 1)
;;                 (immval 100)
;;                 (sset 0 1)
;;                 (epop)
;;                 (epop)))

;; ;;; let* 5
;; (test-compile '(lambda () (let* ((x 1)(y 2)) x))
;;               '((asm-close 0 0
;;                            ((eframe)
;;                             (immval 1)(push)
;;                             (ecommit 1)
;;                             (eframe)
;;                             (immval 2)(push)
;;                             (ecommit 1)
;;                             (sref 0 1)
;;                             (return)))))

;; ;;; let* 6
;; (test-compile '(let* ((x 1)(y x)) x)
;;               '((eframe)
;;                 (immval 1)(push)
;;                 (ecommit 1)
;;                 (eframe)
;;                 (sref 0 0)(push)
;;                 (ecommit 1)
;;                 (sref 0 1)
;;                 (epop)
;;                 (epop)))

;; ;;; let* 7
;; (test-compile '(let* ((x 1)(y 2)) (cons x y) (cons y x))
;;               '((eframe)
;;                 (immval 1)(push)
;;                 (ecommit 1)
;;                 (eframe)
;;                 (immval 2)(push)
;;                 (ecommit 1)
;;                 (frame)
;;                 (sref 0 1)(push)
;;                 (sref 0 0)(push)
;;                 (gref cons (main))
;;                 (call 2)
;;                 (arity -1)
;;                 (frame)
;;                 (sref 0 0)(push)
;;                 (sref 0 1)(push)
;;                 (gref cons (main))
;;                 (call 2)
;;                 (arity 1)
;;                 (epop)
;;                 (epop)))

;; ;;; let* 8
;; (test-compile '(let ((x 1))
;;                  (lambda ()
;;                    (let* ((y 2)
;;                           (z x))
;;                      #f)))
;;               '((eframe)
;;                 (immval 1) (push)
;;                 (ecommit 1)
;;                 (asm-close 1 0 ((eframe)
;;                                 (immval 2) (push)
;;                                 (ecommit 1)
;;                                 (eframe)
;;                                 (sref 0 1) (push)
;;                                 (ecommit 1)
;;                                 (immval #f)
;;                                 (return)))
;;                 (epop)))

;; ;;; let* 9
;; (test-compile '(let ((x 1))
;;                  (lambda ()
;;                    (let* ((y 2)
;;                           (z 3))
;;                      x)))
;;               '((eframe)
;;                 (immval 1) (push)
;;                 (ecommit 1)
;;                 (asm-close 1 0 ((eframe)
;;                                 (immval 2) (push)
;;                                 (ecommit 1)
;;                                 (eframe)
;;                                 (immval 3) (push)
;;                                 (ecommit 1)
;;                                 (sref 0 2)
;;                                 (return)))
;;                 (epop)))


;; ;;; letrec 1
;; (test-compile '(letrec ((x 1)(y 2)) x)
;;               '((emine 2)
;;                 (eframe)
;;                 (immval 1)(push)
;;                 (immval 2)(push)
;;                 (edemine 2 0)
;;                 (sref 0 0)
;;                 (epop)))

;; ;;; letrec 2
;; (test-compile '(letrec ((x (lambda () y))(y 100)) x)
;;               '((emine 2)
;;                 (eframe)
;;                 (asm-close 1 0 ((sref 1 0)(return)))(push)
;;                 (immval 100)(push)
;;                 (edemine 2 0)
;;                 (sref 0 0)
;;                 (epop)))

;; ;;; letrec 3
;; (test-compile '(letrec ((x 1)(y 2)) (set! y 10) y)
;;               '((emine 2)
;;                 (eframe)
;;                 (immval 1)(push)
;;                 (immval 2)(push)
;;                 (edemine 2 0)
;;                 (immval 10)
;;                 (sset 1 0)
;;                 (sref 1 0)
;;                 (epop)))

;; ;;; letrec 4
;; (test-compile '(lambda () (letrec ((x 1)(y 2)) x))
;;               '((asm-close 0 0
;;                            ((emine 2)
;;                             (eframe)
;;                             (immval 1)(push)
;;                             (immval 2)(push)
;;                             (edemine 2 0)
;;                             (sref 0 0)
;;                             (return)))))

;; ;;; letrec 5
;; (test-compile '(letrec ())
;;               '((undef)))

;; ;;; letrec 6
;; (test-compile '(letrec ((x 1)(y 2)) (cons x y) (cons y x))
;;               '((emine 2)
;;                 (eframe)
;;                 (immval 1)(push)
;;                 (immval 2)(push)
;;                 (edemine 2 0)
;;                 (frame)
;;                 (sref 0 0)(push)
;;                 (sref 1 0)(push)
;;                 (gref cons (main))
;;                 (call 2)
;;                 (arity -1)
;;                 (frame)
;;                 (sref 1 0)(push)
;;                 (sref 0 0)(push)
;;                 (gref cons (main))
;;                 (call 2)
;;                 (arity 1)
;;                 (epop)))

;; ;;; letrec* 1
;; (test-compile '(letrec* ((x 1)(y 2)) x)
;;               '((emine 2)
;;                 (immval 1)(demine 0 0)
;;                 (immval 2)(demine 1 0)
;;                 (sref 0 0)
;;                 (epop)))

;; ;;; letrec a 2
;; (test-compile '(letrec* ((x (lambda () y))(y 100)) x)
;;               '((emine 2)
;;                 (asm-close 1 0 ((sref 1 0)(return)))(demine 0 0)
;;                 (immval 100)(demine 1 0)
;;                 (sref 0 0)
;;                 (epop)))

;; ;;; letrec a 3
;; (test-compile '(letrec* ((x 1)(y 2)) (set! y 10) y)
;;               '((emine 2)
;;                 (immval 1)(demine 0 0)
;;                 (immval 2)(demine 1 0)
;;                 (immval 10)
;;                 (sset 1 0)
;;                 (sref 1 0)
;;                 (epop)))

;; ;;; letrec a 4
;; (test-compile '(lambda () (letrec* ((x 1)(y 2)) x))
;;               '((asm-close 0 0
;;                            ((emine 2)
;;                             (immval 1)(demine 0 0)
;;                             (immval 2)(demine 1 0)
;;                             (sref 0 0)
;;                             (return)))))

;; ;;; letrec a 5
;; (test-compile '(letrec* ())
;;               '((undef)))

;; ;;; letrec a 6
;; (test-compile '(letrec* ((x 1)(y 2)) (cons x y) (cons y x))
;;               '((emine 2)
;;                 (immval 1)(demine 0 0)
;;                 (immval 2)(demine 1 0)
;;                 (frame)
;;                 (sref 0 0)(push)
;;                 (sref 1 0)(push)
;;                 (gref cons (main))
;;                 (call 2)
;;                 (arity -1)
;;                 (frame)
;;                 (sref 1 0)(push)
;;                 (sref 0 0)(push)
;;                 (gref cons (main))
;;                 (call 2)
;;                 (arity 1)
;;                 (epop)))

;; ;;; internal definition 1
;; (test-compile '(lambda () (define x 1) (define y 2) x)
;;               '((asm-close 0 0
;;                            ((emine 2)
;;                             (immval 1)(demine 0 0)
;;                             (immval 2)(demine 1 0)
;;                             (sref 0 0)
;;                             (return)))))

;; ;;; internal definition 2
;; (test-compile '(lambda () (define x (lambda () y)) (define y 100) x)
;;               '((asm-close 0 0
;;                            ((emine 2)
;;                             (asm-close 1 0 ((sref 1 0)(return)))(demine 0 0)
;;                             (immval 100)(demine 1 0)
;;                             (sref 0 0)
;;                             (return)))))

;; ;;; internal definittion 3
;; (test-compile '(lambda () (define x 1) (define y 2) (set! y 10) y)
;;               '((asm-close 0 0
;;                            ((emine 2)
;;                             (immval 1)(demine 0 0)
;;                             (immval 2)(demine 1 0)
;;                             (immval 10)
;;                             (sset 1 0)
;;                             (sref 1 0)
;;                             (return)))))

;; ;;; internal definition 4
;; (test-compile '(lambda ()
;;                  (define x 1)
;;                  (begin
;;                    (define y 2)
;;                    x))
;;               '((asm-close 0 0
;;                            ((emine 2)
;;                             (immval 1)(demine 0 0)
;;                             (immval 2)(demine 1 0)
;;                             (sref 0 0)
;;                             (return)))))

;; ;;; begin 1
;; (test-compile '(begin (cons 'a 'b) (cons 'x 'y))
;;               '((frame)
;;                 (immval a)(push)
;;                 (immval b)(push)
;;                 (gref cons (main))
;;                 (call 2)
;;                 (arity -1)
;;                 (frame)
;;                 (immval x)(push)
;;                 (immval y)(push)
;;                 (gref cons (main))
;;                 (call 2)
;;                 (arity 1)))

;; ;;; begin 2
;; (test-compile '(begin (define gvar 1))
;;               '((immval 1)
;;                 (gdef gvar (main))))


;; ;;; begin 3
;; (test-compile '(begin)
;;               '((undef)))

;; ;;; refer bound variable 1
;; (test-compile '(lambda (f1 f2) (lambda (b1 b2) b2))
;;               '((asm-close 0 2
;;                            ((asm-close 0 2
;;                                        ((sref 1 0)(return)))
;;                             (return)))))

;; ;;; refer bound variable 2
;; (test-compile '(lambda (f1 b2) (lambda (b1 b2) b2))
;;               '((asm-close 0 2
;;                            ((asm-close 0 2
;;                                        ((sref 1 0)(return)))
;;                             (return)))))

;; ;;; refer bound variable 3
;; (test-compile '(lambda (f1 b2) (lambda (b1 b2) (set! b2 'a) b2))
;;               '((asm-close 0 2
;;                            ((asm-close 0 2
;;                                        ((box 1 0)
;;                                         (immval a)(sset 1 0)
;;                                         (sref 1 0)
;;                                         (return)))
;;                             (return)))))

;; ;;; set bound variable 1
;; (test-compile '(lambda (f1 f2) (lambda (b1 b2) (set! b2 'a)))
;;               '((asm-close 0 2
;;                            ((asm-close 0 2
;;                                        ((box 1 0)(immval a)(sset 1 0)(return)))
;;                             (return)))))

;; ;;; set bound variable 2
;; (test-compile '(lambda (f1 b2) (lambda (b1 b2) (set! b2 'a)))
;;               '((asm-close 0 2
;;                            ((asm-close 0 2
;;                                        ((box 1 0)(immval a)(sset 1 0)(return)))
;;                             (return)))))

;; ;;; refer free variable 1
;; (test-compile '(lambda (f1 f2) (lambda (b1 b2) f2))
;;               '((asm-close 0 2
;;                            ((asm-close 1 2
;;                                        ((sref 1 1)(return)))
;;                             (return)))))

;; ;;; refer free variable 2
;; (test-compile '(lambda (f1 f2) (lambda (b1 b2) (set! f2 'a) f2))
;;               '((asm-close 0 2
;;                            ((box 1 0)
;;                             (asm-close 1 2
;;                                        ((immval a)(sset 1 1)
;;                                         (sref 1 1)
;;                                         (return)))
;;                             (return)))))

;; ;;; set free variable 1
;; (test-compile '(lambda (f1 f2) (lambda (b1 b2) (set! f2 'a)))
;;               '((asm-close 0 2
;;                            ((box 1 0)
;;                             (asm-close 1 2
;;                                        ((immval a)(sset 1 1)(return)))
;;                             (return)))))

;; ;;; if 1
;; (test-compile '(if 'a 'b 'c)
;;               '((immval a)
;;                 (jmpf lbl_if-a_1)
;;                 (immval b)
;;                 (jmp lbl_if-j_0)
;;                 (label lbl_if-a_1)
;;                 (immval c)
;;                 (label lbl_if-j_0)))


;; ;;; if 2
;; (test-compile '(if 'a 'b)
;;               '((immval a)
;;                 (jmpf lbl_if-a_1)
;;                 (immval b)
;;                 (jmp lbl_if-j_0)
;;                 (label lbl_if-a_1)
;;                 (undef)
;;                 (label lbl_if-j_0)))

;; ;;; if 3
;; (test-compile '(lambda () (if 'a 'b 'c))
;;               '((asm-close 0 0
;;                            ((immval a)
;;                             (jmpf lbl_if-a_0)
;;                             (immval b)
;;                             (return)
;;                             (label lbl_if-a_0)
;;                             (immval c)
;;                             (return)
;;                             ))))


;; ;;; if 4
;; (test-compile '(lambda () (if 'a 'b))
;;               '((asm-close 0 0
;;                            ((immval a)
;;                             (jmpf lbl_if-a_0)
;;                             (immval b)
;;                             (return)
;;                             (label lbl_if-a_0)
;;                             (undef)
;;                             (return)))))
;; ;;; cond 001
;; (test-compile '(cond)
;;               '((undef)
;;                 (label lbl_cond-j_0)))

;; ;;; cond 002
;; (test-compile '(cond (else))
;;               '((undef)
;;                 (label lbl_cond-j_0)))


;; ;;; cond 003
;; (test-compile '(cond (else 'a))
;;               '((immval a)
;;                 (label lbl_cond-j_0)))

;; ;;; cond 004
;; (test-compile '(cond ('a 'b))
;;               '((immval a)
;;                 (jmpt lbl_cond-c_1)
;;                 (undef)
;;                 (jmp lbl_cond-j_0)
;;                 (label lbl_cond-c_1)
;;                 (immval b)
;;                 (label lbl_cond-j_0)))

;; ;;; cond 005
;; (test-compile '(cond ('a 'b)('c 'd))
;;               '((immval a)
;;                 (jmpt lbl_cond-c_1)
;;                 (immval c)
;;                 (jmpt lbl_cond-c_2)
;;                 (undef)
;;                 (jmp lbl_cond-j_0)
;;                 (label lbl_cond-c_2)
;;                 (immval d)
;;                 (jmp lbl_cond-j_0)
;;                 (label lbl_cond-c_1)
;;                 (immval b)
;;                 (label lbl_cond-j_0)))

;; ;;; cond 006
;; (test-compile '(cond ('a => write))
;;               '((immval a)
;;                 (jmpt lbl_cond-c_1)
;;                 (undef)
;;                 (jmp lbl_cond-j_0)
;;                 (label lbl_cond-c_1)
;;                 (frame)
;;                 (push)
;;                 (gref write (main))
;;                 (call 1)
;;                 (arity 1)
;;                 (label lbl_cond-j_0)))

;; ;;; cond 007
;; (test-compile '(cond ('a => write)('b => display))
;;               '((immval a)
;;                 (jmpt lbl_cond-c_1)
;;                 (immval b)
;;                 (jmpt lbl_cond-c_2)
;;                 (undef)
;;                 (jmp lbl_cond-j_0)
;;                 (label lbl_cond-c_2)
;;                 (frame)
;;                 (push)
;;                 (gref display (main))
;;                 (call 1)
;;                 (arity 1)
;;                 (jmp lbl_cond-j_0)
;;                 (label lbl_cond-c_1)
;;                 (frame)
;;                 (push)
;;                 (gref write (main))
;;                 (call 1)
;;                 (arity 1)
;;                 (label lbl_cond-j_0)))

;; ;;; cond 008
;; (test-compile '(cond ('a))
;;               '((immval a)
;;                 (jmpt lbl_cond-j_0)
;;                 (undef)
;;                 (label lbl_cond-j_0)))

;; ;;; cond 009
;; (test-compile '(cond ('a)('b 'c))
;;               '((immval a)
;;                 (jmpt lbl_cond-j_0)
;;                 (immval b)
;;                 (jmpt lbl_cond-c_1)
;;                 (undef)
;;                 (jmp lbl_cond-j_0)
;;                 (label lbl_cond-c_1)
;;                 (immval c)
;;                 (label lbl_cond-j_0)))

;; ;;; cond 010
;; (test-compile '(cond ('a 'b)('c))
;;               '((immval a)
;;                 (jmpt lbl_cond-c_1)
;;                 (immval c)
;;                 (jmpt lbl_cond-j_0)
;;                 (undef)
;;                 (jmp lbl_cond-j_0)
;;                 (label lbl_cond-c_1)
;;                 (immval b)
;;                 (label lbl_cond-j_0)))

;; ;;; cond 011
;; (test-compile '(lambda () (cond))
;;               '((asm-close 0 0
;;                            ((undef)(return)))))

;; ;;; cond 012
;; (test-compile '(lambda () (cond (else)))
;;               '((asm-close 0 0
;;                            ((undef)(return)))))

;; ;;; cond 013
;; (test-compile '(lambda () (cond (else 'a)))
;;               '((asm-close 0 0
;;                            ((immval a)
;;                             (return)))))

;; ;;; cond 014
;; (test-compile '(lambda () (cond ('a 'b)))
;;               '((asm-close 0 0
;;                            ((immval a)
;;                             (jmpt lbl_cond-c_0)
;;                             (undef)
;;                             (return)
;;                             (label lbl_cond-c_0)
;;                             (immval b)
;;                             (return)))))

;; ;;; cond 015
;; (test-compile '(lambda () (cond ('a)))
;;               '((asm-close 0 0
;;                            ((immval a)
;;                             (jmpt lbl_cond-c_0)
;;                             (undef)
;;                             (return)
;;                             (label lbl_cond-c_0)
;;                             (return)))))

;; ;;; cond 016
;; (test-compile '(lambda () (cond ('a)('b 'c)))
;;               '((asm-close 0 0
;;                            ((immval a)
;;                             (jmpt lbl_cond-c_0)
;;                             (immval b)
;;                             (jmpt lbl_cond-c_1)
;;                             (undef)
;;                             (return)
;;                             (label lbl_cond-c_1)
;;                             (immval c)
;;                             (return)
;;                             (label lbl_cond-c_0)
;;                             (return)))))

;; ;;; cond 017
;; (test-compile '(lambda () (cond ('a 'b)('c)))
;;               '((asm-close 0 0
;;                            (   (immval a)
;;                                (jmpt lbl_cond-c_0)
;;                                (immval c)
;;                                (jmpt lbl_cond-c_1)
;;                                (undef)
;;                                (return)
;;                                (label lbl_cond-c_1)
;;                                (return)
;;                                (label lbl_cond-c_0)
;;                                (immval b)
;;                                (return)))))

;; ;;; cond 018
;; (test-compile '(lambda () (cond ('a => write)))
;;               '((asm-close 0 0
;;                            ((immval a)
;;                             (jmpt lbl_cond-c_0)
;;                             (undef)
;;                             (return)
;;                             (label lbl_cond-c_0)
;;                             (eframe)
;;                             (push)
;;                             (gref write (main))
;;                             (tcall 1)))))

;; ;;; and 001
;; (test-compile '(and)
;;               '((immval #t)))

;; ;;; and 002
;; (test-compile '(and 'a)
;;               '((immval a)))

;; ;;; and 003
;; (test-compile '(and 'a 'b)
;;               '((immval a)
;;                 (jmpf lbl_and-j_0)
;;                 (immval b)
;;                 (label lbl_and-j_0)))

;; ;;; and 004
;; (test-compile '(and (null? 'a) (null? 'b))
;;               '((frame)
;;                 (immval a)
;;                 (push)
;;                 (gref null? (main))
;;                 (call 1)
;;                 (arity 1)
;;                 (jmpf lbl_and-j_0)
;;                 (frame)
;;                 (immval b)
;;                 (push)
;;                 (gref null? (main))
;;                 (call 1)
;;                 (arity 1)
;;                 (label lbl_and-j_0)))

;; ;;; and 005
;; (test-compile '(lambda () (and))
;;               '((asm-close 0 0
;;                            ((immval #t)(return)))))

;; ;;; and 006
;; (test-compile '(lambda () (and 'a))
;;               '((asm-close 0 0
;;                            ((immval a)(return)))))

;; ;;; and 007
;; (test-compile '(lambda () (and 'a 'b))
;;               '((asm-close 0 0
;;                            ((immval a)
;;                             (jmpf lbl_and-j_0)
;;                             (immval b)
;;                             (return)
;;                             (label lbl_and-j_0)
;;                             (return)))))
;; ;;; and 008
;; (test-compile '(lambda () (and (null? 'a) (null? 'b)))
;;               '((asm-close 0 0
;;                            ((frame)
;;                             (immval a)
;;                             (push)
;;                             (gref null? (main))
;;                             (call 1)
;;                             (arity 1)
;;                             (jmpf lbl_and-j_0)
;;                             (eframe)
;;                             (immval b)
;;                             (push)
;;                             (gref null? (main))
;;                             (tcall 1)
;;                             (label lbl_and-j_0)
;;                             (return)))))

;; ;;; or 001
;; (test-compile '(or)
;;               '((immval #f)))

;; ;;; or 002
;; (test-compile '(or 'a)
;;               '((immval a)))

;; ;;; or 003
;; (test-compile '(or 'a 'b)
;;               '((immval a)
;;                 (jmpt lbl_or-j_0)
;;                 (immval b)
;;                 (label lbl_or-j_0)))

;; ;;; or 004
;; (test-compile '(or (null? 'a) (null? 'b))
;;               '((frame)
;;                 (immval a)
;;                 (push)
;;                 (gref null? (main))
;;                 (call 1)
;;                 (arity 1)
;;                 (jmpt lbl_or-j_0)
;;                 (frame)
;;                 (immval b)
;;                 (push)
;;                 (gref null? (main))
;;                 (call 1)
;;                 (arity 1)
;;                 (label lbl_or-j_0)))

;; ;;; or 005
;; (test-compile '(lambda () (or))
;;               '((asm-close 0 0
;;                            ((immval #f)(return)))))

;; ;;; or 006
;; (test-compile '(lambda () (or 'a))
;;               '((asm-close 0 0
;;                            ((immval a)(return)))))

;; ;;; or 007
;; (test-compile '(lambda () (or 'a 'b))
;;               '((asm-close 0 0
;;                            ((immval a)
;;                             (jmpt lbl_or-j_0)
;;                             (immval b)
;;                             (return)
;;                             (label lbl_or-j_0)
;;                             (return)))))

;; ;;; or 008
;; (test-compile '(lambda () (or (null? 'a) (null? 'b)))
;;               '((asm-close 0 0
;;                            ((frame)
;;                             (immval a)
;;                             (push)
;;                             (gref null? (main))
;;                             (call 1)
;;                             (arity 1)
;;                             (jmpt lbl_or-j_0)
;;                             (eframe)
;;                             (immval b)
;;                             (push)
;;                             (gref null? (main))
;;                             (tcall 1)
;;                             (label lbl_or-j_0)
;;                             (return)))))

;; ;;; when 001
;; (test-compile '(when 'a)
;;               '((immval a)
;;                 (jmpf lbl_when-a_1)
;;                 (undef)
;;                 (jmp lbl_when-j_0)
;;                 (label lbl_when-a_1)
;;                 (undef)
;;                 (label lbl_when-j_0)))

;; ;;; when 002
;; (test-compile '(when 'a 'b)
;;               '((immval a)
;;                 (jmpf lbl_when-a_1)
;;                 (immval b)
;;                 (jmp lbl_when-j_0)
;;                 (label lbl_when-a_1)
;;                 (undef)
;;                 (label lbl_when-j_0)))


;; ;;; when 003
;; (test-compile '(lambda () (when 'a))
;;               '((asm-close 0 0
;;                            ((immval a)
;;                             (jmpf lbl_when-a_1)
;;                             (undef)
;;                             (return)
;;                             (label lbl_when-a_1)
;;                             (undef)
;;                             (return)))))

;; ;;; when 004
;; (test-compile '(lambda () (when 'a 'b))
;;               '((asm-close 0 0
;;                            ((immval a)
;;                             (jmpf lbl_when-a_1)
;;                             (immval b)
;;                             (return)
;;                             (label lbl_when-a_1)
;;                             (undef)
;;                             (return)))))

;; ;;; when 005
;; (test-compile '(when (null? '()) (cons 'a 'b))
;;               '((frame)
;;                 (immval ())
;;                 (push)
;;                 (gref null? (main))
;;                 (call 1)
;;                 (arity 1)
;;                 (jmpf lbl_when-a_1)
;;                 (frame)
;;                 (immval a)
;;                 (push)
;;                 (immval b)
;;                 (push)
;;                 (gref cons (main))
;;                 (call 2)
;;                 (arity 1)
;;                 (jmp lbl_when-j_0)
;;                 (label lbl_when-a_1)
;;                 (undef)
;;                 (label lbl_when-j_0)))

;; ;;; when 006
;; (test-compile '(lambda () (when (null? '()) (cons 'a 'b)))
;;               '((asm-close 0 0
;;                            ((frame)
;;                             (immval ())
;;                             (push)
;;                             (gref null? (main))
;;                             (call 1)
;;                             (arity 1)
;;                             (jmpf lbl_when-a_1)
;;                             (eframe)
;;                             (immval a)
;;                             (push)
;;                             (immval b)
;;                             (push)
;;                             (gref cons (main))
;;                             (tcall 2)
;;                             (label lbl_when-a_1)
;;                             (undef)
;;                             (return)))))

;; ;;; when 007
;; (test-compile '(when 'z (cons 'a 'b) (cons 'c 'd))
;;               '((immval z)
;;                 (jmpf lbl_when-a_1)
;;                 (frame)
;;                 (immval a)(push)
;;                 (immval b)(push)
;;                 (gref cons (main))
;;                 (call 2)
;;                 (arity -1)
;;                 (frame)
;;                 (immval c)(push)
;;                 (immval d)(push)
;;                 (gref cons (main))
;;                 (call 2)
;;                 (arity 1)
;;                 (jmp lbl_when-j_0)
;;                 (label lbl_when-a_1)
;;                 (undef)
;;                 (label lbl_when-j_0)))

;; ;;; unless 001
;; (test-compile '(unless 'a)
;;               '((immval a)
;;                 (jmpt lbl_unless-a_1)
;;                 (undef)
;;                 (jmp lbl_unless-j_0)
;;                 (label lbl_unless-a_1)
;;                 (undef)
;;                 (label lbl_unless-j_0)))

;; ;;; unless 002
;; (test-compile '(unless 'a 'b)
;;               '((immval a)
;;                 (jmpt lbl_unless-a_1)
;;                 (immval b)
;;                 (jmp lbl_unless-j_0)
;;                 (label lbl_unless-a_1)
;;                 (undef)
;;                 (label lbl_unless-j_0)))

;; ;;; unless 003
;; (test-compile '(lambda () (unless 'a))
;;               '((asm-close 0 0
;;                            ((immval a)
;;                             (jmpt lbl_unless-a_1)
;;                             (undef)
;;                             (return)
;;                             (label lbl_unless-a_1)
;;                             (undef)
;;                             (return)))))

;; ;;; unless 004
;; (test-compile '(lambda () (unless 'a 'b))
;;               '((asm-close 0 0
;;                            ((immval a)
;;                             (jmpt lbl_unless-a_1)
;;                             (immval b)
;;                             (return)
;;                             (label lbl_unless-a_1)
;;                             (undef)
;;                             (return)))))

;; ;;; unless 005
;; (test-compile '(unless (null? '()) (cons 'a 'b))
;;               '((frame)
;;                 (immval ())
;;                 (push)
;;                 (gref null? (main))
;;                 (call 1)
;;                 (arity 1)
;;                 (jmpt lbl_unless-a_1)
;;                 (frame)
;;                 (immval a)
;;                 (push)
;;                 (immval b)
;;                 (push)
;;                 (gref cons (main))
;;                 (call 2)
;;                 (arity 1)
;;                 (jmp lbl_unless-j_0)
;;                 (label lbl_unless-a_1)
;;                 (undef)
;;                 (label lbl_unless-j_0)))

;; ;;; unless 006
;; (test-compile '(lambda () (unless (null? '()) (cons 'a 'b)))
;;               '((asm-close 0 0
;;                            ((frame)
;;                             (immval ())
;;                             (push)
;;                             (gref null? (main))
;;                             (call 1)
;;                             (arity 1)
;;                             (jmpt lbl_unless-a_1)
;;                             (eframe)
;;                             (immval a)
;;                             (push)
;;                             (immval b)
;;                             (push)
;;                             (gref cons (main))
;;                             (tcall 2)
;;                             (label lbl_unless-a_1)
;;                             (undef)
;;                             (return)))))

;; ;;; unless 007
;; (test-compile '(unless 'z (cons 'a 'b) (cons 'c 'd))
;;               '((immval z)
;;                 (jmpt lbl_unless-a_1)
;;                 (frame)
;;                 (immval a)(push)
;;                 (immval b)(push)
;;                 (gref cons (main))
;;                 (call 2)
;;                 (arity -1)
;;                 (frame)
;;                 (immval c)(push)
;;                 (immval d)(push)
;;                 (gref cons (main))
;;                 (call 2)
;;                 (arity 1)
;;                 (jmp lbl_unless-j_0)
;;                 (label lbl_unless-a_1)
;;                 (undef)
;;                 (label lbl_unless-j_0)))

;; ;;; do 001
;; (test-compile '(do ((x 'ix 'sx)
;;                     (y 'iy 'sy))
;;                    ('t 'e)
;;                  'c)
;;               '((eframe)
;;                 (immval ix)
;;                 (push)
;;                 (immval iy)
;;                 (push)
;;                 (ecommit 2)
;;                 (label lbl_do-s_1)
;;                 (immval t)
;;                 (jmpt lbl_do-e_0)
;;                 (immval c)
;;                 (eframe)
;;                 (immval sx)
;;                 (push)
;;                 (immval sy)
;;                 (push)
;;                 (erebind 2)
;;                 (jmp lbl_do-s_1)
;;                 (label lbl_do-e_0)
;;                 (immval e)
;;                 (epop)))

;; ;;; do 002
;; (test-compile '(do ((x 'ix)
;;                     (y 'iy 'sy))
;;                    ('t 'e)
;;                  'c)
;;               '((eframe)
;;                 (immval ix)
;;                 (push)
;;                 (immval iy)
;;                 (push)
;;                 (ecommit 2)
;;                 (label lbl_do-s_1)
;;                 (immval t)
;;                 (jmpt lbl_do-e_0)
;;                 (immval c)
;;                 (eframe)
;;                 (sref 0 0)
;;                 (push)
;;                 (immval sy)
;;                 (push)
;;                 (erebind 2)
;;                 (jmp lbl_do-s_1)
;;                 (label lbl_do-e_0)
;;                 (immval e)
;;                 (epop)))

;; ;;; do 003
;; (test-compile '(do ((x 'ix 'sx)
;;                     (y 'iy 'sy))
;;                    ('t 'e)
;;                  (set! y 1))
;;               '((eframe)
;;                 (immval ix)
;;                 (push)
;;                 (immval iy)
;;                 (push)
;;                 (ecommit 2)
;;                 (label lbl_do-s_1)
;;                 (box 1 0)
;;                 (immval t)
;;                 (jmpt lbl_do-e_0)
;;                 (immval 1)
;;                 (sset 1 0)
;;                 (eframe)
;;                 (immval sx)
;;                 (push)
;;                 (immval sy)
;;                 (push)
;;                 (erebind 2)
;;                 (jmp lbl_do-s_1)
;;                 (label lbl_do-e_0)
;;                 (immval e)
;;                 (epop)))

;; ;;; do 004
;; (test-compile '(do ()
;;                    ('t)
;;                  )
;;               '((label lbl_do-s_1)
;;                 (immval t)
;;                 (jmpt lbl_do-e_0)
;;                 (jmp lbl_do-s_1)
;;                 (label lbl_do-e_0)
;;                 (undef)))

;; ;;; do 005
;; (test-compile '(lambda ()
;;                  (do ((x 'ix 'sx)
;;                       (y 'iy 'sy))
;;                      ('t 'e)
;;                    'c))
;;               '((asm-close 0 0
;;                            ((eframe)
;;                             (immval ix)
;;                             (push)
;;                             (immval iy)
;;                             (push)
;;                             (ecommit 2)
;;                             (label lbl_do-s_1)
;;                             (immval t)
;;                             (jmpt lbl_do-e_0)
;;                             (immval c)
;;                             (eframe)
;;                             (immval sx)
;;                             (push)
;;                             (immval sy)
;;                             (push)
;;                             (erebind 2)
;;                             (jmp lbl_do-s_1)
;;                             (label lbl_do-e_0)
;;                             (immval e)
;;                             (return)))))

;; ;;; do 006
;; (test-compile '(lambda ()
;;                  (do ((x 'ix 'sx)
;;                       (y 'iy 'sy))
;;                      ('t (cons 'a 'b))
;;                    'c))
;;               '((asm-close 0 0
;;                            ((eframe)
;;                             (immval ix)
;;                             (push)
;;                             (immval iy)
;;                             (push)
;;                             (ecommit 2)
;;                             (label lbl_do-s_1)
;;                             (immval t)
;;                             (jmpt lbl_do-e_0)
;;                             (immval c)
;;                             (eframe)
;;                             (immval sx)
;;                             (push)
;;                             (immval sy)
;;                             (push)
;;                             (erebind 2)
;;                             (jmp lbl_do-s_1)
;;                             (label lbl_do-e_0)
;;                             (eframe)
;;                             (immval a)
;;                             (push)
;;                             (immval b)
;;                             (push)
;;                             (gref cons (main))
;;                             (tcall 2)))))

;; ;;; do 007
;; (test-compile '(lambda ()
;;                  (do ()
;;                      ('t)
;;                    ))
;;               '((asm-close 0 0
;;                            ((label lbl_do-s_1)
;;                             (immval t)
;;                             (jmpt lbl_do-e_0)
;;                             (jmp lbl_do-s_1)
;;                             (label lbl_do-e_0)
;;                             (undef)
;;                             (return)))))

;; ;;; do 008
;; (test-compile '(do ((x 'ix 'sx)
;;                     (y 'iy 'sy))
;;                    ('t (cons 'a 'b) (cons 'c 'd))
;;                  (cons 'e 'f)
;;                  (cons 'g 'h))
;;               '((eframe)
;;                 (immval ix)
;;                 (push)
;;                 (immval iy)
;;                 (push)
;;                 (ecommit 2)
;;                 (label lbl_do-s_1)
;;                 (immval t)
;;                 (jmpt lbl_do-e_0)
;;                 (frame)
;;                 (immval e)(push)
;;                 (immval f)(push)
;;                 (gref cons (main))
;;                 (call 2)
;;                 (arity -1)
;;                 (frame)
;;                 (immval g)(push)
;;                 (immval h)(push)
;;                 (gref cons (main))
;;                 (call 2)
;;                 (arity -1)
;;                 (eframe)
;;                 (immval sx)
;;                 (push)
;;                 (immval sy)
;;                 (push)
;;                 (erebind 2)
;;                 (jmp lbl_do-s_1)
;;                 (label lbl_do-e_0)
;;                 (frame)
;;                 (immval a)(push)
;;                 (immval b)(push)
;;                 (gref cons (main))
;;                 (call 2)
;;                 (arity -1)
;;                 (frame)
;;                 (immval c)(push)
;;                 (immval d)(push)
;;                 (gref cons (main))
;;                 (call 2)
;;                 (arity 1)
;;                 (epop)))

;; (test-print-summary)
