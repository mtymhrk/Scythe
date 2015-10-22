
(select-module (scythe internal cmpl-env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export (current-module) 'new-rdepth)
(define (new-rdepth)
  (cons (cons -1 '()) '()))

(module-export (current-module) 'rdepth-ref)
(define (rdepth-ref rd)
  (car (car rd)))

(module-export (current-module) 'rdepth-expand!)
(define (rdepth-expand! rd)
  (set-car! rd (cons -1 (car rd))))

(module-export (current-module) 'rdepth-contract!)
(define (rdepth-contract! rd)
  (let* ((stk1 (car rd))
         (stk2 (cdr stk1))
         (n1 (car stk1))
         (n2 (car stk2)))
    (when (> n1 n2) (set-car! stk2 n1))
    (set-car! rd stk2)))

(module-export (current-module) 'rdepth-set!)
(define (rdepth-set! rd n)
  (let ((x (car (car rd))))
    (when (> n x)
      (set-car! (car rd) n))))

(module-export (current-module) 'rdepth-add!)
(define (rdepth-add! rd adder)
  (let ((n (+ (car (car rd)) adder)))
    (when (>= n -1)
      (set-car! (car rd) n))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export (current-module) 'make-env)
(define (make-env module)
  module)

(module-export (current-module) 'env-outmost?)
(define (env-outmost? env)
  (module? env))

(module-export (current-module) 'env-extend)
(define (env-extend vars varg asg env)
  (let* ((vars (if (vector? vars) vars (list->vector vars)))
         (len (vector-length vars))
         (assigned (make-vector len asg)))
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
