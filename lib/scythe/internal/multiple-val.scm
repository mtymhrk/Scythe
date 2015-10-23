
(select-module (scythe internal multiple-val))

(define (map proc lst)
  (if (null? lst)
      lst
      (cons (proc (car lst)) (map proc (cdr lst)))))

(define (decons-formals exp)
  (let loop ((exp exp) (val ()))
    (cond ((pair? exp)
           (loop (cdr exp) (cons (car exp) val)))
          ((null? exp)
           (values val #f))
          (else
           (values (cons exp val) #t)))))

(define (decons-define-values exp)
  (let ((x (cdr exp)) (formals #f) (producer #f))
    (unless (pair? x)
      (error "malformed define-values" exp))
    (set! formals (car x))
    (set! x (cdr x))
    (unless (pair? x)
      (error "malformed define-vlaues" exp))
    (set! producer (car x))
    (set! x (cdr x))
    (unless (null? x)
      (error "malformed define-values" exp))
    (let-values (((vals flg) (decons-formals formals)))
      (values vals flg producer))))

(define (ctor-val-defs-inn vals r acc)
  (if (null? vals)
      acc
      (ctor-val-defs-inn (cdr vals) r
                         (cons (list (r 'define) (car vals) +undef+)
                               acc))))

(define (ctor-val-defs vals r)
  (ctor-val-defs-inn vals r '()))

(define (ctor-let-val-form-inn vals acc)
  (if (null? vals)
      acc
      (ctor-let-val-form-inn (cdr vals) (cons (car vals) acc))))

(define (ctor-let-val-form vals flg)
  (if flg
      (ctor-let-val-form-inn (cdr vals) (car vals))
      (ctor-let-val-form-inn vals '())))

(define (ctor-val-inits-inn ids vals r acc)
  (if (null? ids)
      acc
      (ctor-val-inits-inn (cdr ids) (cdr vals) r
                          (cons (list (r 'set!) (car vals) (car ids))
                                acc))))

(define (ctor-val-inits ids vals r)
  (ctor-val-inits-inn ids vals r '()))


;;;
;;; define-values シンタックスを以下のような変換を行うマクロとして定義する
;;;
;;;   1. (define-values (<variable-name-1> ...) <expression>)
;;;   2. (define-values (<variable-name-1> <variable-naem-2> ... . <variable-name-N>)
;;;                     <expression>)
;;;   3. (define-values <variable-name-1> <expression>)
;;;
;;; [変換後]
;;;
;;; 1. (begin
;;;      (define <variable-name-1> <undefined-value>) ...
;;;      (define <dummy-variable> (let-values (((tmp1 ...) <expression>))
;;;                                 (set! <variable-name-1> tmp1) ...)))
;;;
;;; 2. (begin
;;;      (define <variable-name-1> <undefined-value>) ...
;;;      (define <dummy-variable> (let-values (((tmp1 tmp2 ... . tmpN) <expression>))
;;;                                 (set! <variable-name-1> tmp1)
;;;                                 (set! <variable-name-2> tmp2) ...
;;;                                 (set! <variable-name-N> tmpN))))
;;;
;;; 3. (begin
;;;      (define <variable-name-1> <undefined-value>)
;;;      (define <dummy-variable> (let-values ((tmp1 <expression>))
;;;                                 (set! <variable-name-1> tmp1))))
;;;
(module-export (current-module) 'define-values)
(define-syntax define-values
  (er-macro-transformer
   (lambda (f r c)
     (let-values (((variables vflag producer) (decons-define-values f)))
       (define tmp-ids (map r variables))
       `(,(r 'begin)
         ,@(ctor-val-defs variables r)
         (,(r 'define) ,(r 'dummy-variable)
          (,(r 'let-values) ((,(ctor-let-val-form tmp-ids vflag) ,producer))
           ,@(ctor-val-inits tmp-ids variables r))))))))
