
(select-module (scythe internal dynamic-env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dynamic-wind

(module-export (current-module) 'dynamic-wind)
(define (dynamic-wind before thunk after)
  (letrec ((b (lambda ()
                (before)
                (push-dynamic-wind-handler b a)))
           (a (lambda ()
                (pop-dynamic-wind-handler)
                (after))))
    (b)
    (let-values ((x (thunk)))
      (a)
      (apply values x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; with-exception-handler

(module-export (current-module) 'with-exception-handler)
(define (with-exception-handler handler thunk)
  (dynamic-wind
      (lambda () (push-exception-handler handler))
      thunk
      pop-exception-handler))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parameterize

(define (decons-parameterize exp)
  (let ((x (cdr exp)) (body #f))
    (unless (pair? x)
      (error "malformed parameterize" exp))
    (set! body (cdr x))
    (let loop ((binds (car x)) (params '()) (vals '()))
      (if (null? binds)
          (values params vals body)
          (begin
            (unless (pair? binds)
              (error "malformed ~a" exp))
            (let ((x (car binds)) (p #f) (v #f))
              (unless (pair? x)
                (error "malformed ~a" exp))
              (set! p (car x))
              (set! x (cdr x))
              (unless (pair? x)
                (error "malformed ~a" exp))
              (set! v (car x))
              (unless (null? (cdr x))
                (error "malformed ~a" exp))
              (loop (cdr binds) (cons p params) (cons v vals))))))))

;;;
;;; parameterize シンタックスを以下のような変換を行うマクロとして定義する
;;;
;;;   (parameterize ((<param> <val>) ..)
;;;      <expr> ...)
;;;
;;; [変換後]
;;;
;;;   (dynamic-wind
;;;      (lambda () (push-dynamic-bindings (<param> <val> #f) ...))
;;;      (lambda () <expr> ...)
;;;      pop-dynamic-bindings)
;;;
(module-export (current-module) 'parameterize)
(define-syntax parameterize
  (er-macro-transformer
   (lambda (f r c)
     (let-values (((params vals body) (decons-parameterize f)))
       `(,(r 'dynamic-wind)
         (,(r 'lambda) ()
            (,(r 'push-dynamic-bindings)
             ,@(let loop ((params params) (vals vals) (acc '()))
                 (if (null? params)
                     acc
                     (loop (cdr params) (cdr vals)
                           (cons (list (car params) (car vals) #f)
                                 acc))))))
         (,(r 'lambda) () ,@body)
         ,(r 'pop-dynamic-bindings))))))
