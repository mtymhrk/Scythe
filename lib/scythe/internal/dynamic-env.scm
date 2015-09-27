
(select-module (scythe internal dynamic-env))

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

(module-export (current-module) 'with-exception-handler)
(define (with-exception-handler handler thunk)
  (dynamic-wind
      (lambda () (push-exception-handler handler))
      thunk
      pop-exception-handler))

