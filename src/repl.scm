

(select-module (scythe repl))

(define (read-eval-print-loop)
  (when (call/cc
         (lambda (k)
           (with-exception-handler
            (lambda (e) (display e) (newline) (k #t))
            (lambda ()
              (display "> ") (flush-output-port)
              (let rec ((exp (read)))
                (when (eof-object? exp) (k #f))
                (let-values ((val (eval exp)))
                  (if (null? val)
                      (newline)
                      (let loop ((v val))
                        (unless (null? v)
                          (write (car v))
                          (newline)
                          (loop (cdr v))))))
                (display "> ") (flush-output-port)
                (rec (read)))))))
    (read-eval-print-loop)))
