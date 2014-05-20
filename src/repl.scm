

(with-module (scythe internal repl)
  (define (read-eval-print-loop)
    (when (call/cc
           (lambda (k)
             (with-exception-handler
              (lambda (e) (display e) (newline) (k #t))
              (lambda ()
                (display "> ") (flush-output-port)
                (let rec ((exp (read)))
                  (when (eof-object? exp) (k #f))
                  (write (eval exp))
                  (newline)
                  (display "> ") (flush-output-port)
                  (rec (read)))))))
      (read-eval-print-loop))))
