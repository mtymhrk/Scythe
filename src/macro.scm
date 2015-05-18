
(select-module (scythe internal macro))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Explicit Renaming

(define (er-macro-make-rename-proc def-env)
  (let ((memo '()))
    (lambda (x)
      (cond ((symbol? x)
             (let* ((m (assq x memo))
                    (i (if m (cdr m) (make-identifier x def-env))))
               (unless m
                 (set! memo (cons (cons x i) memo)))
               i))
            ((identifier? x)
             x)
            (else
             (error "macro: rename: invalid argument" x))))))

(define (er-macro-make-compare-proc use-env)
  (lambda (x y)
    (let-values (((xv xi xl xe) (env-find-identifier use-env x))
                 ((yv yi yl ye) (env-find-identifier use-env y)))
      (and (eq? xv yv)
           (or (eq? xe ye)
               (and (env-outmost? xe) (env-outmost? ye)))))))

(module-export (current-module) 'er-macro-transformer)
(define (er-macro-transformer expander)
  (lambda (form)
    (expander form
              (er-macro-make-rename-proc (current-macro-env-def))
              (er-macro-make-compare-proc (current-macro-env-use)))))

