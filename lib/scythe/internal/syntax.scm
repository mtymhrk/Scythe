
(select-module (scythe internal syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax

(define syntax (make-record-type 'syntax))

(module-export (current-module) 'syntax?)
(define syntax? (let ((type syntax))
                  (lambda (obj)
                    (and (record? obj)
                         (eq? (record-type obj) type)))))

(module-export (current-module) 'make-syntax)
(define make-syntax (let ((type syntax))
                      (lambda (key handler)
                        (make-record type 2 key handler))))

(module-export (current-module) 'syntax-keyword)
(define (syntax-keyword syx)
  (record-ref syx 0))

(module-export (current-module) 'syntax-handler)
(define (syntax-handler syx)
  (record-ref syx 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macro

(define macro (make-record-type 'macro))

(module-export (current-module) 'macro?)
(define macro? (let ((type macro))
                  (lambda (obj)
                    (and (record? obj)
                         (eq? (record-type obj) type)))))

(module-export (current-module) 'make-macro)
(define make-macro (let ((type macro))
                      (lambda (trans env)
                        (make-record type 2 trans env))))

(module-export (current-module) 'macro-transformer)
(define (macro-transformer mac)
  (record-ref mac 0))

(module-export (current-module) 'macro-env)
(define (macro-env mac)
  (record-ref mac 1))

(module-export (current-module) 'macro-yield-transformer)
(define (macro-yield-transformer mac form use-env)
  ((macro-transformer mac) form use-env (macro-env mac)))
