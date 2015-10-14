
(select-module (scythe internal identifier))

;;; Identifier を Record オブジェクトととして実装する。define-record-type シ
;;; ンタックスを使って実装していないのは define-record-type をマクロで実装し
;;; ていて、マクロシステムの実装に Identifier を使用しているので、依存関係を
;;; 循環させないため。

(module-export (current-module) 'identifier)
(define identifier (make-record-type 'identifier))

(module-export (current-module) 'identifier?)
(define identifier? (let ((type identifier))
                      (lambda (obj)
                        (and (record? obj)
                             (eq? (record-type obj) type)))))

(module-export (current-module) 'make-identifier)
(define make-identifier (let ((type identifier))
                          (lambda (name env)
                            (make-record type 2 name env))))

(module-export (current-module) 'identifier-name)
(define (identifier-name ident)
  (record-ref ident 0))

(module-export (current-module) 'identifier-env)
(define (identifier-env ident)
  (record-ref ident 1))
