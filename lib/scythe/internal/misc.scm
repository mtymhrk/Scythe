
(select-module (scythe internal misc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Control features

;;; XXX:
;;;  map と for-each の実装はリストが全て循環リストであった場合にエラーにな
;;;  らずループし続ける (R7RS ではエラー)

(define (div-car-cdr lists)
  (let loop ((lists lists) (cars '()) (cdrs '()))
    (cond ((null? lists)
           (values (reverse cars) (reverse cdrs)))
          ((pair? (car lists))
           (loop (cdr lists)
                 (cons (car (car lists)) cars)
                 (cons (cdr (car lists)) cdrs)))
          (else
           (values #f #f)))))

(module-export (current-module) 'map)
(define (map proc lst . lists)
  (let loop ((lists (cons lst lists)) (acc '()))
    (let-values (((cars cdrs) (div-car-cdr lists)))
      (if cars
          (loop cdrs (cons (apply proc cars) acc))
          (reverse acc)))))

(module-export (current-module) 'for-each)
(define (for-each proc lst . lists)
  (let loop ((lists (cons lst lists)))
    (let-values (((cars cdrs) (div-car-cdr lists)))
      (cond (cars
             (apply proc cars)
             (loop cdrs))
            (else
             +undef+)))))


(define (ctor-proc-collect-nth-elm acs-proc len-proc)
  (lambda (seqs idx)
    (let loop ((seqs seqs) (acc '()))
      (if (null? seqs)
          (reverse acc)
          (let ((seq (car seqs)))
            (if (< idx (len-proc seq))
                (loop (cdr seqs) (cons (acs-proc seq idx) acc))
                #f))))))

(define (seq-map proc seqs nth-elms)
  (let loop ((idx 0) (acc '()))
    (let ((elms (nth-elms seqs idx)))
      (if elms
          (loop (+ idx 1) (cons (apply proc elms) acc))
          (reverse acc)))))

(define (seq-for-each proc seqs nth-elms)
  (let loop ((idx 0))
    (let ((elms (nth-elms seqs idx)))
      (cond (elms
             (apply proc elms)
             (loop (+ idx 1)))
            (else
             +undef+)))))

(define str-nth-chrs (ctor-proc-collect-nth-elm string-ref string-length))

(module-export (current-module) 'string-map)
(define (string-map proc str . strings)
  (list->string (seq-map proc (cons str strings) str-nth-chrs)))

(module-export (current-module) 'string-for-each)
(define (string-for-each proc str . strings)
  (seq-for-each proc (cons str strings) str-nth-chrs))


(define vec-nth-elms (ctor-proc-collect-nth-elm vector-ref vector-length))

(module-export (current-module) 'vector-map)
(define (vector-map proc vec . vectors)
  (list->vector (seq-map proc (cons vec vectors) vec-nth-elms)))

(module-export (current-module) 'vector-for-each)
(define (vector-for-each proc vec . vectors)
  (seq-for-each proc (cons vec vectors) vec-nth-elms))
