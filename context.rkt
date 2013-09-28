#lang racket

;; (define (exist-define define-name define-list)
;;   (member define-name define-list))

(define-syntax (mustache-ctx x)
  (syntax-case x ()
    [(_ name '((k1 v1 ...) (k2 v2 ...) ...))
     (let ([make-id
            (lambda (template . ids)
              (let ([str (apply format template (map syntax->datum ids))])
                (datum->syntax x (string->symbol str))))])

     (with-syntax ([(mustache-key ...)
                    (map (lambda (f) (make-id "mustache-~a"  f))
                         (syntax->list #'(k1 k2 ...)))]
                   [(key ...) #'(k1 k2 ...)])
       #'(begin
           (define (mustache-key ctx) (hash-ref ctx 'key))
           ...
           (define name (make-immutable-hash
                         (list (mustache-data (quote (k1 v1 ...)))
                               (mustache-data (quote (k2 v2 ...)))
                               ...))))))]))

(define-syntax (mustache-data x)
  (syntax-case x ()
    [(_ (quote (key val ...)))
     #'(cons (quote key) (mustache-val val ...))]))

(define-syntax (mustache-val x)
  (syntax-case x ()
    [(_ '((k1 v1 ...) (k2 v2 ...) ...))
     #'(make-immutable-hash (list (mustache-data (quote (k1 v1 ...)))
                                  (mustache-data (quote (k2 v2 ...)))
                                  ...))]

    [(_ val)
     #'val]
    [(_ val1 val2 ...)
     #'(list (mustache-val val1) (mustache-val val2) ...)]))

;; Following not work :
;; (let ([ctx2 '((name "Jim" "Ronan") (age 24) (admin #t))])
;;   (mustache-ctx ctx-2))


;; FIXME: generate defines for nested mustache
(define (mustache-current ctx) (ctx))
(define (mustache-name ctx) (ctx))
(define (mustache-url ctx) (ctx))


; (mustache-ctx '((name "Jim") (age 24) (admin #t)))
;(define hash-test
  (mustache-ctx ctx

   ;; CTX-1
    ;; '((name "Foo1" "Foo2" "Foo3" "Foo4") (age 24) (admin #t)

  ;; CTX-2
    '((header (lambda () "Colors"))
      (item '((name "red") (current #t) (url "#Red"))
            '((name "green") (current #f) (url "#Green"))
            '((name "blue") (current #f) (url "#Blue")))
      (link (lambda (self) (not (eq? (mustache-current self) #t))))
      (list (lambda (self) (not (eq? (length (mustache-item self)) 0))))
      (empty (lambda (self) (eq? (length (mustache-item self)) 0)))
))
