#lang racket

(define (exist-define define-name define-list)
  (member define-name define-list))

(define-syntax (mustache-ctx x)
  (syntax-case x ()
    [(_ '((k1 v1 ...) (k2 v2 ...) ...))
     #'(make-immutable-hash (list (mustache-data (quote (k1 v1 ...)))
                                  (mustache-data (quote (k2 v2 ...)))
                                  ...))]))

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


(define (self-current ctx) (ctx))
(define (self-item ctx) (ctx))

; (mustache-ctx '((name "Jim") (age 24) (admin #t)))
(define hash-test
  (mustache-ctx

   ;; CTX-1
    ;; '((name "Foo1" "Foo2" "Foo3" "Foo4") (age 24) (admin #t)

  ;; CTX-2
    '((header (lambda () "Colors"))
      (item '((name "red") (current #t) (url "#Red"))
            '((name "green") (current #f) (url "#Green"))
            '((name "blue") (current #f) (url "#Blue")))
      (link (lambda (self) (not (eq? (self-current self) #t))))
      (list (lambda (self) (not (eq? (length (self-item self)) 0))))
      (empty (lambda (self) (eq? (length (self-item self)) 0)))
)))

hash-test
