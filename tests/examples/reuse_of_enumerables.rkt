#lang racket/base

(provide (all-defined-out))

(define reuse_of_enumerables-name "reuse_of_enumerables")

(define reuse_of_enumerables-template
  (string-append "examples/" reuse_of_enumerables-name ".html"))

(define reuse_of_enumerables-res
  (string-append "examples/" reuse_of_enumerables-name ".txt"))

(define reuse_of_enumerables-stx #''((terms
                                      '((name "t1") (index 0))
                                      '((name "t2") (index 1)))))

(define reuse_of_enumerables-mock
  (let*
      ([refs
        (make-hash
         (list
          (cons 'terms (λ (ctx) (hash-ref ctx 'terms)))
          (cons 'name (λ (ctx) (hash-ref ctx 'name)))
          (cons 'index (λ (ctx) (hash-ref ctx 'index)))))]
       [rastache-ref
        (λ (ctx key) ((hash-ref refs key) ctx))]
       [context
        (make-hash
         (list
          (cons 'terms (list
                        (make-hash
                         (list (cons 'name "t1")
                               (cons 'index 0)))
                        (make-hash
                         (list (cons 'name "t2")
                               (cons 'index 1)))))))])

  (cons context rastache-ref)))
