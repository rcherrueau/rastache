#lang racket/base

(provide (all-defined-out))

(define boolean-name "boolean")

(define boolean-template
  (string-append "examples/" boolean-name ".html"))

(define boolean-res
  (string-append "examples/" boolean-name ".txt"))

(define boolean-stx
  #''((name "Jim") (age 24) (admin #t)))

(define boolean-mock-ctx
  (let*
      ([refs
        (make-hash
         (list
          (cons 'name (λ (ctx) (hash-ref ctx 'name)))
          (cons 'age (λ (ctx) (hash-ref ctx 'age)))
          (cons 'admin (λ (ctx) (hash-ref ctx 'admin)))))]
       [rastache-ref
        (λ (ctx key) ((hash-ref refs key) ctx))]
       [context
        (make-hash
         (list
          (cons 'name "Jim")
          (cons 'age 24)
          (cons 'admin #t)))])

  (cons context rastache-ref)))
