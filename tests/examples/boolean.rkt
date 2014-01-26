#lang racket/base

(require "../../scanner.rkt")

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

(define boolean-mock-tokens
  (list
   (token 'static "" null)
   (token 'section 'name (list
                          (token 'static "" null)
                          (token 'etag 'name null)
                          (token 'static "" null)))
   (token 'static "\n" null)
   (token 'section 'age (list
                         (token 'static "" null)
                         (token 'etag 'age null)
                         (token 'static "" null)))
   (token 'static "\n" null)
   (token 'section 'admin (list
                           (token 'static "admin" null)))
   (token 'static "" null)))
