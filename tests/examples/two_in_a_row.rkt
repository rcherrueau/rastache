#lang racket/base

(require "../../scanner.rkt")

(provide (all-defined-out))

(define two_in_a_row-name "two_in_a_row")

(define two_in_a_row-template
  (string-append "examples/" two_in_a_row-name ".html"))

(define two_in_a_row-res
  (string-append "examples/" two_in_a_row-name ".txt"))

(define two_in_a_row-stx
  #''((name "Joe") (greeting "Welcome")))

(define two_in_a_row-mock-ctx
  (let*
      ([refs
        (make-hash
         (list
          (cons 'greeting (λ (ctx) (hash-ref ctx 'greeting)))
          (cons 'name (λ (ctx) (hash-ref ctx 'name)))))]
       [rastache-ref
        (λ (ctx key) ((hash-ref refs key) ctx))]
       [context
        (make-hash
         (list
          (cons 'name "Joe")
          (cons 'greeting "Welcome")))])

  (cons context rastache-ref)))

(define  two_in_a_row-mock-tokens
  (list
   (token 'static "" null)
   (token 'etag 'greeting null)
   (token 'static ", " null)
   (token 'etag 'name null)
   (token 'static "!" null)))
