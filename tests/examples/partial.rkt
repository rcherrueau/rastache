#lang racket/base

(require "../../scanner.rkt")

(provide (all-defined-out))

(define partial-name "partial")

(define partial-template
  (string-append "examples/" partial-name ".html"))

(define partial-res
  (string-append "examples/" partial-name ".txt"))

(define partial-stx #''((title "Welcome")))

(define partial-mock-ctx
  (let*
      ([refs
        (make-hash
         (list
          (cons 'title (λ (ctx) (hash-ref ctx 'title)))))]
       [rastache-ref
        (λ (ctx key) ((hash-ref refs key) ctx))]
       [context
        (make-hash
         (list
          (cons 'title "Welcome")))])

  (cons context rastache-ref)))

(define  partial-mock-tokens
  (list
   (token 'static "<h1>" null)
   (token 'etag 'title null)
   (token 'static "</h1>\n" null)
   (token 'partial "inner_partial.html" null)
   (token 'static "" null)))
