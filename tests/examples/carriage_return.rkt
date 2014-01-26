#lang racket/base

(require "../../scanner.rkt")

(provide (all-defined-out))

(define carriage_return-name "carriage_return")

(define carriage_return-template
  (string-append "examples/" carriage_return-name ".html"))

(define carriage_return-res
  (string-append "examples/" carriage_return-name ".txt"))

(define carriage_return-stx
  #''((foo "Hello World")))

(define carriage_return-mock-ctx
  (let*
      ([refs
        (make-hash
         (list
          (cons 'foo (λ (ctx) (hash-ref ctx 'foo)))))]
       [rastache-ref
        (λ (ctx key) ((hash-ref refs key) ctx))]
       [context
        (make-hash
         (list
          (cons 'foo "Hello World")))])

  (cons context rastache-ref)))

(define  carriage_return-mock-tokens
  (list
   (token 'static "<b>\r\n" null)
   (token 'etag 'foo null)
   (token 'static "\r\n</b>\n" null)))
