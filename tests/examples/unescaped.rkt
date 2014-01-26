#lang racket/base

(require "../../scanner.rkt")

(provide (all-defined-out))

(define unescaped-name "unescaped")

(define unescaped-template
  (string-append "examples/" unescaped-name ".html"))

(define unescaped-res
  (string-append "examples/" unescaped-name ".txt"))

(define unescaped-stx
  #''((title (λ () "Bear > Shark"))))

(define unescaped-mock-ctx
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
          (cons 'title (λ (ctx) "Bear > Shark"))))])

  (cons context rastache-ref)))

(define  unescaped-mock-tokens
  (list
   (token 'static "<h1>" null)
   (token 'utag 'title null)
   (token 'static "</h1>" null)))
