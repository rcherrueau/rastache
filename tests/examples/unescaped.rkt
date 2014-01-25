#lang racket/base

(provide (all-defined-out))

(define unescaped-name "unescaped")

(define unescaped-template
  (string-append "examples/" unescaped-name ".html"))

(define unescaped-res
  (string-append "examples/" unescaped-name ".txt"))

(define unescaped-stx
  #''((title (λ () "Bear > Shark"))))

(define unescaped-mock
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
          (cons 'title (λ () "Bear > Shark"))))])

  (cons context rastache-ref)))
