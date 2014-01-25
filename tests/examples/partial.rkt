#lang racket/base

(provide (all-defined-out))

(define partial-name "partial")

(define partial-template
  (string-append "examples/" partial-name ".html"))

(define partial-res
  (string-append "examples/" partial-name ".txt"))

(define partial-stx #''((title "Welcome")))

(define partial-mock
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
