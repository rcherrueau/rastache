#lang racket/base

(provide (all-defined-out))

(define deep_partial-name "deep_partial")

(define deep_partial-template
  (string-append "examples/" deep_partial-name ".html"))

(define deep_partial-res
  (string-append "examples/" deep_partial-name ".txt"))

(define deep_partial-stx
  #''((title "Welcome")))

(define deep_partial-mock-ctx
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
