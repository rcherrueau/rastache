#lang racket/base

(provide (all-defined-out))

(define error_not_found-name "error_not_found")

(define error_not_found-template
  (string-append "examples/" error_not_found-name ".html"))

(define error_not_found-res
  (string-append "examples/" error_not_found-name ".txt"))

(define error_not_found-stx
  #''((bar 2)))

(define error_not_found-mock
  (let*
      ([refs
        (make-hash
         (list
          (cons 'bar (λ (ctx) (hash-ref ctx 'bar)))))]
       [rastache-ref
        (λ (ctx key) ((hash-ref refs key) ctx))]
       [context
        (make-hash
         (list
          (cons 'bar 2)))])

  (cons context rastache-ref)))
