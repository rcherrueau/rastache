#lang racket/base

(provide (all-defined-out))

(define escaped-name "escaped")

(define escaped-template
  (string-append "examples/" escaped-name ".html"))

(define escaped-res
  (string-append "examples/" escaped-name ".txt"))

(define escaped-stx
  #''((title (λ () "Bear > Shark"))))

(define escaped-mock-ctx
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
          (cons 'title "Bear > Shark")))])

  (cons context rastache-ref)))
