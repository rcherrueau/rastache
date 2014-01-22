#lang racket/base

(provide (all-defined-out))

(define inverted-name "inverted")

(define inverted-template
  (string-append "examples/" inverted-name ".html"))

(define inverted-res
  (string-append "examples/" inverted-name ".txt"))

(define inverted-stx
  #''((admin #f)
      (person '((name "Jim")))))

(define inverted-mock
  (let*
      ([refs
        (make-hash
         (list
          (cons 'admin (λ (ctx) (hash-ref ctx 'admin)))
          (cons 'person (λ (ctx) (hash-ref ctx 'person)))
          (cons 'name (λ (ctx) (hash-ref ctx 'name)))))]
       [rastache-ref
        (λ (ctx key) ((hash-ref refs key) ctx))]
       [context
        (make-hash
         (list
          (cons 'admin #f)
          (cons 'person (make-hash
                         (list (cons 'name "Jim"))))))])

  (cons context rastache-ref)))
