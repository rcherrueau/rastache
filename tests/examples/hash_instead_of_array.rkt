#lang racket/base

(provide (all-defined-out))

(define hioa-name "hash_instead_of_array")

(define hioa-template
  (string-append "examples/" hioa-name ".html"))

(define hioa-res
  (string-append "examples/" hioa-name ".txt"))

(define hioa-stx
  #''((person '((name "Chris")))))

(define hioa-mock-ctx
  (let* ([refs
          (make-hash
           (list
            (cons 'person (λ (ctx) (hash-ref ctx 'person)))
            (cons 'name (λ (ctx) (hash-ref ctx 'name)))))]
         [rastache-ref
          (λ (ctx key) ((hash-ref refs key) ctx))]
         [context
          (make-hash
           (list
            (cons 'person
                  (make-hash
                   (list
                    (cons 'name "Chris"))))))])

  (cons context rastache-ref)))
