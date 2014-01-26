#lang racket/base

(require "../../scanner.rkt")

(provide (all-defined-out))

(define inverted-name "inverted")

(define inverted-template
  (string-append "examples/" inverted-name ".html"))

(define inverted-res
  (string-append "examples/" inverted-name ".txt"))

(define inverted-stx
  #''((admin #f)
      (person '((name "Jim")))))

(define inverted-mock-ctx
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

(define  inverted-mock-tokens
  (list
   (token 'static "" null)
   (token 'section 'admin (list
                           (token 'static "Admin." null)))
   (token 'static "\n" null)
   (token 'inverted-section 'admin (list
                                    (token 'static "Not Admin." null)))
   (token 'static "\n" null)
   (token 'section 'person (list
                            (token 'static "Hi " null)
                            (token 'etag 'name null)
                            (token 'static "!" null)))
   (token 'static "\n" null)))
