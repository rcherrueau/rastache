#lang racket/base

(require "../../scanner.rkt")

(provide (all-defined-out))

(define twice-name "twice")

(define twice-template
  (string-append "examples/" twice-name ".html"))

(define twice-res
  (string-append "examples/" twice-name ".txt"))

(define twice-stx #''((person '((name "tom")))))

(define twice-mock-ctx
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
                    (cons 'name "tom"))))))])

  (cons context rastache-ref)))

(define  twice-mock-tokens
  (list
   (token 'static "" null)
   (token 'section 'person (list
                            (token 'static "" null)
                            (token 'etag 'name null)
                            (token 'static "" null)))
   (token 'static "\n" null)
   (token 'section 'person (list
                            (token 'static "" null)
                            (token 'etag 'name null)
                            (token 'static "" null)))
   (token 'static "\n" null)))
