#lang racket/base

(provide (all-defined-out))

(define simple-name "simple")

(define simple-template
  (string-append "examples/" simple-name ".html"))

(define simple-res
  (string-append "examples/" simple-name ".txt"))

(define simple-stx
  #''((name "Chris")
      (value 10000)
      (taxed_value (λ (self)
                     (let ([val (rastache-ref self 'value)])
                       (inexact->exact (- val (* val 0.4))))))
      (in_ca #t)
      (owner null)))

(define simple-mock
  (let*
      ([refs
        (make-hash
         (list
          (cons 'value (λ (ctx) (hash-ref ctx 'value)))
          (cons 'taxed_value (λ (ctx) (hash-ref ctx 'taxed_value)))
          (cons 'in_ca (λ (ctx) (hash-ref ctx 'in_ca)))
          (cons 'owner (λ (ctx) (hash-ref ctx 'owner)))
          (cons 'name (λ (ctx) (hash-ref ctx 'name)))))]
       [rastache-ref
        (λ (ctx key) ((hash-ref refs key) ctx))]
       [context
        (make-hash
         (list
          (cons 'name "Chris")
          (cons 'value 10000)
          (cons 'taxed_value (λ (self)
                                (let ([val (rastache-ref self 'value)])
                                  (inexact->exact (- val (* val 0.4))))))
          (cons 'in_ca #t)
          (cons 'owner null)))])

  (cons context rastache-ref)))
