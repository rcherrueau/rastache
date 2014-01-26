#lang racket/base

(require "../../scanner.rkt")

(provide (all-defined-out))

(define rwsn-name "recursion_with_same_names")

(define rwsn-template
  (string-append "examples/" rwsn-name ".html"))

(define rwsn-res
  (string-append "examples/" rwsn-name ".txt"))

(define rwsn-stx
  #''((name "name")
      (description "desc")
      (terms
       '((name "t1") (index 0))
       '((name "t2") (index 1)))))

(define rwsn-mock-ctx
  (let*
      ([refs
        (make-hash
         (list
          (cons 'description (λ (ctx) (hash-ref ctx 'description)))
          (cons 'terms (λ (ctx) (hash-ref ctx 'terms)))
          (cons 'index (λ (ctx) (hash-ref ctx 'index)))
          (cons 'name (λ (ctx) (hash-ref ctx 'name)))))]
       [rastache-ref
        (λ (ctx key) ((hash-ref refs key) ctx))]
       [context
        (make-hash
         (list
          (cons 'name "name")
          (cons 'description "desc")
          (cons 'terms (list
                        (make-hash
                         (list (cons 'name "t1")
                               (cons 'index 0)))
                        (make-hash
                         (list (cons 'name "t2")
                               (cons 'index 1)))))))])

  (cons context rastache-ref)))

(define  rwsn-mock-tokens
  (list
   (token 'static "" null)
   (token 'etag 'name null)
   (token 'static "\n" null)
   (token 'etag 'description null)
   (token 'static "\n" null)
   (token 'section 'terms (list
                           (token 'static "\n  " null)
                           (token 'etag 'name null)
                           (token 'static "\n  " null)
                           (token 'etag 'index null)
                           (token 'static "" null)))
   (token 'static "\n" null)))
