#lang racket/base

(require "../parser.rkt")

(provide (all-defined-out))

(define hioa-name "hash_instead_of_array")

(define hioa-template
  (string-append hioa-name ".html"))

(define hioa-res
  (string-append hioa-name ".txt"))

(define hioa-ctx
  #hash{(person . #hash{(name . "Chris")})})

(define  hioa-mock-tokens
  (list
   (token 'static "" null)
   (token 'section 'person (list
                            (token 'static "\n  Name: " null)
                            (token 'etag 'name null)
                            (token 'static "\n" null)
                            ))
   (token 'static "" null)))
