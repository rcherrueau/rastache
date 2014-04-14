#lang racket/base

(require "../../scanner.rkt")

(provide (all-defined-out))

(define rwsn-name "recursion_with_same_names")

(define rwsn-template
  (string-append "examples/" rwsn-name ".html"))

(define rwsn-res
  (string-append "examples/" rwsn-name ".txt"))

(define rwsn-ctx
  #hash{(name . "name")
        (description . "desc")
        (terms . [#hash{(name . "t1") (index . 0)}
                  #hash{(name . "t2") (index . 1)}])})

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
