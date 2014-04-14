#lang racket/base

(require "../../scanner.rkt")

(provide (all-defined-out))

(define reuse_of_enumerables-name "reuse_of_enumerables")

(define reuse_of_enumerables-template
  (string-append "examples/" reuse_of_enumerables-name ".html"))

(define reuse_of_enumerables-res
  (string-append "examples/" reuse_of_enumerables-name ".txt"))

(define reuse_of_enumerables-ctx
  #hash{(terms . [#hash{(name . "t1") (index . 0)}
                  #hash{(name ."t2") (index . 1)}])})

(define  reuse_of_enumerables-mock-tokens
  (list
   (token 'static "" null)
   (token 'section 'terms (list
                           (token 'static "\n  " null)
                           (token 'etag 'name null)
                           (token 'static "\n  " null)
                           (token 'etag 'index null)
                           (token 'static "" null)))
   (token 'static "" null)
   (token 'section 'terms (list
                           (token 'static "\n  " null)
                           (token 'etag 'name null)
                           (token 'static "\n  " null)
                           (token 'etag 'index null)
                           (token 'static "" null)))
   (token 'static "\n" null)))
