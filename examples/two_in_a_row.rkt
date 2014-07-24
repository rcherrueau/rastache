#lang racket/base

(require "../parser.rkt")

(provide (all-defined-out))

(define two_in_a_row-name "two_in_a_row")

(define two_in_a_row-template
  (string-append two_in_a_row-name ".html"))

(define two_in_a_row-res
  (string-append two_in_a_row-name ".txt"))

(define two_in_a_row-ctx
  #hash{(name . "Joe") (greeting . "Welcome")})

(define  two_in_a_row-mock-tokens
  (list
   (token 'static "" null)
   (token 'etag 'greeting null)
   (token 'static ", " null)
   (token 'etag 'name null)
   (token 'static "!" null)))
