#lang racket/base

(require "../parser.rkt")

(provide (all-defined-out))

(define partial-name "partial")

(define partial-template
  (string-append partial-name ".html"))

(define partial-res
  (string-append partial-name ".txt"))

(define partial-ctx
  #hash{(title . "Welcome")})

(define  partial-mock-tokens
  (list
   (token 'static "<h1>" null)
   (token 'etag 'title null)
   (token 'static "</h1>\n" null)
   (token 'partial "inner_partial.html" null)
   (token 'static "" null)))
