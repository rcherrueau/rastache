#lang racket/base

(require "../scanner.rkt")

(provide (all-defined-out))

(define deep_partial-name "deep_partial")

(define deep_partial-template
  (string-append deep_partial-name ".html"))

(define deep_partial-res
  (string-append deep_partial-name ".txt"))

(define deep_partial-ctx
  #hash{(title . "Welcome")})

(define  deep_partial-mock-tokens
  (list
   (token 'static "<h1>First: " null)
   (token 'etag 'title null)
   (token 'static "</h1>\n" null)
   (token 'partial "partial.html" null)
   (token 'static "" null)))
