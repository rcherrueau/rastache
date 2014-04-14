#lang racket/base

(require "../../scanner.rkt")

(provide (all-defined-out))

(define escaped-name "escaped")

(define escaped-template
  (string-append "examples/" escaped-name ".html"))

(define escaped-res
  (string-append "examples/" escaped-name ".txt"))

(define escaped-ctx
  `#hash{(title . ,(λ self "Bear > Shark"))})

(define  escaped-mock-tokens
  (list
   (token 'static "<h1>" null)
   (token 'etag 'title null)
   (token 'static "</h1>" null)))
