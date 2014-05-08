#lang racket/base

(require "../scanner.rkt")

(provide (all-defined-out))

(define escaped-name "escaped")

(define escaped-template
  (string-append escaped-name ".html"))

(define escaped-res
  (string-append escaped-name ".txt"))

(define escaped-ctx
  `#hash{(title . ,(λ _ "Bear > Shark"))})

(define  escaped-mock-tokens
  (list
   (token 'static "<h1>" null)
   (token 'etag 'title null)
   (token 'static "</h1>" null)))
