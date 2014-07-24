#lang racket/base

(require "../parser.rkt")

(provide (all-defined-out))

(define unescaped-name "unescaped")

(define unescaped-template
  (string-append unescaped-name ".html"))

(define unescaped-res
  (string-append unescaped-name ".txt"))

(define unescaped-ctx
  `#hash{(title . ,(λ _ "Bear > Shark"))})

(define  unescaped-mock-tokens
  (list
   (token 'static "<h1>" null)
   (token 'utag 'title null)
   (token 'static "</h1>" null)))
