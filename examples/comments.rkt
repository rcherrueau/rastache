#lang racket/base

(require "../scanner.rkt")

(provide (all-defined-out))

(define comments-name "comments")

(define comments-template
  (string-append comments-name ".html"))

(define comments-res
  (string-append comments-name ".txt"))

(define comments-ctx
  `#hash{(title . ,(λ _ "A Comedy of Errors"))})

(define  comments-mock-tokens
  (list
   (token 'static "<h1>" null)
   (token 'etag 'title null)
   (token 'static "" null)
   (token 'static "</h1>\n" null)))
