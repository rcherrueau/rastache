#lang racket/base

(require "../scanner.rkt")

(provide (all-defined-out))

(define twice-name "twice")

(define twice-template
  (string-append twice-name ".html"))

(define twice-res
  (string-append twice-name ".txt"))

(define twice-ctx
  #hash{(person . #hash{(name . "tom")})})

(define  twice-mock-tokens
  (list
   (token 'static "" null)
   (token 'section 'person (list
                            (token 'static "" null)
                            (token 'etag 'name null)
                            (token 'static "" null)))
   (token 'static "\n" null)
   (token 'section 'person (list
                            (token 'static "" null)
                            (token 'etag 'name null)
                            (token 'static "" null)))
   (token 'static "\n" null)))
