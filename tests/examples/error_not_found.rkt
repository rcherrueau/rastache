#lang racket/base

(require "../../scanner.rkt")

(provide (all-defined-out))

(define error_not_found-name "error_not_found")

(define error_not_found-template
  (string-append "examples/" error_not_found-name ".html"))

(define error_not_found-res
  (string-append "examples/" error_not_found-name ".txt"))

(define error_not_found-ctx
  #hash{(bar . 2)})

(define  error_not_found-mock-tokens
  (list
   (token 'static "" null)
   (token 'etag 'foo null)
   (token 'static "" null)))
