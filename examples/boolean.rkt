#lang racket/base

(require "../scanner.rkt")

(provide (all-defined-out))

(define boolean-name "boolean")

(define boolean-template
  (string-append boolean-name ".html"))

(define boolean-res
  (string-append boolean-name ".txt"))

(define boolean-ctx
  #hash{(name . "Jim")(age . 24)(admin . #t)})

(define boolean-mock-tokens
  (list
   (token 'static "" null)
   (token 'section 'name (list
                          (token 'static "" null)
                          (token 'etag 'name null)
                          (token 'static "" null)))
   (token 'static "\n" null)
   (token 'section 'age (list
                         (token 'static "" null)
                         (token 'etag 'age null)
                         (token 'static "" null)))
   (token 'static "\n" null)
   (token 'section 'admin (list
                           (token 'static "admin" null)))
   (token 'static "" null)))
