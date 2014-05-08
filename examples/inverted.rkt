#lang racket/base

(require "../scanner.rkt")

(provide (all-defined-out))

(define inverted-name "inverted")

(define inverted-template
  (string-append inverted-name ".html"))

(define inverted-res
  (string-append inverted-name ".txt"))

(define inverted-ctx
  #hash{(admin . #f)
        (person . #hash{(name . "Jim")})})

(define  inverted-mock-tokens
  (list
   (token 'static "" null)
   (token 'section 'admin (list
                           (token 'static "Admin." null)))
   (token 'static "\n" null)
   (token 'inverted-section 'admin (list
                                    (token 'static "Not Admin." null)))
   (token 'static "\n" null)
   (token 'section 'person (list
                            (token 'static "Hi " null)
                            (token 'etag 'name null)
                            (token 'static "!" null)))
   (token 'static "\n" null)))
