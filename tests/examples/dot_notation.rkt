#lang racket/base

(require "../../scanner.rkt")

(provide (all-defined-out))

(define dot_notation-name "dot_notation")

(define dot_notation-template
  (string-append "examples/" dot_notation-name ".html"))

(define dot_notation-res
  (string-append "examples/" dot_notation-name ".txt"))

(define dot_notation-ctx
`#hash{
  (person . #hash{(name . "Chris") (in_ca . #t)})
  (price  . #hash{(value . 10000)})
  (states . #hash{(ca .
              #hash{(taxed_value . ,(λ (self)
                                       (let ([val
                                              (rast-ref* self 'price 'value)])
                                         (- val (* val 0.4)))))})})})

(define  dot_notation-mock-tokens
  (list
))
