#lang racket/base

(require "../../scanner.rkt"
         "../../context.rkt")

(provide (all-defined-out))

(define simple-name "simple")

(define simple-template
  (string-append "examples/" simple-name ".html"))

(define simple-res
  (string-append "examples/" simple-name ".txt"))

(define simple-ctx
  `#hash{(name . "Chris")
         (value . 10000)
         (taxed_value . ,(λ (self)
                            (let ([val (rast-ref self 'value)])
                              (inexact->exact (- val (* val 0.4))))))
         (in_ca . #t)
         (owner . ,null)})

(define  simple-mock-tokens
  (list
   (token 'static "Hello " null)
   (token 'etag 'name null)
   (token 'static "\nYou have just won $" null)
   (token 'etag 'value null)
   (token 'static "!\n" null)
   (token 'section 'in_ca (list
                           (token 'static "\nWell, $" null)
                           (token 'etag 'taxed_value null)
                           (token 'static ", after taxes.\n" null)))
   (token 'static "\nLove, " null)
   (token 'etag 'owner null)
   (token 'static "\n" null)))
