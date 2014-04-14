#lang racket/base

(require "../../scanner.rkt")

(provide (all-defined-out))

(define delimiters-name "delimiters")

(define delimiters-template
  (string-append "examples/" delimiters-name ".html"))

(define delimiters-res
  (string-append "examples/" delimiters-name ".txt"))

(define delimiters-ctx
  #hash{(first . "It worked the first time.")
        (second . "And it worked the second time.")
        (third . "Then, surprisingly, it worked the third time.")
        (fourth . "Fourth time also fine!.")})

(define  delimiters-mock-tokens
  (list
))
