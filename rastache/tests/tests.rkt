#lang racket/base

(require rackunit
         rackunit/text-ui
         "comments.rkt"
         "delimiters.rkt"
         "interpolation.rkt"
         "inverted.rkt"
         "lambdas.rkt"
         "sections.rkt")

(module+ test
  (displayln "Comments tests:")
  (run-tests comment-tests)
  (displayln "Delimiters tests:")
  (run-tests delimiters-tests)
  (displayln "Interpolation tests:")
  (run-tests interpolation-tests)
  (displayln "Inverted tests:")
  (run-tests inverted-tests)
  (displayln "Lambdas tests:")
  (run-tests lambdas-tests)
  (displayln "Sections tests:")
  (run-tests sections-tests))
