#lang racket/base

(require rackunit
         rackunit/text-ui
         "comments.rkt"
         "delimiters.rkt"
         "interpolation.rkt"
         "inverted.rkt"
         "lambdas.rkt"
         "sections.rkt")

(module+ tests
  (run-tests comment-tests)
  (run-tests delimiters-tests)
  (run-tests interpolation-tests)
  (run-tests inverted-tests)
  (run-tests lambdas-tests)
  (run-tests sections-tests))
