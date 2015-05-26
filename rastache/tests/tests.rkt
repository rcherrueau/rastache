#lang racket/base

(require rackunit
         rackunit/text-ui
         "comments.rkt"
         "interpolation.rkt"
         "inverted.rkt"
         "lambdas.rkt"
         "sections.rkt"
         "partials.rkt"
         "fillers.rkt")

(module+ test
  (displayln "Comments tests:")
  (run-tests comment-tests)
  (displayln "Interpolation tests:")
  (run-tests interpolation-tests)
  (displayln "Inverted tests:")
  (run-tests inverted-tests)
  (displayln "Lambdas tests:")
  (run-tests lambdas-tests)
  (displayln "Sections tests:")
  (run-tests sections-tests)
  (displayln "Partial tests:")
  (run-tests partials-tests)
  (displayln "Filler tests:")
  (run-tests fillers-tests))
