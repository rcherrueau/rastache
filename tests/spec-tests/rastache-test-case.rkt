#lang racket/base

(require rackunit
         "../../rastache.rkt"
         (for-syntax racket/base))

(provide (all-defined-out))

; Rastache test case
(define-syntax (rast-t-case stx)
  (syntax-case stx ()
    [(_ t-name hash t-template t-expected t-error-msg)
     #'(test-case
        t-name
        (let ([rendered (open-output-string)]
              [tokens (rastache-compile/open-string t-template)]
              [expected t-expected])
          (rastache-render tokens hash rendered)
          (check-equal? (get-output-string rendered)
                        expected
                        t-error-msg)))]))
