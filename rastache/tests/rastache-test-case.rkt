#lang racket/base

(require rackunit
         racket/format
         "../commons.rkt"
         "../parser.rkt"
         "../renderer.rkt"
         "../main.rkt"
         (for-syntax racket))

(provide (all-defined-out))

; Rastache test case
(define-syntax (rast-t-case stx)
  (syntax-case stx ()
    ;; rast-t-case: test-name context template expected fail-msg
    ;; expended to rackunit test case.
    [(_ t-name t-ctx t-template t-expected t-error-msg)
     #'(test-case
        t-name
        (let ([rendered (open-output-string)]
              [tokens (rast-compile/open-string t-template)]
              [expected t-expected])
          (rast-render tokens t-ctx rendered)
          (check-equal? (get-output-string rendered)
                        expected
                        t-error-msg)))]

    [(_ t-name t-ctx t-template t-expected t-mock-token t-error-msg)
     (let ([make-name (λ (type syntax)
                         (format "~a -- ~a"
                                 type
                                 (syntax->datum syntax)))])
       (with-syntax
           ([scanner-t-name (make-name "PARSER" #'t-name)]
            [scanner-t-error-msg (make-name "PARSER" #'t-error-msg)]
            [render-t-name (make-name "RENDER" #'t-name)]
            [render-t-error-msg (make-name "RENDER" #'t-error-msg)])
         #'(begin
             (test-case
              scanner-t-name
              (let ([template (open-input-string t-template)])
                (check-equal? (~a (tokenize template))
                              (~a t-mock-token)
                              scanner-t-error-msg)))
             (test-case
              render-t-name
              (let ([rendered (open-output-string)])
                (render t-mock-token t-ctx rendered)
                (check-equal? (get-output-string rendered)
                              t-expected
                              render-t-error-msg))))))]))
