#lang racket/base

(require rackunit
         racket/format
         "../scanner.rkt"
         "../renderer.rkt"
         "../rastache.rkt"
         (for-syntax racket))

(provide (all-defined-out))

; Rastache test case
(define-syntax (rast-t-case stx)
  (syntax-case stx ()
    ;; rast-t-case: test-name context template expected fail-msg
    ;; expended to rackunit test case.
    [(_ t-name hash t-template t-expected t-error-msg)
     #'(test-case
        t-name
        (let ([rendered (open-output-string)]
              [tokens (rastache-compile/open-string t-template)]
              [expected t-expected])
          (rastache-render tokens hash rendered)
          (check-equal? (get-output-string rendered)
                        expected
                        t-error-msg)))]

    [(_ t-name hash t-template t-expected t-mock-token t-error-msg)
     (let ([make-name (λ (type syntax)
                         (format "~a -- ~a"
                                 type
                                 (syntax->datum syntax)))])
       (with-syntax
           ([scanner-t-name (make-name "SCANNER" #'t-name)]
            [scanner-t-error-msg (make-name "SCANNER" #'t-error-msg)]
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
                (render t-mock-token hash rendered)
                (check-equal? (get-output-string rendered)
                              t-expected
                              render-t-error-msg))))))]))
