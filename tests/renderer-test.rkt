#lang racket/base

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; A racket Mustache template engine.

; Tests for renderer.
(require rackunit
         rackunit/text-ui
         racket/file
         "../renderer.rkt"
         "../scanner.rkt"
         "examples/complex.rkt"
         "examples/partial.rkt"
         "examples/reuse_of_enumerables.rkt"
         "examples/twice.rkt"
)

;; TODO: close port with custodian
(define renderer-tests
  (test-suite
   "Tests for the rastache renderer"

   (test-case
    "Complex Renderer Test"

    (let ([template (open-input-file complex-template)]
          [rendered (open-output-string)]
          [expected (file->string complex-res)])
      (render (tokenize template)
              complex-mock
              rendered)
      (check-equal? (get-output-string rendered)
                     expected
                     "complex render fails")))

   (test-case
    "Partial Renderer Test"

    (let ([template (open-input-file partial-template)]
          [rendered (open-output-string)]
          [expected (file->string partial-res)])
      (render (tokenize template)
              partial-mock
              rendered)
      (check-equal? (get-output-string rendered)
                     expected
                     "partial render fails")))

   (test-case
    "Reuse_Of_Enumerables Renderer Test"

    (let ([template (open-input-file reuse_of_enumerables-template)]
          [rendered (open-output-string)]
          [expected (file->string reuse_of_enumerables-res)])
      (render (tokenize template)
              reuse_of_enumerables-mock
              rendered)
      (check-equal? (get-output-string rendered)
                     expected
                     "reuse_of_enumerables render fails")))

   (test-case
    "Twice Renderer Test"

    (let ([template (open-input-file twice-template)]
          [rendered (open-output-string)]
          [expected (file->string twice-res)])
      (render (tokenize template)
              twice-mock
              rendered)
      (check-equal? (get-output-string rendered)
                     expected
                     "twice render fails")))))

(run-tests renderer-tests)
