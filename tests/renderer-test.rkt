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
         "examples/boolean.rkt"
         "examples/carriage_return.rkt"
         "examples/comments.rkt"
         "examples/complex.rkt"
         "examples/deep_partial.rkt"
         "examples/error_not_found.rkt"
         "examples/escaped.rkt"
         "examples/hash_instead_of_array.rkt"
         "examples/inverted.rkt"
         "examples/partial.rkt"
         "examples/recursion_with_same_names.rkt"
         "examples/reuse_of_enumerables.rkt"
         "examples/simple.rkt"
         "examples/twice.rkt"
         "examples/two_in_a_row.rkt"
         "examples/unescaped.rkt")

(define renderer-tests
  (let ([test-cust (make-custodian)])
    (test-suite
     "Tests for the rastache renderer"
     #:after (λ () (custodian-shutdown-all test-cust))

     (test-case
      "Boolean Renderer Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file boolean-template)]
              [rendered (open-output-string)]
              [expected (file->string boolean-res)])
          (render (tokenize template)
                  boolean-mock-ctx
                  rendered)
          (check-equal? (get-output-string rendered)
                        expected
                        "boolean render fails"))))

     (test-case
      "Carriage_Return Renderer Test"
      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file carriage_return-template)]
              [rendered (open-output-string)]
              [expected (file->string carriage_return-res)])
          (render (tokenize template)
                  carriage_return-mock-ctx
                  rendered)
          (check-equal? (get-output-string rendered)
                        expected
                        "carriage_return render fails"))))

     (test-case
      "Comments Renderer Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file comments-template)]
              [rendered (open-output-string)]
              [expected (file->string comments-res)])
          (render (tokenize template)
                  comments-mock-ctx
                  rendered)
          (check-equal? (get-output-string rendered)
                        expected
                        "comments render fails"))))

     (test-case
      "Complex Renderer Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file complex-template)]
              [rendered (open-output-string)]
              [expected (file->string complex-res)])
          (render (tokenize template)
                  complex-mock-ctx
                  rendered)
          (check-equal? (get-output-string rendered)
                        expected
                        "complex render fails"))))

     ;; (test-case
     ;;  "Deep_Partial Renderer Test"

     ;;  (let ([template (open-input-file deep_partial-template)]
     ;;        [rendered (open-output-string)]
     ;;        [expected (file->string deep_partial-res)])
     ;;    (render (tokenize template)
     ;;            deep_partial-mock-ctx
     ;;            rendered)
     ;;    (check-equal? (get-output-string rendered)
     ;;                   expected
     ;;                   "deep_partial render fails")))

     (test-case
      "Error_Not_Found Renderer Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file error_not_found-template)]
              [rendered (open-output-string)]
              [expected (file->string error_not_found-res)])
          (render (tokenize template)
                  error_not_found-mock-ctx
                  rendered)
          (check-equal? (get-output-string rendered)
                        expected
                        "error_not_found render fails"))))

     (test-case
      "Escaped Renderer Test"
      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file escaped-template)]
              [rendered (open-output-string)]
              [expected (file->string escaped-res)])
          (render (tokenize template)
                  escaped-mock-ctx
                  rendered)
          (check-equal? (get-output-string rendered)
                        expected
                        "escaped render fails"))))

     (test-case
      "Hash_Instead_Of_Array Renderer Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file hioa-template)]
              [rendered (open-output-string)]
              [expected (file->string hioa-res)])
          (render (tokenize template)
                  hioa-mock-ctx
                  rendered)
          (check-equal? (get-output-string rendered)
                        expected
                        "hash_instead_of_array render fails"))))

     (test-case
      "Inverted Renderer Test"
      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file inverted-template)]
              [rendered (open-output-string)]
              [expected (file->string inverted-res)])
          (render (tokenize template)
                  inverted-mock-ctx
                  rendered)
          (check-equal? (get-output-string rendered)
                        expected
                        "inverted render fails"))))

     ;; (test-case
     ;;  "Partial Renderer Test"

     ;;  (let ([template (open-input-file partial-template)]
     ;;        [rendered (open-output-string)]
     ;;        [expected (file->string partial-res)])
     ;;    (render (tokenize template)
     ;;            partial-mock-ctx
     ;;            rendered)
     ;;    (check-equal? (get-output-string rendered)
     ;;                   expected
     ;;                   "partial render fails")))

     (test-case
      "Recursion_With_Same_Names Renderer Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file rwsn-template)]
              [rendered (open-output-string)]
              [expected (file->string rwsn-res)])
          (render (tokenize template)
                  rwsn-mock-ctx
                  rendered)
          (check-equal? (get-output-string rendered)
                        expected
                        "recursion_with_same_names render fails"))))

     (test-case
      "Reuse_Of_Enumerables Renderer Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file reuse_of_enumerables-template)]
              [rendered (open-output-string)]
              [expected (file->string reuse_of_enumerables-res)])
          (render (tokenize template)
                  reuse_of_enumerables-mock-ctx
                  rendered)
          (check-equal? (get-output-string rendered)
                        expected
                        "reuse_of_enumerables render fails"))))

     (test-case
      "Simple Renderer Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file simple-template)]
              [rendered (open-output-string)]
              [expected (file->string simple-res)])
          (render (tokenize template)
                  simple-mock-ctx
                  rendered)
          (check-equal? (get-output-string rendered)
                        expected
                        "simple render fails"))))

     (test-case
      "Twice Renderer Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file twice-template)]
              [rendered (open-output-string)]
              [expected (file->string twice-res)])
          (render (tokenize template)
                  twice-mock-ctx
                  rendered)
          (check-equal? (get-output-string rendered)
                        expected
                        "twice render fails"))))

     (test-case
      "Two_In_A_Row Renderer Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file two_in_a_row-template)]
              [rendered (open-output-string)]
              [expected (file->string two_in_a_row-res)])
          (render (tokenize template)
                  two_in_a_row-mock-ctx
                  rendered)
          (check-equal? (get-output-string rendered)
                        expected
                        "two_in_a_row render fails"))))

     ;; (test-case
     ;;  "Unescaped Renderer Test"

     ;;  (let ([template (open-input-file unescaped-template)]
     ;;        [rendered (open-output-string)]
     ;;        [expected (file->string unescaped-res)])
     ;;    (render (tokenize template)
     ;;            unescaped-mock-ctx
     ;;            rendered)
     ;;    (check-equal? (get-output-string rendered)
     ;;                   expected
     ;;                   "unescaped render fails")))
)))

(run-tests renderer-tests)
