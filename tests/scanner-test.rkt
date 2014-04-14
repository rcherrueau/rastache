#lang racket/base

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; A racket Mustache template engine.

; Tests for the scanner.
(require rackunit
         rackunit/text-ui
         racket/file
         racket/format
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

(define scanner-tests
  (let ([test-cust (make-custodian)])
    (test-suite
     "Tests for the rastache scanner"

     #:after (λ () (custodian-shutdown-all test-cust))

     (test-case
      "Boolean Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file boolean-template)])
          (check-equal? (~a (tokenize template))
                        (~a boolean-mock-tokens)
                        "boolean tokenize fails"))))

     (test-case
      "Carriage_Return Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file carriage_return-template)])
          (check-equal? (~a (tokenize template))
                        (~a carriage_return-mock-tokens)
                        "carriage_return tokenize fails"))))

     (test-case
      "Comments Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file comments-template)])
          (check-equal? (~a (tokenize template))
                        (~a comments-mock-tokens)
                        "comments tokenize fails"))))

     (test-case
      "Complex Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file complex-template)])
          (check-equal? (~a (tokenize template))
                        (~a complex-mock-tokens)
                        "complex tokenize fails"))))

     (test-case
      "Deep_Partial Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file deep_partial-template)])
          (check-equal? (~a (tokenize template))
                        (~a deep_partial-mock-tokens)
                        "deep_partial tokenize fails"))))

     (test-case
      "Error_Not_Found Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file error_not_found-template)])
          (check-equal? (~a (tokenize template))
                        (~a error_not_found-mock-tokens)
                        "error_not_found tokenize fails"))))

     (test-case
      "Escaped Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file escaped-template)])
          (check-equal? (~a (tokenize template))
                        (~a escaped-mock-tokens)
                        "escaped tokenize fails"))))

     (test-case
      "Hash_Instead_Of_Array Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file hioa-template)])
          (check-equal? (~a (tokenize template))
                        (~a hioa-mock-tokens)
                        "hash_instead_of_array tokenize fails"))))

     (test-case
      "Inverted Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file inverted-template)])
          (check-equal? (~a (tokenize template))
                        (~a inverted-mock-tokens)
                        "inverted tokenize fails"))))

     (test-case
      "Partial Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file partial-template)])
          (check-equal? (~a (tokenize template))
                        (~a partial-mock-tokens)
                        "partial tokenize fails"))))

     (test-case
      "Recursion_With_Same_Names Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file rwsn-template)])
          (check-equal? (~a (tokenize template))
                        (~a rwsn-mock-tokens)
                        "recursion_with_same_names tokenize fails"))))

     (test-case
      "Reuse_Of_Enumerables Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file reuse_of_enumerables-template)])
          (check-equal? (~a (tokenize template))
                        (~a reuse_of_enumerables-mock-tokens)
                        "reuse_of_enumerables tokenize fails"))))

     (test-case
      "Simple Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file simple-template)])
          (check-equal? (~a (tokenize template))
                        (~a simple-mock-tokens)
                        "simple tokenize fails"))))

     (test-case
      "Twice Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file twice-template)])
          (check-equal? (~a (tokenize template))
                        (~a twice-mock-tokens)
                        "twice tokenize fails"))))

     (test-case
      "Two_In_A_Row Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file two_in_a_row-template)])
          (check-equal? (~a (tokenize template))
                        (~a two_in_a_row-mock-tokens)
                        "two_in_a_row tokenize fails"))))

     (test-case
      "Unescaped Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file unescaped-template)])
          (check-equal? (~a (tokenize template))
                        (~a unescaped-mock-tokens)
                        "unescaped tokenize fails"))))
     )))

(run-tests scanner-tests)
