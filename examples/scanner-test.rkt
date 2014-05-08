#lang racket/base

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; Mustache template engine for Racket

; Tests for the scanner.
(require rackunit
         rackunit/text-ui
         racket/file
         racket/format
         "../scanner.rkt"
         "boolean.rkt"
         "carriage_return.rkt"
         "comments.rkt"
         "complex.rkt"
         "deep_partial.rkt"
         ; "delimiters.rkt"
         ; "dot_notation.rkt"
         "error_not_found.rkt"
         "escaped.rkt"
         "hash_instead_of_array.rkt"
         "inverted.rkt"
         "partial.rkt"
         "recursion_with_same_names.rkt"
         "reuse_of_enumerables.rkt"
         "simple.rkt"
         "twice.rkt"
         "two_in_a_row.rkt"
         "unescaped.rkt")

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
                        (~a boolean-mock-tokens)))))

     (test-case
      "Carriage_Return Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file carriage_return-template)])
          (check-equal? (~a (tokenize template))
                        (~a carriage_return-mock-tokens)))))

     (test-case
      "Comments Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file comments-template)])
          (check-equal? (~a (tokenize template))
                        (~a comments-mock-tokens)))))

     (test-case
      "Complex Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file complex-template)])
          (check-equal? (~a (tokenize template))
                        (~a complex-mock-tokens)))))

     (test-case
      "Deep_Partial Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file deep_partial-template)])
          (check-equal? (~a (tokenize template))
                        (~a deep_partial-mock-tokens)))))

     #;
     (test-case
      "Delimiters Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file delimiters-template)])
          (check-equal? (~a (tokenize template))
                        (~a delimiters-mock-tokens)))))

     #;
     (test-case
      "Dot_Notation Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file dot_notation-template)])
          (check-equal? (~a (tokenize template))
                        (~a dot_notation-mock-tokens)))))

     (test-case
      "Error_Not_Found Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file error_not_found-template)])
          (check-equal? (~a (tokenize template))
                        (~a error_not_found-mock-tokens)))))

     (test-case
      "Escaped Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file escaped-template)])
          (check-equal? (~a (tokenize template))
                        (~a escaped-mock-tokens)))))

     (test-case
      "Hash_Instead_Of_Array Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file hioa-template)])
          (check-equal? (~a (tokenize template))
                        (~a hioa-mock-tokens)))))

     (test-case
      "Inverted Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file inverted-template)])
          (check-equal? (~a (tokenize template))
                        (~a inverted-mock-tokens)))))

     (test-case
      "Partial Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file partial-template)])
          (check-equal? (~a (tokenize template))
                        (~a partial-mock-tokens)))))

     (test-case
      "Recursion_With_Same_Names Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file rwsn-template)])
          (check-equal? (~a (tokenize template))
                        (~a rwsn-mock-tokens)))))

     (test-case
      "Reuse_Of_Enumerables Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file reuse_of_enumerables-template)])
          (check-equal? (~a (tokenize template))
                        (~a reuse_of_enumerables-mock-tokens)))))

     (test-case
      "Simple Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file simple-template)])
          (check-equal? (~a (tokenize template))
                        (~a simple-mock-tokens)))))

     (test-case
      "Twice Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file twice-template)])
          (check-equal? (~a (tokenize template))
                        (~a twice-mock-tokens)))))

     (test-case
      "Two_In_A_Row Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file two_in_a_row-template)])
          (check-equal? (~a (tokenize template))
                        (~a two_in_a_row-mock-tokens)))))

     (test-case
      "Unescaped Scanner Test"

      (parameterize ([current-custodian test-cust])
        (let ([template (open-input-file unescaped-template)])
          (check-equal? (~a (tokenize template))
                        (~a unescaped-mock-tokens)))))
     )))

(run-tests scanner-tests)
