#lang racket

(require "../main.rkt")


(define bench-list (list "boolean"
                         "carriage_return"
                         "comments"
                         "complex"
                         "deep_partial"
                         "dot_notation"
                         "error_not_found"
                         "escaped"
                         "hash_instead_of_array"
                         "inner_partial"
                         "inverted"
                         "partial"
                         "recursion_with_same_names"
                         "reuse_of_enumerables"
                         "simple"
                         "tenthousand"
                         "twice"
                         "two_in_a_row"
                         "unescaped"
))

(void (map
 (lambda (bench-id)
   (define template-file (string-append "test/examples/" bench-id ".html"))
   (define tokens (tokenize template-file))

   (displayln (format "== Tokens for ~a==" bench-id))
   (map display-token tokens))
 bench-list))
