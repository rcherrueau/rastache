#lang racket/base

(require racket/file)

(define bench-list (list "boolean"
                         "carriage_return"
                         "comments"
                         "complex"
                         "deep_partial"
                         "delimiters"
                         "dot_notation"
                         "error_not_found"
                         "escaped"
                         "hash_instead_of_array"
                         "inverted"
                         "partial"
                         "recursion_with_same_names"
                         "reuse_of_enumerables"
                         "simple"
                         "twice"
                         "two_in_a_row"
                         "unescaped"))

(define (print-test test-name out)
  (define test-template (string-append test-name ".html"))
  (define test-context (string-append test-name ".rkt"))
  (define test-result (string-append test-name ".txt"))

  (fprintf out "~n~n* ~a~n" (string-upcase test-name))

  ;; Template
  (fprintf out "*** Template~n")
  (fprintf out "#+BEGIN_EXAMPLE~n")
  (displayln (file->string test-template) out)
  (fprintf out "#+END_EXAMPLE~n")

  ;; Context
  (fprintf out "*** Context~n")
  (fprintf out "#+BEGIN_SRC racket~n")
  (displayln (file->string test-context) out)
  (fprintf out "#+END_SRC~n")

  ;; Result
  (fprintf out "*** Result~n")
  (fprintf out "#+BEGIN_EXAMPLE~n")
  (displayln (file->string test-result) out)
  (fprintf out "#+END_EXAMPLE~n"))

(let ([out (open-output-file "README.org" #:exists 'replace)])
  (map (λ (test-name) (print-test test-name out))
       bench-list)
  (close-output-port out))
