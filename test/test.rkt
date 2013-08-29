#lang racket

(require rackunit)
(require "../main.rkt")

;; check-good-render?:
;;   mustache-render  mustache-file musatche-data-file rendered-file
;;   -> doesn't return
;; Check that the mustache-renderer rendes correctly mustache-file
;; using mustache-data-file. The mustache-renderer production will be
;; equivalent to rendered-file.
(define (check-good-render?
         mustache-renderer mustache-file mustache-data-file rendered-file)
  (with-handlers ([exn:fail:filesystem? (lambda (v) (fail v))])
  (check-equal?
     (mustache-renderer mustache-file (load mustache-data-file))
     (port->string (open-input-file rendered-file))
     (format "Error on ~a test. Find test at examples/~a.*"
             mustache-file mustache-file))))

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
                         "inner_partial"
                         "inverted"
                         "partial"
                         "recursion_with_same_names"
                         "reuse_of_enumerables"
                         "simple"
                         "tenthousand"
                         "twice"
                         "two_in_a_row"
                         "unascaped"
))

(void (map
 (lambda (bench-id)
   (define mustache-file (string-append "examples/" bench-id ".html"))
   (define mustache-data-file (string-append "examples/" bench-id ".rkt"))
   (define rendered-file (string-append "examples/" bench-id ".txt"))

   (check-good-render? render-file
                       mustache-file
                       mustache-data-file
                       rendered-file))
 bench-list))
