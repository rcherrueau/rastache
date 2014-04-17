#lang racket/base

; Comment tags represent content that should never appear in the
; resulting output.
;
; The tag's content may contain any substring (including newlines)
; EXCEPT the closing delimiter.
;
; Comment tags SHOULD be treated as standalone when appropriate.

(require rackunit
         rackunit/text-ui
         "../../rastache.rkt"
         (for-syntax racket/base))

(provide (all-defined-out))

(define-syntax (rast-t-case stx)
  (syntax-case stx ()
    [(_ t-name t-template t-expected t-error-msg)
     #'(test-case
        t-name
        (let ([rendered (open-output-string)]
              [tokens (rastache-compile/open-string t-template)]
              [expected t-expected])
          (rastache-render tokens #hash() rendered)
          (check-equal? (get-output-string rendered)
                        expected
                        t-error-msg)))]))


(define comment-tests
  (test-suite
   "Comment tests"

   (rast-t-case "Inline"
                "12345{{! Comment Block! }}67890"
                "1234567890"
                "Comment blocks should be removed from the template.")

   (rast-t-case "Multiline"
                "12345{{!
                   This is a
                   multi-line comment...
                 }}67890"
                "1234567890"
                "Multiline comments should be permitted.")

   (rast-t-case "Standalone"
                "Begin.
                   {{! Comment Block! }}
                 End."
                "Begin.
                 End."
                "All standalone comment lines should be removed.")

   (test-case
    "Indented Standalone"

    (let ([rendered (open-output-string)]
          [tokens (rastache-compile/open-string
                    "Begin.
                       {{! Comment Block! }}
                     End.")]
          [expected "Begin.
                     End."])
      (rastache-render tokens #hash() rendered)
      (check-equal? (get-output-string rendered)
                    expected
                    "All standalone comment lines should be removed.")))
   (test-case
    "Standalone Line Endings"

    (let ([rendered (open-output-string)]
          [tokens (rastache-compile/open-string
                    "|\r\n{{! Standalone Comment }}\r\n|")]
          [expected "|\r\n|"])
      (rastache-render tokens #hash() rendered)
      (check-equal? (get-output-string rendered)
                    expected
                    "'\r\n' should be considered a newline for standalone tags.")))

   (test-case
    "Standalone Without Previous Line"

    (let ([rendered (open-output-string)]
          [tokens (rastache-compile/open-string
                    "  {{! I'm Still Standalone }}\n!")]
          [expected "!"])
      (rastache-render tokens #hash() rendered)
      (check-equal? (get-output-string rendered)
                    expected
                    "Standalone tags should not require a newline to precede them.")))

   (test-case
    "Standalone Without Newline"

    (let ([rendered (open-output-string)]
          [tokens (rastache-compile/open-string
                    "!\n  {{! I'm Still Standalone }}")]
          [expected "!\n"])
      (rastache-render tokens #hash() rendered)
      (check-equal? (get-output-string rendered)
                    expected
                    "Standalone tags should not require a newline to follow them.")))

   (test-case
    "Multiline Standalone"

    (let ([rendered (open-output-string)]
          [tokens (rastache-compile/open-string
                    "Begin.
                     {{!
                     Something's going on here...
                     }}
                     End.")]
          [expected "Begin.
                     End."])
      (rastache-render tokens #hash() rendered)
      (check-equal? (get-output-string rendered)
                    expected
                    "All standalone comment lines should be removed.")))

   (test-case
    "Indented Multiline Standalone"

    (let ([rendered (open-output-string)]
          [tokens (rastache-compile/open-string
                    "Begin.
                     {{!
                       Something's going on here...
                     }}
                     End.")]
          [expected "Begin.
                     End."])
      (rastache-render tokens #hash() rendered)
      (check-equal? (get-output-string rendered)
                    expected
                    "All standalone comment lines should be removed.")))

   (test-case
    "Indented Inline"

    (let ([rendered (open-output-string)]
          [tokens (rastache-compile/open-string
                    "  12 {{! 34 }}\n")]
          [expected "  12 \n"])
      (rastache-render tokens #hash() rendered)
      (check-equal? (get-output-string rendered)
                    expected
                    "Inline comments should not strip whitespace")))

   (test-case
    "Surrounding Whitespace"

    (let ([rendered (open-output-string)]
          [tokens (rastache-compile/open-string
                    "12345 {{! Comment Block! }} 67890")]
          [expected "12345  67890"])
      (rastache-render tokens #hash() rendered)
      (check-equal? (get-output-string rendered)
                    expected
                    "Comment removal should preserve surrounding whitespace.")))
))

(run-tests comment-tests)
