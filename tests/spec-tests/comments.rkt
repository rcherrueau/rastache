#lang racket/base

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; Mustache template engine for Racket

; Comment tags represent content that should never appear in the
; resulting output.
;
; The tag's content may contain any substring (including newlines)
; EXCEPT the closing delimiter.
;
; Comment tags SHOULD be treated as standalone when appropriate.

(require rackunit
         rackunit/text-ui
         "rastache-test-case.rkt")

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

   (rast-t-case "Indented Standalone"
                "Begin.
                   {{! Comment Block! }}
                 End."
                "Begin.
                 End."
                "All standalone comment lines should be removed.")

   (rast-t-case "Standalone Line Endings"
                "|\r\n{{! Standalone Comment }}\r\n|"
                "|\r\n|"
                "'\r\n' should be considered a newline for standalone tags.")

   (rast-t-case "Standalone Without Previous Line"
                "  {{! I'm Still Standalone }}\n!"
                "!"
                "Standalone tags should not require a newline to precede them.")

   (rast-t-case "Standalone Without Newline"
                "!\n  {{! I'm Still Standalone }}"
                "!\n"
                "Standalone tags should not require a newline to follow them.")

   (rast-t-case "Multiline Standalone"
                "Begin.
                 {{!
                   Something's going on here...
                 }}
                 End."
                "Begin.
                 End."
                "All standalone comment lines should be removed.")

   (rast-t-case "Indented Multiline Standalone"
                "Begin.
                 {{!
                   Something's going on here...
                 }}
                 End."
                "Begin.
                 End."
                "All standalone comment lines should be removed.")

   (rast-t-case "Indented Inline"
                "  12 {{! 34 }}\n"
                "  12 \n"
                "Inline comments should not strip whitespace")

   (rast-t-case "Surrounding Whitespace"
                "12345 {{! Comment Block! }} 67890"
                "12345  67890"
                "Comment removal should preserve surrounding whitespace.")))

(run-tests comment-tests)
