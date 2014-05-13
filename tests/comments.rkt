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
         "../scanner.rkt"
         "rastache-test-case.rkt")

(define comment-tests
  (test-suite
   "Comment tests"

   (rast-t-case "Inline"
                #hash()
                "12345{{! Comment Block! }}67890"
                "1234567890"
                (list (token 'static "12345" null)
                      (token 'static "67890" null))
                "Comment blocks should be removed from the template.")

   (rast-t-case "Multiline"
                #hash()
                "12345{{!
                   This is a
                   multi-line comment...
                 }}67890"
                "1234567890"
                (list (token 'static "12345" null)
                      (token 'static "67890" null))
                "Multiline comments should be permitted.")

   (rast-t-case "Standalone"
                #hash()
                "Begin.
                 {{! Comment Block! }}
                 End."
                "Begin.
                 End."
                (list (token 'static "Begin.\n" null)
                      (token 'static "                 End."  null))
                "All standalone comment lines should be removed.")

   (rast-t-case "Indented Standalone"
                #hash()
                "Begin.
                   {{! Comment Block! }}
                 End."
                "Begin.
                 End."
                (list (token 'static "Begin.\n" null)
                      (token 'static "                 End."  null))
                "All standalone comment lines should be removed.")

   (rast-t-case "Standalone Line Endings"
                #hash()
                ;; Template of this test is equivalent to:
                ;; "|
                ;;  {{! Standalone Comment }}
                ;;  |"
                "|\r\n{{! Standalone Comment }}\r\n|"
                "|\r\n|"
                (list (token 'static "|\r\n" null)
                      (token 'static "|" null))
                "'\r\n' should be considered a newline for standalone tags.")

   (rast-t-case "Standalone Without Previous Line"
                #hash()
                ;; Template of this test is equivalent to:
                ;; "  {{! Standalone Comment }}
                ;;  !"
                "  {{! I'm Still Standalone }}\n!"
                "!"
                (list (token 'static "" null)
                      (token 'static "!" null))
                "Standalone tags should not require a newline to precede them.")

   (rast-t-case "Standalone Without Newline"
                #hash()
                "!\n  {{! I'm Still Standalone }}"
                "!\n"
                (list (token 'static "!\n" null))
                "Standalone tags should not require a newline to follow them.")

   (rast-t-case "Multiline Standalone"
                #hash()
                "Begin.
                 {{!
                 Something's going on here...
                 }}
                 End."
                "Begin.
                 End."
                (list (token 'static "Begin.\n" null)
                      (token 'static "                 End."  null))
                "All standalone comment lines should be removed.")

   (rast-t-case "Indented Multiline Standalone"
                #hash()
                "Begin.
                 {{!
                   Something's going on here...
                 }}
                 End."
                "Begin.
                 End."
                (list (token 'static "Begin.\n" null)
                      (token 'static "                 End."  null))
                "All standalone comment lines should be removed.")

   (rast-t-case "Indented Inline"
                #hash()
                "  12 {{! 34 }}\n"
                "  12 \n"
                (list (token 'static "  12 " null)
                      (token 'static "\n" null))
                "Inline comments should not strip whitespace")

   (rast-t-case "Surrounding Whitespace"
                #hash()
                "12345 {{! Comment Block! }} 67890"
                "12345  67890"
                (list (token 'static "12345 " null)
                      (token 'static " 67890" null))
                "Comment removal should preserve surrounding whitespace.")))

(run-tests comment-tests)
