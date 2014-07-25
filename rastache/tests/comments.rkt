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

(provide comment-tests)

(require rackunit
         rackunit/text-ui
         "../commons.rkt"
         "rastache-test-case.rkt")

(define comment-tests
  (test-suite
   "Comment tests"

   (rast-t-case "Inline"
                #hash()
                "12345{{! Comment Block! }}67890"
                "1234567890"
                (list (token-delimiter "{{" "}}")
                      (token-static "12345")
                      (token-static "67890"))
                "Comment blocks should be removed from the template.")

   (rast-t-case "Multiline"
                #hash()
                "12345{{!
                   This is a
                   multi-line comment...
                 }}67890"
                "1234567890"
                (list (token-delimiter "{{" "}}")
                      (token-static "12345")
                      (token-static "67890"))
                "Multiline comments should be permitted.")

   (rast-t-case "Standalone"
                #hash()
                "Begin.
                 {{! Comment Block! }}
                 End."
                "Begin.
                 End."
                (list (token-delimiter "{{" "}}")
                      (token-static "Begin.")
                      (token-static "\n")
                      (token-static "                 End."))
                "All standalone comment lines should be removed.")

   (rast-t-case "Indented Standalone"
                #hash()
                "Begin.
                   {{! Comment Block! }}
                 End."
                "Begin.
                 End."
                (list (token-delimiter "{{" "}}")
                      (token-static "Begin.")
                      (token-static "\n")
                      (token-static "                 End."))
                "All standalone comment lines should be removed.")

   (rast-t-case "Standalone Line Endings"
                #hash()
                "|\r\n{{! Standalone Comment }}\r\n|"
                "|\r\n|"
                ; Template should be considered as:
                ; "|↩
                ;  {{! Standalone Comment }}
                ;  |"
                (list (token-delimiter "{{" "}}")
                      (token-static "|\r")
                      (token-static "\n")
                      (token-static "|"))
                "'\r\n' should be considered a newline for standalone tags.")

   (rast-t-case "Standalone Without Previous Line"
                #hash()
                "  {{! I'm Still Standalone }}\n!"
                "!"
                ; Template should be considered as:
                ; "  {{! Standalone Comment }}
                ;  !"
                (list (token-delimiter "{{" "}}")
                      (token-static "!"))
                "Standalone tags should not require a newline to precede them.")

   (rast-t-case "Standalone Without Newline"
                #hash()
                "!\n  {{! I'm Still Standalone }}"
                "!\n"
                ; Template should be considered as:
                ; "!↩
                ;    {{! Standalone Comment }}"
                (list (token-delimiter "{{" "}}")
                      (token-static "!")
                      (token-static "\n"))
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
                (list (token-delimiter "{{" "}}")
                      (token-static "Begin.")
                      (token-static "\n")
                      (token-static "                 End."))
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
                (list (token-delimiter "{{" "}}")
                      (token-static "Begin.")
                      (token-static "\n")
                      (token-static "                 End."))
                "All standalone comment lines should be removed.")

   (rast-t-case "Indented Inline"
                #hash()
                "  12 {{! 34 }}\n"
                "  12 \n"
                (list (token-delimiter "{{" "}}")
                      (token-static "  12 ")
                      (token-static "")
                      (token-static "\n"))
                "Inline comments should not strip whitespace")

   (rast-t-case "Surrounding Whitespace"
                #hash()
                "12345 {{! Comment Block! }} 67890"
                "12345  67890"
                (list (token-delimiter "{{" "}}")
                      (token-static "12345 ")
                      (token-static " 67890"))
                "Comment removal should preserve surrounding whitespace.")))
