#lang racket/base

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; Mustache template engine for Racket

; Set Delimiter tags are used to change the tag delimiters for all
; content following the tag in the current compilation unit.
;
; The tag's content MUST be any two non-whitespace sequences
; (separated by whitespace) EXCEPT an equals sign ('=') followed by
; the current closing delimiter.
;
; Set Delimiter tags SHOULD be treated as standalone when appropriate.

(require rackunit
         rackunit/text-ui
         "rastache-test-case.rkt")

(define delimiters-tests
  (test-suite
   "Delimiters tests"

   (rast-t-case "Pair Behavior"
                #hash{(text . "Hey!")}
                "{{=<% %>=}}(<%text%>)"
                "(Hey!)"
                "The equals sign (used on both sides) should permit delimiter changes.")
   (rast-t-case "Special Characters"
                #hash{(text . "It worked!")}
                "({{=[ ]=}}[text])"
                "(It worked!)"
                "Characters with special meaning regexen should be valid delimiters.")

   (rast-t-case "Sections"
                #hash{(section . #t)
                      (data . "I got interpolated.")}
                "[
                 {{#section}}
                   {{data}}
                   |data|
                 {{/section}}

                 {{= | | =}}
                 |#section|
                   {{data}}
                   |data|
                 |/section|
                 ]"
                "[
                   I got interpolated.
                   |data|

                   {{data}}
                   I got interpolated.
                 ]"
                "Delimiters set outside sections should persist.")

   (rast-t-case "Inverted Sections"
                #hash{(section . #f)
                      (data . "I got interpolated.")}
                "[
                 {{^section}}
                   {{data}}
                   |data|
                 {{/section}}

                 {{= | | =}}
                 |^section|
                   {{data}}
                   |data|
                 |/section|
                 ]"
                "[
                   I got interpolated.
                   |data|

                   {{data}}
                   I got interpolated.
                 ]"
                "Delimiters set outside inverted sections should persist.")

   ;; TODO partials

   ;; Whitespace Sensitivity
   (rast-t-case "Surrounding Whitespace"
                #hash()
                "| {{=@ @=}} |"
                "|  |"
                "Surrounding whitespace should be left untouched.")

   (rast-t-case "Outlying Whitespace (Inline)"
                #hash()
                " | {{=@ @=}}\n"
                " | \n"
                "Whitespace should be left untouched.")

   (rast-t-case "Standalone Tag"
                #hash()
               "Begin.
                {{=@ @=}}
                End."
               "Begin.
                End."
               "Standalone lines should be removed from the template.")

   (rast-t-case "Indented Standalone Tag"
                #hash()
                "Begin.
                   {{=@ @=}}
                 End."
                "Begin.
                 End."
                "Indented standalone lines should be removed from the template.")

   (rast-t-case "Standalone Line Endings"
                #hash()
                "|\r\n{{= @ @ =}}\r\n|"
                "|\r\n|"
                "'\r\n' should be considered a newline for standalone tags.")

   (rast-t-case "Standalone Without Previous Line"
                #hash()
                "  {{=@ @=}}\n="
                "="
                "Standalone tags should not require a newline to precede them.")

   (rast-t-case "Standalone Without Newline"
                #hash()
                "=\n  {{=@ @=}}"
                "=\n"
                "Standalone tags should not require a newline to follow them.")

   ;; Whitespace Insensitivity
   (rast-t-case "Pair with Padding"
                #hash()
                "|{{= @   @ =}}|"
                "||"
                "Superfluous in-tag whitespace should be ignored.")))

(run-tests delimiters-tests)
