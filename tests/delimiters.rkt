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
         "../scanner.rkt"
         "rastache-test-case.rkt")

(define delimiters-tests
  (test-suite
   "Delimiters tests"

   (rast-t-case "Pair Behavior"
                #hash{(text . "Hey!")}
                "{{=<% %>=}}(<%text%>)"
                "(Hey!)"
                (list (token 'static "" null)
                      (token 'static "(" null)
                      (token 'etag 'text null)
                      (token 'static ")" null))
                "The equals sign (used on both sides) should permit delimiter changes.")

   (rast-t-case "Special Characters"
                #hash{(text . "It worked!")}
                "({{=[ ]=}}[text])"
                "(It worked!)"
                (list (token 'static "(" null)
                      (token 'static "" null)
                      (token 'etag 'text null)
                      (token 'static ")" null))
                "Characters with special meaning regexen should be valid delimiters.")

   (rast-t-case "Sections"
                #hash{(section . #t)
                      (data . "I got interpolated.")}
                "[
                 {{#section}}
                   {{data}}
                   |data|
                 {{/section}}

                 {{=| |=}}
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
                (list
                 (token 'static "[\n" null)
                 (token 'section
                        'section
                        (list
                         (token 'static "                   " null)
                         (token 'etag 'data null)
                         (token 'static "\n" null)
                         (token 'static "                   |data|\n" null)))
                 (token 'static "\n" null)
                 (token 'section
                        'section
                        (list
                         (token 'static "                   {{data}}\n" null)
                         (token 'static "                   " null)
                         (token 'etag 'data null)
                         (token 'static "\n" null)))
                 (token 'static "                 ]" null))
                "Delimiters set outside sections should persist.")

   (rast-t-case "Inverted Sections"
                #hash{(section . #f)
                      (data . "I got interpolated.")}
                "[
                 {{^section}}
                   {{data}}
                   |data|
                 {{/section}}

                 {{=| |=}}
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
                (list
                 (token 'static "[\n" null)
                 (token 'inverted-section
                        'section
                        (list
                         (token 'static "                   " null)
                         (token 'etag 'data null)
                         (token 'static "\n" null)
                         (token 'static "                   |data|\n" null)))
                 (token 'static "\n" null)
                 (token 'inverted-section
                        'section
                        (list
                         (token 'static "                   {{data}}\n" null)
                         (token 'static "                   " null)
                         (token 'etag 'data null)
                         (token 'static "\n" null)))
                 (token 'static "                 ]" null))
                "Delimiters set outside inverted sections should persist.")

   ;; TODO partials

   ;; Whitespace Sensitivity
   (rast-t-case "Surrounding Whitespace"
                #hash()
                "| {{=@ @=}} |"
                "|  |"
                (list (token 'static "| " null)
                      (token 'static " |" null))
                "Surrounding whitespace should be left untouched.")

   (rast-t-case "Outlying Whitespace (Inline)"
                #hash()
                " | {{=@ @=}}\n"
                " | \n"
                (list (token 'static " | " null)
                      (token 'static "\n" null))
                "Whitespace should be left untouched.")

   (rast-t-case "Standalone Tag"
                #hash()
                "Begin.
                 {{=@ @=}}
                 End."
                "Begin.
                 End."
               (list (token 'static "Begin.\n" null)
                     (token 'static "                 End." null))
               "Standalone lines should be removed from the template.")

   (rast-t-case "Indented Standalone Tag"
                #hash()
                "Begin.
                   {{=@ @=}}
                 End."
                "Begin.
                 End."
                (list (token 'static "Begin.\n" null)
                      (token 'static "                 End." null))
                "Indented standalone lines should be removed from the template.")

   (rast-t-case "Standalone Line Endings"
                #hash()
                "|\r\n{{=@ @=}}\r\n|"
                "|\r\n|"
                ; Template should be considered as:
                ; "|↩
                ;  {{=@ @=}}
                ;  |"
                (list (token 'static "|\r\n" null)
                      (token 'static "|" null))
                "'\r\n' should be considered a newline for standalone tags.")

   (rast-t-case "Standalone Without Previous Line"
                #hash()
                "  {{=@ @=}}\n="
                "="
                ; Template should be considered as:
                ; "  {{=@ @=}}
                ;  ="
                (list (token 'static "" null)
                      (token 'static "=" null))
                "Standalone tags should not require a newline to precede them.")

   (rast-t-case "Standalone Without Newline"
                #hash()
                "=\n  {{=@ @=}}"
                "=\n"
                (list (token 'static "=\n" null))
                "Standalone tags should not require a newline to follow them.")

   ;; Whitespace Insensitivity
   (rast-t-case "Pair with Padding"
                #hash()
                "|{{= @   @ =}}|"
                "||"
                (list (token 'static "|" null)
                      (token 'static "|" null))
                "Superfluous in-tag whitespace should be ignored.")))

(run-tests delimiters-tests)
