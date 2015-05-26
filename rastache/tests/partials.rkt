#lang racket/base

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; Mustache template engine for Racket

; Partial tags are used to expand an external template into the
; current template.
;
; The tag's content MUST be a non-whitespace character sequence NOT
; containing the current closing delimiter.
;
; This tag's content names the partial to inject. Set Delimiter tags
; MUST NOT affect the parsing of a partial. The partial MUST be
; rendered against the context stack local to the tag. If the named
; partial cannot be found, the empty string SHOULD be used instead, as
; in interpolations.
;
; Partial tags SHOULD be treated as standalone when appropriate. If
; this tag is used standalone, any whitespace preceding the tag should
; treated as indentation, and prepended to each line of the partial
; before rendering.
(provide partials-tests)

(require rackunit
         rackunit/text-ui
         net/url
         "../commons.rkt"
         "rastache-test-case.rkt")

(define partials-tests
  (test-suite
   "Partials tests"

   (rast-t-case "Basic Behavior"
                #hash()
                "\"{{>partials/text1}}\""
                "\"from partial\n\""
                (list (token-static "\"")
                      (token-partial (string->url "partials/text1"))
                      (token-static "\""))
                "The greater-than operator should expand to the named partial.")

   (rast-t-case "Failed Lookup"
                #hash()
                "\"{{>partials/text2}}\""
                "\"\""
                (list (token-static "\"")
                      (token-partial (string->url "partials/text2"))
                      (token-static "\""))
                "The empty string should be used when the named partial is not found.")

   (rast-t-case "Context"
                #hash{ (text . "content") }
                "\"{{>partials/partial1}}\""
                "\"*content*\""
                (list (token-static "\"")
                      (token-partial (string->url "partials/partial1"))
                      (token-static "\""))
                "The greater-than operator should operate within the current context.")

   (rast-t-case "Recursion"
                #hash{ (content . "X")
                       (nodes . [ #hash{ (content . "Y")
                                         (nodes . []) } ]) }
                "{{>partials/node}}"
                "X<Y<>\n>\n"
                (list (token-partial (string->url "partials/node")))
                "The greater-than operator should properly recurse.")

   ;; Whitespace Sensitivity

   (rast-t-case "Surrounding Whitespace"
                #hash()
                "| {{>partials/partial2}} |"
                "| \t|\t\n |"
                (list (token-static "| ")
                      (token-partial (string->url "partials/partial2"))
                      (token-static " |"))
                "The greater-than operator should not alter surrounding whitespace.")

   (rast-t-case "Inline Indentation"
                #hash{ (data . "|") }
                "  {{data}}  {{> partials/partial3}}\n"
                "  |  >\n>\n"
                (list (token-static "  ")
                      (token-etag 'data)
                      (token-static "  ")
                      (token-partial (string->url "partials/partial3"))
                      (token-static "")
                      (token-static "\n"))
                "Whitespace should be left untouched.")

   (rast-t-case "Standalone Line Endings"
                #hash()
                "|\r\n{{>partials/partial4}}\r\n|"
                "|\r\n>|"
                (list (token-static "|\r")
                      (token-static "\n")
                      (token-partial (string->url "partials/partial4"))
                      (token-static "|"))
                "'\r\n' should be considered a newline for standalone tags.")

   #;
   ;; Unsuported by mustache.js
   (rast-t-case "Standalone Without Previous Line"
                #hash()
                "  {{>partials/partial3}}\n>"
                ;; Each line of the partial should be indented before
                ;; rendering.
                "  >\n  >>"
                (list (token-partial (string->url "partials/partial3"))
                      (token-static ">"))
                "Standalone tags should not require a newline to precede them.")

   #;
   ;; Unsuported by mustache.js
   (rast-t-case "Standalone Without Newline"
                #hash()
                ">\n  {{>partials/partial3}}"
                ;; Each line of the partial should be indented before
                ;; rendering.
                ">\n  >\n  >"
                (list (token-static ">")
                      (token-static "\n")
                      (token-partial (string->url "partials/partial3")))
                "Standalone tags should not require a newline to follow them.")

   #;
   ;; Unsuported by mustache.js
   (rast-t-case "Standalone Indentation"
                #hash()
                "
                \\
                 {{>partials/partial5}}
                /"
                "
                \
                 |
                 <
                ->
                 |
                /"
                (list (token-static "")
                      (token-static "\n")
                      (token-static "                \\")
                      (token-static "\n")
                      (token-partial (string->url "partials/partial5"))
                      (token-static "                /"))
                "Each line of the partial should be indented before rendering.")

   ;; Whitespace Insensitivity

   (rast-t-case "Padding Whitespace"
                #hash{ (boolean . #t) }
                "|{{> partials/partial6 }}|"
                "|[]|"
                (list (token-static "|")
                      (token-partial (string->url "partials/partial6"))
                      (token-static "|"))
                "Superfluous in-tag whitespace should be ignored.")))
