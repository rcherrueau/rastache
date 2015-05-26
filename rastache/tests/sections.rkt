#lang racket/base

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; Mustache template engine for Racket

; Section tags and End Section tags are used in combination to wrap a
; section of the template for iteration
;
; These tags' content MUST be a non-whitespace character sequence NOT
; containing the current closing delimiter; each Section tag MUST be
; followed by an End Section tag with the same content within the same
; section.
;
; This tag's content names the data to replace the tag. Name
; resolution is as follows:
;
; 1) Split the name on periods; the first part is the name to resolve,
; any remaining parts should be retained.
;
; 2) Walk the context stack from top to bottom, finding the first
; context that is a) a hash containing the name as a key OR b) an
; object responding to a method with the given name.
;
; 3) If the context is a hash, the data is the value associated with
; the name.
;
; 4) If the context is an object and the method with the given name
; has an arity of 1, the method SHOULD be called with a String
; containing the unprocessed contents of the sections; the data is the
; value returned.
;
; 5) Otherwise, the data is the value returned by calling the method
; with the given name.
;
; 6) If any name parts were retained in step 1, each should be
; resolved against a context stack containing only the result from the
; former resolution. If any part fails resolution, the result should
; be considered falsey, and should interpolate as the empty string.
;
; If the data is not of a list type, it is coerced into a list as
; follows: if the data is truthy (e.g. `!!data == true`), use a
; single-element list containing the data, otherwise use an empty
; list.
;
; For each element in the data list, the element MUST be pushed onto
; the context stack, the section MUST be rendered, and the element
; MUST be popped off the context stack.
;
; Section and End Section tags SHOULD be treated as standalone when
; appropriate.

(provide sections-tests)

(require rackunit
         rackunit/text-ui
         "../commons.rkt"
         "rastache-test-case.rkt")

(define sections-tests
  (test-suite
   "Sections tests"

   (rast-t-case "Truthy"
                #hash{( boolean . #t )}
                "\"{{#boolean}}This should be rendered.{{/boolean}}\""
                "\"This should be rendered.\""
                (list (token-static "\"")
                      (token-sec 'boolean
                                 (list (token-static "This should be rendered."))
                                 #f)
                      (token-static "\""))
                "Truthy sections should have their contents rendered.")

   (rast-t-case "Falsey"
                #hash{( boolean . #f )}
                "\"{{#boolean}}This should not be rendered.{{/boolean}}\""
                "\"\""
                (list (token-static "\"")
                      (token-sec 'boolean (list (token-static "This should not be rendered.")) #f)
                      (token-static "\""))
                "Falsey sections should have their contents omitted.")

   (rast-t-case "Context"
                #hash{( context . #hash{( name . "Joe" )} )}
                "\"{{#context}}Hi {{name}}.{{/context}}\""
                "\"Hi Joe.\""
                (list (token-static "\"")
                      (token-sec 'context (list (token-static "Hi ")
                                                (token-etag 'name)
                                                (token-static ".")) #f)
                      (token-static "\""))
                "Objects and hashes should be pushed onto the context stack.")

   (rast-t-case "Deeply Nested Contexts"
                #hash{ (a . #hash{ (one   . 1) })
                       (b . #hash{ (two   . 2) })
                       (c . #hash{ (three . 3) })
                       (d . #hash{ (four  . 4) })
                       (e . #hash{ (five  . 5) }) }
                "{{#a}}
                 {{one}}
                 {{#b}}
                 {{one}}{{two}}{{one}}
                 {{#c}}
                 {{one}}{{two}}{{three}}{{two}}{{one}}
                 {{#d}}
                 {{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}
                 {{#e}}
                 {{one}}{{two}}{{three}}{{four}}{{five}}{{four}}{{three}}{{two}}{{one}}
                 {{/e}}
                 {{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}
                 {{/d}}
                 {{one}}{{two}}{{three}}{{two}}{{one}}
                 {{/c}}
                 {{one}}{{two}}{{one}}
                 {{/b}}
                 {{one}}
                 {{/a}}"
                "                 1
                 121
                 12321
                 1234321
                 123454321
                 1234321
                 12321
                 121
                 1\n"
                (list
                 (token-sec
                  'a `(,(token-static "                 ")
                       ,(token-etag 'one)
                       ,(token-static "")
                       ,(token-static "\n")
                       ,(token-sec
                         'b `(,(token-static "                 ")
                              ,(token-etag 'one)
                              ,(token-static "")
                              ,(token-etag 'two)
                              ,(token-static "")
                              ,(token-etag 'one)
                              ,(token-static "")
                              ,(token-static "\n")
                              ,(token-sec
                                'c `(,(token-static "                 ")
                                     ,(token-etag 'one)
                                     ,(token-static "")
                                     ,(token-etag 'two)
                                     ,(token-static "")
                                     ,(token-etag 'three)
                                     ,(token-static "")
                                     ,(token-etag 'two)
                                     ,(token-static "")
                                     ,(token-etag 'one)
                                     ,(token-static "")
                                     ,(token-static "\n")
                                     ,(token-sec
                                       'd `(,(token-static "                 ")
                                            ,(token-etag 'one)
                                            ,(token-static "")
                                            ,(token-etag 'two)
                                            ,(token-static "")
                                            ,(token-etag 'three)
                                            ,(token-static "")
                                            ,(token-etag 'four)
                                            ,(token-static "")
                                            ,(token-etag 'three)
                                            ,(token-static "")
                                            ,(token-etag 'two)
                                            ,(token-static "")
                                            ,(token-etag 'one)
                                            ,(token-static "")
                                            ,(token-static "\n")
                                            ,(token-sec
                                              'e `(,(token-static "                 ")
                                                   ,(token-etag 'one)
                                                   ,(token-static "")
                                                   ,(token-etag 'two)
                                                   ,(token-static "")
                                                   ,(token-etag 'three)
                                                   ,(token-static "")
                                                   ,(token-etag 'four)
                                                   ,(token-static "")
                                                   ,(token-etag 'five)
                                                   ,(token-static "")
                                                   ,(token-etag 'four)
                                                   ,(token-static "")
                                                   ,(token-etag 'three)
                                                   ,(token-static "")
                                                   ,(token-etag 'two)
                                                   ,(token-static "")
                                                   ,(token-etag 'one)
                                                   ,(token-static "")
                                                   ,(token-static "\n")) #f)
                                            ,(token-static "                 ")
                                            ,(token-etag 'one)
                                            ,(token-static "")
                                            ,(token-etag 'two)
                                            ,(token-static "")
                                            ,(token-etag 'three)
                                            ,(token-static "")
                                            ,(token-etag 'four)
                                            ,(token-static "")
                                            ,(token-etag 'three)
                                            ,(token-static "")
                                            ,(token-etag 'two)
                                            ,(token-static "")
                                            ,(token-etag 'one)
                                            ,(token-static "")
                                            ,(token-static "\n")) #f)
                                     ,(token-static "                 ")
                                     ,(token-etag 'one)
                                     ,(token-static "")
                                     ,(token-etag 'two)
                                     ,(token-static "")
                                     ,(token-etag 'three)
                                     ,(token-static "")
                                     ,(token-etag 'two)
                                     ,(token-static "")
                                     ,(token-etag 'one)
                                     ,(token-static "")
                                     ,(token-static "\n")) #f)
                              ,(token-static "                 ")
                              ,(token-etag 'one)
                              ,(token-static "")
                              ,(token-etag 'two)
                              ,(token-static "")
                              ,(token-etag 'one)
                              ,(token-static "")
                              ,(token-static "\n")) #f)
                       ,(token-static "                 ")
                       ,(token-etag 'one)
                       ,(token-static "")
                       ,(token-static "\n")) #f))
                "All elements on the context stack should be accessible.")

   (rast-t-case "List"
                #hash{ (list . (#hash{ (item . 1) }
                                #hash{ (item . 2) }
                                #hash{ (item . 3) })) }
                "\"{{#list}}{{item}}{{/list}}\""
                "\"123\""
                (list (token-static "\"")
                      (token-sec 'list (list (token-static "")
                                             (token-etag 'item)
                                             (token-static "")) #f)
                      (token-static "\""))
                "Lists should be iterated; list items should visit the context stack.")

   (rast-t-case "Empty List"
                #hash{( list . () )}
                "\"{{#list}}Yay lists!{{/list}}\""
                "\"\""
                (list (token-static "\"")
                      (token-sec 'list (list (token-static "Yay lists!")) #f)
                      (token-static "\""))
                "Empty lists should behave like falsey values.")

   (rast-t-case "Doubled"
                #hash{( bool . #t ) ( two . "second" )}
                "{{#bool}}
                 * first
                 {{/bool}}
                 * {{two}}
                 {{#bool}}
                 * third
                 {{/bool}}"
                "                 * first
                 * second
                 * third\n"
                (list (token-sec 'bool (list (token-static "                 * first")
                                             (token-static "\n")) #f)
                      (token-static "                 * ")
                      (token-etag 'two)
                      (token-static "")
                      (token-static "\n")
                      (token-sec 'bool (list (token-static "                 * third")
                                             (token-static "\n")) #f))
                "Multiple sections per template should be permitted.")

   (rast-t-case "Nested (Truthy)"
                #hash{( bool . #t )}
                "| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |"
                "| A B C D E |"
                (list (token-static "| A ")
                      (token-sec 'bool (list (token-static "B ")
                                             (token-sec 'bool (list (token-static "C")) #f)
                                             (token-static " D")) #f)
                      (token-static " E |"))
                "Nested truthy sections should have their contents rendered.")

   (rast-t-case "Nested (Falsey)"
                #hash{( bool . #f )}
                "| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |"
                "| A  E |"
                (list (token-static "| A ")
                      (token-sec 'bool (list (token-static "B ")
                                             (token-sec 'bool (list (token-static "C")) #f)
                                             (token-static " D")) #f)
                      (token-static " E |"))
                "Nested falsey sections should be omitted.")

   (rast-t-case "Context Misses"
                #hash()
                "[{{#missing}}Found key 'missing'!{{/missing}}]"
                "[]"
                (list (token-static "[")
                      (token-sec 'missing (list (token-static "Found key 'missing'!")) #f)
                      (token-static "]"))
                "Failed context lookups should be considered falsey.")

   ;; Implicit Iterators
   (rast-t-case "Implicit Iterator - String"
                #hash{ (list . ("a" "b" "c" "d" "e")) }
                "\"{{#list}}({{.}}){{/list}}\""
                "\"(a)(b)(c)(d)(e)\""
                (list (token-static "\"")
                      (token-sec 'list (list (token-static "(")
                                             (token-etag 'self)
                                             (token-static ")")) #f)
                      (token-static "\""))
                "Implicit iterators should directly interpolate strings.")

   (rast-t-case "Implicit Iterator - Integer"
                #hash{ (list . (1 2 3 4 5)) }
                "\"{{#list}}({{.}}){{/list}}\""
                "\"(1)(2)(3)(4)(5)\""
                (list (token-static "\"")
                      (token-sec 'list (list (token-static "(")
                                             (token-etag 'self)
                                             (token-static ")")) #f)
                      (token-static "\""))
                "Implicit iterators should cast integers to strings and interpolate.")

   (rast-t-case "Implicit Iterator - Decimal"
                #hash{ (list . (1.10 2.20 3.30 4.40 5.50) )}
                "\"{{#list}}({{.}}){{/list}}\""
                "\"(1.1)(2.2)(3.3)(4.4)(5.5)\""
                (list (token-static "\"")
                      (token-sec 'list (list (token-static "(")
                                             (token-etag 'self)
                                             (token-static ")")) #f)
                      (token-static "\""))
                "Implicit iterators should cast decimals to strings and interpolate.")

   ;; Dotted Names
   (rast-t-case "Dotted Names - Truthy"
                #hash{( a . #hash{( b . #hash{( c . #t )} )} )}
                "\"{{#a.b.c}}Here{{/a.b.c}}\" == \"Here\""
                "\"Here\" == \"Here\""
                (list
                 (token-static "\"")
                 (token-sec
                  'a `(,(token-sec
                         'b `(,(token-sec
                                'c `(,(token-static "Here")) #f)) #t)) #t)
                 (token-static "\" == \"Here\""))
                "Dotted names should be valid for Section tags.")

   (rast-t-case "Dotted Names - Falsey"
                #hash{( a . #hash{( b . #hash{( c . #f )} )} )}
                "\"{{#a.b.c}}Here{{/a.b.c}}\" == \"\""
                "\"\" == \"\""
                (list
                 (token-static "\"")
                 (token-sec
                  'a `(,(token-sec
                         'b `(,(token-sec
                                'c `(,(token-static "Here")) #f)) #t)) #t)
                 (token-static "\" == \"\""))
                "Dotted names should be valid for Section tags.")

   (rast-t-case "Dotted Names - Broken Chains"
                #hash{( a . #hash() )}
                "\"{{#a.b.c}}Here{{/a.b.c}}\" == \"\""
                "\"\" == \"\""
                (list
                 (token-static "\"")
                 (token-sec
                  'a `(,(token-sec
                         'b `(,(token-sec
                                'c `(,(token-static "Here")) #f)) #t)) #t)
                 (token-static "\" == \"\""))
                "Dotted names that cannot be resolved should be considered falsey.")

   ;; Whitespace Sensitivity
   (rast-t-case "Surrounding Whitespace"
                #hash{( boolean . #t )}
                " | {{#boolean}}\t|\t{{/boolean}} | \n"
                " | \t|\t | \n"
                (list (token-static " | ")
                      (token-sec 'boolean (list (token-static "\t|\t")) #f)
                      (token-static " | ")
                      (token-static "\n"))
                "Sections should not alter surrounding whitespace.")

   (rast-t-case "Internal Whitespace"
                #hash{( boolean . #t )}
                " | {{#boolean}} {{! Important Whitespace }}\n {{/boolean}} | \n"
                " |  \n  | \n"
                (list (token-static " | ")
                      (token-sec 'boolean (list (token-static " ")
                                                (token-static "")
                                                (token-static "\n")
                                                (token-static " ")) #f)
                      (token-static " | ")
                      (token-static "\n"))
                "Sections should not alter internal whitespace.")

   (rast-t-case "Indented Inline Sections"
                #hash{( boolean . #t )}
                " {{#boolean}}YES{{/boolean}}\n {{#boolean}}GOOD{{/boolean}}\n"
                " YES\n GOOD\n"
                ; Template should be considered as:
                ; ␣{{#boolean}}YES{{/boolean}}↩
                ; ␣{{#boolean}}GOOD{{/boolean}}↩
                (list (token-static " ")
                      (token-sec 'boolean (list (token-static "YES")) #f)
                      (token-static "")
                      (token-static "\n")
                      (token-static " ")
                      (token-sec 'boolean (list (token-static "GOOD")) #f)
                      (token-static "")
                      (token-static "\n"))
                "Single-line sections should not alter surrounding whitespace.")

   (rast-t-case "Standalone Lines"
                #hash{( boolean . #t )}
                "| This Is
                 {{#boolean}}
                 |
                 {{/boolean}}
                 | A Line"
                "| This Is
                 |
                 | A Line"
                (list (token-static "| This Is")
                      (token-static "\n")
                      (token-sec 'boolean (list (token-static "                 |")
                                                (token-static "\n")) #f)
                      (token-static "                 | A Line"))
                "Standalone lines should be removed from the template.")

   (rast-t-case "Indented Standalone Lines"
                #hash{( boolean . #t )}
                "| This Is
                   {{#boolean}}
                 |
                   {{/boolean}}
                 | A Line"
                "| This Is
                 |
                 | A Line"
                (list (token-static "| This Is")
                      (token-static "\n")
                      (token-sec 'boolean (list (token-static "                 |")
                                                (token-static "\n")) #f)
                      (token-static "                 | A Line"))
                "Indented standalone lines should be removed from the template.")

   (rast-t-case "Standalone Line Endings"
                #hash{( boolean . #t )}
                "|\r\n{{#boolean}}\r\n{{/boolean}}\r\n|"
                "|\r\n|"
                ; Template should be considered as:
                ; "|↩
                ;  {{#boolean}}
                ;  {{/boolean}}
                ;  |"
                (list (token-static "|\r")
                      (token-static "\n")
                      (token-sec 'boolean (list) #f)
                      (token-static "|"))
                "'\r\n' should be considered a newline for standalone tags.")

   (rast-t-case "Standalone Without Previous Line"
                #hash{( boolean . #t )}
                "  {{#boolean}}\n#{{/boolean}}\n/"
                "#\n/"
                ; Template should be considered as:
                ; "  {{#boolean}}
                ;  #{{/boolean}}↩
                ;  /"
                (list (token-sec 'boolean (list (token-static "#")) #f)
                      (token-static "")
                      (token-static "\n")
                      (token-static "/"))
                "Standalone tags should not require a newline to precede them.")

   (rast-t-case "Standalone Without Newline"
                #hash{( boolean . #t )}
                "#{{#boolean}}\n/\n  {{/boolean}}"
                "#\n/\n"
                ; Template should be considered as
                ; "#{{#boolean}}↩
                ;  /↩
                ;  {{/boolean}}
                (list (token-static "#")
                      (token-sec 'boolean (list (token-static "")
                                                (token-static "\n")
                                                (token-static "/")
                                                (token-static "\n")) #f))
                "Standalone tags should not require a newline to follow them.")

   ;; Whitespace Insensitivity
   (rast-t-case "Padding"
                #hash{( boolean . #t )}
                "|{{# boolean }}={{/ boolean }}|"
                "|=|"
                (list (token-static "|")
                      (token-sec 'boolean (list (token-static "=")) #f)
                      (token-static "|"))
                "Superfluous in-tag whitespace should be ignored.")))
