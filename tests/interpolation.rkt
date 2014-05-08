#lang racket/base

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; Mustache template engine for Racket

; Interpolation tags are used to integrate dynamic content into the
; template.
;
; The tag's content MUST be a non-whitespace character sequence NOT
; containing the current closing delimiter.
;
; This tag's content names the data to replace the tag. A single
; period (`.`) indicates that the item currently sitting atop the
; context stack should be used; otherwise, name resolution is as
; follows:
;   1) Split the name on periods; the first part is the name to
;   resolve, any remaining parts should be retained.
;   2) Walk the context stack from top to bottom, finding the first
;   context that is a) a hash containing the name as a key OR b) an
;   object responding to a method with the given name.
;   3) If the context is a hash, the data is the value associated with
;   the name.
;   4) If the context is an object, the data is the value returned by
;   the method with the given name.
;   5) If any name parts were retained in step 1, each should be
;   resolved against a context stack containing only the result from
;   the former resolution. If any part fails resolution, the result
;   should be considered falsey, and should interpolate as the empty
;   string.
; Data should be coerced into a string (and escaped, if appropriate)
; before interpolation.
;
; The Interpolation tags MUST NOT be treated as standalone.

(require rackunit
         rackunit/text-ui
         "rastache-test-case.rkt")

(define interpolation-tests
  (test-suite
   "Interpolation tests"

   (rast-t-case "No Interpolation"
                #hash()
                "Hello from {Mustache}!"
                "Hello from {Mustache}!"
                "Mustache-free templates should render as-is.")

   (rast-t-case "Basic Interpolation"
                #hash{(subject . "world")}
                "Hello, {{subject}}!"
                "Hello, world!"
                "Unadorned tags should interpolate content into the template.")

   (rast-t-case "HTML Escaping"
                #hash{(forbidden . "& \" < >")}
                "These characters should be HTML escaped: {{forbidden}}"
                "These characters should be HTML escaped: &amp; &quot; &lt; &gt;"
                "Basic interpolation should be HTML escaped.")

   (rast-t-case "Triple Mustache"
                #hash{(forbidden . "& \" < > ")}
                "These characters should not be HTML escaped: {{{forbidden}}}"
                "These characters should not be HTML escaped: &amp; &quot; &lt; &gt;"
                "Triple mustaches should interpolate without HTML escaping.")

   (rast-t-case "Ampersand"
                #hash{(forbidden . "& \" < > ")}
                "These characters should not be HTML escaped: {{&forbidden}}"
                "These characters should not be HTML escaped: &amp; &quot; &lt; &gt;"
                "Ampersand should interpolate without HTML escaping.")

   (rast-t-case "Basic Integer Interpolation"
                #hash{(mph . 85)}
                "\"{{mph}} miles an hour!\""
                "\"85 miles an hour!\""
                "Integers should interpolate seamlessly.")

   (rast-t-case "Triple Mustache Integer Interpolation"
                #hash{(mph . 85)}
                "\"{{{mph}}} miles an hour!\""
                "\"85 miles an hour!\""
                "Integers should interpolate seamlessly.")

   (rast-t-case "Ampersand Integer Interpolation"
                #hash{(mph . 85)}
                "\"{{&mph}} miles an hour!\""
                "\"85 miles an hour!\""
                "Integers should interpolate seamlessly.")

   (rast-t-case "Basic Decimal Interpolation"
                #hash{(power . 1.210)}
                "\"{{power}} jiggawatts!\""
                "\"1.21 jiggawatts!\""
                "Decimals should interpolate seamlessly with proper significance.")

   (rast-t-case "Triple Mustache Decimal Interpolation"
                #hash{(power . 1.210)}
                "\"{{{power}}} jiggawatts!\""
                "\"1.21 jiggawatts!\""
                "Decimals should interpolate seamlessly with proper significance.")

   (rast-t-case "Ampersand Decimal Interpolation"
                #hash{(power . 1.210)}
                "\"{{&power}} jiggawatts!\""
                "\"1.21 jiggawatts!\""
                "Decimals should interpolate seamlessly with proper significance.")

   ;; Context Misses
   (rast-t-case "Basic Context Miss Interpolation"
                #hash()
                "I ({{cannot}}) be seen!"
                "I () be seen!"
                "Failed context lookups should default to empty strings.")

   (rast-t-case "Triple Mustache Context Miss Interpolation"
                #hash()
                "I ({{{cannot}}}) be seen!"
                "I () be seen!"
                "Failed context lookups should default to empty strings.")

   (rast-t-case "Ampersand Context Miss Interpolation"
                #hash()
                "I ({{&cannot}}) be seen!"
                "I () be seen!"
                "Failed context lookups should default to empty strings.")

   ;; Dotted Names
   (rast-t-case "Dotted Names - Basic Interpolation"
                #hash{(person . #hash{(name . "Joe")})}
                "\"{{person.name}}\" == \"{{#person}}{{name}}{{/person}}\""
                "\"Joe\" == \"Joe\""
                "Dotted names should be considered a form of shorthand for sections.")

   (rast-t-case "Dotted Names - Triple Mustache Interpolation"
                #hash{(person . #hash{(name . "Joe")})}
                "\"{{{person.name}}}\" == \"{{#person}}{{{name}}}{{/person}}\""
                "\"Joe\" == \"Joe\""
                "Dotted names should be considered a form of shorthand for sections.")

   (rast-t-case "Dotted Names - Ampersand Interpolation"
                #hash{(person . #hash{(name . "Joe")})}
                "\"{{&person.name}}\" == \"{{#person}}{{&name}}{{/person}}\""
                "\"Joe\" == \"Joe\""
                "Dotted names should be considered a form of shorthand for sections.")

   (rast-t-case "Dotted Names - Arbitrary Depth"
                #hash{(a .
                  #hash{(b .
                    #hash{(c .
                      #hash{(d .
                        #hash{(e .
                          #hash{(name . "Phil")})})})})})}
                "\"{{a.b.c.d.e.name}}\" == \"Phil\""
                "\"Phil\" == \"Phil\""
                "Dotted names should be functional to any level of nesting.")

   (rast-t-case "Dotted Names - Broken Chains"
                #hash{(a . #hash{})}
                "\"{{a.b.c}}\" == \"\""
                "\"\" == \"\""
                "Any falsey value prior to the last part of the name should yield ''.")

   (rast-t-case "Dotted Names - Broken Chain Resolution"
                #hash{( a . #hash{( b . #hash() )} )
                      ( c . #hash{( name . "Jim" )} )}
                "\"{{a.b.c.name}}\" == \"\""
                "\"\" == \"\""
                "Each part of a dotted name should resolve only against its parent.")

   (rast-t-case "Dotted Names - Initial Resolution"
                #hash{( a . #hash{( b .
                              #hash{( c .
                                #hash{( d .
                                  #hash{( e .
                                    #hash{( name . "Phil" )} )})})})})
                      ( b . #hash{( c .
                              #hash{( d .
                                #hash{( e .
                                  #hash{( name . "Wrong")} )})})}) }
                "\"{{#a}}{{b.c.d.e.name}}{{/a}}\" == \"Phil\""
                "\"Phil\" == \"Phil\""
                "The first part of a dotted name should resolve as any other name.")

   (rast-t-case "Dotted Names - Context Precedence"
                #hash{( a . #hash{( b . #hash() )} )
                      ( b . #hash{( c . "ERROR" )} )}
                "{{#a}}{{b.c}}{{/a}}"
                ""
                "Dotted names should be resolved against former resolutions.")

   ;; Whitespace Sensitivity
   (rast-t-case "Interpolation - Surrounding Whitespace"
                #hash{(string . "---")}
                "| {{string}} |"
                "| --- |"
                "Interpolation should not alter surrounding whitespace.")

   (rast-t-case "Triple Mustache - Surrounding Whitespace"
                #hash{(string . "---")}
                "| {{{string}}} |"
                "| --- |"
                "Interpolation should not alter surrounding whitespace.")

   (rast-t-case "Ampersand - Surrounding Whitespace"
                #hash{(string . "---")}
                "| {{&string}} |"
                "| --- |"
                "Interpolation should not alter surrounding whitespace.")

   (rast-t-case "Interpolation - Standalone"
                #hash{(string . "---")}
                "  {{string}}\n"
                "  ---\n"
                "Standalone interpolation should not alter surrounding whitespace.")

   (rast-t-case "Triple Mustache - Standalone"
                #hash{(string . "---")}
                "  {{{string}}}\n"
                "  ---\n"
                "Standalone interpolation should not alter surrounding whitespace.")

   (rast-t-case "Ampersand - Standalone"
                #hash{(string . "---")}
                "  {{&string}}\n"
                "  ---\n"
                "Standalone interpolation should not alter surrounding whitespace.")

   ;; Whitespace Insensitivity
   (rast-t-case "Interpolation With Padding"
                #hash{(string . "---")}
                "|{{ string }}|"
                "|---|"
                "Superfluous in-tag whitespace should be ignored.")

   (rast-t-case "Triple Mustache With Padding"
                #hash{(string . "---")}
                "|{{{ string }}}|"
                "|---|"
                "Superfluous in-tag whitespace should be ignored.")

   (rast-t-case "Ampersand With Padding"
                #hash{(string . "---")}
                "|{{& string }}|"
                "|---|"
                "Superfluous in-tag whitespace should be ignored.")))


(run-tests interpolation-tests)