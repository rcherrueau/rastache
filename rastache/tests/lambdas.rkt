#lang racket/base

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; Mustache template engine for Racket

; Lambdas are a special-cased data type for use in interpolations and
; sections.
;
; When used as the data value for an Interpolation tag, the lambda
; MUST be treatable as an arity 0 function, and invoked as such. The
; returned value MUST be rendered against the default delimiters, then
; interpolated in place of the lambda.
;
; When used as the data value for a Section tag, the lambda MUST be
; treatable as an arity 1 function, and invoked as such (passing a
; String containing the unprocessed section contents). The returned
; value MUST be rendered against the current delimiters, then
; interpolated in place of the section.
(provide lambdas-tests)

(require rackunit
         rackunit/text-ui
         "../commons.rkt"
         "rastache-test-case.rkt")

;; Global variable for "Interpolation - Multiple Calls" test
(define g 0)

(define lambdas-tests
  (test-suite
   "Lambdas tests"
   #:before (λ () (set! g 0))
   (rast-t-case "Interpolation"
                `#hash{( lambda . ,(λ _ "world") )}
                "Hello, {{lambda}}!"
                "Hello, world!"
                (list (token-delimiter "{{" "}}")
                      (token-static "Hello, ")
                      (token-etag 'lambda)
                      (token-static "!"))
                "A lambda's return value should be interpolated.")

   (rast-t-case "Interpolation - Expansion"
                `#hash{( planet . "world" )
                       ( lambda . ,(λ (_ render) (render "{{planet}}")) )}
                "Hello, {{lambda}}!"
                "Hello, world!"
                (list (token-delimiter "{{" "}}")
                      (token-static "Hello, ")
                      (token-etag 'lambda)
                      (token-static "!"))
                "A lambda's return value should be parsed.")

   (rast-t-case "Interpolation - Alternate Delimiters"
                `#hash{( planet . "world" )
                       ( lambda . ,(λ (_ render) (render "|planet| => {{planet}}")) )}
                "{{= | | =}}\nHello, (|&lambda|)!"
                "Hello, (|planet| => world)!"
                (list (token-delimiter "{{" "}}")
                      (token-delimiter "|" "|")
                      (token-static "Hello, (")
                      (token-utag 'lambda)
                      (token-static ")!"))
                "A lambda's return value should parse with the default delimiters.")

   (rast-t-case "Interpolation - Multiple Calls"
                `#hash{( lambda . ,(λ _ (set! g (add1 g)) g) )}
                "{{lambda}} == {{{lambda}}} == {{lambda}}"
                "1 == 2 == 3"
                (list (token-delimiter "{{" "}}")
                      (token-static "")
                      (token-etag 'lambda)
                      (token-static " == ")
                      (token-utag 'lambda)
                      (token-static " == ")
                      (token-etag 'lambda)
                      (token-static ""))
                "Interpolated lambdas should not be cached.")

   (rast-t-case "Escaping"
                `#hash{( lambda . ,(λ _ ">") )}
                "<{{lambda}}{{{lambda}}}"
                "<&gt;>"
                (list (token-delimiter "{{" "}}")
                      (token-static "<")
                      (token-etag 'lambda)
                      (token-static "")
                      (token-utag 'lambda)
                      (token-static ""))
                "Lambda results should be appropriately escaped.")

   (rast-t-case "Section"
                `#hash{( x      . "Error!" )
                       ( lambda . ,(λ (text _)
                                      (if (equal? text "{{x}}")
                                          "yes" "no")) )}
                "<{{#lambda}}{{x}}{{/lambda}}>"
                "<yes>"
                (list (token-delimiter "{{" "}}")
                      (token-static "<")
                      (token-sec 'lambda (list (token-static "")
                                               (token-etag 'x)
                                               (token-static ""))
                                 #f)
                      (token-static ">"))
                "Lambdas used for sections should receive the raw section string.")

   (rast-t-case "Section - Expansion"
                `#hash{ (planet . "Earth")
                        (lambda . ,(λ (text render)
                                      (render (string-append text
                                                             "{{planet}}"
                                                             text)))) }
                "<{{#lambda}}-{{/lambda}}>"
                "<-Earth->"
                (list (token-delimiter "{{" "}}")
                      (token-static "<")
                      (token-sec 'lambda (list (token-static "-")) #f)
                      (token-static ">"))
                "Lambdas used for sections should have their results parsed.")

   (rast-t-case "Section - Alternate Delimiters"
                `#hash{ (planet . "Earth")
                        (lambda . ,(λ (text render)
                                      (render
                                       (string-append text
                                                      "{{planet}} => |planet|"
                                                      text)))) }
                "{{= | | =}}<|#lambda|-|/lambda|>"
                "<-{{planet}} => Earth->"
                (list (token-delimiter "{{" "}}")
                      (token-static "")
                      (token-delimiter "|" "|")
                      (token-static "<")
                      (token-sec 'lambda (list (token-static "-")) #f)
                      (token-static ">"))
                "Lambdas used for sections should parse with the current delimiters.")

   (rast-t-case "Section - Multiple Calls"
                `#hash{(lambda . ,(λ (text render)
                                     (render (string-append "__" text "__")))) }
                "{{#lambda}}FILE{{/lambda}} != {{#lambda}}LINE{{/lambda}}"
                "__FILE__ != __LINE__"
                (list (token-delimiter "{{" "}}")
                      (token-static "")
                      (token-sec 'lambda (list (token-static "FILE")) #f)
                      (token-static " != ")
                      (token-sec 'lambda (list (token-static "LINE")) #f)
                      (token-static ""))
                "Lambdas used for sections should not be cached.")

   (rast-t-case "Inverted Section"
                `#hash{( lambda . ,(λ (text) #f) )}
                "<{{^lambda}}{{static}}{{/lambda}}>"
                "<>"
                (list (token-delimiter "{{" "}}")
                      (token-static "<")
                      (token-inv-sec 'lambda (list (token-static "")
                                                   (token-etag 'static)
                                                   (token-static ""))
                                     #f)
                      (token-static ">"))
                "Lambdas used for inverted sections should be considered truthy.")))
