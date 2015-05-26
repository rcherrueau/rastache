#lang racket/base

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; Mustache template engine for Racket

(provide fillers-tests)

(require rackunit
         rackunit/text-ui
         "../commons.rkt"
         "rastache-test-case.rkt")

;; Global variable for "Interpolation - Multiple Calls" test
(define g 0)

(define fillers-tests
  (test-suite
   "Fillers tests"
   #:before (λ () (set! g 0))

   ;; -- Interpolation
   (rast-t-case "Basic Interpolation"
                #hash()
                "{{+subject:: \"world\"}}Hello, {{subject}}!"
                "Hello, world!"
                (list (token-static "")
                      (token-filler 'subject "world")
                      (token-static "Hello, ")
                      (token-etag 'subject)
                      (token-static "!"))
                "Unadorned tags should interpolate content into the template.")

   (rast-t-case "HTML Escaping"
                #hash()
                "{{+forbidden:: \"& \\\" < >\"}}These characters should be HTML escaped: {{forbidden}}"
                "These characters should be HTML escaped: &amp; &quot; &lt; &gt;"
                (list (token-static "")
                      (token-filler 'forbidden "& \" < >")
                      (token-static "These characters should be HTML escaped: ")
                      (token-etag 'forbidden)
                      (token-static ""))
                "Basic interpolation should be HTML escaped.")

   (rast-t-case "Triple Mustache"
                #hash()
                "{{+forbidden:: \"& \\\" < >\"}}These characters should not be HTML escaped: {{{forbidden}}}"
                "These characters should not be HTML escaped: & \" < >"
                (list (token-static "")
                      (token-filler 'forbidden "& \" < >")
                      (token-static "These characters should not be HTML escaped: ")
                      (token-utag 'forbidden)
                      (token-static ""))
                "Triple mustaches should interpolate without HTML escaping.")

   (rast-t-case "Ampersand"
                #hash()
                "{{+forbidden:: \"& \\\" < >\"}}These characters should not be HTML escaped: {{&forbidden}}"
                "These characters should not be HTML escaped: & \" < >"
                (list (token-static "")
                      (token-filler 'forbidden "& \" < >")
                      (token-static "These characters should not be HTML escaped: ")
                      (token-utag 'forbidden)
                      (token-static ""))
                "Ampersand should interpolate without HTML escaping.")

   (rast-t-case "Basic Integer Interpolation"
                #hash()
                "{{+mph:: 88}}\"{{mph}} miles an hour!\""
                "\"88 miles an hour!\""
                (list (token-static "")
                      (token-filler 'mph 88)
                      (token-static "\"")
                      (token-etag 'mph)
                      (token-static " miles an hour!\""))
                "Integers should interpolate seamlessly.")

   (rast-t-case "Triple Mustache Integer Interpolation"
                #hash()
                "{{+mph:: 88}}\"{{{mph}}} miles an hour!\""
                "\"88 miles an hour!\""
                (list (token-static "")
                      (token-filler 'mph 88)
                      (token-static "\"")
                      (token-utag 'mph)
                      (token-static " miles an hour!\""))
                "Integers should interpolate seamlessly.")

   (rast-t-case "Ampersand Integer Interpolation"
                #hash{}
                "{{+mph:: 88}}\"{{&mph}} miles an hour!\""
                "\"88 miles an hour!\""
                (list (token-static "")
                      (token-filler 'mph 88)
                      (token-static "\"")
                      (token-utag 'mph)
                      (token-static " miles an hour!\""))
                "Integers should interpolate seamlessly.")

   (rast-t-case "Basic Decimal Interpolation"
                #hash{}
                "{{+power:: 1.210}}\"{{power}} jiggawatts!\""
                "\"1.21 jiggawatts!\""
                (list (token-static "")
                      (token-filler 'power 1.210)
                      (token-static "\"")
                      (token-etag 'power)
                      (token-static " jiggawatts!\""))
                "Decimals should interpolate seamlessly with proper significance.")

   (rast-t-case "Triple Mustache Decimal Interpolation"
                #hash{}
                "{{+power:: 1.210}}\"{{{power}}} jiggawatts!\""
                "\"1.21 jiggawatts!\""
                (list (token-static "")
                      (token-filler 'power 1.210)
                      (token-static "\"")
                      (token-utag 'power)
                      (token-static " jiggawatts!\""))
                "Decimals should interpolate seamlessly with proper significance.")

   (rast-t-case "Ampersand Decimal Interpolation"
                #hash{}
                "{{+power:: 1.210}}\"{{&power}} jiggawatts!\""
                "\"1.21 jiggawatts!\""
                (list (token-static "")
                      (token-filler 'power 1.210)
                      (token-static "\"")
                      (token-utag 'power)
                      (token-static " jiggawatts!\""))
                "Decimals should interpolate seamlessly with proper significance.")

   ;; Dotted Names
   (rast-t-case "Dotted Names - Basic Interpolation"
                #hash{}
                "{{+person:: #hash{(name . \"Joe\")} }}\"{{person.name}}\" == \"{{#person}}{{name}}{{/person}}\""
                "\"Joe\" == \"Joe\""
                (list (token-static "")
                      (token-filler 'person #hash{(name . "Joe")})
                      (token-static "\"")
                      (token-sec 'person (list (token-etag 'name)) #t)
                      (token-static "\" == \"")
                      (token-sec 'person (list (token-static "")
                                               (token-etag 'name)
                                               (token-static "")) #f)
                      (token-static "\""))
                "Dotted names should be considered a form of shorthand for sections.")

   (rast-t-case "Dotted Names - Context Precedence"
                #hash{}
                "{{+a:: #hash{( b . #hash() )} }}{{+b:: #hash{( c . \"ERROR\" )} }}{{#a}}{{b.c}}{{/a}}"
                ""
                (list
                 (token-static "")
                 (token-filler 'a #hash{( b . #hash() )})
                 (token-static "")
                 (token-filler 'b #hash{( c . "ERROR" )})
                 (token-static "")
                 (token-sec 'a `(,(token-static "")
                                 ,(token-sec 'b `(,(token-etag 'c)) #t)
                                 ,(token-static ""))
                            #f)
                 (token-static ""))
                "Dotted names should be resolved against former resolutions.")

   ;; -- Lambda
   (rast-t-case "Interpolation"
                #hash{}
                ;; FIXME: using λ in definition doesn't work; maybe a
                ;; bug of byte->string/utf-8 in parser.
                "{{+lambda:: (lambda _ \"world\")}}Hello, {{lambda}}!"
                "Hello, world!"
                "A lambda's return value should be interpolated.")

   (rast-t-case "Interpolation - Expansion"
                #hash{}
                "{{+planet:: \"world\"}}{{+lambda:: (lambda (_ render) (render \"{{planet}}\"))}}Hello, {{lambda}}!"
                "Hello, world!"
                "A lambda's return value should be parsed.")


   ;; Note: put g in the namespace of mustache-ns
   (namespace-set-variable-value! 'g 0 #t mustache-ns)
   (rast-t-case "Interpolation - Multiple Calls"
                #hash{}
                "{{+lambda:: (lambda _ (set! g (add1 g)) g)}}{{lambda}} == {{{lambda}}} == {{lambda}}"
                "1 == 2 == 3"
                "Interpolated lambdas should not be cached.")

   (rast-t-case "Escaping"
                #hash{}
                "<{{+lambda:: (lambda _ \">\")}}{{lambda}}{{{lambda}}}"
                "<&gt;>"
                "Lambda results should be appropriately escaped.")

   (rast-t-case "Section"
                #hash{}
                "<{{+x:: \"Error!\"}}{{+lambda:: (lambda (text _)(if (equal? text \"{{x}}\") \"yes\" \"no\")))}}{{#lambda}}{{x}}{{/lambda}}>"
                "<yes>"
                "Lambdas used for sections should receive the raw section string.")

   (rast-t-case "Section - Expansion"
                #hash{}
                "<{{+planet:: \"Earth\"}}{{+lambda:: (lambda (text render)(render (string-append text \"{{planet}}\" text)))}}{{#lambda}}-{{/lambda}}>"
                "<-Earth->"
                "Lambdas used for sections should have their results parsed.")

   (rast-t-case "Section - Multiple Calls"
                #hash{}
                "{{+lambda:: (lambda (text render) (render (string-append \"__\" text \"__\")))}}{{#lambda}}FILE{{/lambda}} != {{#lambda}}LINE{{/lambda}}"
                "__FILE__ != __LINE__"
                "Lambdas used for sections should not be cached.")

   (rast-t-case "Inverted Section"
                #hash{}
                "{{+lambda:: (lambda (text) #f)}}<{{^lambda}}{{static}}{{/lambda}}>"
                "<>"
                "Lambdas used for inverted sections should be considered truthy.")))
