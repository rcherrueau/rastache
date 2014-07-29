#lang racket/base

(require rastache)

(define template
#<<HERESTRING
{{#name}}{{name}}{{/name}}
{{#age}}{{age}}{{/age}}
{{#admin}}admin{{/admin}}
HERESTRING
)

(rastache-compile/render (open-input-string template)
                         #hash{ (name . "Jim")
                                (age . 24)
                                (admin . #t) }
                         (current-output-port))
