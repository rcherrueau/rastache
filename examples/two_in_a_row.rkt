#lang racket/base

(require rastache)

(define template
#<<HERESTRING
{{greeting}}, {{name}}!
HERESTRING
)

(rastache-compile/render (open-input-string template)
                         #hash{ (name . "Joe")
                                (greeting . "Welcome") }
                         (current-output-port))
