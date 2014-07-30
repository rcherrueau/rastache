#lang racket/base

(require rastache)

(define template
#<<HERESTRING
{{greeting}}, {{name}}!
HERESTRING
)

(rast-compile/render (open-input-string template)
                         #hash{ (name . "Joe")
                                (greeting . "Welcome") }
                         (current-output-port))
