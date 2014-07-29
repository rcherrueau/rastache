#lang racket/base

(require rastache)

(define template
#<<HERESTRING
{{#person}}
  Name: {{name}}
{{/person}}
HERESTRING
)

(rastache-compile/render (open-input-string template)
                         #hash{ (person . #hash{ (name . "Chris") }) }
                         (current-output-port))
