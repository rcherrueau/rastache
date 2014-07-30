#lang racket/base

(require rastache)

(define template
#<<HERESTRING
{{#person}}
  Name: {{name}}
{{/person}}
HERESTRING
)

(rast-compile/render (open-input-string template)
                         #hash{ (person . #hash{ (name . "Chris") }) }
                         (current-output-port))
