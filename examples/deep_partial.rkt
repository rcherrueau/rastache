#lang racket/base

(require rastache)

(define template
#<<HERESTRING
<h1>First: {{title}}</h1>
{{>partial.html}}
HERESTRING
)

(rastache-compile/render (open-input-string template)
                         #hash{ (title . "Welcome") }
                         (current-output-port))
