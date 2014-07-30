#lang racket/base

(require rastache)

(define template
#<<HERESTRING
<h1>{{title}}</h1>
{{>inner_partial.html}}
HERESTRING
)

(rast-compile/render (open-input-string template)
                         #hash{ (title . "Welcome") }
                         (current-output-port))
