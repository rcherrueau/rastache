#lang racket/base

(require "../rastache/rastache.rkt")

(define template
#<<HERESTRING
<h1>{{title}}</h1>
{{>inner_partial.html}}
HERESTRING
)

(rastache-compile/render (open-input-string template)
                         #hash{ (title . "Welcome") }
                         (current-output-port))
