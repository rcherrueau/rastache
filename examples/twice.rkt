#lang racket/base

(require "../rastache/rastache.rkt")

(define template
#<<HERESTRING
{{#person}}{{name}}{{/person}}
{{#person}}{{name}}{{/person}}
HERESTRING
)

(rastache-compile/render (open-input-string template)
                         #hash{(person . #hash{(name . "tom")})}
                         (current-output-port))
