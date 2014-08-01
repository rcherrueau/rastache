#lang racket/base

(require rastache)

(define template
#<<HERESTRING
{{#person}}{{name}}{{/person}}
{{#person}}{{name}}{{/person}}
HERESTRING
)

(rast-compile/render (open-input-string template)
                     #hash{(person . #hash{(name . "tom")})}
                     (current-output-port))
