#lang racket/base

(require "../rastache/rastache.rkt")

(define template
#<<HERESTRING
{{ name }}
{{ description }}
{{#terms}}
  {{name}}
  {{index}}
{{/terms}}
HERESTRING
)

(rastache-compile/render (open-input-string template)
                         #hash{(name . "name")
                               (description . "desc")
                               (terms . [#hash{(name . "t1") (index . 0)}
                                         #hash{(name . "t2") (index . 1)}])}
                         (current-output-port))
