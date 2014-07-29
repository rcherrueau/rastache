#lang racket/base

(require rastache)

(define template
#<<HERESTRING
{{#terms}}
  {{name}}
  {{index}}
{{/terms}}
{{#terms}}
  {{name}}
  {{index}}
{{/terms}}
HERESTRING
)

(rastache-compile/render (open-input-string template)
                         #hash{(terms . [#hash{(name . "t1") (index . 0)}
                                         #hash{(name ."t2") (index . 1)}])}
                         (current-output-port))
