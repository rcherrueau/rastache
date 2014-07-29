#lang racket/base

(require rastache)

(define template
#<<HERESTRING
{{#admin}}Admin.{{/admin}}
{{^admin}}Not Admin.{{/admin}}
{{#person}}Hi {{name}}!{{/person}}
HERESTRING
)

(rastache-compile/render (open-input-string template)
                         #hash{ (admin . #f)
                                (person . #hash{(name . "Jim")}) }
                         (current-output-port))
