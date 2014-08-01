#lang racket/base

(require rastache)

(define template
#<<HERESTRING
{{#admin}}Admin.{{/admin}}
{{^admin}}Not Admin.{{/admin}}
{{#person}}Hi {{name}}!{{/person}}
HERESTRING
)

(rast-compile/render (open-input-string template)
                     #hash{ (admin . #f)
                            (person . #hash{(name . "Jim")}) }
                     (current-output-port))
