#lang racket/base

(require "../rastache/rastache.rkt")

(define template
#<<HERESTRING
Hello {{name}}
You have just won ${{value}}!
{{#in_ca}}
Well, ${{ taxed_value }}, after taxes.
{{/in_ca}}
Love, {{owner}}
HERESTRING
)

(rastache-compile/render (open-input-string template)
                         `#hash{(name . "Chris")
                                (value . 10000)
                                (taxed_value . ,(λ (self)
                                                   (let ([val (rast-ref self 'value)])
                                                     (inexact->exact (- val (* val 0.4))))))
                                (in_ca . #t)
                                (owner . ,null)}
                         (current-output-port))
