#lang racket/base

(require "../rastache.rkt"
         "../context.rkt")

(define template
#<<HERESTRING
Hello {{person.name}}
You have just won ${{price.value}}!
{{#person.in_ca}}
Well, ${{ states.ca.taxed_value }}, after taxes.
{{/person.in_ca}}
HERESTRING
)

(rastache-compile/render (open-input-string template)
                         `#hash{(person . #hash{(name . "Chris") (in_ca . #t)})
                                (price  . #hash{(value . 10000)})
                                (states . #hash{(ca .
                                                    #hash{(taxed_value .
                                                            ,(λ (self)
                                                                (let ([val
                                                                       (rast-ref* self 'price 'value)])
                                                                  (- val (* val 0.4)))))})})}
                         (current-output-port))
