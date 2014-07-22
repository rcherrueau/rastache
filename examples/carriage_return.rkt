#lang racket/base

(require "../rastache/rastache.rkt")

(define template
#<<HERESTRING
<b>
{{foo}}
</b>
HERESTRING
)

(rastache-compile/render (open-input-string template)
                         #hash{(foo . "Hello World")}
                         (current-output-port))
