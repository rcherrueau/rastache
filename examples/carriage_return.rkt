#lang racket/base

(require rastache)

(define template
#<<HERESTRING
<b>
{{foo}}
</b>
HERESTRING
)

(rast-compile/render (open-input-string template)
                     #hash{(foo . "Hello World")}
                     (current-output-port))
