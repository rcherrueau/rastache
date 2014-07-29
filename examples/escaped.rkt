#lang racket/base

(require rastache)

(define template
#<<HERESTRING
<h1>{{title}}</h1>
HERESTRING
)

(rastache-compile/render (open-input-string template)
                         `#hash{(title . ,(λ _ "Bear > Shark"))}
                         (current-output-port))
