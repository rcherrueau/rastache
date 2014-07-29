#lang racket/base

(require rastache)

(define template
#<<HERESTRING
<h1>{{title}}{{! just something interesting... or not... }}</h1>
HERESTRING
)

(rastache-compile/render (open-input-string template)
                         `#hash{(title . ,(λ _ "A Comedy of Errors"))}
                         (current-output-port))
