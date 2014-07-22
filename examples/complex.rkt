#lang racket/base

(require "../rastache/rastache.rkt")

(define template
#<<HERESTRING
<h1>{{header}}</h1>{{#list}}<ul>{{#item}}{{#current}}<li><strong>{{name}}</strong></li>{{/current}}{{#link}}<li><a href="{{url}}">{{name}}</a></li>{{/link}}{{/item}}</ul>{{/list}}{{#empty}}<p>The list is empty.</p>{{/empty}}
HERESTRING
)

(rastache-compile/render (open-input-string template)
                         `#hash{(header . ,(λ _ "Colors"))
                                (item . [#hash{(name . "red") (current . #t) (url . "#Red")}
                                         #hash{(name . "green") (current . #f) (url . "#Green")}
                                         #hash{(name . "blue") (current . #f) (url . "#Blue")}])
                                (link . ,(λ (self) (not (eq? (rast-ref self 'current) #t))))
                                (list . ,(λ (self) (not (eq? (length (rast-ref self 'item)) 0))))
                                (empty . ,(λ (self) (eq? (length (rast-ref self 'item)) 0)))}
                         (current-output-port))
