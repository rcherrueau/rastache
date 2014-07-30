#lang racket/base

(require rastache)

(define template
#<<HERESTRING
{{=<% %>=}}* <% first %>
* <% second %>
<%=lol lol=%>
* lol third lol
lol={{ }}=lol
* {{ fourth }}
HERESTRING
)

(rast-compile/render (open-input-string template)
                         #hash{ (first . "It worked the first time.")
                                (second . "And it worked the second time.")
                                (third . "Then, surprisingly, it worked the third time.")
                                (fourth . "Fourth time also fine!.") }
                         (current-output-port))
