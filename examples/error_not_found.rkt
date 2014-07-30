#lang racket/base

(require rastache)

(rast-compile/render (open-input-string "{{foo}}")
                         #hash{(bar . 2)}
                         (current-output-port))
