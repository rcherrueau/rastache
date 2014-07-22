#lang racket/base

(require "../rastache/rastache.rkt")

(rastache-compile/render (open-input-string "{{foo}}")
                         #hash{(bar . 2)}
                         (current-output-port))
