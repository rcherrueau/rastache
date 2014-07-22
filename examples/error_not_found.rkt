#lang racket/base

(require "../rastache.rkt"
         "../context.rkt")

(rastache-compile/render (open-input-string "{{foo}}")
                         #hash{(bar . 2)}
                         (current-output-port))
