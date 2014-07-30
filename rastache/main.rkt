#lang racket/base

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; Mustache template engine for Racket

(provide rast-ref
         rast-ref*
         (all-defined-out))

; ______________________________________________________________________________
; import and implementation

(require "commons.rkt"
         "parser.rkt"
         "renderer.rkt")

(define (rast-compile template)
  (tokenize template))

(define (rast-compile/open-file mustache-file)
  (define template (open-input-file mustache-file))
  (define tokens (rast-compile template))

  (when (not (port-closed? template))
    (close-input-port template))

  tokens)

(define (rast-compile/open-string mustache-string)
  (define template (open-input-string mustache-string))
  (define tokens (rast-compile template))

  (when (not (port-closed? template))
    (close-input-port template))

  tokens)

(define (rast-render tokens context stream)
  (render tokens context stream))

(define (rast-compile/render template context stream)
  (render (tokenize template) context stream))
