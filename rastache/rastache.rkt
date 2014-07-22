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
         "scanner.rkt"
         "renderer.rkt")

(define (rastache-compile template)
  (tokenize template))

(define (rastache-compile/open-file mustache-file)
  (define template (open-input-file mustache-file))
  (define tokens (rastache-compile template))

  (when (not (port-closed? template))
    (close-input-port template))

  tokens)

(define (rastache-compile/open-string mustache-string)
  (define template (open-input-string mustache-string))
  (define tokens (rastache-compile template))

  (when (not (port-closed? template))
    (close-input-port template))

  tokens)

(define (rastache-render tokens context stream)
  (render tokens context stream))

(define (rastache-compile/render template context stream)
  (render (tokenize template) context stream))
