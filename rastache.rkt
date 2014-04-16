#lang racket/base

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; Mustache template engine for Racket

(provide (all-defined-out))

; ______________________________________________________________________________
; import and implementation

(require "scanner.rkt"
         "context.rkt"
         "renderer.rkt")

(define (rastache-compile template [open-tag "{{"] [close-tag "}}"])
  (tokenize template open-tag close-tag))

(define (rastache-compile/open-file
         mustache-file [open-tag "{{"] [close-tag "}}"])
  (define template (open-input-file mustache-file))
  (define tokens (rastache-compile template open-tag close-tag))

  (when (not (port-closed? template))
    (close-input-port template))

  tokens)

(define (rastache-compile/open-string
         mustache-string [open-tag "{{"] [close-tag "}}"])
  (define template (open-input-string mustache-string))
  (define tokens (rastache-compile template open-tag close-tag))

  (when (not (port-closed? template))
    (close-input-port template))

  tokens)

(define (rastache-render tokens context stream)
  (render tokens context stream))

(define (rastache-compile/render
         template context stream [open-tag "{{"] [close-tag "}}"])
  (render (tokenize template open-tag close-tag) context stream))
