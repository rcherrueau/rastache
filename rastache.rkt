#lang racket/base

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; A racket Mustache template engine.

(require "scanner.rkt"
         "context.rkt"
         "renderer.rkt")

(define (rastache-compile template [open-tag "{{"] [close-tag "}}"])
  (tokenize template open-tag close-tag))

(define (rastache-compile/open-file
         name mustache-file [open-tag "{{"] [close-tag "}}"])
  (define template (open-input-file mustache-file))
  (define tokens (rastache-compile name template open-tag close-tag))

  (when (not (port-closed? template))
    (close-input-port template))

  tokens)

(define (rastache-compile/open-string
         name mustache-string [open-tag "{{"] [close-tag "}}"])
  (define template (open-input-string mustache-string))
  (define tokens (rastache-compile name template open-tag close-tag))

  (when (not (port-closed? template))
    (close-input-port template))

  tokens)

(define (rastache-render tokens context stream)
  (render tokens context stream))
