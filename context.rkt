#lang racket/base

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; A racket Mustache template engine.

(require syntax/parse)

; ______________________________________________________________________________
; import and implementation

; rastache-expr := '( rastache-data ... )
(define-syntax-class rastache-expr
  #:description "rastache expression"
  (pattern '( rd:rastache-data ...+ )))

; rastache-data := ( key rastache-val ...)
(define-syntax-class rastache-data
  #:description "rastache data"
  (pattern (k rv:rastache-val ...+)))

; rastache-val := rastache-expr
;               | expr
(define-syntax-class rastache-val
  (pattern re:rastache-expr)
  (pattern rhs:expr))
