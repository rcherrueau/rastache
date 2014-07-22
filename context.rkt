#lang racket/base

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; Mustache template engine for Racket

; Macro for mustache expressions.
;
; Use #hash expression and quasiquoting to create the context.
; http://docs.racket-lang.org/guide/hash-tables.html
; http://docs.racket-lang.org/reference/quasiquote.html
; > `#hash((k1 . "v1") (k2 . "v2") (k3 . ,(lambda (x) x)))
; '#hash((k1 . "v1") (k2 . "v2") (k3 . #<procedure>))
; > `#hash((k1 . "v1") (k2 . "v2") (k3 . ,((lambda () 'test))))
; '#hash((k1 . "v1") (k2 . "v2") (k3 . test))
;

(provide rast-ref
         rast-ref*)

; ______________________________________________________________________________
; import and implementation

(require racket/base
         racket/match)

(define rast-ref hash-ref)

(define (rast-ref* ctx . keys)
  (let _rast-ref* [(ks (reverse keys))]
    (match ks
      [(list k)
       (hash-ref ctx k #f)]
      [(list k ks ...)
       (hash-ref (_rast-ref* ks) k #f)])))
