#lang racket/base

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; Mustache template engine for Racket

; Rastache commons.

(provide (all-defined-out))

(require racket/match)

;; Token is a meta-variable for mustache template syntactic
;; categories. Mustache defines 6 syntatic categories, i.e: 'static,
;; 'etag, 'utag, 'section, 'inverted-section and 'partial.
;;
;; see http://mustache.github.io/mustache.5.html for the meaning of
;; each category.
(struct token ()
        #:methods gen:custom-write
        [(define write-proc
           (λ (token port mode)
             (let _token-print ([the-token token]
                                [depth 0])
               (match the-token
                 ;; Static
                 [(token-static content)
                  (write-string (format "~a(token-static ~s)~n"
                                        (make-string depth #\space)
                                        content) port)]
                 ;; Etag
                 [(token-etag key)
                  (write-string (format "~a(token-etag '~s)~n"
                                        (make-string depth #\space)
                                        key) port)]
                 ;; Utag
                 [(token-utag key)
                  (write-string (format "~a(token-utag '~s)~n"
                                        (make-string depth #\space)
                                        key) port)]
                 ;; Section
                 [(token-sec key section dotted?)
                  (write-string (format "~a(token-sec '~s (list~n"
                                        (make-string depth #\space)
                                        key) port)
                  (for-each (λ (t) (_token-print t (+ depth 2))) section)
                  (write-string (format "~a ) ~a)~n"
                                        (make-string (+ depth 2) #\space)
                                        dotted?) port)]
                 ;; Inverted Section
                 [(token-inv-sec key section dotted?)
                  (write-string (format "~a(token-inv-sec '~s (list~n"
                                        (make-string depth #\space)
                                        key) port)
                  (for-each (λ (t) (_token-print t (+ depth 2))) section)
                  (write-string (format "~a ) ~a)~n"
                                        (make-string (+ depth 2) #\space)
                                        dotted?) port)]
                 ;; Partial
                 [(token-partial template)
                  (write-string (format "~a(token-partial ~s)~n"
                                        (make-string depth #\space)
                                        template) port)]

                 ;; Delimiter
                 [(token-delimiter otag ctag)
                  (write-string (format "~a(token-delimiter ~s ~s)~n"
                                        (make-string depth #\space)
                                        otag ctag) port)]

                 ;; Unknown Token
                 [other
                  (write-string (format "~a(token-unknown ~a)~n"
                                        (make-string depth #\space)
                                        other) port)]))))])

;; Static token for static content. `content' contains static text.
(struct token-static token (content))

;; Etag token for variable. `key' contains a key usable with the
;; mustache context.
(struct token-etag token (key))

;; Utag token for unescaped HTML variable. `key' contains a key usable
;; with the mustache context.
(struct token-utag token (key))

;; Section token for section. `key' contains the section name.
;; `section' contains all tokens of this section.
(struct token-sec token (key section dotted?))

;; Inverted Section token for inverted section. `key' contains the
;; section name. `section' contains all tokens of this section.
(struct token-inv-sec token (key section dotted?))

;; Partial token for partials. `template' contains the name of the
;; mustache template to include.
(struct token-partial token (template))

;; Delimiter token for delimiters. `otag' contains mustache opening
;; tag. `ctaf contains mustache closing tag.
(struct token-delimiter token (otag ctag))

;; Symbol in rastach-context behind period tag name.
(define period-name 'self)

;; Mustache open keyword identifier
(define open-tag (make-parameter "{{"))

;; Mustache close keyword identifier
(define close-tag (make-parameter "}}"))

; Use #hash expression and quasiquoting to create the context.
; http://docs.racket-lang.org/guide/hash-tables.html
; http://docs.racket-lang.org/reference/quasiquote.html
; > `#hash((k1 . "v1") (k2 . "v2") (k3 . ,(lambda (x) x)))
; '#hash((k1 . "v1") (k2 . "v2") (k3 . #<procedure>))
; > `#hash((k1 . "v1") (k2 . "v2") (k3 . ,((lambda () 'test))))
; '#hash((k1 . "v1") (k2 . "v2") (k3 . test))

;; Returns the value for a `key' in a rastache context.
(define rast-ref hash-ref)

;; Returns the value for a period key (i.e.: key1.key2.key3) in a
;; rastache context. If no value is found `#f' is returned.
(define (rast-ref* ctx . keys)
  (let _rast-ref* [(ks (reverse keys))]
    (match ks
      [(list k)
       (hash-ref ctx k (hash))]
      [(list k ks ...)
       (hash-ref (_rast-ref* ks) k #f)])))
