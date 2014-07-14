#lang racket/base

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; Mustache template engine for Racket

; Mustache template renderer.
;

(provide render)

; ______________________________________________________________________________
; import and implementation

(require xml
         "context.rkt"
         "scanner.rkt")

;; Returns `#t' if the value is a rastache context, `#f' otherwise.
(define rast-context? hash?)

;; Returns an html escaped string.
(define (htmlescape-string string)
  (regexp-replace* #rx"\""
                   (xexpr->string string)
                   (regexp-replace-quote "&quot;")))

(define period-name '|.|)

(define (lookup context key) (hash-ref context key #f))

(define (var-lookup context key)
  (let ([var (lookup context key)])
    (cond
     ;; If var is a lambda: evaluate it
     [(procedure? var)
      (cond
       ;; 0 or arity-at-least arg
       [(or (eq? (procedure-arity var) 0)
            (arity-at-least? (procedure-arity var)))
        (var)]
       ;; 1 arg
       [(eq? (procedure-arity var) 1)
        (var context)]
       [else
        (error
         "Error: The lambda should have zero or one argument")])]
     ;; Else var is a val: return it
     [else var])))

;; Render a mustache tokens thanks to the rendering context.
;; render: (list token) rast-context port-out -> void
(define (render tokens context stream)
  (let _render ([the-tokens tokens]
                [the-ctx context])
    (cond
     ;; No more tokens
     [(null? the-tokens)]
     ;; Process token
     [else
      (define the-token (car the-tokens))
      (define sigil (token-sigil the-token))
      (define content (let ([content (token-content the-token)])
                        ;; If tag name is a periode ".", change it by
                        ;; 'self key.
                        (if (eq? content period-name)
                            'self
                            content)))
      (define section (token-section the-token))

      (case sigil
        ;; Static content
        ['static
         (display content stream)
         (_render (cdr the-tokens) the-ctx)]

        ;; Variable
        ['etag
         (define val (var-lookup the-ctx content))
         (display (cond
                   [(null? val) ""]
                   [(and (boolean? val) (not val)) ""]
                   [(number? val) (number->string val)]
                   [else (htmlescape-string val)]) stream)
         (_render (cdr the-tokens) the-ctx)]

        ;; Unescaped variable
        ['utag
         (define val (var-lookup the-ctx content))
         (display (cond
                   [(null? val) ""]
                   [(and (boolean? val) (not val)) ""]
                   [(number? val) (number->string val)]
                   [else val]) stream)
         (_render (cdr the-tokens) the-ctx)]

        ;; Section
        ['section
         (define val (lookup the-ctx content))
         (cond
          ;; Non-empty list
          [(and (list? val) (not (null? val)))
           (for-each
            (λ (the-val)
              (_render section
                       (if (rast-context? the-val)
                           ;; Render with the-val context
                           the-val
                           ;; Render with general context overriding
                           ;; by the-val put at 'self position
                           (hash-set the-ctx 'self the-val))))
            val)]
          ;; Lambda
          [(procedure? val)
           (unless (eq? 2 (procedure-arity val))
             (error "Error: The lambda should have two arguments"))

           (display
            ;; FIXME: mustachize has to take into account the update
            ;; of mustahce delimiters
            (val (mustachize section)
                 (λ (txt)
                    (let ([o (open-output-string)])
                      (render (tokenize (open-input-string txt))
                              the-ctx o)
                      (get-output-string o))))
            stream)]
          ;; Non-false value (i.e non-false value, non-empty list,
          ;; non-unexisting key)
          [(and
            ;; non-empty list
            (not (and (list? val) (null? val)))
            ;; non-false value / non-unexisting key
            (not (and (boolean? val) (not val))))
           ;; Render with general context overriding
           ;; by the-val put at 'self position
           (_render section
                    (if (rast-context? val)
                        ;; Render with val context
                        val
                        ;; Render with general context overriding
                        ;; by the-val put at 'self position
                        (hash-set the-ctx 'self val)))])
         (_render (cdr the-tokens) the-ctx)]

        ;; Inverted Section
        ['inverted-section
         (define val (lookup the-ctx content))
         ;; In contrast with section, we call the inverted section if
         ;; tha value is false or the list is empty.
         ;; See https://github.com/janl/mustache.js/issues/186
         (when (or
                ;; empty list
                (and (list? val) (null? val))
                ;; false value / un-existing key
                (and (boolean? val) (not val)))
           (_render section the-ctx))
         (_render (cdr the-tokens) the-ctx)]

        ;; ; TODO Parial
        ;; ['partial ]

        ;; If this is a unknow token, proceed without processing this
        ;; token
        [else
         (_render (cdr the-tokens) the-ctx)])])))
