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
; Reads a list of tokens and renders the template. The rendering is
; done in a stream.
(provide render)

; ______________________________________________________________________________
; import and implementation
(require "commons.rkt"
         "parser.rkt"
         racket/match
         net/url
         xml)

;; Returns #t if value is a rastache context, #f otherwise.
(define rast-context? hash?)

;; Returns the value of `key' in a rastache context if any. Otherwise
;; #f.
(define (lookup context key) (hash-ref context key #f))

;; Returns the value of `key' in a rastache context if any. If the
;; value is a lambda, then the lambda is applied. If `key' doesn't
;; exist in the rastache context, it returns #f.
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
       ;; 1 arg: give context
       [(eq? (procedure-arity var) 1)
        (var context)]
       ;; 2 args: give context and render-function
       [(eq? (procedure-arity var) 2)
        (var context
             (λ (txt)
                (let ([o (open-output-string)])
                  (render
                   ;; A lambda's return value should be parse with the
                   ;; default delimiters (see Lambdas tests >
                   ;; Interpolation - Alternate Delimiters)
                   (parameterize ([open-tag "{{"]
                                  [close-tag "}}"])
                    (tokenize (open-input-string txt)))
                   context
                   o)
                  (get-output-string o))))]
       [else
        (error (format (string-append "Error: The lambda ~s "
                                      "should have zero, one "
                                      "or two argument(s)") var))])]
     ;; Else var is a val: return it
     [else var])))

;; Returns the value of a `key' in a rastache context when current
;; token is a section or inverted section. If `key' doesn't exist in
;; the rastache context, it returns #f.
(define (sec-lookup context key)
  (let ([the-val (lookup context key)])
    (cond
     ;; In section, if the val is a procedure with an arity different
     ;; of 2, the procedure should be applied.
     [(and (procedure? the-val)
           (not (equal? (procedure-arity the-val) 2)))
      (var-lookup context key)]
     [else the-val])))

;; Returns #t if `val' is a rastache non-false value (i.e.: non-false
;; value, non-empty list, non-unexisting key). Otherwise #f.
(define (non-false? val)
  (and
   ;; non-empty list
   (not (and (list? val) (null? val)))
   ;; non-false value / non-unexisting key
   (not (and (boolean? val) (not val)))))

;; Returns #t if `val' is a rastache non-empty list. Otherwise #f.
(define (non-empty-list? val)
  (and (list? val) (not (null? val))))

;; Returns an html escaped string.
(define (htmlescape-string string)
  (regexp-replace* #rx"\""
                   (xexpr->string string)
                   (regexp-replace-quote "&quot;")))

;; Render a mustache tokens thanks to the rendering context.
;; render: (list token) rast-context port-out -> void
(define (render tokens context stream)

  (let _render ([the-tokens tokens]
                [the-ctx context])
    (cond
     ;; No more tokens
     [(null? the-tokens)
      (void)]
     ;; Process token
     [else
      (define the-token (car the-tokens))
      (match the-token
        ;; Static
        [(token-static content)
         (display content stream)
         (_render (cdr the-tokens) the-ctx)]

        ;; Etag
        [(token-etag key)
         (define val (var-lookup the-ctx key))
         (display (cond
                   [(null? val) ""]
                   [(and (boolean? val) (not val)) ""]
                   [(number? val) (number->string val)]
                   [else (htmlescape-string val)]) stream)
         (_render (cdr the-tokens) the-ctx)]

        ;; Utag
        [(token-utag key)
         (define val (var-lookup the-ctx key))
         (display (cond
                   [(null? val) ""]
                   [(and (boolean? val) (not val)) ""]
                   [(number? val) (number->string val)]
                   [else val]) stream)
         (_render (cdr the-tokens) the-ctx)]

        ;; Dotted Name
        [(token-sec key section #t)
         (define val (var-lookup the-ctx key))
         (cond
          ;; Non-false value
          [(non-false? val)
           (_render section
                    (cond
                     ;; `val' is rastache context and this is a dotted
                     ;; name. Render with `val' context
                     [(rast-context? val) val]
                     ;; `val' is not a rastache context. Render with
                     ;; general context overriding by `val' put at
                     ;; `period-name' position
                     [else (hash-set the-ctx period-name val)]))])
         (_render (cdr the-tokens) the-ctx)]

        ;; Section
        [(token-sec key section #f)
         (define val (sec-lookup the-ctx key))
         (cond
          ;; Non-empty list
          [(non-empty-list? val)
           ;; Render for each items of the list
           (for-each
            (λ (the-val)
               (_render section
                        (cond
                         ;; `the-val' is rastache context but this is
                         ;; not a dotted name section. Render with
                         ;; general context overriding by `the-val'
                         ;; content
                         [(rast-context? the-val)
                          (foldl (λ (kv ctx) (hash-set ctx (car kv) (cdr kv)))
                                 the-ctx (hash->list the-val))]
                         ;; `the-val' is not a rastache context.
                         ;; Render with general context overriding by
                         ;; `the-val' put at `period-name' position
                         [else (hash-set the-ctx period-name the-val)])))
            val)]
          ;; Lambda
          [(procedure? val)
           (unless (eq? (procedure-arity val) 2)
             (error (format (string-append "Error: The lambda ~s "
                                           "should have zero, one "
                                           "or two argument(s)") val)))
           ;; Pass text and render-function as arguments
           (display
            (val (mustachize section)
                 (λ (txt) (let ([o (open-output-string)])
                            (render (tokenize (open-input-string txt))
                                    the-ctx
                                    o)
                            (get-output-string o))))
            stream)]
          ;; Non-false value
          [(non-false? val)
           (_render section
                    (cond
                     ;; `val' is rastache context but this is not a
                     ;; dotted name section. Render with general
                     ;; context overriding by `val' content
                     [(rast-context? val)
                      (foldl (λ (kv ctx) (hash-set ctx (car kv) (cdr kv)))
                             the-ctx (hash->list val))]
                     ;; `val' is not a rastache context.Render with
                     ;; general context overriding by `val' put at
                     ;; `period-name' position
                     [else (hash-set the-ctx period-name val)]))])
         (_render (cdr the-tokens) the-ctx)]

        ;; Inverted Section
        [(token-inv-sec key inv-section #f)
         (define val (sec-lookup the-ctx key))
         ;; In contrast with section, we call the inverted section if
         ;; tha value is false, the list is empty or the key is
         ;; missed.
         (when (not (non-false? val))
           (_render inv-section the-ctx))
         (_render (cdr the-tokens) the-ctx)]

        ;; Inverted Section with Dotted Names
        [(token-inv-sec key inv-section #t)
         ;; If val is evaluated to false, go to the last inverted
         ;; section of this dotted name and renders `inv-section'.
         ;; Otherwise go deeper and test again.
         (define val (sec-lookup the-ctx key))
         (cond
          [(not (non-false? val))
           ;; Render the deepest inv-section
           (let render-inv-sec ([t (car inv-section)])
             (match t
               ;; Not the last inverted section of this dotted name
               ;; => Go deeper
               [(token-inv-sec k is #t)
                (render-inv-sec (car is))]
               ;; Last inverted section of this dotted name
               ;; => Render section
               [(token-inv-sec k is #f)
                (_render is the-ctx)]))]
          [else
           ;; Render with context seting to val
           (_render inv-section
                    (cond
                     ;; `val' is rastache context and this is a dotted
                     ;; name. Render with `val' context
                     [(rast-context? val) val]
                     ;; `val' is not a rastache context. Render with
                     ;; general context overriding by `val' put at
                     ;; `period-name' position
                     [else (hash-set the-ctx period-name val)]))])
         (_render (cdr the-tokens) the-ctx)]

        ;; Partial
        [(token-partial template-url)
         (define protocol (url-scheme template-url))
         (cond
          ;; If url has no protocol, the default is `file'
          [(or (not protocol) (equal? protocol "file"))
           (define partial-template
             (open-input-file (url->path template-url)))
           (render (tokenize partial-template) the-ctx stream)
           (when (not (port-closed? partial-template))
             (close-input-port partial-template))]
          [else
           (error (format (string-append "Error: The url ~s provide "
                                         "an unsuported protocol")
                          (url->string template-url)))])
         (_render (cdr the-tokens) the-ctx)]

        ;; Delimiter
        [(token-delimiter new-otag new-ctag)
         (parameterize ([open-tag new-otag]
                        [close-tag new-ctag])
           (_render (cdr the-tokens) the-ctx))]

        ;; If this is a unknow token: Error!
        [other
         (error (format (string-append "Unknown token ~s "
                                       "while rendering") other))])])))
