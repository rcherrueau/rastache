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
(require racket/match
         xml
         "context.rkt"
         "scanner.rkt")

;; Returns `#t' if the value is a rastache context, `#f' otherwise.
(define rast-context? hash?)

;; Returns an html escaped string.
(define (htmlescape-string string)
  (regexp-replace* #rx"\""
                   (xexpr->string string)
                   (regexp-replace-quote "&quot;")))

(define period-name 'self)

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

        ;; Section
        [(token-sec key section _)
         (define val (lookup the-ctx key))
         (cond
          ;; Section key is a Non-empty list
          [(and (list? val) (not (null? val)))
           (for-each
            (λ (the-val)
               (_render section
                        (if (rast-context? the-val)
                            ;; Render with the-val context
                            the-val
                            ;; Render with general context overriding
                            ;; by the-val put at `period-name'
                            ;; position
                            (hash-set the-ctx period-name the-val))))
            val)]
          ;; Section key is a Lambda
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
           ;; Render with general context overriding by the-val put at
           ;; `period-name' position
           (_render section
                    (if (rast-context? val)
                        ;; Render with val context
                        val
                        ;; Render with general context overriding by
                        ;; the-val put at `period-name' position
                        (hash-set the-ctx period-name val)))])
         (_render (cdr the-tokens) the-ctx)]

        ;; Inverted Section
        [(token-inv-sec key inv-section #f)
         (define val (lookup the-ctx key))
         ;; In contrast with section, we call the inverted section if
         ;; tha value is false or the list is empty.
         (when (or
                ;; empty list
                (and (list? val) (null? val))
                ;; false value / un-existing key
                (and (boolean? val) (not val)))
           (_render inv-section the-ctx))
         (_render (cdr the-tokens) the-ctx)]

        ;; Inverted Section with Dotted Names
        [(token-inv-sec key inv-section #t)
         ;; If val is evaluated to false, go to the last inverted
         ;; section of this dotted name and renders `inv-section'.
         ;; Else go deeper and test again.
         (define val (lookup the-ctx key))
         (if (or
              ;; empty list
              (and (list? val) (null? val))
              ;; false value / un-existing key
              (and (boolean? val) (not val)))

             ;; False value
             (let render-inv-sec ([t (car inv-section)])
               (match t
                 ;; Not the last inverted section of this dotted name
                 ;; => Go deeper
                 [(token-inv-sec k is #t)
                  (render-inv-sec (car is))]
                 ;; Last inverted section of this dotted name
                 ;; => render section
                 [(token-inv-sec k is #f)
                  (_render is the-ctx)]))

             ;; True value:
             ;; Render with context seting to val
             (_render inv-section
                    (if (rast-context? val)
                        ;; Render with val context
                        val
                        ;; Render with context setting to val
                        `#hash{( self . ,val )})))
         (_render (cdr the-tokens) the-ctx)]

        ;; Partial
        [(token-partial template)
         ;; TODO: implements me!
         (_render (cdr the-tokens) the-ctx)]

        ;; If this is a unknow token: Error!
        [other
         (error (format "Unknow token type~a~n" other))])])))
