#lang racket/base

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; A racket Mustache template engine.

; Mustache template renderer.
;

(provide render)

; ______________________________________________________________________________
; import and implementation

(require xml
         "context.rkt"
         "scanner.rkt")

(define (render tokens context stream)
  ;; Returns `#t' if the value is a rastache context, `#f' otherwise.
  (define rastache-context? hash?)

  ;; Lookup to the correct value. If no value find, then this function
  ;; retun a empty string as in spec.
  ;; lookup: rastace-context -> symbol -> string
  (define (lookup the-ctx the-key)

    ;; Returns `#t' if the lookup calls is done with context updating,
    ;; `#f' otherwise.
    (define context-update? (not (eq? the-ctx context)))

    ;; Returns `#t' if rastache context `the-ctx' contains a value for
    ;; the given `key', `#f' otherwise.
    (define (context-hash-key? the-ctx the-key)
      (hash-has-key? the-ctx the-key))

    ;; Returns the value for `the-key' in context `the-context'.
    (define (lookup-current-context the-ctx the-key)
      (let ([val (rast-ref the-ctx the-key)])
        (cond [(procedure? val)
               (with-handlers
                   ([exn:fail:contract? (λ (n) (displayln n) "")])
                 (val the-ctx))]
              [else val])))

    ;; If we are in a context update case, and the value is a
    ;; procedure, then, the context passes to the procedure could be
    ;; the current context or the general context. According to the
    ;; specification, first we execute the procedure with the current
    ;; context. If a `exn:contract:fail' exception is raised, next we
    ;; execute the procedure with the general context. Finally, if we
    ;; still get an error, we return the empty string as result of the
    ;; application of the procedure.
    (define (lookup-rastache-context current-ctx the-key)
      (let ([val (rast-ref context the-key)])
        (cond [(procedure? val)
               ; If application fails, then try with general
               ; context
               (with-handlers
                   ([exn:fail:contract?
                     (λ (n)
                       ; If application fails, then return empty
                       ; string
                       (with-handlers
                           ([exn:fail:contract? (λ (n) "")])
                       (val context)))])
                 (val current-ctx))]
              [else val])))

    ; During the lookup, there are two different case:
    ; - Context Update case :: The lookup is done first on the current
    ;   context. If there is no result, the look up is done then on
    ;   the general context.
    ; - Otherwise case :: The lookup is done on the current context.
    (if context-update?
        (cond
         ; Do the lookup on the current context
         [(context-hash-key? the-ctx the-key)
          (lookup-current-context the-ctx the-key)]
         ; Do the lookup on the general context
         [(context-hash-key? context the-key)
          (lookup-rastache-context the-ctx the-key)]
         [else ""])
        (cond
         [(context-hash-key? context the-key)
          (lookup-current-context context the-key)]
         [else ""])))

  ;; Do the render on tokens recursively.
  ;; render_: (listof token) rastache-context -> void
  (define (render_ tokens the-ctx)
    (cond
     [(null? tokens)]
     [else
      (define token (car tokens))
      (define sigil (token-sigil token))
      (define content (token-content token))
      (define section (token-section token))

      (case sigil
        ; Static content
        ['static
         (display content stream)
         (render_ (cdr tokens) the-ctx)]

        ; Variable
        ['etag
         (define val (lookup the-ctx content))
         (display (cond
                   [(null? val) ""]
                   [(number? val) (number->string val)]
                   [else (xexpr->string val)]) stream)
         (render_ (cdr tokens) the-ctx)]

        ; Unescaped variable
        ['utag
         (display (lookup the-ctx content) stream)
         (render_ (cdr tokens) the-ctx)]

        ; Section
        ['section
         (define val (lookup the-ctx content))
         (cond
          [(list? val)
           (for-each (λ (v)
                       (if (rastache-context? v)
                           (render_ section v)
                           (render_ section the-ctx)))
                     val)]
          [(rastache-context? val)
           (render_ section val)]
          [(boolean? val)
           ; if val is not `#t' do nothing. This case will be process
           ; in `'inverted-section' if any.
           (when val (render_ section the-ctx))]
          [else
           (render_ section the-ctx)])
         (render_ (cdr tokens) the-ctx)]

        ; Inverted Section
        ['inverted-section
         (define val (lookup the-ctx content))
         ; In contrast with section, we call the inverted section if
         ; tha value is false or the list is empty.
         ; See https://github.com/janl/mustache.js/issues/186
         (when (or (not val)
                   (and (list? val) (null? val)))
           (render_ section the-ctx))
         (render_ (cdr tokens) the-ctx)]

        ;; ; TODO Parial
        ;; ['partial ]

        ; If this is a unknow token, proceed without processing this
        ; token
        [else
         (render_ (cdr tokens) the-ctx)])]))

  (render_ tokens context))
