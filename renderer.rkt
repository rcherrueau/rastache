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
         "scanner.rkt")

(define (render tokens context stream)
  (define-values (rastache-ctx rastache-ref) (make-context context))

  ;; Returns `#t' if the value is a rastache context, `#f' otherwise.
  (define rastache-context? hash?)

  ;; Lookup to the correct value. If no value find, then this function
  ;; retun a empty string as in spec.
  ;; lookup: rastace-context -> symbol -> string
  (define (lookup the-ctx the-key)

    ;; Returns `#t' if the lookup calls is done with context updating,
    ;; `#f' otherwise.
    (define context-update? (not (eq? the-ctx rastache-ctx)))

    ;; Returns `#t' if rastache context `the-ctx' contains a value for
    ;; the given `key', `#f' otherwise.
    (define (context-hash-key? the-ctx the-key)
      (hash-has-key? the-ctx the-key))

    ;; Returns the value for `the-key' in context `the-context'.
    (define (lookup-current-context the-ctx the-key)
      (let ([val (rastache-ref the-ctx the-key)])
        (cond [(procedure? val)
               (with-handlers
                   ([exn:fail:contract? (lambda (n) "")])
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
      (let ([val (rastache-ref rastache-ctx the-key)])
        (cond [(procedure? val)
               ; If application fails, then try with general
               ; context
               (with-handlers
                   ([exn:fail:contract?
                     (lambda (n)
                       ; If application fails, then return empty
                       ; string
                       (with-handlers
                           ([exn:fail:contract? (lambda (n) "")])
                       (val rastache-ctx)))])
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
         [(context-hash-key? rastache-ctx the-key)
          (lookup-rastache-context the-ctx the-key)]
         [else ""])
        (cond
         [(context-hash-key? the-ctx the-key)
          (lookup-current-context the-ctx the-key)]
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
         (display (xexpr->string (lookup the-ctx content)) stream)
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
           (for-each (lambda (v)
                       (if (rastache-context? v)
                           (render_ section v)
                           (render_ section the-ctx)))
                     val)]
          [(rastache-context? val)
           (render_ section val)]
          [(boolean? val)
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

  (render_ tokens rastache-ctx))

;; ;; Debug only
;; (let ([template (open-input-file "tests/examples/complex.html")])
;;   (render (tokenize template) 'mock (current-output-port))
;;   (displayln "")
;;   (when (not (port-closed? template))
;;     (close-input-port template)))

;; ;; Mock for context maker
;; (define (make-context context)
;;   (define rastache-ref (lambda (context key)
;;                          ((hash-ref refs key) context)))

;;   (define context
;;     (make-hash
;;      (list
;;       (cons 'header (lambda (self) "Colors"))
;;       (cons 'item (list
;;                    (make-hash
;;                     (list (cons 'name "red")
;;                           (cons 'current #t)
;;                           (cons 'url "#Red")))
;;                    (make-hash
;;                     (list (cons 'name "green")
;;                           (cons 'current #f)
;;                           (cons 'url "#Green")))
;;                    (make-hash
;;                     (list (cons 'name "blue")
;;                           (cons 'current #f)
;;                           (cons 'url "#Blue")))
;;                    ))
;;       (cons 'link (lambda (self) (not (eq? (rastache-ref self 'current) #t))))
;;       (cons 'list (lambda (self) (not (eq? (length (rastache-ref self 'item)) 0))))
;;       (cons 'empty (lambda (self) (eq? (length (rastache-ref self 'item)) 0))))))

;;   (define refs
;;     (make-hash
;;      (list
;;       (cons 'empty (lambda (ctx)  (hash-ref ctx 'empty)))
;;       (cons 'list (lambda (ctx) (hash-ref ctx 'list)))
;;       (cons 'item (lambda (ctx) (hash-ref ctx 'item)))
;;       (cons 'name (lambda (ctx) (hash-ref ctx 'name)))
;;       (cons 'current (lambda (ctx) (hash-ref ctx 'current)))
;;       (cons 'url (lambda (ctx) (hash-ref ctx 'url)))
;;       (cons 'header (lambda (ctx) (hash-ref ctx 'header)))
;;       (cons 'link (lambda (ctx) (hash-ref ctx 'link))))))

;;   (values context rastache-ref))
