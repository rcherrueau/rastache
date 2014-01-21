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

(require xml
         "scanner.rkt")
; ______________________________________________________________________________
; import and implementation

;; Mock for context maker
(define (make-context context)
  (define rastache-ref (lambda (context key)
                        ((hash-ref refs key) context)))

  (define context
    (make-hash
     (list
      (cons 'header (lambda (self) "Colors"))
      (cons 'item (list
                   (make-hash
                    (list (cons 'name "red")
                          (cons 'current #t)
                          (cons 'url "#Red")))
                   (make-hash
                    (list (cons 'name "green")
                          (cons 'current #f)
                          (cons 'url "#Green")))
                   (make-hash
                    (list (cons 'name "blue")
                          (cons 'current #f)
                          (cons 'url "#Blue")))))
      (cons 'link (lambda (self) (not (eq? (rastache-ref self 'current) #t))))
      (cons 'list (lambda (self) (not (eq? (length (rastache-ref self 'item)) 0))))
      (cons 'empty (lambda (self) (eq? (length (rastache-ref self 'item)) 0))))))

  (define refs
    (make-hash
     (list
      (cons 'empty (lambda (ctx)  (hash-ref ctx 'empty)))
      (cons 'list (lambda (ctx) (hash-ref ctx 'list)))
      (cons 'item (lambda (ctx) (hash-ref ctx 'item)))
      (cons 'name (lambda (ctx) (hash-ref ctx 'name)))
      (cons 'current (lambda (ctx) (hash-ref ctx 'current)))
      (cons 'url (lambda (ctx) (hash-ref ctx 'url)))
      (cons 'header (lambda (ctx) (hash-ref ctx 'header)))
      (cons 'link (lambda (ctx) (hash-ref ctx 'link))))))

  (values context rastache-ref))


(define (render tokens context stream)
  (define-values (rastache-ctx rastache-ref) (make-context context))

  ;; Lookup to the correct value. If no value find, then this function
  ;; retun a empty string as in spec.
  (define (lookup the-ctx the-key)
    (define (rastache-has-key? the-ctx the-key)
      (hash-has-key? the-ctx the-key))

    (define (rastache-lookup the-ctx the-key)
      (define val (rastache-ref the-ctx the-key))
      (cond
       [(procedure? val) (val the-ctx)]
       [else val]))

    (if (not (eq? the-ctx rastache-ctx))
        (cond
         [(rastache-has-key? the-ctx the-key)
          (rastache-lookup the-ctx the-key)]
         [(rastache-has-key? rastache-ctx the-key)
          (rastache-lookup rastache-ctx the-key)]
         [else ""])
        (cond
         [(rastache-has-key? the-ctx the-key)
          (rastache-lookup the-ctx the-key)]
         [else ""])))

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

        ;; Section
        ['section
         (define val (lookup the-ctx content))
         (cond
          [(list? val)
           (map (lambda (the-new-ctx)
                  (render_ section the-new-ctx))
                val)]
          [(hash? val)
           (render_ section val)]
          [(eq? val #f)]
          [else
           (render_ section the-ctx)])
         (render_ (cdr tokens) the-ctx)]

        ;; Proceed without processing this token
        [else
         (render_ (cdr tokens) the-ctx)])]))

  (render_ tokens rastache-ctx))


(let ([template (open-input-file "tests/examples/complex.html")])
  (render (tokenize template) 'mock (current-output-port))
  (displayln "")
  (when (not (port-closed? template))
    (close-input-port template)))
