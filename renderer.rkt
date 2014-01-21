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
      (cons 'empty (lambda (self) "Colors"))
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

  (define (lookup the-ctx the-key)
    (define val (rastache-ref the-ctx the-key))

    (cond
     [(procedure? val) (val the-ctx)]
     [else val]))

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
         (print content stream)]

        ; Variable
        ['etag
         (print (xexpr->string (lookup the-ctx content)) stream)]

        ; Unescaped variable
        ['utag
         (print (lookup the-ctx content) stream)])]))







  (display "lala" stream))

(render null 'mock (current-output-port))
