#lang racket/base

; Use #hash expression and quasiquoting to create the context.
; http://docs.racket-lang.org/guide/hash-tables.html
; http://docs.racket-lang.org/reference/quasiquote.html

; > `#hash((k1 . "v1") (k2 . "v2") (k3 . ,(lambda (x) x)))
; '#hash((k1 . "v1") (k2 . "v2") (k3 . #<procedure>))
; > `#hash((k1 . "v1") (k2 . "v2") (k3 . ,((lambda () 'test))))
; '#hash((k1 . "v1") (k2 . "v2") (k3 . test))

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; A racket Mustache template engine.

; Macro for mustache expressions.
;
; Mustache Expression Syntax:
; mustache-expr := '( mustache-data ... )
; mustache-data := ( datum val ... )
; val :=  mustache-expr | racket-expr
;
; Mustache Expression Example:
; '((header "Colors")
;   (item '((name "red") (current #t) (url "#Red"))
;         '((name "green") (current #f) (url "#Green"))
;         '((name "blue") (current #f) (url "#Blue")))
;   (link (lambda () '42))
;   (list (lambda (self) (not (eq? (length (mustache-item self)) 0))))
;   (empty (lambda (self) (eq? (length (mustache-item self)) 0))))
;
; Macro Usage:
; (mustache-make-ctx
;  my-mustache-ctx
;  '((header "Colors")
;    (item '((name "red") (current #t) (url "#Red"))
;          '((name "green") (current #f) (url "#Green"))
;          '((name "blue") (current #f) (url "#Blue")))
;    (link (lambda () '42))
;    (list (lambda (self) (not (eq? (length (mustache-item self)) 0))))
;    (empty (lambda (self) (eq? (length (mustache-item self)) 0)))))
;
; Macro Output:
; (define my-mustache-ctx
;   '#hash((empty . #<procedure>)
;          (list . #<procedure>)
;          (item
;           .
;           (#hash((name . "red") (current . #t) (url . "#Red"))
;            #hash((name . "green") (current . #f) (url . "#Green"))
;            #hash((name . "blue") (current . #f) (url . "#Blue"))))
;          (header . "Colors")
;          (link . #<procedure>)))
; (define (mustache-empty ctx) (hash-ref ctx 'empty))
; (define (mustache-list ctx) (hash-ref ctx 'list))
; (define (mustache-item ctx) (hash-ref ctx 'item))
; (define (mustache-name ctx) (hash-ref ctx 'name))
; (define (mustache-current ctx) (hash-ref ctx 'current))
; (define (mustache-url ctx) (hash-ref ctx 'url))
; (define (mustache-header ctx) (hash-ref ctx 'header))
; (define (mustache-link ctx) (hash-ref ctx 'link))

(provide mustache-make-ctx)

; ______________________________________________________________________________
; import and implementation

(require (for-syntax racket/base
                     syntax/id-table
                     racket/dict))

;; Free-id-table which stores already defined mustache accessor. Thus,
;; the following mustache expression: '('((foo 1)) '((foo 2))) will
;; produce one and only one accessor that is
;; `(define (mustache-foo ctx) (hash-ref ctx foo))'
(define-for-syntax global-mustache-defines (make-free-id-table))

;; Produces an identifier using `lctx' for the lexical context. The
;; format string must use only `~a' placeholders. Identifier is
;; automatically converted to a symbol.
(define-for-syntax (make-id lctx template id)
  (let ([str (format template (syntax->datum id))])
    (datum->syntax lctx (string->symbol str))))

;; Constructs global accessors for mustach-data(s) of a mustache-expr.
(define-syntax (mustache-make-defines x)
  ;; Treats each mustache-data as follow: First, treats the first
  ;; mustache-data `kv1'. Then, calls `mustache-make-defines'
  ;; recursively on the rest of the `mustache-expr'.
  (syntax-case x ()
    ;; mustache-expr?
    [(_ '(kv1 kv2 ...))
     ;; `tail' is the `mustache-make-defines' recursive call on the
     ;; rest of mustache-data(s) of the mustache-expr `kv2 ...'.
     (with-syntax
         ([tail
           (syntax-case #'(kv2 ...) ()
             [() #'()]
             [(kv2 kv3 ...)
              #'((mustache-make-defines '(kv2 kv3 ...)))])])
       ;; Treat the first mustache-data
       (syntax-case #'kv1 ()
         ;; mustache-data?
         [(key v1 v2 ...)
          (with-syntax
              ;; Construct the global define accessor. It uses the
              ;; free-id-table `global-mustache-defines' to test if
              ;; the define is already defined or not.
              ([make-the-define
                (with-syntax
                    ([mustache-key
                      (make-id #'kv1 "mustache-~a" #'key)])
                  (if (not (dict-has-key? global-mustache-defines
                                          #'mustache-key))
                      (let ([k #'key])
                        (free-id-table-set! global-mustache-defines
                                            #'mustache-key
                                            #t)
                        #'(define (mustache-key ctx) (hash-ref ctx 'key)))
                      #'(void)))]
               ;; Takes a look to the values of the current
               ;; mustache-data. If one of those values is a
               ;; mustache-expr. Then, it calls
               ;; `mustache-make-defines' on it.
               [(values ...)
                (map (lambda (v)
                       (syntax-case v ()
                         ;; mustache-expr?
                         ['(kv1 kv2 ...)
                          #'(mustache-make-defines '(kv1 kv2 ...))]
                         ;; other mustach-data?
                         [v #'(void)]))
                     (syntax->list #'(v1 v2 ...)))])
            #'(begin
                make-the-define
                values ... . tail))]))]))

;; Constructs the hash-table for a specific mustache-expr.
(define-syntax (mustache-make-htable x)
  (syntax-case x ()
    ;; many mustache-data(s) in a mustache-expr
    [(_ '([key1 val1 ...] [key2 val2 ...] ...))
     #'(make-immutable-hash
        (list (cons 'key1 (mustache-make-val val1 ...))
              (cons 'key2 (mustache-make-val val2 ...))
              ...))]
    ;; One unique mustache-data in a mustache-expr
    [(_ '(key val ...))
     #'(make-immutable-hash
        (list (cons 'key (mustache-make-val val ...))))]))


;; Constructs the value for the val part of a mustache-data.
(define-syntax (mustache-make-val x)
  (syntax-case x ()
    ;; mustache-expr?
    [(_ '(kv1 kv2 ...))
     #'(mustache-make-htable '(kv1 kv2 ...))]
    ;; unique-val?
    [(_ v)
     #'v]
    ;; list-of-vals?
    [(_ v1 v2 ...)
     #'(list (mustache-make-val v1)
             (mustache-make-val v2)
             ...)]))

;; Constructs the mustache context for a mustech-expr. Two parts
;; compose the context. The first part is an hash-table that indexes
;; all elements. The second part is a collection of accessors to
;; access to each elements of the hash-table.
;;
;; If the mustache-expr is store in a variable, it's really important
;; to store-it like that:
;; (define-for-syntax mustache-expr (lambda () '((foo "bar"))))
;; And then,
;; (mustache-make-ctx context mustache-expr)
;;
;; Is is also possible to import mustache-expr from an another file or
;; module. For instance there is the file simple.rkt containng:
;; (provide mustache-expr)
;; (define mustache-expr (lambda () '((foo "bar"))))
;;
;; Then, in the module that uses macro, import the mustache-expr
;; using:
;; (require (for-syntax "mustache-expr"))
;;
;; see Compile and Run-Time Phases from Racket Guide
;; http://docs.racket-lang.org/guide/stx-phases.html
(define-syntax (mustache-make-ctx x)
  (syntax-case x ()
    [(_ ctx-name '(kv ...))
     #'(begin
         (mustache-make-defines '(kv ...))
         (define ctx-name (mustache-make-htable '(kv  ...))))]
    [(_ ctx-name mustache-expr-id)
     (with-syntax ([mustache-expr
                    ((eval #'mustache-expr-id))])
       #'(mustache-make-ctx ctx-name 'mustache-expr))]))

;; This
;;(define-for-syntax expr (lambda () '((foo "Hello World"))))
;;(mustache-make-ctx lala expr)
;; Or
;; (mustache-make-ctx lala '((foo "hello world")))
