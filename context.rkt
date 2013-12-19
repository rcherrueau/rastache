#lang racket/base

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
(define-syntax (mustache-make-ctx x)
  (syntax-case x ()
    [(_ ctx-name '(kv ...))
     #'(begin
         (mustache-make-defines '(kv ...))
         (define ctx-name (mustache-make-htable '(kv ...))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Tests

;; (mustache-make-ctx
;;  ctx
;;  '((header (lambda () "Colors"))
;;    (link (lambda (self) (not (eq? (mustache-current self) #t))))
;;    (list (lambda (self) (not (eq? (length (mustache-item self)) 0))))
;;    (item '((name "red") (current #t) (url "#Red"))
;;          '((name "green") (current #f) (url "#Green"))
;;          '((name "blue") (current #f) (url "#Blue")))
;;    (empty (lambda (self) (eq? (length (mustache-item self)) 0)))))

;; ((mustache-header ctx))

;; (for ([inner-ctx (mustache-item ctx)])
;;   (printf "~s ~s ~s~n"
;;           (mustache-name inner-ctx)
;;           (mustache-current inner-ctx)
;;           (mustache-url inner-ctx)))

;; (mustache-make-defines
;;  '((header (lambda () "Colors"))
;;    (link (lambda (self) (not (eq? (mustache-current self) #t))))
;;    (list (lambda (self) (not (eq? (length (mustache-item self)) 0))))
;;    (item '((name "red") (current #t) (url "#Red"))
;;          '((name "green") (current #f) (url "#Green"))
;;          '((name "blue") (current #f) (url "#Blue")))
;;    (empty (lambda (self) (eq? (length (mustache-item self)) 0)))))

;; (define ctx
;;   (mustache-make-htable
;;    '((header (lambda () "Colors"))
;;      (link (lambda (self) (not (eq? (mustache-current self) #t))))
;;      (list (lambda (self) (not (eq? (length (mustache-item self)) 0))))
;;      (item '((name "red") (current #t) (url "#Red"))
;;            '((name "green") (current #f) (url "#Green"))
;;            '((name "blue") (current #f) (url "#Blue")))
;;      (empty (lambda (self) (eq? (length (mustache-item self)) 0))))))

;; (mustache-make-val "foo")
;; ;; > "foo"

;; (mustache-make-val '42 '43 '44)
;; ;; '(42 43 44)

;; ((mustache-make-val (lambda (self) self)) '42)
;; ;; > 42
