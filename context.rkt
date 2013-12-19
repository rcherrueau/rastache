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

;; Construct global accessors for mustach-data(s) of a mustache-expr.
(define-syntax (mustache-make/defines x)
  ;; Treat each mustache-data. Treat the first one mustache-data `kv1'
  ;; and call `mustache-make/defines' recursively on the rest of the
  ;; `mustache-expr'.
  (syntax-case x ()
    ;; mustache-expr?
    [(_ '(kv1 kv2 ...))
     ;; `tail' is the `mustache-make/defines' recursive call on the
     ;; rest of mustache-data(s) of the mustache-expr `kv2 ...'.
     (with-syntax
         ([tail
           (syntax-case #'(kv2 ...) ()
             [() #'()]
             [(kv2 kv3 ...)
              #'((mustache-make/defines '(kv2 kv3 ...)))])])
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
               ;; `mustache-make/defines' on it.
               [(values ...)
                (map (lambda (v)
                       (syntax-case v ()
                         ;; mustache-expr?
                         ['(kv1 kv2 ...)
                          #'(mustache-make/defines '(kv1 kv2 ...))]
                         ;; other mustach-data?
                         [v #'(void)]))
                     (syntax->list #'(v1 v2 ...)))])
            #'(begin
                make-the-define
                values ... . tail))]))]))

(mustache-make/defines
 '((header (lambda () "Colors"))
   (link (lambda (self) (not (eq? (mustache-current self) #t))))
   (list (lambda (self) (not (eq? (length (mustache-item self)) 0))))
   (item '((name "red") (current #t) (url "#Red"))
         '((name "green") (current #f) (url "#Green"))
         '((name "blue") (current #f) (url "#Blue")))
   (empty (lambda (self) (eq? (length (mustache-item self)) 0)))))
        ;; (defineine (mustache-name c) c)

;
;(define (la x) x)
;
;(syntax-original? #'la)


;; (define (exist-define define-name define-list)
;;   (member define-name define-list))


;; http://stackoverflow.com/a/2890557

; (null? (cdr (syntax-e #'((m m m m m m) (d d d d d)))))
; (null? (cdr (syntax-e #'((m m m m m m)))))

;; (define-syntax (mustache-ctx x)
;;   (struct mdc (defines contexts))

;;   ; Create an datum from a template readable with `format'.
;;   (define (make-id template . ids)
;;     (define str (apply format template (map syntax->datum ids)))
;;     (datum->syntax x (string->symbol str)))

;;   (define (has-definitions? m-data)
;;     (not (null? (cdr (syntax-e m-data)))))

;;   (define (extract-defs-ctx m-data mdc)
;;     (cond
;;      [(has-definitions? m-data)
;;       (let* ([m-data-e (syntax-e m-data)]
;;              [new-ctx (car m-data-e)]
;;              [new-defs (cadr m-data-e)]
;;              [old-ctx (mdc-ctx



;;   (define (extract-defs-ctxs m-datas)
;;     (define m-defs-ctxs
;;       (foldl extract-defs-ctx (mdc (begin) (begin)) m-datas))

;;     #`(begin
;;         #,(mdc-defines m-defs-ctxs)
;;         #,(mdc-contextes m-defs-ctxs)))

;;   (syntax-case x ()
;;     [(_ name '((k1 v1 ...) (k2 v2 ...) ...))
;;      (with-syntax ([(key ...) #'(k1 k2 ...)]
;;                    [(mustache-key ...)
;;                     (map (lambda (f) (make-id "mustache-~a" f))
;;                          (syntax->list #'(k1 k2 ...)))]
;;                    [(mustache-define-and-mustache-ctx)
;;                     (lala (syntax->list #'((mustache-data (quote (k1 v1 ...)))
;;                                            (mustache-data (quote (k2 v2 ...)))
;;                                            ...)))])
;;        #'(begin
;;            (begin
;;              (define (mustache-key ctx) (hash-ref ctx 'key))
;;              ...)
;;            (begin
;;              (define name (make-immutable-hash
;;                          (list (mustache-data (quote (k1 v1 ...)))
;;                                (mustache-data (quote (k2 v2 ...)))
;;                                ...))))))]))
;;           ;; (mustache-define-and-mustache-ctx)))))])))

;; (define-syntax (mustache-data x)
;;   (syntax-case x ()
;;     [(_ (quote (key val ...)))
;;      #'(cons (quote key) (mustache-val val ...))]))

;; (define-syntax (mustache-val x)
;;   (syntax-case x ()
;;     [(_ '((k1 v1 ...) (k2 v2 ...) ...))
;;      #'(make-immutable-hash (list (mustache-data (quote (k1 v1 ...)))
;;                                   (mustache-data (quote (k2 v2 ...)))
;;                                   ...))]

;;     [(_ val)
;;      #'val]
;;     [(_ val1 val2 ...)
;;      #'(list (mustache-val val1) (mustache-val val2) ...)]))

;; ;; Following not work :
;; ;; (let ([ctx2 '((name "Jim" "Ronan") (age 24) (admin #t))])
;; ;;   (mustache-ctx ctx-2))


;; ;; FIXME: generate defines for nested mustache
;; (define (mustache-current ctx) (hash-ref ctx 'current))
;; (define (mustache-name ctx) (hash-ref ctx 'name))
;; (define (mustache-url ctx) (hash-ref ctx 'url))


; (mustache-ctx ctx '((name "Jim") (age 24) (admin #t)))
;; (mustache-ctx ctx

;;    ;; CTX-1
;;     ;; '((name "Foo1" "Foo2" "Foo3" "Foo4") (age 24) (admin #t)

;;   ;; CTX-2
;;     '((header (lambda () "Colors"))
;;       (item '((name "red") (current #t) (url "#Red"))
;;             '((name "green") (current #f) (url "#Green"))
;;             '((name "blue") (current #f) (url "#Blue")))
;;       (link (lambda (self) (not (eq? (mustache-current self) #t))))
;;       (list (lambda (self) (not (eq? (length (mustache-item self)) 0))))
;;       (empty (lambda (self) (eq? (length (mustache-item self)) 0)))
;; ))

;; (define-syntax (mustache-ctx x)
;;   (syntax-case x ()
;;     [(_ name '((k1 v1 ...) (k2 v2 ...) ...))
;;      (let ([make-id
;;             (lambda (template . ids)
;;               (let ([str (apply format template (map syntax->datum ids))])
;;                 (datum->syntax x (string->symbol str))))])

;;      (with-syntax ([(mustache-key ...)
;;                     (map (lambda (f) (make-id "mustache-~a"  f))
;;                          (syntax->list #'(k1 k2 ...)))]
;;                    [(key ...) #'(k1 k2 ...)])
;;        #'(begin
;;            (define (mustache-key ctx) (hash-ref ctx 'key))
;;            ...
;;            (define name (make-immutable-hash
;;                          (list (mustache-data (quote (k1 v1 ...)))
;;                                (mustache-data (quote (k2 v2 ...)))
;;                                ...))))))]))

;; (define-syntax (mustache-data x)
;;   (syntax-case x ()
;;     [(_ (quote (key val ...)))
;;      #'(cons (quote key) (mustache-val val ...))]))

;; (define-syntax (mustache-val x)
;;   (syntax-case x ()
;;     [(_ '((k1 v1 ...) (k2 v2 ...) ...))
;;      #'(make-immutable-hash (list (mustache-data (quote (k1 v1 ...)))
;;                                   (mustache-data (quote (k2 v2 ...)))
;;                                   ...))]

;;     [(_ val)
;;      #'val]
;;     [(_ val1 val2 ...)
;;      #'(list (mustache-val val1) (mustache-val val2) ...)]))

;; ;; Following not work :
;; ;; (let ([ctx2 '((name "Jim" "Ronan") (age 24) (admin #t))])
;; ;;   (mustache-ctx ctx-2))


;; ;; FIXME: generate defines for nested mustache
;; (define (mustache-current ctx) (ctx))
;; (define (mustache-name ctx) (ctx))
;; (define (mustache-url ctx) (ctx))


;; ; (mustache-ctx '((name "Jim") (age 24) (admin #t)))
;; ;(define hash-test
;;   (mustache-ctx ctx

;;    ;; CTX-1
;;     ;; '((name "Foo1" "Foo2" "Foo3" "Foo4") (age 24) (admin #t)

;;   ;; CTX-2
;;     '((header (lambda () "Colors"))
;;       (item '((name "red") (current #t) (url "#Red"))
;;             '((name "green") (current #f) (url "#Green"))
;;             '((name "blue") (current #f) (url "#Blue")))
;;       (link (lambda (self) (not (eq? (mustache-current self) #t))))
;;       (list (lambda (self) (not (eq? (length (mustache-item self)) 0))))
;;       (empty (lambda (self) (eq? (length (mustache-item self)) 0)))
;; ))
