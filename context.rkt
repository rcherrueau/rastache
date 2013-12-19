#lang racket/base

(require (for-syntax racket/base
                     syntax/id-table
                     racket/dict))

(define-for-syntax global (make-free-id-table))

(define-for-syntax (make-id lctx template id)
    (define str (format template (syntax->datum id)))
    (datum->syntax lctx (string->symbol str)))

;; iters over each key and each value at all level
(define-syntax (md_4 orig-x)
  (let domd ([x orig-x])
    (syntax-case x ()
      [(_ '(kv1 kv2 ...))
       (with-syntax ([tail
                      (syntax-case #'(kv2 ...) ()
                        [() #'()]
                        [(kv2 kv3 ...)
                         (with-syntax ([rest (domd #'(md_4 '(kv2 kv3 ...)))])
                           #'(rest))])])
         (syntax-case #'kv1 ()
           [(key v1 v2 ...)
            (with-syntax ([make-mustache-define
                           (with-syntax ([mustache-key
                                          (make-id #'kv1 "mustache-~a" #'key)])
                             (if (not (dict-has-key? global #'key))
                                 (let ([k #'key])
                                   (free-id-table-set! global k #t)
                                   #'(define (mustache-key ctx) (hash-ref ctx 'key)))
                                 #'(void)))]
                          [(values ...)
                           (map (lambda (v)
                                  (syntax-case v ()
                                    ;; mustache-expr?
                                    ['(kv1 kv2 ...)
                                     (with-syntax
                                         ([rest (domd #'(md_4 '(kv1 kv2 ...)))])
                                       #'rest)]
                                    ;; other mustach-data?
                                    [v #'(printf "value: ~a~n" 'v)]))
                                (syntax->list #'(v1 v2 ...)))])
              #'(begin make-mustache-define values ... . tail))]))])))

(displayln "---------------- md_4")
(md_4
 '((header (lambda () "Colors"))
   (link (lambda (self) (not (eq? (mustache-current self) #t))))
   (list (lambda (self) (not (eq? (length (mustache-item self)) 0))))
   (item '((name "red") (current #t) (url "#Red"))
         '((name "green") (current #f) (url "#Green"))
         '((name "blue") (current #f) (url "#Blue")))
   (empty (lambda (self) (eq? (length (mustache-item self)) 0)))))

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
