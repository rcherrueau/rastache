#lang racket/base

(provide (all-defined-out))

(define complex-name "complex")

(define complex-template
  (string-append "examples/" complex-name ".html"))

(define complex-res
  (string-append "examples/" complex-name ".txt"))

(define complex-stx
  #''((header (λ () "Colors"))
      (item '((name "red") (current #t) (url "#Red"))
            '((name "green") (current #f) (url "#Green"))
            '((name "blue") (current #f) (url "#Blue")))
      (link (λ (self) (not (eq? (rastache-ref self 'current) #t))))
      (list (λ (self) (not (eq? (length (rastache-ref self 'item)) 0))))
      (empty (λ (self) (eq? (length (rastache-ref self 'item)) 0)))))

(define complex-mock-ctx
  (let*
      ([refs
        (make-hash
         (list
          (cons 'empty (λ (ctx)  (hash-ref ctx 'empty)))
          (cons 'list (λ (ctx) (hash-ref ctx 'list)))
          (cons 'item (λ (ctx) (hash-ref ctx 'item)))
          (cons 'name (λ (ctx) (hash-ref ctx 'name)))
          (cons 'current (λ (ctx) (hash-ref ctx 'current)))
          (cons 'url (λ (ctx) (hash-ref ctx 'url)))
          (cons 'header (λ (ctx) (hash-ref ctx 'header)))
          (cons 'link (λ (ctx) (hash-ref ctx 'link)))))]
       [rastache-ref
        (λ (ctx key) ((hash-ref refs key) ctx))]
       [context
        (make-hash
         (list
          (cons 'header (λ (self) "Colors"))
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
                              (cons 'url "#Blue")))
                       ))
          (cons 'link (λ (self) (not (eq? (rastache-ref self 'current) #t))))
          (cons 'list (λ (self) (not (eq? (length (rastache-ref self 'item)) 0))))
          (cons 'empty (λ (self) (eq? (length (rastache-ref self 'item)) 0)))))])

  (cons context rastache-ref)))
