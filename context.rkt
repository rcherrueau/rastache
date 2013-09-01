#lang racket

;; http://stackoverflow.com/a/2890557

;; (define-syntax-rule (mustache-context-struct ctx)
;;   (struct self ((car (first ctx)))))

;; (define-syntax-rule (mustache-context-instance ctx)
;;   (displayln "blabla"))

;; (define-syntax-rule (mustache-context ctx)
;;   (begin
;;     (mustache-context-struct ctx)
;;     (mustache-context-instance ctx)))

(define-syntax (defstruct-litee stx)
  (syntax-case stx ()
    [(_ name)
     (let ([make-id
            (lambda (template id)
              (let ([str (format template (syntax->datum id))])
                (datum->syntax stx (string->symbol str))))])
     (with-syntax ([make-name (make-id "make-~a" #'name)]
                   [name? (make-id "~a?" #'name)])
       #'(begin
           (define (make-name x) (printf x))
           (define (name? x) (list 'lala x)))))]))


(define-syntax (defstruct-lite stx)
  (syntax-case stx ()
    [(defstruct-lite name field ...)
     (let ([make-id
            (lambda (template . ids)
              (let ([str (apply format template (map syntax->datum ids))])
                (datum->syntax stx (string->symbol str))))])
       (with-syntax ([make-name (make-id "make-~a" #'name)])
         #'(begin
             (define (make-name) (list 'name)))))]))


(define-syntax (hello stx)
    (syntax-case stx ()
      [(_ name place)
       (with-syntax ([print-name #'(printf "~a\n" 'name)]
                     [print-place #'(printf "~a\n" 'place)])
         #'(begin
             (define (name times)
               (printf "Hello\n")
               (for ([i (in-range 0 times)])
                    print-name))
             (define (place times)
               (printf "From\n")
               (for ([i (in-range 0 times)])
                    print-place))))]))
