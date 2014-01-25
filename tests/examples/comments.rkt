#lang racket/base

(provide (all-defined-out))

(define comments-name "comments")

(define comments-template
  (string-append "examples/" comments-name ".html"))

(define comments-res
  (string-append "examples/" comments-name ".txt"))

(define comments-stx
  #''((title (λ () "A Comedy of Errors"))))

(define comments-mock
  (let*
      ([refs
        (make-hash
         (list
          (cons 'title (λ (ctx) (hash-ref ctx 'title)))))]
       [rastache-ref
        (λ (ctx key) ((hash-ref refs key) ctx))]
       [context
        (make-hash
         (list
          (cons 'title "A Comedy of Errors")))])

  (cons context rastache-ref)))
