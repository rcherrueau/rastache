#lang racket/base

(require "../../scanner.rkt")

(provide (all-defined-out))

(define complex-name "complex")

(define complex-template
  (string-append "examples/" complex-name ".html"))

(define complex-res
  (string-append "examples/" complex-name ".txt"))

(define complex-ctx
  (let ([rast-ref hash-ref])
  `#hash{(header . ,(λ () "Colors"))
         (item . [#hash{(name ."red") (current . #t) (url . "#Red")}
                  #hash{(name . "green") (current . #f) (url . "#Green")}
                  #hash{(name . "blue") (current . #f) (url . "#Blue")}])
         (link . ,(λ (self) (not (eq? (rast-ref self 'current) #t))))
         (list . ,(λ (self) (not (eq? (length (rast-ref self 'item)) 0))))
         (empty . ,(λ (self) (eq? (length (rast-ref self 'item)) 0)))}))

(define  complex-mock-tokens
  (list
   (token 'static "<h1>" null)
   (token 'etag 'header null)
   (token 'static "</h1>" null)
   (token 'section 'list
     (list
      (token 'static "<ul>" null)
      (token 'section 'item
        (list
         (token 'static "" null)
         (token 'section 'current
           (list
            (token 'static "<li><strong>" null)
            (token 'etag 'name null)
            (token 'static "</strong></li>" null)))
         (token 'static "" null)
         (token 'section 'link
           (list
            (token 'static "<li><a href=\"" null)
            (token 'etag 'url null)
            (token 'static "\">" null)
            (token 'etag 'name null)
            (token 'static "</a></li>" null)))
         (token 'static "" null)))
      (token 'static "</ul>" null)))
   (token 'static "" null)
   (token 'section 'empty
     (list
      (token 'static "<p>The list is empty.</p>" null)))
   (token 'static "\n" null)))
