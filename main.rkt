#lang racket

(define open-tag "{{")
(define close-tag "}}")

(struct token (state content))

(define (parse-template mustache-file)
  (define template (open-input-file mustache-file))

  (define (scan-static pos tokens)
    (define open-tag-pos
      (regexp-match-peek-positions open-tag template))

    (case
        [(false? open-tag-pos)
         (parse 'end -1 (append tokens (token 'static (port->string template))))]
        [else
         (define content-length (- (first open-tag-pos) pos))
         (define content (make-string content-length))

         (read-string! content template 0 content-length)
         (parse 'tag (second open-tag-pos)
                (append tokens (token 'static content)))]))

  (define (scan-tag pos tokens)
    (define close-tag-pos
      (regexp-match-peek-positions close-tag template))

    (when (not close-tag-pos) (error "Bad syntaxe"))

    (parse 'static (second close-tag-pos)
           (append tokens (token 'etag "lala"))))


  ; parse: content tokens -> tokens
  (define (parse state pos tokens)
    (case
      [(eq? state 'static) (scan-static pos tokens)]
      [(eq? state 'tag) (scan-tag pos tokens)]
      [else tokens]))

  (parse 'static 0 empty))
