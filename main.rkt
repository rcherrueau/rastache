#lang racket

(define open-tag "{{")
(define close-tag "}}")

(struct token (state content))

(define (parse-template mustache-file)
  (define template (open-input-file mustache-file))

  (define (scan-static tokens)
    (define open-tag-pos
      (regexp-match-peek-positions open-tag template))

    (cond
        [(not open-tag-pos)
         (parse 'end (append tokens (list (token 'static (port->string template)))))]
        [else
         (define content-length (car (first open-tag-pos)))
         (define content (make-string content-length))

         (read-string! content template 0 content-length)
         (parse 'tag (append tokens (list (token 'static content))))]))

  (define (scan-tag tokens)
    (define close-tag-pos
      (regexp-match-peek-positions close-tag template))

    (when (not close-tag-pos) (error "Bad syntaxe"))

    (define content-length (- (car (first close-tag-pos)) (string-length open-tag)))
    (define content (make-string content-length))

    (void (read-string (string-length open-tag) template))
    (read-string! content template 0 content-length)
    (void (read-string (string-length close-tag) template))
    (parse 'static (append tokens (list (token 'etag content)))))


  ; parse: content tokens -> tokens
  (define (parse state tokens)
    (cond
      [(eq? state 'static) (scan-static tokens)]
      [(eq? state 'tag) (scan-tag tokens)]
      [else tokens]))

  (parse 'static empty))

(define (display-token token)
  (format "state: ~a, content: ~a" (token-state token) (token-content token)))