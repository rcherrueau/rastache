#lang racket

(struct token (sigil content section))

(define (mustache-tokenize mustache-file [open-tag "{{"] [close-tag "}}"])
  (define template (open-input-file mustache-file))
  (define state-pattern
    (pregexp
     (string-append
      "^"
      "\\s*"                            ; Skip any whitespace
      "(#|\\^|/|=|!|<|>|&|\\{)?"        ; Check for a tag type and capture it
      "\\s*"                            ; Skip any whitespace
      "([^(?:\\}?"
        (regexp-quote close-tag) ")]+)" ; Capture the text inside of the tag
      "\\s*"                            ; Skip any whitespace
      "\\}?"                            ; Skip balancing '}' if it exists
      "(.*)$")))                        ; Capture the rest of the string


  (define (scan-static tokens otag ctag)
    (define open-tag-pos
      (regexp-match-peek-positions open-tag template))

    (cond
        [(not open-tag-pos)
         (scan 'end
                (append tokens (list (token 'static
                                            (port->string template)
                                            empty)))
                otag
                ctag)]
        [else
         (define content-length (car (first open-tag-pos)))
         (define content (make-string content-length))

         (read-string! content template 0 content-length)
         (scan 'tag
                (append tokens (list (token 'static content empty)))
                otag
                ctag)]))

  (define (scan-tag tokens otag ctag)
    (define close-tag-pos
      (regexp-match-peek-positions close-tag template))

    (when (not close-tag-pos) (error "Bad syntaxe"))

    (define content-length (- (car (first close-tag-pos)) (string-length open-tag)))
    (define content (make-string content-length))

    (void (read-string (string-length open-tag) template))
    (read-string! content template 0 content-length)
    (void (read-string (string-length close-tag) template))

    (define l (regexp-match state-pattern content))
    (define sigil (second l))
    (define value (third l))

    (case sigil
      [(#f)
       (scan 'static
              (append tokens (list (token 'etag value empty)))
              otag
              ctag)]
      [(">" "<")
       (scan 'static
              (append tokens (list (token 'partial value) empty))
              otag
              ctag)]
      [("{" "&")
       (scan 'static
              (append tokens (list (token 'utag value empty)))
              otag
              ctag)]
      [("!")
       (scan 'static tokens otag ctag)]
      [("#")
       (scan 'static
              (append tokens
                      (list (token 'section
                                   value
                                   (scan 'static empty otag ctag))))
              otag
              ctag)]
      [("^")
       (scan 'static
              (append tokens
                      (list (token 'inverted-section
                                   value
                                   (scan 'static empty otag ctag))))
              otag
              ctag)]
      [("/") tokens]))

  ; scan: content tokens -> tokens
  (define (scan state tokens otag ctag)
    (cond
      [(eq? state 'static) (scan-static tokens otag ctag)]
      [(eq? state 'tag) (scan-tag tokens otag ctag)]
      [else
       (when (not (port-closed? template)) (close-input-port template))
       tokens]))

  (scan 'static empty open-tag close-tag))

(define (display-token token)
  (displayln (format "sigil: ~a, content: ~a"
                     (token-sigil token)
                     (token-content token)))

  (when (eq? (token-sigil token) 'section)
    (displayln (format "*** Section ~a ***" (token-content token)))
    (map display-token (token-section token))
    (displayln "*****************")))

(provide (all-defined-out))
