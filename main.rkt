#lang racket

(struct token (sigil content section))

(define (parse-template mustache-file [open-tag "{{"] [close-tag "}}"])
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


  (define (scan-static tokens)
    (define open-tag-pos
      (regexp-match-peek-positions open-tag template))

    (cond
        [(not open-tag-pos)
         (parse 'end (append tokens (list (token 'static (port->string template) empty))))]
        [else
         (define content-length (car (first open-tag-pos)))
         (define content (make-string content-length))

         (read-string! content template 0 content-length)
         (parse 'tag (append tokens (list (token 'static content empty))))]))

  (define (scan-tag tokens)
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
       (parse 'static (append tokens (list (token 'etag value empty))))]
      [(">" "<")
       (parse 'static (append tokens (list (token 'partial value) empty)))]
      [("{" "&")
       (parse 'static (append tokens (list (token 'utag value empty))))]
      [("!")
       (parse 'static tokens)]
      [("#")
       (parse 'static (append tokens
                              (list (token 'section
                                           value
                                           (parse 'static empty)))))]
      [("^")
       (parse 'static (append tokens
                              (list (token 'inverted-section
                                           value
                                           (parse 'static empty)))))]
      [("/") tokens]))

  ; parse: content tokens -> tokens
  (define (parse state tokens)
    (cond
      [(eq? state 'static) (scan-static tokens)]
      [(eq? state 'tag) (scan-tag tokens)]
      [else
       (when (not (port-closed? template)) (close-input-port template))
       tokens]))

  (parse 'static empty))

(define (display-token token)
  (displayln (format "sigil: ~a, content: ~a"
                     (token-sigil token)
                     (token-content token)))

  (when (eq? (token-sigil token) 'section)
    (displayln (format "*** Section ~a ***" (token-content token)))
    (map display-token (token-section token))
    (displayln "*****************")))

(provide (all-defined-out))
