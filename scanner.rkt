#lang racket

(struct token (sigil content section))

(define (make-token sigil content [section empty])
  (token sigil content section))

(define (tokenize mustache-file [open-tag "{{"] [close-tag "}}"])
  (define template (open-input-file mustache-file))

  (define state-pattern
    (pregexp
     (string-append
      "^"
      "\\s*"                            ; Skip any whitespace
      "(#|\\^|/|=|!|<|>|&|\\{)?"        ; Check for a tag type and capture it
      "\\s*"                            ; Skip any whitespace
      "(.+)"                            ; Capture the text inside of the tag
      "\\s*"                            ; Skip any whitespace
      "\\}?"                            ; Skip balancing '}' if it exists
      "(.*)$")))                        ; Capture the rest of the string

  (define (read-content content-length)
    (define content (make-string content-length))
    (read-string! content template 0 content-length)
    content)

  (define (read-open-tag open-tag)
    (void (read-string (string-length open-tag) template)))

  (define (read-close-tag close-tag)
    (void (read-string (string-length close-tag) template)))

  (define (scan-static tokens otag ctag)
    ; Search for mustache opening tag
    (define otag-pos (regexp-match-peek-positions otag template))

    (cond
     ; No more mustache tag
     ; Create a 'static token with the end of template
     [(not otag-pos)
      (define the-token (make-token 'static (port->string template)))
      (scan 'end (append tokens (list the-token)) otag ctag)]

     ; Still mustache tag
     ; Create a 'static token with text until next mustach tag
     [else
      (define content-length (car (first otag-pos)))
      (define content (read-content content-length))
      (define the-token (make-token 'static content))

      (scan 'tag (append tokens (list the-token)) otag ctag)]))

  (define (scan-tag tokens otag ctag)
    ; Consume the mustache opening tag
    (read-open-tag otag)

    ; Search for mustache closing tag
    (define ctag-pos (regexp-match-peek-positions ctag template))
    (when (not ctag-pos) (error "Bad syntax"))

    ; Consume content of mustache tag
    (define content-length (car (first ctag-pos)))
    (define content (read-content content-length))

    ; Consume the mustache closing tag
    (read-close-tag ctag)

    (define l (regexp-match state-pattern content))
    (define sigil (second l))
    (define value (third l))

    (case sigil
      ; Normal
      [(#f)
       (define the-token (make-token 'etag value))
       (scan 'static (append tokens (list the-token)) otag ctag)]

      ; Unescaped HTML
      [("{" "&")
       (define the-token (make-token 'utag value))
       (scan 'static (append tokens (list the-token)) otag ctag)]

      ; Section
      [("#")
       (define the-token (make-token 'section
                                     value
                                     (scan 'static empty otag ctag)))
       (scan 'static (append tokens (list the-token)) otag ctag)]

      ; Inverted Section
      [("^")
       (define the-token (make-token 'inverted-section
                                     value
                                     (scan 'static empty otag ctag)))
       (scan 'static (append tokens (list the-token)) otag ctag)]

      ; End of (Inverted) Section
      [("/") tokens]

      ; Comments
      [("!")
       (scan 'static tokens otag ctag)]

      ; Partial
      [(">" "<")
       (define the-token (make-token 'partial value))
       (scan 'static (append tokens (list the-token)) otag ctag)]

      ; Set delimiters
      [("=")
       (define ll (string-split value))
       (when (not (= (length ll) 2)) (error "Bad syntax"))

       (define new-otag (first ll))
       (define new-ctag (substring (second ll)
                                   0
                                   (sub1 (string-length (second ll)))))

       (scan 'static tokens new-otag new-ctag)]))

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
