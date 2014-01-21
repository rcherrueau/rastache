#lang racket/base

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; A racket Mustache template engine.

; Mustache template parser.
;
; Parse mustache template and generate a list of tokens. The list of
; tokens describes how to render the template.

(provide token?
         token-sigil
         token-content
         token-section
         tokenize)

; ______________________________________________________________________________
; import and implementation

(require racket/list
         racket/port
         racket/string)

;; Token is a meta-variable for mustache template syntactic
;; categories. Mustache defines 6 syntatic categories, i.e: 'static,
;; 'etag, 'utag, 'section, 'inverted-section and 'partial. A token
;; instance stores the syntatic category in the `sigil' attribute. For
;; each category, the instance contains different informations.
;;
;; 'static for static content. `content' contains static text.
;; `section' is always empty.
;;
;; 'etag for variable. `content' contains a key usable with the
;; mustache context. This key is HTML escaped. `section' is always
;; empty.
;;
;; 'utag for unescaped HTML variable. `content' contains a key usable
;; with the mustache context. This key is unescaped HTML. `section' is
;; always empty.
;;
;; 'section for section. `content' contains the section name.
;; `section' contains all tokens of this section.
;;
;; 'inverted-section for inverted section. `content' contains the
;; section name. `section' contains all tokens of this section.
;;
;; 'partial for partials. `content' contains the name of the mustache
;; template to include. `section' is always empty.
;;
;; see http://mustache.github.io/mustache.5.html for the meaning of
;; each category.
(struct token (sigil content section))

;; Construct the list of tokens for a specific template. The
;; `template' has to be an input port that reads bytes from a UTF-8
;; stream.`open-tag' and `close-tag' are mustache keywords
;; identifiers.
;; tokenize: input-port string string -> (listof token)
(define (tokenize template [open-tag "{{"] [close-tag "}}"])

  ;; Token constructor for static
  (define (make-token-static content)
    (token 'static content empty))

  ;; Token constructor for etagn
  (define (make-token-etag content)
    (token 'etag (string->symbol content) empty))

  ;; Token constructor for utag
  (define (make-token-utag content)
    (token 'utag (string->symbol content) empty))

  ;; Token constructor for section
  (define (make-token-section content [section empty])
    (token 'section (string->symbol content) section))

  ;; Token constructor for inverted section
  (define (make-token-inverted-section content [section empty])
    (token 'inverted-section (string->symbol content) section))

  ;; Token constructor for partial
  (define (make-token-partial content)
    (token 'partiel content empty))

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

  ;; Scans the static text until the next mustache tag.
  ;; scan-tag: (listof token) string string -> (listof tokens)
  (define (scan-static tokens otag ctag)
    ; Search for mustache opening tag
    (define otag-pos (regexp-match-peek-positions otag template))

    (cond
     ; No more mustache tag
     ; Create a 'static token with the end of template
     [(not otag-pos)
      (define the-token (make-token-static (port->string template)))
      (scan 'end (append tokens (list the-token)) otag ctag)]

     ; Still mustache tag
     ; Create a 'static token with text until next mustach tag
     [else
      (define content-length (car (first otag-pos)))
      (define content (read-content content-length))
      (define the-token (make-token-static content))

      (scan 'tag (append tokens (list the-token)) otag ctag)]))

  ;; Scans a mustache tag. It applies differents scan strategies
  ;; depending on the tag category.
  ;; scan-tag: (listof token) string string -> (listof tokens)
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
       (define the-token (make-token-etag value))
       (scan 'static (append tokens (list the-token)) otag ctag)]

      ; Unescaped HTML
      [("{" "&")
       (define the-token (make-token-utag value))
       (scan 'static (append tokens (list the-token)) otag ctag)]

      ; Section
      [("#")
       (define the-token (make-token-section value
                                             (scan 'static empty otag ctag)))
       (scan 'static (append tokens (list the-token)) otag ctag)]

      ; Inverted Section
      [("^")
       (define the-token (make-token-inverted-section
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
       (define the-token (make-token-partial value))
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

  ;; Scans the text and instanciate tokens. The state indicates which
  ;; scan to opare. 'static is for the scan of static text until the
  ;; next mustache tag. 'tag is for the scan of mustache tag. The tag
  ;; scan applies differents scan strategies depending on the tag
  ;; category. At start of a new scan, you want to do a 'static scan!
  ;; scan: symbol (listof tokens) string string -> (listof tokens)
  (define (scan state tokens otag ctag)
    (cond
      [(eq? state 'static) (scan-static tokens otag ctag)]
      [(eq? state 'tag) (scan-tag tokens otag ctag)]
      [else
       tokens]))

  (scan 'static empty open-tag close-tag))
