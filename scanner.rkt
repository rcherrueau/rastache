#lang racket/base

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; Mustache template engine for Racket

; Mustache template parser.
;
; Parse mustache template and generate a list of tokens. The list of
; tokens describes how to render the template.

(provide (struct-out token)
         tokenize)

; ______________________________________________________________________________
; utils
(define (token-print token port mode)
  (let _token-print ([the-token token]
                     [depth 0])
    (define sigil (token-sigil the-token))
    (define content (token-content the-token))
    (define section (token-section the-token))

    (cond
      [(or (eq? sigil 'section) (eq? sigil 'inverted-section))
       (write-string
        (format "~a(token '~s '~s (list~n"
                (make-string depth #\space)
                sigil
                content) port)
       (for-each (lambda (t)
                   (_token-print t (+ depth 2)))
                 section)
       (write-string
        (format "~a ))~n"
                (make-string (+ depth 2) #\space)) port)]
      [(or (eq? sigil 'static) (eq? sigil 'partial))
       (write-string (format "~a(token '~s ~s null)~n"
                             (make-string depth #\space)
                             sigil
                             content) port)]
      [else
       (write-string (format "~a(token '~s '~s null)~n"
                             (make-string depth #\space)
                             sigil
                             content) port)])))

; ______________________________________________________________________________
; import and implementation
(require racket/port
         racket/string)

;; Token is a meta-variable for mustache template syntactic
;; categories. Mustache defines 6 syntatic categories, i.e: 'static,
;; 'etag, 'utag, 'section, 'inverted-section and 'partial. A token
;; instance stores the syntatic category in the `sigil' attribute. For
;; each category the instance contains different informations.
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
(struct token (sigil content section)
        #:methods gen:custom-write
        [(define write-proc token-print)])

;; Construct the list of tokens for a specific template. The
;; `template' has to be an input port that reads bytes from a UTF-8
;; stream.`open-tag' and `close-tag' are mustache keywords
;; identifiers.
;; tokenize: input-port string string -> (listof token)
(define (tokenize template [open-tag "{{"] [close-tag "}}"])

  ;; Token constructor for static
  (define (make-token-static content)
    (token 'static content null))

  ;; Token constructor for etag
  (define (make-token-etag content)
    (token 'etag (string->symbol (string-trim content)) null))

  ;; Token constructor for utag
  (define (make-token-utag content)
    (token 'utag (string->symbol (string-trim content)) null))

  ;; Token constructor for section
  (define (make-token-section content [section null])
    (token 'section (string->symbol (string-trim content)) section))

  ;; Token constructor for inverted section
  (define (make-token-inverted-section content [section null])
    (token 'inverted-section (string->symbol (string-trim content)) section))

  ;; Token constructor for partial
  (define (make-token-partial content)
    (token 'partial content null))

  ;; Make the pattern that recognizes standalone tag. `otag' and
  ;; `ctag' should be patterns that matche exactly the original
  ;; open-tag and close-tag.
  ;; make-standalone-pattern: string string -> pregexp
  (define (make-standalone-pattern otag-quoted ctag-quoted)
    (pregexp
     (string-append "^\\s*"
                    otag-quoted
                    "(!|#|\\^|/|>|=)"
                    "\\s*"
                    "("
                      ".*?"ctag-quoted"\\s*"
                    "|"
                      "[^("ctag-quoted")]*"
                    ")")))

  (define state-pattern
    (pregexp
     (string-append
      "^"
      "\\s*" ; Skip any whitespace
      "(#|\\^|/|=|!|<|>|&|\\{)?" ; Check for a tag type and capture it
      "\\s*" ; Skip any whitespace
      "(.+)" ; Capture the text inside of the tag
      "\\s*" ; Skip any whitespace
      "\\}?" ; Skip balancing '}' if it exists
      "(.*)$"))) ; Capture the rest of the string

  read-byte
  (define is-standalone? regexp-match-exact?)

  ;; Returns a srting containing the next line of bytes from `in'.
  ;; read-line: in -> (values line/eof linefeed?)
  (define (read-line in)
    (let rline ([acc ""])
      (let ([c (read-char in)])
        (cond
         [(eof-object? c)
          (if (eq? acc "")
              (values eof #f)
              (values acc #f))]
         [(eq? c #\newline)
          (values acc #t)]
         [else
          (rline (string-append acc (string c)))]))))

  ;; Returns a string containing the next multi-line of bytes from
  ;; `in'.
  ;; read-multiline: in ctag -> (values line/eof linefeed? ctag-pos)
  (define (read-multiline in line ctag)
    (let rmline ([acc (port->string line)])
      (let*-values ([(new-line linefeed?) (read-line in)]
                    [(ctag-pos) (unless (eof-object? new-line)
                                  (regexp-match-peek-positions ctag
                                            (open-input-string new-line)))]
                    [(full-line) (unless (eof-object? new-line)
                                   (string-append acc new-line))])
        (cond
         [(eof-object? new-line)
          (error "Error while reading a multi-ligne tag")]
         [ctag-pos
          (let* ([acc-length (string-length acc)]
                 [full-ctag-pos (list(cons (+ acc-length (car (car ctag-pos))) (+ acc-length (cdr (car ctag-pos)))))])
            (values (open-input-string full-line)
                    linefeed?
                    full-ctag-pos))]
         [else
          (rmline full-line)]))))


  ;; Reads and consumes open-tag from the template. `open-tag-quoted'
  ;; should be a pattern that matches exactly the original open-tag.
  (define (read-open-tag open-tag-quoted port)
    (void (regexp-match open-tag-quoted port)))

  ;; Reads and consumes close-tag from the template.
  ;; `close-tag-quoted' should be a pattern that matches exactly the
  ;; original `close-tag'.
  (define (read-close-tag close-tag-quoted port)
    (void (regexp-match close-tag-quoted port)))

  (define (scan-tag line tokens otag ctag standalone? linefeed? standalone-pattern)
    ; Consume the mustache opening tag
    (read-open-tag otag line)

    ; Search for mustache closing tag

    (define ctag-pos (regexp-match-peek-positions ctag line))
    ; S'il n'y a pas de ctag => je suis dans un multiligne.
    ; Il faut donc constriure la nouvelle ligne jusqu'au prochain ctag
    ; je peux faire ça avec un flodr
    (define-values (new-line new-linefeed? new-ctag-pos)
      (if (not ctag-pos)
          (read-multiline template line ctag)
          (values line linefeed? ctag-pos)))

    ; Consume content of mustache tag
    (define content-length (car (car new-ctag-pos)))
    (define content (read-string content-length new-line))

    ; Consume the mustache closing tag
    (read-close-tag ctag new-line)

    (define l (regexp-match state-pattern content))
    (define sigil (cadr l))
    (define value (caddr l))

    (case sigil
      ; Normal
      [(#f)
       (define the-token (make-token-etag value))
       (scan-static new-line (append tokens (list the-token))
                    otag ctag standalone? new-linefeed? standalone-pattern)]

      ; Unescaped HTML
      [("{" "&")
       ; if unescaped starting with "{", then consumes the closing "}"
       (when (equal? sigil "{") (void (read-string (string-length "}") new-line)))
       (define the-token (make-token-utag value))
       (scan-static new-line (append tokens (list the-token))
                    otag ctag standalone? new-linefeed? standalone-pattern)]

      ; Section
      [("#")
       ;; eol is for end-of-line
       (define-values (sec-tokens eol eol-standalone? eol-linefeed?)
         (scan-static new-line (list) otag ctag standalone? new-linefeed? standalone-pattern))
       (define the-token (make-token-section value sec-tokens))
       (scan-static eol (append tokens (list the-token))
                    otag ctag eol-standalone? eol-linefeed? standalone-pattern)]

      ; Inverted Section
      [("^")
       ;; eol is for end-of-line
       (define-values (inverted-tokens  eol eol-standalone? eol-linefeed?)
         (scan-static new-line (list) otag ctag standalone? new-linefeed? standalone-pattern))
       (define the-token (make-token-inverted-section value inverted-tokens))
       (scan-static eol (append tokens (list the-token))
                    otag ctag eol-standalone? eol-linefeed? standalone-pattern)]

      ; End of (Inverted) Section
      [("/") (values tokens new-line standalone? new-linefeed?)]

      ; Comments
      [("!")
       (scan-static new-line tokens otag ctag standalone? new-linefeed? standalone-pattern)]

      ; Partial
      [(">" "<")
       (define the-token (make-token-partial value))
       (scan-static new-line (append tokens (list the-token))
                    otag ctag standalone? new-linefeed? standalone-pattern)]

      ; Set delimiters
      [("=")
       (define ll (string-split value))
       (when (< (length ll) 2) (error "Bad delimeter syntax"))

       (define new-otag (regexp-quote (car ll)))
       (define new-ctag (regexp-quote
                         (if (= (length ll) 2)
                             (substring (cadr ll) 0
                                        (sub1 (string-length (cadr ll))))
                             ; Superfluous in-tag whitespace should be ignored:
                             ; {{= @   @ =}}
                             (cadr ll))))
       (define new-standalone-pattern
         (make-standalone-pattern new-otag new-ctag))

       (scan-static new-line tokens new-otag new-ctag
                    standalone? new-linefeed? new-standalone-pattern)]))

  ;; Scans the static text until the next mustache tag.
  ;; scan-static:
  ;;  (listof token) string string boolean pregexp -> (listof tokens)
  (define (scan-static line tokens otag ctag standalone? linefeed?
                       standalone-pattern)
    ; Search for the mustache opening tag
    (define otag-pos (regexp-match-peek-positions otag line))

    (cond
     ; No more mustache tag
     ; Create a static token with the rest of the template.
     [(not otag-pos)
      (define content (port->string line))
      (define new-tokens
        ; Don't keep the value of a standalone line.
        (cond
          [standalone?
           tokens]
          [linefeed?
           (append tokens (list (make-token-static content)
                                 (make-token-static "\n")))]
          [else
           (append tokens (list (make-token-static content)))]))
      (scan new-tokens otag ctag #f standalone-pattern)]

     ; Still mustache tag
     ; Creates a static token with the text until next mustache tag.
     ; Then processes the mustache tag.
     [else
      (define content-length (car (car otag-pos)))
      (define content (read-string content-length line))
      (define new-tokens
        ; Don't keep the value of a standalone line.
        (if (not standalone?)
            (append tokens (list (make-token-static content)))
            tokens))
      (scan-tag line new-tokens otag ctag standalone? linefeed?
                standalone-pattern)]))

  ;; Scans the text and instanciate tokens. `otag' and `ctag' should
  ;; be patterns that matche exactly the original open-tag and
  ;; close-tag.
  ;; scan: (listof tokens) string string pregexp -> (listof tokens)
  (define (scan tokens otag ctag standalone? standalone-pattern)
    (define-values (line linefeed?) (read-line template))
    (cond
     [(eof-object? line)
      tokens]
     [else
      (scan-static (open-input-string line) tokens otag ctag
                   (is-standalone? standalone-pattern line)
                   linefeed?
                   standalone-pattern)]))

  ;; empty string est reconnu comme eof. Ce qui fait que le template "
  ;; \n" qui devrai être interpréter comme
  ;; "  <|"
  ;; ""
  ;; est finalement seulement interpréter en " <|". Ce qui merdouille
  ;; le template. Un solution est donc de passer une regepx sur la
  ;; template poour connaitre la forme finale de la template c-a-d
  ;; "\n" ou non et faire en fonction.

  (let ([otag-quoted (regexp-quote open-tag)]
        [ctag-quoted (regexp-quote close-tag)])
    (scan (list)
          otag-quoted
          ctag-quoted
          #f
          (make-standalone-pattern otag-quoted ctag-quoted))))
