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
; Parses mustache template and generates a list of tokens. The list of
; tokens describes how to render the template.
(provide tokenize
         mustachize)

; ______________________________________________________________________________
; import and implementation
(require "commons.rkt"
         racket/match
         racket/port
         racket/string
         net/url)

;; Internal representation of a line in a mustache template. A line
;; gets two properties in addition to the content. `linefeed?' is true
;; if line got a linefeed at parse time. `standalone?' is true if line
;; is standalone.
(struct line (content linefeed? standalone?))

;; Returns #t if line is `eof', #f otherwise.
(define (line-eof?  line)
  (and (line? line)
       (eof-object? (line-content line))))

;; Make the pattern that recognizes standalone line. Variables
;; `otag-quoted' and `ctag-quoted' should be patterns that match
;; exactly the original open-tag and close-tag.
;; make-standalone-pattern: string string -> pregexp
(define (make-standalone-pattern otag-quoted ctag-quoted)
  (pregexp
   (string-append "(^\\s*"
                    otag-quoted
                    "(!|#|\\^|/|>|=|\\+)"
                    "\\s*"
                    "(.*?"ctag-quoted"\\s*"
                    "|"
                    "[^("ctag-quoted")]*)"
                  "|"
                   "^\\s*"ctag-quoted"\\s*)")))

(define state-pattern
  (pregexp
   (string-append
    "^"
    "\\s*" ; Skip any whitespace
    "(#|\\^|/|=|!|<|>|&|\\{)?" ; Check for a tag type and capture it
    "\\s*" ; Skip any whitespacep
    "(.+)" ; Capture the text inside of the tag
    "\\s*" ; Skip any whitespace
    "\\}?" ; Skip balancing '}' if it exists
    "(.*)$"))) ; Capture the rest of the string


(define line-pattern (regexp "(?m:^.*$)"))
(define linefeed-pattern (regexp "^\n"))
(define is-standalone? regexp-match-exact?)

;; Returns a srting containing the next line of bytes from `in'.
;; read-line: in pregexp -> line
(define (read-line in standalone-pattern)
  ;; Seems to be the right way to get string results
  ;; https://groups.google.com/d/msg/racket-users/2s0Lyr4NO-A/uDbh5uZf-c0J
  (define the-line (bytes->string/utf-8
                    (car (regexp-match line-pattern in))))
  (define with-linefeed (and (regexp-match linefeed-pattern in) #t))

  (cond
   ;; No more line
   [(and (equal? the-line "")
         (not with-linefeed))
    (line eof #f #t)]
   ;; Some lines
   [else
    (line (open-input-string the-line)
          with-linefeed
          (is-standalone? standalone-pattern the-line))]))

;; Returns a number pair which refers to the range position of the
;; mustache opening tag, #f otherwise. Variables `otag-quoted' is a
;; pattern that matches exactly the original mustache opening tag.
(define (open-tag-position otag-quoted port)
  (let ([otag-pos (regexp-match-peek-positions otag-quoted port)])
    (if otag-pos (car otag-pos) #f)))

;; Returns a number pair wich refers to the range position of the
;; mustache closing tag, #f otherwise. Variables `ctag-quoted' is a
;; pattern that matches exactly the original mustache closing tag.
(define (close-tag-position ctag-quoted port)
  (let ([ctag-pos (regexp-match-peek-positions ctag-quoted port)])
    (if ctag-pos (car ctag-pos) #f)))

;; Consumes the mustache opening tag from the template. `otag-quoted'
;; is a pattern that matches exactly the original mustache opening
;; tag.
(define (read-open-tag otag-quoted port)
  (void (regexp-match otag-quoted port)))

;; Consumes the mustache content from the template. `ctag-quoted' is a
;; pattern that matches exactly the mustache opening tag. `line' is
;; the current parsed line. `template is the rest of the
;; template. `standalone-pattern' is the patter to recognize
;; standalone line.
(define (read-content ctag-quoted line template standalone-pattern)
  ;; First, test if ctag is in current line or not, if yes the
  ;; standalon is the value of the current line, if not the standalone
  ;; is the value of the future line.
  (define ctag-pos (close-tag-position ctag-quoted
                                       (line-content line)))

  (cond
    ;; closing tag finds in current line
    [ctag-pos
     (define content-length (car ctag-pos))
     (define content (read-string content-length (line-content line)))
     (values content line)]
    ;; Find mustache closing tag in the rest of the template
    [else
     (define tplt (input-port-append #f (line-content line) template))
     (define new-ctag-pos (close-tag-position ctag-quoted
                                              tplt))
     (define content-length (car new-ctag-pos))
     (define content (read-string content-length tplt))

     ;; Get the rest of the current line
     (define line-rest (read-line tplt standalone-pattern))
     (values content line-rest)]))

;; Consumes the mustache closing tag from the template. `ctag-quoted'
;; is a pattern that matches exactly the original mustache closing
;; tag.
(define (read-close-tag ctag-quoted port)
  (void (regexp-match ctag-quoted port)))

;; Makes a token key from a string
(define make-key string->symbol)

;; Construct the list of tokens for a specific template. The
;; `template' has to be an input port that reads bytes from a UTF-8
;; stream.
;; tokenize: input-port -> (listof token)
(define (tokenize template)
  ; __________________________________________________________________
  ; tokenize parameters

  ;; Pattern that matche exactly the original open-tag.
  (define otag-quoted (make-parameter
                       (regexp-quote (open-tag))))
  ;; Pattern that matche exactly the original close-tag.
  (define ctag-quoted (make-parameter
                       (regexp-quote (close-tag))))
  ;; Pattern that recognizes standalone line.
  (define standalone-pattern (make-parameter
                              (make-standalone-pattern (otag-quoted)
                                                       (ctag-quoted))))
  ; __________________________________________________________________
  ; tokenize implementation

  ;; Parses the text and instanciate tokens.
  (let parse ([tokens (list (token-delimiter (open-tag)
                                             (close-tag)))])
    ;; Util function wich constructs tokens from dotted names:
    ;; 'etag: {{a.b}} produces {{#a}}{{b}}{{/a}}
    ;; 'utag: {{&a.b}} produces {{#a}}{{&b}}{{/a}}
    ;; 'sec:
    ;;   {{#a.b}}{{c}}{{/a.b}} produces {{#a}}{{#b}}{{c}}{{/b}}{{/a}}
    ;; 'inv-sec:
    ;;   {{^a.b}}{{c}}{{/a.b}} produces {{^a}}{{^b}}{{c}}{{/b}}{{/a}}
    (define (make-dotted-tokens dotted-names
                                #:type type
                                #:sec-tokens [nested '()])
      (let _mdt ([tags dotted-names])
        (cond
         [(> (length tags) 1)
          (define token-maker (cond [(eq? type 'inv-sec)
                                     token-inv-sec]
                                    [else token-sec]))
          (token-maker (make-key (car tags))
                       (list (_mdt (cdr tags)))
                       #t)]
         [else
          (case type
            ['etag (token-etag (make-key (car tags)))]
            ['utag (token-utag (make-key (car tags)))]
            ['sec (token-sec (make-key (car tags)) nested #f)]
            ['inv-sec (token-inv-sec (make-key (car tags))
                                     nested
                                     #f)])])))

    ;; Util function which continues the parsing of a line after
    ;; parsing current tag.
    (define (parse-ahead line tokens new-token)
      (parse-static line (cons new-token tokens)))

    ;; Util function which parses a tag.
    (define (parse-tag line tokens)
      ;; Consume the mustache opening tag
      (read-open-tag (otag-quoted) (line-content line))

      ;; Consume content of mustache tag
      (define-values (content new-line)
        (read-content (ctag-quoted)
                      line
                      template
                      (standalone-pattern)))

      ;; Consume the mustache closing tag
      (read-close-tag (ctag-quoted) (line-content new-line))

      ;; Process mustache tag
      (define l (regexp-match state-pattern content))
      (define sigil (cadr l))
      (define value (caddr l))
      (case sigil
        ;; Etag
        [(#f)
         (define the-token
           (let ([name (regexp-split #rx"\\." (string-trim value))])
             (cond
              ;; Single period
              [(equal? value ".")
               (token-etag period-name)]
              ;; Simple Etag
              [(equal? (length name) 1)
               (token-etag (make-key (car name)))]
              ;; Dotted names
              [else
               (make-dotted-tokens name #:type 'etag)])))
         (parse-ahead new-line tokens the-token)]

        ;; Unescaped HTML
        [("{" "&")
         ;; if unescaped starting with "{", then consumes the closing "}"
         (when (equal? sigil "{")
           (read-string (string-length "}") (line-content new-line)))

         (define the-token
           (let ([name (regexp-split #rx"\\." (string-trim value))])
             (cond
              ;; Single period
              [(equal? value ".")
               (token-utag period-name)]
              ;; Simple Utag
              [(equal? (length name) 1)
               (token-utag (make-key (car name)))]
              ;; Dotted names
              [else
               (make-dotted-tokens name #:type 'utag)])))
         (parse-ahead new-line tokens the-token)]

        ;; Section
        [("#")
         ;; First compute nested tokens; eol is for end-of-line
         (define-values (nested-tokens eol)
           (parse-static new-line (list)))
         ;; Then add nested tokens and continue with the end of line.
         ;; Dotted names should be considered a form of shorthand for
         ;; sections.
         (define the-token
           (let ([name (regexp-split #rx"\\." (string-trim value))])
             (cond
              ;; Simple section name
              [(equal? (length name) 1)
               (token-sec (make-key (car name)) nested-tokens #f)]
              ;; Dotted names
              [else
               (make-dotted-tokens name
                                   #:type 'sec
                                   #:sec-tokens nested-tokens)])))
           (parse-ahead eol tokens the-token)]

        ;; Inverted Section
        [("^")
         ;; First compute nested tokens; eol is for end-of-line
         (define-values (nested-tokens eol)
           (parse-static new-line (list)))
         ;; Then add nested tokens and continue with the end of line.
         ;; Dotted names should be considered a form of shorthand for
         ;; sections.
         (define the-token
           (let ([name (regexp-split #rx"\\." (string-trim value))])
             (cond
              ;; Simple section name
              [(equal? (length name) 1)
               (token-inv-sec (make-key (car name)) nested-tokens #f)]
              ;; Section name with periods
              [else
               (make-dotted-tokens name
                                   #:type 'inv-sec
                                   #:sec-tokens nested-tokens)])))
         (parse-ahead eol tokens the-token)]

        ;; End of (Inverted) Section
        [("/") (values (reverse tokens) new-line)]

        ;; Comments
        [("!")
         (parse-static new-line tokens)]

        ;; Partial
        [(">" "<")
         (define the-token
           (token-partial (string->url (string-trim value))))
         (parse-static new-line (cons the-token tokens))]

        ;; Set delimiters
        [("=")
         (define ll (string-split value))
         (when (< (length ll) 2)
           (error "Bad delimeter syntax"))
         (parameterize* ([open-tag
                          (car ll)]
                         [close-tag
                          (cond
                           [(equal? (length ll) 2)
                            (substring (cadr ll)
                                       0
                                       (sub1 (string-length (cadr ll))))]
                           [else
                            ;; Superfluous in-tag whitespace should be ignored:
                            ;; {{= @   @ =}}
                            (cadr ll)])]
                         [otag-quoted
                          (regexp-quote (open-tag))]
                         [ctag-quoted
                          (regexp-quote (close-tag))]
                         [standalone-pattern
                          (make-standalone-pattern (otag-quoted)
                                                   (ctag-quoted))])
           (define the-token (token-delimiter (open-tag) (close-tag)))
           (parse-static new-line (cons the-token tokens)))]))

    ;; Util function which parses static content.
    (define (parse-static line tokens)
      ;; Search for the mustache opening tag
      (define otag-pos (open-tag-position (otag-quoted)
                                          (line-content line)))

      ;; Process the static part
      (cond
       ;; No more mustache tag:
       ;; Create a static token with the rest of the template.
       [(not otag-pos)
        (define content (port->string (line-content line)))
        (define new-tokens
          ;; Don't keep the value of a standalone line.
          (cond
           [(line-standalone? line)
            tokens]
           ;; Linefeed line: add the rest of the template and linefeed
           ;; token.
           [(line-linefeed? line)
            (cons (token-static "\n")
                  (cons (token-static content) tokens))]
           ;; Non-linefeed line: solo add the rest of the template.
           [else
            (cons (token-static content) tokens)]))
        (parse new-tokens)]

       ;; Still mustache tag:
       ;; Create a static token with the text until next mustache tag.
       ;; Then process the mustache tag.
       [else
        (define content-length (car otag-pos))
        (define content (read-string content-length (line-content line)))
        (define new-tokens
          ;; Don't keep the value of a standalone line.
          (cond
           [(not (line-standalone? line))
            (cons (token-static content) tokens)]
           [else
            tokens]))
        (parse-tag line new-tokens)]))

    (define line (read-line template (standalone-pattern)))

    (cond
     [(line-eof? line) (reverse tokens)]
     [else
      (parse-static line tokens)])))

;; Constructs a mustache template from a list of tokens.
;; mustachize: (list token) -> string
(define (mustachize tokens)
  (cond
   [(null? tokens) ""]
   [else
    (define token (car tokens))
    (match token
      ;; Static
      [(token-static content)
       (string-append content (mustachize (cdr tokens)))]
      ;; Etag
      [(token-etag key)
       (string-append (open-tag) (symbol->string key) (close-tag)
                      (mustachize (cdr tokens)))]
      ;; Utag
      [(token-utag key)
       (string-append (open-tag) "&" (symbol->string key) (close-tag)
                      (mustachize (cdr tokens)))]
      ;; Section
      [(token-sec key section _)
       (let ([sec-name (symbol->string key)])
         (string-append (open-tag) "#" sec-name (close-tag)
                        (mustachize section)
                        (open-tag) "/" sec-name (close-tag)
                        (mustachize (cdr tokens))))]
      ;; Inverted Section
      [(token-inv-sec key section _)
       (let ([sec-name (symbol->string key)])
         (string-append (open-tag) "^" sec-name (close-tag)
                        (mustachize section)
                        (open-tag) "/" sec-name (close-tag)
                        (mustachize (cdr tokens))))]
      ;; Partial
      [(token-partial template)
       (string-append (open-tag) "> " template (close-tag)
                      (mustachize (cdr tokens)))]
      ;; Delimiter
      [(token-delimiter new-otag new-ctag)
       (string-append (open-tag) "=" new-otag " " new-ctag "=" (close-tag)
                      (parameterize ([open-tag new-otag]
                                     [close-tag new-ctag])
                        (mustachize (cdr tokens))))]
      ;; if this is a unknow token, proceed without processing this
      ;; token
      [_
       (mustachize (cdr tokens))])]))
