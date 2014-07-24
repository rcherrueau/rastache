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
(provide tokenize
         mustachize)

; ______________________________________________________________________________
; import and implementation
(require "commons.rkt"
         racket/match
         racket/port
         racket/string)

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
    "\\s*" ; Skip any whitespacep
    "(.+)" ; Capture the text inside of the tag
    "\\s*" ; Skip any whitespace
    "\\}?" ; Skip balancing '}' if it exists
    "(.*)$"))) ; Capture the rest of the string

;; Returns a srting containing the next line of bytes from `in'.
;; read-line: in pregexp -> line
(define (read-line in standalone-pattern
                   [return-port? #t]
                   [test-standalone? #t])
  (define is-standalone? regexp-match-exact?)
  (let _read-line ([acc ""])
    (let ([c (read-char in)])
      (cond
       [(eof-object? c)
        (if (eq? acc "")
            (line eof #f #t)
            (line (if return-port?
                      (open-input-string acc)
                      acc)
                  #f
                  (and test-standalone?
                       (is-standalone? standalone-pattern acc))))]
       [(eq? c #\newline)
        (line (if return-port?
                  (open-input-string acc)
                  acc)
              #t
              (and test-standalone?
                   (is-standalone? standalone-pattern acc)))]
       [else
        (_read-line (string-append acc (string c)))]))))

;; Reads the stream line by line until recognizing the line that
;; contains close tag and returns the reading content. Mustache
;; element could be on multiple line. The method reads mustache
;; element on multiple line.
;; read-multiline: in string ctag pregexp -> (values line ctag-pos)
(define (read-multiline in starter ctag standalone-pattern)
  (let _read-multiline ([acc (port->string (line-content starter))])
    (define new-line (read-line in standalone-pattern #f #f))
    (define ctag-pos (unless (line-eof? new-line)
                       (close-tag-position
                        ctag
                        (open-input-string (line-content new-line)))))
    (define full-line (unless (line-eof? new-line)
                        (string-append acc (line-content new-line))))
    (cond
     ;; End of `in' without mustache closing tag
     ;; => Error!
     [(line-eof? new-line)
      (error "Error while reading a multi-line tag")]
     ;; Mustache closing tag at the end of `new-line'
     ;; => Returns concatenation of full-line and new-line
     [ctag-pos
      (let* ([acc-length (string-length acc)]
             [full-ctag-pos
              (cons (+ acc-length (car ctag-pos))
                    (+ acc-length (cdr ctag-pos)))])
        (values
         (line (open-input-string full-line)
               (line-linefeed? new-line)
               (line-standalone? starter))
         full-ctag-pos))]
     ;; No mustache closing tag
     ;; => Go ahead
     [else
      (_read-multiline full-line)])))

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

;; Consumes the mustache closing tag from the template. `ctag-quoted'
;; is a pattern that matches exactly the original mustache closing
;; tag.
(define (read-close-tag ctag-quoted port)
  (void (regexp-match ctag-quoted port)))

;; Makes a token key from a string
(define key string->symbol)

;; Construct the list of tokens for a specific template. The
;; `template' has to be an input port that reads bytes from a UTF-8
;; stream.`open-tag' and `close-tag' are mustache keywords
;; identifiers.
;; tokenize: input-port string string -> (listof token)
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
  ;; parse: (listof tokens) string string pregexp -> (listof tokens)
  (let parse ([tokens (list (token-delimiter (open-tag)
                                             (close-tag)))])
    ;; Util function wich constructs tokens for dotted names:
    ;; 'etag: {{a.b}} produces
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
          (token-maker (key (car tags)) (list (_mdt (cdr tags))) #t)]
         [else
          (case type
            ['etag (token-etag (key (car tags)))]
            ['utag (token-utag (key (car tags)))]
            ['sec (token-sec (key (car tags)) nested #f)]
            ['inv-sec (token-inv-sec (key (car tags)) nested #f)])])))

    ;; Util function which continues the parsing of a line after
    ;; parsing current tag.
    (define (parse-ahead line tokens new-token)
      (parse-static line (append tokens (list new-token))))

    ;; Util function which parses a tag.
    (define (parse-tag line tokens)
      ;; Consume the mustache opening tag
      (read-open-tag (otag-quoted) (line-content line))

      ;; Search for mustache closing tag
      (define ctag-pos (close-tag-position (ctag-quoted)
                                           (line-content line)))
      (define-values (new-line new-ctag-pos)
        (cond
         [(not ctag-pos) (read-multiline template
                                         line
                                         (ctag-quoted)
                                         (standalone-pattern))]
         [else (values line ctag-pos)]))

      ;; Consume content of mustache tag
      (define content-length (car new-ctag-pos))
      (define content (read-string content-length
                                   (line-content new-line)))

      ;; Consume the mustache closing tag
      (read-close-tag (ctag-quoted) (line-content new-line))

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
               (token-etag (key (car name)))]
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
               (token-utag (key (car name)))]
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
               (token-sec (key (car name)) nested-tokens #f)]
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
               (token-inv-sec (key (car name)) nested-tokens #f)]
              ;; Section name with periods
              [else
               (make-dotted-tokens name
                                   #:type 'inv-sec
                                   #:sec-tokens nested-tokens)])))
         (parse-ahead eol tokens the-token)]

        ;; End of (Inverted) Section
        [("/") (values tokens new-line)]

        ;; Comments
        [("!")
         (parse-static new-line tokens)]

        ;; Partial
        [(">" "<")
         (define the-token (token-partial value))
         (parse-static new-line (append tokens (list the-token)))]

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
           (parse-static new-line (append tokens (list the-token))))]))

    ;; Util function which parses static content.
    (define (parse-static line tokens)
      ;; Search for the mustache opening tag
      (define otag-pos (open-tag-position (otag-quoted)
                                          (line-content line)))

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
            (append tokens (list (token-static content)
                                 (token-static "\n")))]
           ;; Non-linefeed line: solo add the rest of the template.
           [else
            (append tokens (list (token-static content)))]))
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
            (append tokens (list (token-static content)))]
           [else
            tokens]))
        (parse-tag line new-tokens)]))

    (define line (read-line template (standalone-pattern)))

    (cond
     [(line-eof? line) tokens]
     [else
      (parse-static line tokens)])))

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
