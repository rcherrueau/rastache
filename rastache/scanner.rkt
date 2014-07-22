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

;; Construct the list of tokens for a specific template. The
;; `template' has to be an input port that reads bytes from a UTF-8
;; stream.`open-tag' and `close-tag' are mustache keywords
;; identifiers.
;; tokenize: input-port string string -> (listof token)
(define (tokenize template [open-tag "{{"] [close-tag "}}"])
  (define otag-quoted (regexp-quote open-tag))
  (define ctag-quoted (regexp-quote close-tag))
  (define pattern (make-standalone-pattern otag-quoted ctag-quoted))

  ;; Scans the text and instanciate tokens. `otag' and `ctag' should
  ;; be patterns that matche exactly the original open-tag and
  ;; close-tag.
  ;; scan: (listof tokens) string string pregexp -> (listof tokens)
  (let scan ([tokens (list (token-delimiter open-tag close-tag))]
             [otag otag-quoted]
             [ctag ctag-quoted]
             [standalone-pattern pattern])

    (define (scan-tag line tokens otag ctag standalone-pattern)
      ;; Consume the mustache opening tag
      (read-open-tag otag (line-content line))

      ;; Search for mustache closing tag
      (define ctag-pos (close-tag-position ctag (line-content line)))
      (define-values (new-line new-ctag-pos)
        (if (not ctag-pos)
            (read-multiline template line ctag standalone-pattern)
            (values line ctag-pos)))

      ;; Consume content of mustache tag
      (define content-length (car new-ctag-pos))
      (define content (read-string content-length (line-content new-line)))

      ;; Consume the mustache closing tag
      (read-close-tag ctag (line-content new-line))

      (define l (regexp-match state-pattern content))
      (define sigil (cadr l))
      (define value (caddr l))

      (case sigil
        ;; Etag
        [(#f)
         (let ([periods-split (regexp-split #rx"\\." (string-trim value))]
               [scan-ahead (lambda (the-token)
                             (scan-static new-line
                                          (append tokens (list the-token))
                                          otag ctag standalone-pattern))])
           (cond
            ;; Single period
            [(equal? value ".")
             ;; Period tag name is changed by `period-name'
             (scan-ahead (token-etag period-name))]
            ;; Simple Etag
            [(equal? (length periods-split) 1)
             (scan-ahead (token-etag (string->symbol (car periods-split))))]
            ;; Dotted names should be considered a form of shorthand
            ;; for sections
            [else
             (scan-ahead
              (let make-token ([tags periods-split])
                (if (> (length tags) 1)
                    (token-sec (string->symbol (car tags))
                               (list (make-token (cdr tags))) #t)
                    (token-etag (string->symbol (car tags))))))]))]

        ;; Unescaped HTML
        [("{" "&")
         ;; if unescaped starting with "{", then consumes the closing "}"
         (when (equal? sigil "{")
           (read-string (string-length "}") (line-content new-line)))
         (let ([periods-split (regexp-split #rx"\\." (string-trim value))]
               [scan-ahead (lambda (the-token)
                             (scan-static new-line
                                          (append tokens (list the-token))
                                          otag ctag standalone-pattern))])
           (cond
            ;; Single period
            [(equal? value ".")
             ;; Period tag name is changed by `period-name.
             (scan-ahead (token-utag period-name))]
            ;; Simple Utag
            [(equal? (length periods-split) 1)
             (scan-ahead (token-utag (string->symbol (car periods-split))))]
            ;; Dotted names should be considered a form of shorthand
            ;; for sections
            [else
             (scan-ahead
              (let make-token ([tags periods-split])
                (if (> (length tags) 1)
                    (token-sec (string->symbol (car tags))
                               (list (make-token (cdr tags))) #t)
                    (token-utag (string->symbol (car tags))))))]))]

        ;; Section
        [("#")
         ;; First compute nested tokens; eol is for end-of-line
         (define-values (nested-tokens eol)
           (scan-static new-line (list) otag ctag standalone-pattern))
         ;; Then add nested tokens and continue with the end of line.
         ;; Dotted names should be considered a form of shorthand for
         ;; sections.
         (let ([periods-split (regexp-split #rx"\\." (string-trim value))]
               [scan-ahead (lambda (the-token)
                             (scan-static eol
                                          (append tokens (list the-token))
                                          otag ctag standalone-pattern))])
           (cond
            ;; Simple section name
            [(equal? (length periods-split) 1)
             (scan-ahead (token-sec (string->symbol (car periods-split))
                                    nested-tokens #f))]
            ;; Section name with periods
            [else
             (scan-ahead
              (let make-token ([tags periods-split])
                (if (> (length tags) 1)
                    (token-sec (string->symbol (car tags))
                               (list (make-token (cdr tags))) #t)
                    (token-sec (string->symbol (car tags))
                               nested-tokens #f))))]))]

        ;; Inverted Section
        [("^")
         ;; First compute nested tokens; eol is for end-of-line
         (define-values (nested-tokens eol)
           (scan-static new-line (list) otag ctag standalone-pattern))
         ;; Then add nested tokens and continue with the end of line.
         ;; Dotted names should be considered a form of shorthand for
         ;; sections.
         (let ([periods-split (regexp-split #rx"\\." (string-trim value))]
               [scan-ahead (lambda (the-token)
                             (scan-static eol
                                          (append tokens (list the-token))
                                          otag ctag standalone-pattern))])
           (cond
            ;; Simple section name
            [(equal? (length periods-split) 1)
             (scan-ahead (token-inv-sec (string->symbol (car periods-split))
                                        nested-tokens #f))]
            ;; Section name with periods
            [else
             (scan-ahead
              (let make-token ([tags periods-split])
                (if (> (length tags) 1)
                    (token-inv-sec (string->symbol (car tags))
                                   (list (make-token (cdr tags))) #t)
                    (token-inv-sec (string->symbol (car tags))
                                   nested-tokens #f))))]))]

        ;; End of (Inverted) Section
        [("/") (values tokens new-line)]

        ;; Comments
        [("!")
         (scan-static new-line tokens otag ctag standalone-pattern)]

        ;; Partial
        [(">" "<")
         (define the-token (token-partial value))
         (scan-static new-line (append tokens (list the-token))
                      otag ctag standalone-pattern)]

        ;; Set delimiters
        [("=")
         (define ll (string-split value))
         (when (< (length ll) 2) (error "Bad delimeter syntax"))

         (define new-otag (car ll))
         (define new-ctag (if (= (length ll) 2)
                              (substring (cadr ll) 0
                                         (sub1 (string-length (cadr ll))))
                              ;; Superfluous in-tag whitespace should be ignored:
                              ;; {{= @   @ =}}
                              (cadr ll)))
         (define the-token (token-delimiter new-otag new-ctag))
         (define new-otag-quoted (regexp-quote new-otag))
         (define new-ctag-quoted (regexp-quote new-ctag))
         (define new-standalone-pattern
           (make-standalone-pattern new-otag-quoted new-ctag-quoted))

         (scan-static new-line (append tokens (list the-token))
                      new-otag-quoted new-ctag-quoted
                      new-standalone-pattern)]))

    (define (scan-static line tokens otag ctag standalone-pattern)
      ;; Search for the mustache opening tag
      (define otag-pos (open-tag-position otag (line-content line)))

      (cond
       ;; No more mustache tag
       ;; Create a static token with the rest of the template.
       [(not otag-pos)
        (define content (port->string (line-content line)))
        (define new-tokens
          ;; Don't keep the value of a standalone line.
          (cond
           [(line-standalone? line)
            tokens]
           ;; Add the linefeed token on the linefeed line
           [(line-linefeed? line)
            (append tokens (list (token-static content)
                                 (token-static "\n")))]
           [else
            (append tokens (list (token-static content)))]))
        (scan new-tokens otag ctag standalone-pattern)]

       ;; Still mustache tag
       ;; Creates a static token with the text until next mustache tag.
       ;; Then processes the mustache tag.
       [else
        (define content-length (car otag-pos))
        (define content (read-string content-length (line-content line)))
        (define new-tokens
          ;; Don't keep the value of a standalone line.
          (if (not (line-standalone? line))
              (append tokens (list (token-static content)))
              tokens))
        (scan-tag line new-tokens otag ctag standalone-pattern)]))

    (define line (read-line template standalone-pattern))

    (cond
     [(line-eof? line) tokens]
     [else
      (scan-static line tokens otag ctag standalone-pattern)])))

(define (mustachize tokens [otag "{{"] [ctag "}}"])
  (cond
   [(null? tokens) ""]
   [else
    (define token (car tokens))
    (match token
      ;; Static
      [(token-static content)
       (string-append content (mustachize (cdr tokens) otag ctag))]
      ;; Etag
      [(token-etag key)
       (string-append otag (symbol->string key) ctag
                      (mustachize (cdr tokens) otag ctag))]
      ;; Utag
      [(token-utag key)
       (string-append otag "&" (symbol->string key) ctag
                      (mustachize (cdr tokens) otag ctag))]
      ;; Section
      [(token-sec key section _)
       (let ([sec-name (symbol->string key)])
         (string-append otag "#" sec-name ctag
                        (mustachize section otag ctag)
                        otag "/" sec-name ctag
                        (mustachize (cdr tokens) otag ctag)))]
      ;; Inverted Section
      [(token-inv-sec key section _)
       (let ([sec-name (symbol->string key)])
         (string-append otag "^" sec-name ctag
                        (mustachize section otag ctag)
                        otag "/" sec-name ctag
                        (mustachize (cdr tokens) otag ctag)))]
      ;; Partial
      [(token-partial template)
       (string-append otag "> " template ctag
                      (mustachize (cdr tokens) otag ctag))]
      ;; Delimiter
      [(token-delimiter o c)
       (string-append otag "=" o " " c "=" ctag
                      (mustachize (cdr tokens) o c))]
      ;; if this is a unknow token, proceed without processing this
      ;; token
      [_
       (mustachize (cdr tokens))])]))
