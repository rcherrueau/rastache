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
        [(define write-proc
           (lambda (token port mode)
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
                                       content) port)]))))])

;; Internal representation of a line in a mustache template. A line
;; gets two properties in addition to the content. `linefeed?' is true
;; if line got a linefeed at parse time. `standalone?' is true if line
;; is standalone.
(struct line (content linefeed? standalone?))

;; Returns #t if line is `eof', #f otherwise.
(define (line-eof?  line)
  (and (line? line)
       (eof-object? (line-content line))))

;; Token constructor for static token.
(define (token-static content)
  (token 'static content null))

;; Token constructor for etag token.
(define (token-etag content)
  (token 'etag (string->symbol content) null))

;; Token constructor for utag token.
(define (token-utag content)
  (token 'utag (string->symbol (string-trim content)) null))

;; Token constructor for section token.
(define (token-sec content [section null])
  (token 'section (string->symbol (string-trim content)) section))

;; Token constructor for inverted section token.
(define (token-inv-sec content [section null])
  (token 'inverted-section (string->symbol (string-trim content)) section))

;; Token constructor for partial token.
(define (token-partial content)
  (token 'partial content null))

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
    "\\s*" ; Skip any whitespace
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

;; Reads the stream until recognizing the close tag and returns the
;; reading content. Mustache element could be on multiple line. The
;; method reads mustache element on multiple line.
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
      (error "Error while reading a multi-ligne tag")]
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
  (let scan ([tokens (list)]
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
            ;; Single periode
            [(equal? value ".")
             (scan-ahead (token-etag "."))]
            ;; Simple Etag
            [(equal? (length periods-split) 1)
             (scan-ahead (token-etag (car periods-split)))]
            ;; Dotted names should be considered a form of shorthand
            ;; for sections
            [else
             (scan-ahead
              (let make-token ([tags periods-split])
                (if (> (length tags) 1)
                    (token-sec (car tags) (list (make-token (cdr tags))))
                    (token-etag (car tags)))))]))]

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
            ;; Single periode
            [(equal? value ".")
             (scan-ahead (token-utag "."))]
            ;; Simple Etag
            [(equal? (length periods-split) 1)
             (scan-ahead (token-utag (car periods-split)))]
            ;; Dotted names should be considered a form of shorthand
            ;; for sections
            [else
             (scan-ahead
              (let make-token ([tags periods-split])
                (if (> (length tags) 1)
                    (token-sec (car tags) (list (make-token (cdr tags))))
                    (token-utag (car tags)))))]))]

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
             (scan-ahead (token-sec (car periods-split) nested-tokens))]
            ;; Section name with periods
            [else
             (scan-ahead
              (let make-token ([tags periods-split])
                (if (> (length tags) 1)
                    (token-sec (car tags) (list (make-token (cdr tags))))
                    (token-sec (car tags) nested-tokens))))]))]

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
             (scan-ahead (token-inv-sec (car periods-split) nested-tokens))]
            ;; Section name with periods
            [else
             (scan-ahead
              (let make-token ([tags periods-split])
                (if (> (length tags) 1)
                    (token-sec (car tags) (list (make-token (cdr tags))))
                    (token-inv-sec (car tags) nested-tokens))))]))]

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

         (define new-otag (regexp-quote (car ll)))
         (define new-ctag (regexp-quote
                           (if (= (length ll) 2)
                               (substring (cadr ll) 0
                                          (sub1 (string-length (cadr ll))))
                               ;; Superfluous in-tag whitespace should be ignored:
                               ;; {{= @   @ =}}
                               (cadr ll))))
         (define new-standalone-pattern
           (make-standalone-pattern new-otag new-ctag))

         (scan-static new-line tokens new-otag new-ctag
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
