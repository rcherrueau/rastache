#lang racket/base

;                      /\ \__                /\ \
;  _ __    __      ____\ \ ,_\    __      ___\ \ \___      __
; /\`'__\/'__`\   /',__\\ \ \/  /'__`\   /'___\ \  _ `\  /'__`\
; \ \ \//\ \L\.\_/\__, `\\ \ \_/\ \L\.\_/\ \__/\ \ \ \ \/\  __/
;  \ \_\\ \__/.\_\/\____/ \ \__\ \__/.\_\ \____\\ \_\ \_\ \____\
;   \/_/ \/__/\/_/\/___/   \/__/\/__/\/_/\/____/ \/_/\/_/\/____/
; A racket Mustache template engine.

; Tests for rastache.
(require rackunit
         rackunit/text-ui
         "examples/boolean.rkt"
         "examples/carriage_return.rkt"
         "../context.rkt"
         "../scanner.rkt")


;; For debug only
;; (define (display-token token)
;;   (displayln (format "sigil: ~a, content: ~a"
;;                      (token-sigil token)
;;                      (token-content token)))

;;   (when (eq? (token-sigil token) 'section)
;;     (displayln (format "*** Section ~a ***" (token-content token)))
;;     (map display-token (token-section token))
;;     (displayln "*****************")))

;; Tests of the context module
(define context-tests
  (test-suite
   "Tests for the mustache-make-ctx macro"

   (test-case
    carriage_return-name

    (mustache-make-ctx ctx carriage-return-expr)
    (printf "~s~n" ctx)

    (check-equal? ctx
                  #hash((foo . "Hello World"))
                  "bad htable generation")
    (check-equal? (mustache-foo ctx) "Hello World" "bad mustache-foo semantic"))

   (test-case
    "'((name \"Jim\") (age 24) (admin #t))"

    (mustache-make-ctx ctx
                       '((name "Jim") (age 24) (admin #t)))

    (check-equal? ctx
                  #hash((name . "Jim") (admin . #t) (age . 24))
                  "bad htable generation")
    (check-equal? (mustache-name ctx) "Jim" "bad mustache-name semantic")
    (check-equal? (mustache-age ctx) 24 "bad mustache-age semantic")
    (check-equal? (mustache-admin ctx) #t "bad mustache-admin semantic"))

   (test-case
    "((title (lambda () \"A Comedy of Errors\")))"

    (mustache-make-ctx ctx
                       '((title (lambda () "A Comedy of Errors"))))

    (check-equal? ((mustache-title ctx))
                  "A Comedy of Errors"
                  "bad mustache-name semantic"))

   (test-case
    "'((header (lambda () \"Colors\"))
  (item '((name \"red\") (current #t) (url \"#Red\"))
        '((name \"green\") (current #f) (url \"#Green\"))
        '((name \"blue\") (current #f) (url \"#Blue\")))
  (link (lambda (self) (not (eq? (mustache-current self) #t))))
  (list (lambda (self) (not (eq? (length (mustache-item self)) 0))))
  (empty (lambda (self) (eq? (length (mustache-item self)) 0))))"

    (mustache-make-ctx
     ctx
     '((header (lambda () "Colors"))
       (item '((name "red") (current #t) (url "#Red"))
             '((name "green") (current #f) (url "#Green"))
             '((name "blue") (current #f) (url "#Blue")))
       (link (lambda (self) (not (eq? (mustache-current self) #t))))
       (list (lambda (self) (not (eq? (length (mustache-item self)) 0))))
       (empty (lambda (self) (eq? (length (mustache-item self)) 0)))))

    (check-equal? ((mustache-header ctx))
                  "Colors"
                  "bad mustache-header semantic")
    (check-equal? (mustache-item ctx)
                  '(#hash((name . "red") (current . #t) (url . "#Red"))
                    #hash((name . "green") (current . #f) (url . "#Green"))
                    #hash((name . "blue") (current . #f) (url . "#Blue")))
                  "bad mustache-item semantic")
    (check-equal? (mustache-name (car (mustache-item ctx)))
                  "red"
                  "bad mustache-name semantic")
    (check-equal? (mustache-current (cadr (mustache-item ctx)))
                  #f
                  "bad mustache-current semantic")
    (check-equal? (mustache-url (caddr (mustache-item ctx)))
                  "#Blue"
                  "bad mustache-url semantic")
    (check-equal? ((mustache-link ctx) (car (mustache-item ctx)))
                  #f
                  "bad mustache-link semantic")
    (check-equal? ((mustache-list ctx) ctx)
                  #t
                  "bad mustache-list semantic")
    (check-equal? ((mustache-empty ctx) ctx)
                  #f
                  "bad mustache-empty semantic"))))

;; Tests of the scanner module
;; (define scanner-tests
;;   (test-suite
;;    ""


(run-tests context-tests)
