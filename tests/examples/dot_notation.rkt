'((person
   '((name "Chris")
     (in_ca #t)))
  (price
   '((value 10000)))
  (states
   '((ca
      '((taxed_value (lambda (self)
                       (define val (mustache-value (mustache-price self)))
                       (- val (* val 0.4))))))))))
