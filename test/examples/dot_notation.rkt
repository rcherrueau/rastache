'(
  (person
   (name "Chris")
   (in_ca #t))
  (price 10000)
  (state
   (ca
    (taxed_value (lambda (self)
                   (define val (self-value (self-price self)))
                   (- val (* val 0.4)))))))
