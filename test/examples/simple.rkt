'(
  (name "Chris")
  (value 10000)
  (taxed_value (lambda (self)
                 (define val (self-value self))
                 (- val (* val 0.4))))
  (in_ca #t)
  (owner null))
