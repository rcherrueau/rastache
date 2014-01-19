'((person '((name "Chris") (in_ca #t)))
  (price  '((value 10000)))
  (states '((ca
             '((taxed_value (lambda (self)
                              (let ([val
                                     (rastache-ref (rastache-ref self 'price)
                                                   'value)])
                                (- val (* val 0.4))))))))))
