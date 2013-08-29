'(
  (header (lambda () "Colors"))
  (item ((name "red") (current #t) (url "#Red"))
        ((name "green") (current #f) (url "#Green"))
        ((name "blue") (current #f) (url "#Blue")))

  (link (lambda (self) (not (eq? (self-current self) #t))))
  (list (lambda (self) (not (eq? (length (self-item self)) 0))))
  (empty (lambda (self) (eq? (length (self-item self)) 0))))
