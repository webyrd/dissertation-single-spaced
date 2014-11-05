(let ((b (cons 2 '())))
  (let ((a (cons 1 b)))
    (list (car a) (cadr a) (car b))))