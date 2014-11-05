(let ((b (frons 2 '())))
  (let ((a (frons (begin (! 5) 1) b)))
    (list (fcar a) (fcadr a) (fcar b))))