(let ((b (frons 2 '())))
  (let ((a (frons 1 b)))
    (list (fcar a) (fcadr a) (fcar b))))