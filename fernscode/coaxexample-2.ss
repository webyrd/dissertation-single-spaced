(let loop ((p (e 5)))
  (cons (car p) (if (car p) (list (cdr p)) (loop ((cdr p) 5)))))

