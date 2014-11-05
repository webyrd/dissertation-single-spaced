(define map
  (timed-lambda (f s)
    (cond
      ((null? s) '())
      (else (frons (f (fcar s)) (map f (fcdr s)))))))
