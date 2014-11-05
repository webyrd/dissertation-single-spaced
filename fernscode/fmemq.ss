(define fmemq
  (timed-lambda (x s)
    (cond
      ((null? s) #f)
      ((eq? (fcar s) x) s)
      (else (fmemq x (fcdr s))))))