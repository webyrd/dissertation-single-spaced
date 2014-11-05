(define product
  (timed-lambda (s)
    (cond
      ((null? s) 1)
      ((zero? (fcar s)) 0)
      (else (* (fcar s) (product (fcdr s)))))))



