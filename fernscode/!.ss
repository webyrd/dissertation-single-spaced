(define !
  (timed-lambda (n)
    (cond
      ((zero? n) 1)
      (else (* n (! (- n 1)))))))
