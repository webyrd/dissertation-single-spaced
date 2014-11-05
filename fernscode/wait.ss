(define wait
  (timed-lambda (n)
    (cond
      ((zero? n) 'done)
      (else (wait (- n 1))))))
