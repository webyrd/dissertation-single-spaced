(define bind-bottom
  (timed-lambda (s f)
    (cond
      ((null? s) '())
      (else (mplus-bottom (f (fcar s)) (bind-bottom (fcdr s) f))))))
