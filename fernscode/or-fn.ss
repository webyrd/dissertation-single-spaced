(define or-bottom
  (timed-lambda (s)
    (cond
      ((null? s) #f)
      ((fcar s) (fcar s))
      (else (or-bottom (fcdr s))))))
