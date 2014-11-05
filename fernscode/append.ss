(define append-bottom
  (timed-lambda (s1 s2)
    (cond
      ((null? s1) s2)
      (else (frons (fcar s1) (append-bottom (fcdr s1) s2))))))