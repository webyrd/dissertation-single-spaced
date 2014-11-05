(define ints-bottom
  (timed-lambda (n)
    (frons n (ints-bottom (add1 n)))))
