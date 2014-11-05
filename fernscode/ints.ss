(define ints-bottom
  (timed-lambda (n)
    (frons n (ints-bottom (+ n 1)))))