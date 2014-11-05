(define take-bottom
  (timed-lambda (n s)
    (cond
      ((null? s) '())
      (else (cons (fcar s)
              (if (and n (= n 1)) '() (take-bottom (and n (- n 1)) (fcdr s))))))))

