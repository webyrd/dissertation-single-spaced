(define cdrdollar
  (timed-lambda (p)
    (cond
      ((engine-tag-L-cdr p) (wait nsteps) (cdrdollar p))
      ((engine-tag-U-cdr p) (coax-d p) (cdrdollar p))
      (else (cdr p)))))
