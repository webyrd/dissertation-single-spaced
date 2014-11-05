(define always-five-bottom
  (timed-lambda (x)
    (disj-bottom (always-five-bottom x) (==-bottom x 5))))
