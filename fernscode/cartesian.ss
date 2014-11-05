(define Cartesian-product-bottom
  (timed-lambda (s1 s2)
    (cond
      ((null? s1) '())
      (else (mplus-bottom (map-bottom (timed-lambda (e) (cons (fcar s1) e)) s2)
                    (Cartesian-product-bottom (fcdr s1) s2))))))
