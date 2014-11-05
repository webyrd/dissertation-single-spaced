(define-syntax mplus-bottom
  (syntax-rules ()
    ((_ s1 s2) (mplus-fn-bottom (fern s1 s2)))))

(define mplus-fn-bottom
  (timed-lambda (p)
    (cond
      ((null? (fcar p)) (fcadr p))
      (else (frons (fcaar p)
              (mplus-bottom (fcadr p) (fcdar p)))))))
