(define-syntax choose
  (syntax-rules ()
    ((_ exp1 exp2) (choose-fn (engine exp1) (engine exp2)))))

(define choose-fn
  (timed-lambda (e1 e2)
    (let ((p (e1 1)))
      (if (car p) (cdr p) (choose-fn e2 (cdr p))))))
