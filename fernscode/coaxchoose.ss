(define-syntax choose-bottom
  (syntax-rules ()
    ((_ exp1 exp2) (choose-aux-bottom (engine exp1) (engine exp2)))))

(define choose-aux-bottom
  (timed-lambda (e1 e2)
    (let ((p (e1 1)))
      (if (car p) (cdr p) (choose-aux-bottom e2 (cdr p))))))
