(define-syntax engine
  (syntax-rules ()
    ((_ e) (make-engine (lambda () e)))))

(define-syntax timed-lambda
  (syntax-rules ()
    ((_ formals b0 b ...) (lambda formals (expend-tick-to-call (lambda () b0 b ...))))))
