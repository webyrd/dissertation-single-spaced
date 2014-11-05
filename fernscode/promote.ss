(define-syntax timed-let
  (syntax-rules ()
    ((_ ((x e) ...) b0 b ...) ((timed-lambda (x ...) b0 b ...) e ...))))

(define promote!
  (timed-lambda (p)
    (cond
      ((engine-tag-L-car p) (wait nsteps) (promote! p))
      ((engine-tag-U-car p)
       (set-car! (car p) 'L)
       (timed-let ((te (car p)))
         (timed-let ((r (promote! (cdr p))))
           (replace! p (car r) (cons te (cdr r)))
           (set-car! te 'U)
           p)))
      (else p))))
