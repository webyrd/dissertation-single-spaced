(define-syntax ==-bottom
  (syntax-rules ()
    ((_ u v)
     (timed-lambda (s)
       (let ((s (unify u v s)))
         (if (not s) (mzero-bottom) (unit-bottom s)))))))

(define-syntax disj-bottom
  (syntax-rules () 
    ((_ g1 g2) (timed-lambda (s) (mplus-bottom (g1 s) (g2 s))))))

(define-syntax conj-bottom
  (syntax-rules () 
    ((_ g1 g2) (timed-lambda (s) (bind-bottom (g1 s) g2)))))