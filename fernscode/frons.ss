(define-syntax frons
  (syntax-rules ()
    ((_ a d) (cons (cons 'U (engine a)) (cons 'U (engine d))))))
