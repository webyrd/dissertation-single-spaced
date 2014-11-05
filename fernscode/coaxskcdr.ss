(define coaxer
  (lambda (get-te set-val!)
    (lambda (q)
      (let ((te (get-te q)))
        (set-car! te 'L)
        (let ((p (coax (cdr te))))
          (let ((b (car p)) (v (cdr p)))
            (when b (set-val! q v))
            (replace! te 'U (if b (engine v) v))))))))

(define coax (lambda (e) (e nsteps)))
(define coax-a (coaxer car set-car!))
(define coax-d (coaxer cdr set-cdr!))
