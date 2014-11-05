(define replace!
  (lambda (p a d)
    (set-car! p a)
    (set-cdr! p d)))