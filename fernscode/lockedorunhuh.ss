(define engine-tag-compare
  (lambda (get-te tag)
    (lambda (q)
      (and (pair? q) (pair? (get-te q)) (eq? (car (get-te q)) tag)))))

(define engine-tag-L-car (engine-tag-compare car 'L))
(define engine-tag-U-car (engine-tag-compare car 'U))
(define engine-tag-L-cdr (engine-tag-compare cdr 'L))
(define engine-tag-U-cdr (engine-tag-compare cdr 'U))
