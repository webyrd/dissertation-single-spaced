(define-record-type var)

(define unify 
  (lambda (t1 t2 s)
    (let ((t1 (walk t1 s)) (t2 (walk t2 s)))
      (cond
        ((eq? t1 t2) s) 
        ((var? t1) (ext-s t1 t2 s))
        ((var? t2) (ext-s t2 t1 s))
        ((and (pair? t1) (pair? t2))
         (let ((s (unify (car t1) (car t2) s)))
           (and s (unify (cdr t1) (cdr t2) s))))
        (else (if (equal? t1 t2) s #f))))))

(define empty-s '())

(define ext-s
  (lambda (x t s) 
    `((,x . ,t) . ,s)))

(define walk
  (lambda (t s)
    (let ((b (assq t s)))
      (if b (walk (cdr b) s) t))))
