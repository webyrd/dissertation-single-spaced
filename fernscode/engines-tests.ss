(import (engines) (ikarus) (testing))
(print-compiling "engines")
(define cons->list
  (lambda (p)
    (cons (car p) (cons (cdr p) '()))))
(define f
  (timed-lambda (n)
    (cond
      ((zero? n) 1)
      (else (* n (f (- n 1)))))))

(define x
  (make-engine
    (lambda ()
      (let ([y (make-engine (lambda () (f 5)))])
        (let ((p (y 1)))
          (if (car p) (cons 0 (cons->list p))
              (set! y (cdr p))))
        (let ((p (y 1)))
          (if (car p) (cons 1 (cons->list p))
              (set! y (cdr p))))
        (let ((p (y 1)))
          (if (car p) (cons 2 (cons->list p))
              (set! y (cdr p))))
        (let ((p (y 1)))
          (if (car p) (cons 3 (cons->list p))
              (set! y (cdr p))))
        (let ((p (y 1)))
          (if (car p) (cons 4 (cons->list p))
              (set! y (cdr p))))
        (let ((p (y 1)))
          (if (car p) (cons 5 (cons->list p))
              (set! y (cdr p))))
        (let ((p (y 1)))
          (if (car p) (cons 6 (cons->list p))
              (set! y (cdr p))))))))

(test "x 1"
  (let ((p (x 1)))
    (if (car p) (cons->list p)
        (begin
          (set! x (cdr p))
          "failed")))
   "failed")

(test "x 100"
  (let ((p (x 100)))
    (if (car p) (cons->list p)
        (begin
          (set! x (cdr p))
          "failed")))
  (list 94 (list 6 0 120)))

(define init (make-engine (lambda () (f 5))))
(define fail (lambda (r) (set! e r) "called fail"))
(define e init)
(test "timed 5!" (f 5) 120)
(test "succeeding engine"
  (let ((p (e 100)))
    (if (car p) (cons->list p)
        (fail (cdr p))))
  '(94 120))

(set! e init) 
(test "failing engine"
  (let ((p (e 3)))
    (if (car p) (cons->list p) (fail (cdr p))))
  "called fail" )
(test "resuming engine"
  (let ((p (e 100)))
    (if (car p) (cons->list p) (fail (cdr p))))
  '(97 120))

(define parent
  (make-engine
    (lambda ()
      (cons 5
        (let loop ([e (make-engine (lambda () (f 5)))])
          (let ((p (e 100)))
            (if (car p) (cons->list p) (loop (cdr e)))))))))

(define failed-parent (lambda (e) (set! parent e) "failed parent"))

(test "failing parent" (cons 7
                             (let ((p (parent 2)))
                               (if (car p)
                                   (cons->list p)
                                   (failed-parent (cdr p)))))
      '(7 . "failed parent"))

(test "resuming child by resuming parent"
  (cons 8 (let ((p (parent 1000)))
            (if (car p) (cons->list p) (failed-parent (cdr p)))))
  '(8 . (996 (5 . (94 120)))))

(define triple
  (make-engine
    (lambda ()
      (cons 11
          (let loop ([e (make-engine
                          (lambda ()
                            (cons 9 (let loop ([e (make-engine (lambda ()
                                                                 (f 15)))])
                                          (let ((p (e 100)))
                                            (if (car p) (cons->list p)
                                                (loop (cdr p))))))))])
            (let ((p (e 10)))
              (if (car p) (cons->list p) (loop (cdr p)))))))))

(test "failing triple"
  (let ((p (triple 7)))
    (if (car p) (cons->list p) ((lambda (e) (set! triple e) "failed triple") (cdr p))))
  "failed triple")
(test "second triple"
  (let ((p (triple 6)))
    (if (car p) (cons->list p) ((lambda (e) (set! triple e) "second triple") (cdr p))))
  "second triple")

(test "t1"
  (let ((p (triple 1)))
    (if (car p) (cons->list p) ((lambda (e) (set! triple e) "triple 1 1") (cdr p))))
  "triple 1 1")

(test "t2"
  (let ((p (triple 1)))
    (if (car p) (cons->list p) ((lambda (e) (set! triple e) "triple 1 2") (cdr p))))
  "triple 1 2")

(test "third triple"
  (let ((p (triple 3)))
    (if (car p) (cons->list p) ((lambda (e) (set! triple e) "third triple") (cdr p))))
  (list 2 (cons 11 (list 4 (cons 9 (list 84 (f 15)))))))

(define first-true
  (lambda proc-list
    (letrec ([q '()]
             [dequeue (lambda () (let ([front (car q)]) (set! q (cdr q)) front))]
             [enqueue (lambda (x) (set! q (append q (list x))))]
             [run (lambda ()
                    (and (not (null? q))
                      (let ((p ((dequeue) 1)))
                        (if (car p) (or (cdr p) (run))
                            ((lambda (e) (enqueue e) (run)) (cdr p))))))])
      (for-each (lambda (proc) (enqueue (make-engine proc))) proc-list)
      (run))))

(define-syntax parallel-or
  (syntax-rules ()
    [(_ e ...)
     (first-true (lambda () e) ...)]))

(set! e init)
(set! fail (lambda (r) (set! e r) #f))
(test "por0" (parallel-or #f (= 1 2) (f 3) (f -1))
      (f 3))
(test "por1" (parallel-or (parallel-or (= 1 2) (parallel-or #f (= (f 3) (f 2))))
                          (parallel-or (f -1) #f (f 5)))
      (f 5))
