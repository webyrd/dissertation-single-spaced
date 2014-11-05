(import (engines) (ferns) (ikarus) (testing))
(define !
  (timed-lambda (n)
    (cond
      ((zero? n) 1)
      (else (* n (! (- n 1)))))))

(define conj
  (timed-lambda (ls)
    (cond
      [(null? ls) #t]
      [(fcar ls) (conj (fcdr ls))]
      [else #f])))

(define w
  (timed-lambda (n)
    (cond
      [(= n 0) (w n)]
      [(= n 1) #t]
      [else #f])))

(define ls1 (fern (w 0) (w 1) (w 2)))

(define foo
  (timed-lambda (n)
    (case n
      [(1 3 5) (foo n)]
      [(2) #t]
      [(4) #f])))

(define ls (fern (foo 1) (foo 2) (foo 3) (foo 4) (foo 5)))

(define take
  (timed-lambda (x n)
    (cond
      ((= n 0) '())
      ((= n 1) (cons (fcar x) '()))
      (else (cons (fcar x) (take (fcdr x) (sub1 n)))))))

(define bar
  (timed-lambda (n)
    (case n
      [(0) 0]
      [(5000) (bar n)]
      [else (bar (sub1 n))])))

(define o (timed-lambda () (o)))

(define frsuccessors
  (timed-lambda (i)
    (frons i (frsuccessors (+ i 1)))))

(define nestme
  (timed-lambda (n v)
    (cond
      ((zero? n) (fern (v)))
      (else (fappend (! -1) (fern (! -1) (! -1)) (! -1) 
                     (nestme (sub1 n) v) (! -1) 
                     (fern (! -1) (! -1)) (! -1))))))

(define alpha
  (timed-lambda (n)
    (if (= n 2)
      (frons (! 5) (alpha (add1 n)))
      (frons (o) (alpha (add1 n))))))

(define beta
   (timed-lambda (n)
     (if (or (= n 2) (= n 3) (= n 4))
       (frons (! 7) (beta (add1 n)))
       (frons (o) (beta (add1 n))))))

(define gamma
   (timed-lambda (n)
      (if (or (= n 0) (= n 1) (= n 2) (= n 3) (= n 4) (= n 5))
        (frons (o) (gamma (add1 n)))
        (frons (! 8) (gamma (add1 n))))))

  (test "pcons1"
        (length (cons (fern (o)) (cons (fern (o)) '())))
        2)

  (test "pcons2"
        (fcaar (cons (fern (! 5)) (cons (fern (o)) '())))
        120)

  (test "pcons3"
        (fcaadr (cons (fern (o)) (cons (fern (! 5)) '())))
        120)

  (test "pcons4"
        (fcaaddr (cons (fern (o)) (cons (fern (! 10)) (cons (fern (! 5)) '()))))
        120)

  (test "dan's binary test 1"
        (take (merge (alpha 0) (gamma 0)) 20)
        (cons (! 5) (vector->list (make-vector 19 (! 8)))))

  (test "dan's binary test 2"
        (take (merge (beta 0) (gamma 0)) 20)
        '(5040 40320 40320 40320 40320 40320 40320 40320 40320 40320 40320 40320 40320 40320 40320 40320 40320 40320 40320 40320))

  (test "dan's test"
        (take (fappend (alpha 0) (beta 0) (gamma 0)) 20)
        '(120 5040 40320 5040 5040 40320 40320 40320 40320 40320 40320 40320 40320 40320 40320 40320 40320 40320 40320 40320))


(test "generalized car" skip
      (fcaaar (fern (fern (fern (o) (o) (o)))
                     (fern (fern (o) (o) (o)))
                     (fern (fern (o) (o) (o) (! 5)))))
      (! 5))
(test "will0" (fcar (fern (! 5) (! 6) (! 7) (! 8))) (! 5))
(test "will1" (fcar (fern (! 5) (! 6))) (! 5))
(test "will2" (fcar (fern (! 5))) (! 5))
(test "timed ground fern" (fcar (frons (! 10) (frons (! 3) 'end))) (! 3))
(test "cons in race" (fcar (frons (! 5) (cons 11 '()))) 11)
(print (fcdr (frons (! 6) (frons (! 3) '()))))
(test "cadr of above" (fcadr (frons (! 6) (frons (! 3) '()))) (! 6))
(test "cons at end" (fcar (frons (! 5) (frons (! 6) (frons (! 7) (cons 12 '()))))) 12)

(test "conj 1" (conj ls1) #f)

(test "mega conj" (conj ls) #f)

(test 
  "last car"
  (fcadddr
    (frons (! 2) (frons (! 3) (frons (! 4) (cons 11 (! 5))))))
  11)
(test "value in last cdr" 
  (fcddddr
    (frons (! 2) (frons (! 3) (frons (! 4) (cons 11 (! 5))))))
  (! 5))

(test "take bottom" (take ls 2) '(#t #f))

(test "dan's test" (fcar (frons (fcdr (frons 11 (bar 5000))) (frons (fcdr (frons 12 (bar 4000))) '()))) 0)

(let ([l (frons (! 5) (! 6))])
  (test "will's test" (fcar (frons (fcar l) (fcar l))) (! 5)))


(let ([l (fern (! 5) (o))])
  (test "pre-divergent" 
        (fcar (frons (fcar l)
                     (fcar (fcdr l)))) 120)
  (test "divergent"
        (fcar (frons (fcar l)
                     (fcar (fcar (frons (fcdr l) '()))))) 120))

(let ([l (fern (fern (! 5) (o)))])
  (test "pre-divergent wrapped" 
        (fcar (frons (fcar (fcar l))
                     (fcar (fcdr (fcar l))))) 120)
  (test "divergent wrapped"
        (fcar (frons (fcar (fcar l))
                     (fcar (fcar (frons (fcdr (fcar l)) '()))))) 120))

(test "append ground" (fcadr (fappend (fern 1 2) (fern 3 4))) 2)

(test "append ground factorials"
      (fcadr (fappend (fern (! 5) (! 6)) (fern (! 7) (! 8))))
      5040)

(test "append divergence" (fcar (fappend (fern (! -1) (! 5)) (fern (! 3) (! -1)))) (! 5))

(test "append meta-divergence"
      (fcar (fappend (fern (! -1) (! 5)) (! -5))) (! 5))

(test "divergence test #1" 
      (fcar (fappend (! -1) 
                     (fern (! -1) (! -1) (! -1) (! 5))
                     (! -1)
                     (fern (! -1) (! -1) (! 6))
                     (! -1)))
      (! 5))

(test "divergence test #2" 
      (fcadr (fappend (! -1)
                      (fern (! -1) (! -1) (! -1) (! 5))
                      (! -1)
                      (fern (! -1) (! -1) (! 6))
                      (! -1)))
      (! 6))

(test "fmap0" (fcar (fmap (lambda (x) (+ x 1)) (fern 1 2 3 4)))
      2)

(test "fmap1" (fcar (fmap (lambda (x) (+ x 1)) (fern (! 1) (! 2) (! 3) (! 4))))
      (+ (! 1) 1))

(test "fmap2" (fcar (fmap (lambda (x) (+ x 1)) (fern (! -1) (! 5))))
      121)

(test "nested fappend"
      (fcar (fappend (fappend (! -1) (fern (! 5)))
                     (! -1)))
      (! 5))

(test "joe's nested"
      (fcadr (fappend (fappend (! -1) (fern (! 5)))
                      (fern (! 5))))
      (! 5))

(test "book-test-1"
      (let ((x 5))
        (fcar (frons (cond ((< x 0) (! -1))
                           (else 1))
                     (frons (cond ((> x 0) (! -1))
                                  (else 1))
                            (frons (! -1) '())))))
      1)

(test "book-test-2"
      (take (frsuccessors 1) 5)
      '(1 2 3 4 5))

(test "nestme-1"
      (fcar (nestme 0 (lambda () (! 5))))
      120)

(test "nestme-2"
      (fcar (nestme 1 (lambda () (! 5))))
      120)

(test "nestme-3"
      (fcar (nestme 2 (lambda () (! 5))))
      120)

(test "nestme-4" skip
       (fcar (nestme 6 (lambda () (! 5))))
       120)

(define !!
  (timed-lambda (n x)
    (cond
      ((zero? x) (begin (write `(,n ,x)) (newline) 1))
      (else (begin (write `(,n ,x)) (newline) (* x (!! n (- x 1))))))))

(test "shared-engine"
  (let ((b (frons (!! 2 2) (!! 6 6))))
    (let ((a (frons (!! 5 5) b)))
      (cons (fcar a) (cons (fcdr b) (cons (fcddr a) '())))))
  '(2 720 720))

 ;repeats the work to get the 720.

;; (define omega
;;   (timed-lambda (i)
;;     (frons i (omega (+ i 1)))))

;; (test "testing lazy streams"
;;       (cardollar (omega 0))
;;       0)

