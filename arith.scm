(load "mk.scm")
(load "umatch.scm")

(define mulo
  (lambda (x y z)
    (exist (a b c)
      (geno a)
      (== x a)
      (geno b)
      (== y b)
      (geno c)
      (== z c)
      (project (a b)
        (== (* a b) c)))))

(define genuo
  (ulambda
    [(())]
    [((u . ,x)) (genuo x)]))

(define addo
  (ulambda
    [(() ,x ,x)]
    [((u . ,x) ,y (u . ,z)) (addo x y z)]))

(define mfo
  (ulambda
    [(() (u . ,y)) (genuo y)]
    [((u . ,x) ,y) (mfo `(u . ,x) y)]))

(define mul1o
  (ulambda
    [(() __ ())]
    [((u . ,x) ,y ,z)
     (exist (z1)
       (mul1o x y z1)
       (addo z1 y z))]))

(define mul2o
  (ulambda
    [(() __ ())]
    [((u . ,x) ,y ,z)
     (exist (z1)
       (addo z1 y z)
       (mul2o x y z1))]))

(define mul3o
  (ulambda
    [(() __ ())]
    [((u . ,x) ,y ,z)
     (exist (z1)
       (addo y z1 z)
       (mul3o x y z1))]))

(define mulo
  (ulambda
    [(() __ ())]
    [((u . __) () ())]
    [((u . ,x) (u . ,y) ,z)
     (exist (z1)
       (addo `(u . ,y) z1 z)
       (mulo x `(u . ,y) z1))]))

(define lesso
  (ulambda
    [(() (__ . __))]
    [((u . ,x) (u . ,y)) (lesso x y)]))

(define semimulo
  (ulambda
    [(() __ ())]
    [((u . __) () ())]
    [((u . ,x) (u . ,y) ,z) (lesso x z) (lesso y `(u . ,x))
     (mulo `(u . ,x) `(u . ,y) z)]))

(define zeroo
  (ulambda
    [(())]))

(define poso
  (ulambda
    [((__ . __))]))

(define gtlo
  (ulambda
    [((__ __ . __))]))

(define genbo
  (ulambda
    [(())]
    [((l . ,x)) (genbo x)]
    [((o . ,x)) (poso x) (genbo x)]))

(define lesslo
  (ulambda
    [(() (__ . __))]
    [((__ . ,x) (__ . ,y)) (lesslo x y)]))

(define full1-addero
  (ulambda
    [(o o o o o)]
    [(o o l l o)]
    [(o l o l o)]
    [(o l l o l)]
    [(l o o l o)]
    [(l o l o l)]
    [(l l o o l)]
    [(l l l l l)]))

(define fulln-addero
  (ulambda
    [(o ,a () ,a)]
    [(o () ,b ,b) (poso b)]
    [(l ,a () ,r) (fulln-addero 'o a '(l) r)]
    [(l () ,b ,r) (poso b) (fulln-addero 'o '(l) b r)]
    [(,cin (l) (l) (,r1 ,r2)) (full1-addero cin 'l 'l r1 r2)]
    [(,cin (l) (,ba . ,bd) (,ra . ,rd)) (poso bd) (poso rd)
     (exist (cout)
       (full1-addero cin 'l ba ra cout)
       (fulln-addero cout '() bd rd))]
    [(,cin ,a (l) ,r) (gtlo a) (gtlo r)
     (fulln-addero cin '(l) a r)]
    [(,cin (,aa . ,ad) (,ba . ,bd) (,ra . ,rd))
     (poso ad) (poso bd) (poso rd)
     (exist (cout)
       (full1-addero cin aa ba ra cout)
       (fulln-addero cout ad bd rd))]))

(define addo
  (lambda (a b c)
    (fulln-addero 'o a b c)))

(define subo
  (lambda (a b c)
    (addo b c a)))

(define lesso
  (lambda (a b)
    (exist (x)
      (poso x)
      (addo a x b))))

(define mulo
  (ulambda
    [(() ,m ())]
    [(,n () ()) (poso n)]
    [((l) ,m ,m) (poso m)]
    [((o . ,nd) ,m (o . ,pd)) (poso m) (poso nd) (poso pd) (mulo nd m pd)]
    [((l . ,nd) ,m ,p) (poso m) (poso nd) (gtlo p)
     (exist (p1)
       (lessl3o p1 p `(l . ,nd) m)
       (mulo nd m p1)
       (addo `(o . ,p1) m p))]))

(define lessl3o
  (ulambda
    [(() (__ . __) __ __)]
    [((__ . ,p1d) (__ . ,pd) () (__ . ,mr)) (lessl3o p1d pd '() mr)]
    [((__ . ,p1d) (__ . ,pd) (__ . ,nd) m) (lessl3o p1d pd nd m)]))

(define samelo
  (ulambda
    [(() ())]
    [((__ . ,x) (__ . ,y)) (samelo x y)]))

 (define divo
  (ulambda
    [(,n ,m () ,r) (== n r) (lesso n m)]
    [(,n ,m (l) ,r) (samelo n m) (addo r m n) (lesso r m)]
    [(,n ,m ,q ,r) (lesslo m n) (lesso r m) (poso q)
     (exist (n1 n2 q1 q2 q2m q2md r1 rd)       
       (splito n r n1 n2)
       (splito q r q1 q2)
       (conde
         ((== '() n1)
          (== '() q1)
          (subo n2 r q2m)
          (mulo q2 m q2m))
         ((poso n1)
          (mulo q2 m q2m)
          (addo q2m r q2md)
          (subo q2md n2 rd)
          (splito rd r r1 '())
          (divo n1 m q1 r1))))]))

(define splito
  (ulambda
    [(() __ () ())]
    [((o ,b . ,n) () (,b . ,n) ())]
    [((l . ,n) () ,n (l))]
    [((o ,b . ,n) (__ . ,r) ,n1 ()) (splito `(,b . ,n) r n1 '())]
    [((l . ,n) (__ . ,r) ,n1 (l)) (splito n r n1 '())]
    [((,b . ,n) (__ . ,r) ,n1 (,b . ,n2)) (poso n2) (splito n r n1 n2)]))
