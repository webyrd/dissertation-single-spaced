;;;  Very simple inferencer from my defense:

(define typo
  (lambda (g e te)
    (conde
      ((exists (x)
         (== `(var ,x) e)
         (lookupo x te g)))
      ((exists (rator trator rand trand)
         (== `(app ,rator ,rand) e)
         (== `(-> ,trand ,te) trator)
         (typo g rator trator)
         (typo g rand trand)))
      ((exists (e^ te^ trand g^)
         (fresh (b)
           (== `(lam ,(tie b e^)) e)
           (== `(-> ,trand ,te^) te)
           (hash b g)
           (== `((,b . ,trand) . ,g) g^)
           (typo g^ e^ te^)))))))

(define lookupo
  (lambda (x tx g)
    (exists (a d)
      (== `(,a . ,d) g)
      (conde
        ((== `(,x . ,tx) a))
        ((exists (x^ tx^)
           (== `(,x^ . ,tx^) a)
           (hash x x^)
           (lookupo x tx d)))))))

;;; (lambda (c) (lambda (d) c))
(run* (q)
  (fresh (c d)
    (typo '() `(lam ,(tie c `(lam ,(tie d `(var ,c))))) q)))

;;; (lambda (c) (c c))
(run* (q)
  (fresh (c)
    (typo '() `(lam ,(tie c `(app (var ,c) (var ,c)))) q)))

;;; (lambda (b) b)
;;; (lambda (b) ((lambda (c) c) b))
;;; ((lambda (b) b) (lambda (c) c))
(run 3 (q) (typo '() q '(-> int int)))


;;;