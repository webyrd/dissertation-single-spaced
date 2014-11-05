;;;; This macro was designed by Will Byrd and implemented by Ramana Kumar.

(define-syntax ulambda
  (syntax-rules ()
    ((_ clause0 clause ...)
     (lambda args (umatch args clause0 clause ...)))))

;;; When you read this code, remember that (,y . ,k) is push y onto the
;;; stack k if in the rhs and pop y from stack if on the lhs.  So, the
;;; fifth clause can be read: drop the next to the top element from k
;;; and cons the top element, y, onto the list of variable (x ...).

(define-syntax exist*
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambdag@ (a)
       (inc
         (let* ((x (var 'x)) ...)
           (bind* (g0 a) g ...)))))))

(define-syntax umatch
  (syntax-rules ()
    ((_ (f x ...) g* . cs) (let ((v (f x ...))) (umatch v g* . cs)))
    ((_ v g* . cs) (umatch-aux v (g* . cs) ()))))

(define-syntax umatch-aux
  (syntax-rules (__ quote unquote expand cons)
    ((_ v () (l ...)) (conde l ...))
    ((_ v (pat) xs ((g ...) . cs) (l ...))
     (umatch-aux v cs (l ... ((exist* xs (== `pat v) g ...)))))
    ((_ v ((pat g ...) . cs) ls)
     (umatch-aux v (pat expand) () ((g ...) . cs) ls))
    ((_ v (__ expand . k) (x ...) cs ls)
     (umatch-aux v (,y . k) (y x ...) cs ls))
    ((_ v (,y expand . k) (x ...) cs ls)
     (umatch-aux v (,y . k) (y x ...) cs ls))
    ((_ v ('c expand . k) xs cs ls)
     (umatch-aux v (c . k) xs cs ls))
    ((_ v ((a . d) expand . k) xs cs ls)
     (umatch-aux v (d expand a expand cons . k) xs cs ls))
    ((_ v (d a expand cons . k) xs cs ls)
     (umatch-aux v (a expand d cons . k) xs cs ls))
    ((_ v (a d cons . k) xs cs ls)
     (umatch-aux v ((a . d) . k) xs cs ls))
    ((_ v (c expand . k) xs cs ls)
     (umatch-aux v (c . k) xs cs ls))))

(define appendo/old
  (lambda (l s out)
    (conde
      ((== '() l) (== s out))
      ((exist (a d res)
         (== `(,a . ,d) l)
         (== `(,a . ,res) out)
         (appendo/old d s res))))))

(pretty-print
  (run 20 (q) (exist (x y z) (appendo/old x y z) (== `(,x ,y ,z) q))))

(define appendo
  (ulambda
    [(() ,s ,s)]
    [((,a . ,d) ,s (,a . ,res)) (appendo d s res)]))

(pretty-print
  (run 20 (q) (exist (x y z) (appendo x y z) (== `(,x ,y ,z) q))))

(define foo
  (ulambda
    [()]
    [((,x . ,y))]))

(define foo
  (ulambda
    [()]
    [((x . y))]))

(define foo
  (ulambda
    [(())]
    [((x . y))]))

(define foo
  (ulambda
    [(())]
    [((,x . ,y))]))
