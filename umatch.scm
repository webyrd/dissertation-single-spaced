;;;; This macro was designed by Will Byrd and implemented by Ramana Kumar.

(define-syntax ulambda
  (syntax-rules ()
    ((_ (a ...) clause0 clause ...)
     (lambda (a ...) (umatch `(,a ...) clause0 clause ...)))))

;;; When you read this code, remember that (,y . ,k) is push y onto the
;;; stack k if in the rhs and pop y from stack if on the lhs.  So, the
;;; fifth clause can be read: drop the next to the top element from k
;;; and push the top element, y, onto the list of variable (x ...).

(define-syntax exist*
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambdag@ (a)
       (inc
         (let* ((x (var 'x)) ...)
           (bind* (g0 a) g ...)))))))

(define-syntax fresh*
  (syntax-rules ()
    ((_ (b ...) g0 g ...)
     (lambdag@ (a)
       (inc
         (let* ((b (nom 'b)) ...)
           (bind* (g0 a) g ...)))))))

(define-syntax umatch
  (syntax-rules ()
    ((_ (f x ...) g* . cs) (let ((v (f x ...))) (umatch v g* . cs)))
    ((_ v g* . cs) (umatch-aux v (g* . cs) ()))))

(define-syntax umatch-aux
  (syntax-rules (__ quote unquote unquote-splicing expand cons)
    ((_ v () (l ...)) (conde l ...))
    ((_ v (pat) xs as ((g ...) . cs) (l ...))
     (umatch-aux v cs (l ... ((fresh* as (exist* xs (== `pat v) g ...))))))
    ((_ v ((__ g0 g ...) . cs) (l ...))
     (umatch-aux v cs (l ... ((exist () g0 g ...)))))
    ((_ v ((pat g ...) . cs) ls)
     (umatch-aux v (pat expand) () () ((g ...) . cs) ls))
    ((_ v (__ expand . k) (x ...) as cs ls)
     (umatch-aux v (,y . k) (y x ...) as cs ls))
    ((_ v (,y expand . k) (x ...) as cs ls)
     (umatch-aux v (,y . k) (y x ...) as cs ls))
    ((_ v (,@b expand . k) xs (a ...) cs ls)
     (umatch-aux v (,b . k) xs (,b a ...) cs ls))    
    ((_ v ('c expand . k) xs as cs ls)
     (umatch-aux v (c . k) xs as cs ls))
    ((_ v ((a . d) expand . k) xs as cs ls)
     (umatch-aux v (d expand a expand cons . k) xs as cs ls))
    ((_ v (d a expand cons . k) xs as cs ls)
     (umatch-aux v (a expand d cons . k) xs as cs ls))
    ((_ v (a d cons . k) xs as cs ls)
     (umatch-aux v ((a . d) . k) xs as cs ls))
    ((_ v (c expand . k) xs as cs ls)
     (umatch-aux v (c . k) xs as cs ls))))

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
  (ulambda (l s out)
    [(() ,s ,s)]
    [((,a . ,d) ,s (,a . ,res)) (appendo d s res)]))

(pretty-print
  (run 20 (q) (exist (x y z) (appendo x y z) (== `(,x ,y ,z) q))))

(pretty-print
  (run 1 (q)
    (umatch '(a b c)
      (__ (== 5 q)))))
