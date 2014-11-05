(define-syntax exist*
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambdag@ (a)
       (inc
         (let* ((x (var 'x)) ...)
           (bind* (g0 a) g ...)))))))

(define-syntax fresh*
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambdag@ (a)
       (inc
         (let* ((x (nom 'x)) ...)
           (bind* (g0 a) g ...)))))))

(define-syntax lambdae
  (syntax-rules ()
    ((_ (x ...) c c* ...)
     (lambda (x ...) (lame conde (x ...) (c c* ...) ())))))

(define-syntax lambdaa
  (syntax-rules ()
    ((_ (x ...) c c* ...)
     (lambda (x ...) (lame conda (x ...) (c c* ...) ())))))

(define-syntax lambdau
  (syntax-rules ()
    ((_ (x ...) c c* ...)
     (lambda (x ...) (lame condu (x ...) (c c* ...) ())))))

(define-syntax lame
  (syntax-rules (__ quote unquote unquote-splicing)
    ((_ co a () (l ...)) (co l ...))
    ((_ co a ((p . g*) . c*) l*)
     (lame co a p a (g* . c*) l*))
    ((_ co a () () (g* . c*) l*)
     (lame co a c* (g* . l*)))
    ((_ co a __ y* (g* . c*) l*)
     (lame co a () () (g* . c*) l*))
    ((_ co a (unquote x) (y ...) (g* . c*) l*)
     (lame co a () () (((exist (x) (== x `((unquote y) ...)) . g*)) . c*) l*))
    ((_ co a (quote x) (y ...) (g* . c*) l*)
     (lame co a () () (((==  (quote x) `((unquote y) ...)) . g*) . c*) l*))
    ((_ co a (__ . z) (y . y*) (g* . c*) l*)
     (lame co a z y* (g* . c*) l*))
    ((_ co a ((unquote x) . z) (y . y*) (g* . c*) l*)
     (lame co a z y* (((exist (x) (== x y) . g*)) . c*) l*))
    ((_ co a ((unquote-splicing x) . z) (y . y*) (g* . c*) l*)
     (lame co a z y* (((fresh (x) (== x y) . g*)) . c*) l*))
    ((_ co a ((quote x) . z) (y . y*) (g* . c*) l*)
     (lame co a z y* (((== (quote x) y) . g*) . c*) l*))
    ((_ co a (x . z) (y . y*) (g* . c*) l*)
     (lame co a z y* (((== x y) . g*) . c*) l*))
    ((_ co a x (y ...) (g* . c*) l*)
     (lame co a () () (((== x `((unquote y) ...)) . g*) . c*) l*))))

(define-syntax matche
  (syntax-rules ()
    ((_ (f x ...) g* . cs)
     (let ((v (f x ...))) (matche v g* . cs)))
    ((_ v g* . cs) (matc conde v (g* . cs) ()))))

(define-syntax matcha
  (syntax-rules ()
    ((_ (f x ...) g* . cs)
     (let ((v (f x ...))) (matcha v g* . cs)))
    ((_ v g* . cs) (matc conda v (g* . cs) ()))))

(define-syntax matchu
  (syntax-rules ()
    ((_ (f x ...) g* . cs)
     (let ((v (f x ...))) (matchu v g* . cs)))
    ((_ v g* . cs) (matc condu v (g* . cs) ()))))

(define-syntax matc
  (syntax-rules (__ quote unquote unquote-splicing expand cons)
    ((_ co v () (l ...)) (co l ...))
    ((_ co v (pat) xs as ((g ...) . cs) (l ...))
     (matc co v cs (l ... ((fresh* as (exist* xs (== `pat v) g ...))))))
    ((_ co v ((__ g0 g ...) . cs) (l ...))
     (matc co v cs (l ... ((exist () g0 g ...)))))
    ((_ co v (((unquote y) g0 g ...) . cs) (l ...))
     (matc co v cs (l ... ((exist (y) (== y v) g0 g ...)))))
    ((_ co v (((unquote-splicing b) g0 g ...) . cs) (l ...))
     (matc co v cs (l ... ((fresh (b) g0 g ...)))))
    ((_ co v ((pat g ...) . cs) ls)
     (matc co v (pat expand) () () ((g ...) . cs) ls))
    ((_ co v (__ expand . k) (x ...) as cs ls)
     (matc co v ((unquote y) . k) (y x ...) as cs ls))
    ((_ co v ((unquote y) expand . k) (x ...) as cs ls)
     (matc co v ((unquote y) . k) (y x ...) as cs ls))
    ((_ co v ((unquote-splicing b) expand . k) xs (a ...) cs ls)
     (matc co v ((unquote b) . k) xs (b a ...) cs ls))
    ((_ co v ((quote c) expand . k) xs as cs ls)
     (matc co v (c . k) xs as cs ls))
    ((_ co v ((a . d) expand . k) xs as cs ls)
     (matc co v (d expand a expand cons . k) xs as cs ls))
    ((_ co v (d a expand cons . k) xs as cs ls)
     (matc co v (a expand d cons . k) xs as cs ls))
    ((_ co v (a d cons . k) xs as cs ls)
     (matc co v ((a . d) . k) xs as cs ls))
    ((_ co v (c expand . k) xs as cs ls)
     (matc co v (c . k) xs as cs ls))))

;; > (run 2 (q) (matche `(,q ,q ,q) 
;;                ((,j ,j ,j) (== 3 j))
;;                ((,a ,a ,a) (== a q))))
;; (_.0 3)
;; > (run 1 (q) ((lambdae (k1 k2 k3) ((,j ,j ,j) (== 3 j))) q q q))
;; (3)
