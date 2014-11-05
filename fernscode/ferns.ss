(library (ferns)
  (export frons promote! timed-lambda expend-tick-to-call
    fcar    fcdr fcaar   fcadr   fcdar   fcddr 
    fcaaar  fcaadr  fcadar  fcaddr fcaaaar fcaaadr fcaadar fcaaddr 
    fcdaar  fcdadr  fcddar  fcdddr fcadaar fcadadr fcaddar fcadddr
    fcdaaar fcdaadr fcdadar fcdaddr fcddaar fcddadr fcdddar fcddddr
    fern fmap fappend merge fmapcan)

  (import (except (rnrs) _) (engines) (rename (pmatch) (? _))
    (only (ikarus) include printf)
    (rnrs mutable-pairs))

  (include "lockedorunhuh.ss")
  (include "replacebang.ss")
  (include "coaxskcdr.ss")
  (include "wait.ss")
  (include "frons.ss")
  (include "promote.ss")
  (include "cdr.ss")
  (include "car.ss")
  (include "cdrdollar.ss")
 ;(include "consdollar.ss")
 ;(include "listdollar.ss")

  (define-syntax fern
    (syntax-rules ()
      ((_) '())
      ((_ e e* ...) (frons e (fern e* ...)))))

  (define fmap
    (lambda (f . fl*)
      (cond
        [(for-each null? fl*) '()]
        [else (frons (apply f (map fcar fl*))
                (apply fmap f (map fcdr fl*)))])))

  (define apply-fappend
    (lambda (fl*)
      (if (null? fl*) '()
        (let ([fl (fcar fl*)])
          (cond
            [(null? fl) (apply-fappend (fcdr fl*))]
            [else
            (frons (fcar fl)
              (apply-fappend
                (frons (fcdr fl)
                  (fcdr fl*))))])))))

  (define-syntax fappend
    (syntax-rules ()
      [(_ e) e]
      [(_ e0 e* ...)
      (apply-fappend (fern e0 e* ...))]))

  (define apply-merge
    (lambda (l&m)
      (cond
        [(null? (fcar l&m)) (fcadr l&m)]
        [else (frons (fcaar l&m) 
                (merge (fcadr l&m) 
                  (fcdar l&m)))])))

  (define-syntax merge
    (syntax-rules ()
      [(_ l m) (apply-merge (fern l m))]))

  (define fmapcan
    (lambda (f fl)
      (cond
        [(null? fl) '()]
        [else (merge (f (fcar fl)) (fmapcan f (fcdr fl)))])))

  (define a fcar)
  (define d fcdr)
  (letrec-syntax
    ([define-fc*r
      (syntax-rules ()
        [(_ (fc*r a/d ...) ...)
        (begin (define fc*r (define-fc*r a/d ...)) ...)]
        [(_ a/d) (lambda (p) (a/d p))]
        [(_ a/d* ... a/d)
        (lambda (p) ((define-fc*r a/d* ...) (a/d p)))])])
    (define-fc*r
      (fcaar a a) (fcaaar a a a) (fcaaaar a a a a)
      (fcadr a d) (fcaadr a a d) (fcaaadr a a a d)
      (fcdar d a) (fcadar a d a) (fcaadar a a d a)
      (fcddr d d) (fcaddr a d d) (fcaaddr a a d d)
      (fcdaar d a a) (fcadaar a d a a) (fcdadr d a d) (fcadadr a d a d)
      (fcddar d d a) (fcaddar a d d a) (fcdddr d d d) (fcadddr a d d d)
      (fcdaaar d a a a) (fcdaadr d a a d) (fcdadar d a d a) (fcdaddr d a d d)
      (fcddaar d d a a) (fcddadr d d a d) (fcdddar d d d a) (fcddddr d d d d))))
