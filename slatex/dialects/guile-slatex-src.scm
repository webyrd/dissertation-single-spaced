(scmxlate-include "make-qualified-names.scm")

(define void
  (lambda ()
    (if #f #f)))

(define-macro fluid-let
  (lambda (xvxv . ee)
    (let ((xx (map car xvxv))
          (vv (map cadr xvxv))
          (old-xx (map (lambda (xv)
                         (string->symbol
                          (string-append "%__"
                                         (symbol->string (car xv))))) xvxv))
          (res '%_*_res))
      `(let ,(map (lambda (old-x x) `(,old-x ,x)) old-xx xx)
         ,@(map (lambda (x v)
                  `(set! ,x ,v)) xx vv)
         (let ((,res (begin ,@ee)))
           ,@(map (lambda (x old-x) `(set! ,x ,old-x)) xx old-xx)
           ,res)))))

(scmxlate-cond
 ((eqv? *operating-system* 'unix)
  (scmxlate-postprocess
   (define *scheme-command-name* "guile")
   (load "dialects/make-echo-script.scm"))))
