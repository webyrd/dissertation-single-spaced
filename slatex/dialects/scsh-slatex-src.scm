(scmxlate-include "make-qualified-names.scm")

(define *int-corresp-to-nul*
 (- (char->integer #\a) 97)) 

(define scsh-int-to-char
  (lambda (n)
    (integer->char
     (+ n *int-corresp-to-nul*))))

(define scsh-char-to-int
  (lambda (c)
    (- (char->integer c)
       *int-corresp-to-nul*)))

(scmxlate-rename
 (integer->char scsh-int-to-char)
 (char->integer scsh-char-to-int))


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

(define void
  (lambda () 
    (if #f #f)))

(define flush-output force-output)

(scmxlate-cond 
 ((eqv? *operating-system* 'unix)
  (scmxlate-postprocess
(define *scheme-command-name* "scsh")

(load "dialects/make-echo-script.scm")
)))

