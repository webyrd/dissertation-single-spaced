
(scmxlate-include "make-qualified-names.scm")

(scmxlate-rename
 (delete bigloo-delete)
 )

(define-macro (fluid-let xvxv . ee)
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
	 ,res))))


(define flush-output
  (lambda z
    ;bigloo's flush-output-port always takes 1 arg
    (flush-output-port
      (if (null? z) (current-output-port) (car z)))))

(define void
  (lambda ()
    (if #f #f)))

(scmxlate-postprocess

(define *scheme-command-name* "bigloo")

(load "dialects/make-echo-script.scm")
)

