(scmxlate-eval
(define system ##shell-command)
)

(scmxlate-include "make-qualified-names.scm")

(scmxlate-rename
 (system ##shell-command)
 )

(scmxlate-cond
 ((eqv? *operating-system* 'unix)
(define delete-file
  (lambda (f)
    (##shell-command (string-append "rm " f))))
))

(define append!
  (lambda (s1 s2)
    ;destructively append lists s1 and s2;
    ;only two args supported because that's
    ;enough for our purposes here
    (cond ((null? s1) s2)
          ((null? s2) s1)
          (else (let loop ((s1 s1))
                  (if (null? (cdr s1))
                      (set-cdr! s1 s2)
                      (loop (cdr s1))))
                s1))))

(define reverse!
  (lambda (s)
    ;reverses list s inplace (i.e., destructively)
    (let loop ((s s) (r '()))
      (if (null? s) r
          (let ((d (cdr s)))
            (set-cdr! s r)
            (loop d s))))))

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

(scmxlate-postprocess

(define *scheme-command-name* "gsi")

(load "dialects/make-echo-script.scm")
)
