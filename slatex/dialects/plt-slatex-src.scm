(scmxlate-insert
 "(module slatex mzscheme
  (provide slatex::process-main-tex-file)
" ;)
 )

(scmxlate-include "make-qualified-names.scm")

(define-syntax setf
  (lambda (so)
    (datum->syntax-object so
      (let ((so-d (syntax-object->datum so)))
        (let ((l (cadr so-d))
              (r (caddr so-d)))
          (if (symbol? l) `(set! ,l ,r)
              (let ((a (car l)))
                (if (eq? a 'list-ref)
                    `(set-car! (list-tail ,@(cdr l)) ,r)
                    `(,(cond ((eq? a 'string-ref) 'string-set!)
                             ((eq? a 'vector-ref) 'vector-set!)
                             ((eq? a 'of) 'the-setter-for-of)
                             (else
                              (error "setf ~s ~s is ill-formed~%" l r)))
                      ,@(cdr l) ,r)))))))))

(scmxlate-postamble)

(scmxlate-insert  
 ;(
 ")
"
 )

(scmxlate-cond
((eqv? *operating-system* 'unix)
  (scmxlate-postprocess
(define *scheme-command-name* "mzscheme")

(system "mv my-slatex-src.scm slatex.ss")

(printf "Generated module file slatex.ss~%")

(system "rm -f slatex callsla.scm")

(when *target-file*
  (let ((n (string-length *target-file*)))
    (when (and (>= n 4) (string=? (substring *target-file* (- n 4) n) ".scm"))
      (set! *target-file*
        (string-append (substring *target-file* 0 (- n 4)) ".ss")))))

(call-with-output-file "slatex"
  (lambda (o)
    (display "echo '" o) (newline o)
    (display "(require (file " o)
    (write (or *target-file* "slatex.ss") o)
    (display "))" o) (newline o)
    (display ";check pathname above is correct for you" o)
    (newline o)
    (display "(slatex::process-main-tex-file \"'$1'\")" o)
    (if (eqv? *dialect* 'scsh)
        (begin
         (newline o)
         (display "(exit)" o)))
    (display "' | " o)
    (display *scheme-command-name* o)
    (newline o)
    (display "if test -f pltexchk.jnk" o) (newline o)
    (display "then tex $1; rm pltexchk.jnk" o) (newline o)
    (display "else latex $1" o) (newline o)
    (display "fi" o) (newline o)))

(system "chmod +x slatex")

)))
