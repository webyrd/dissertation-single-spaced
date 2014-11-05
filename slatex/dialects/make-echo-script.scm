(case *dialect*
  ((scsh)
   (rename-file "my-slatex-src.scm" "slatex.scm")
   (delete-file "slatex")
   (delete-file "callsla.scm"))
  (else
   (system "mv my-slatex-src.scm slatex.scm")
   (system "rm -f slatex callsla.scm")))


(load "dialects/make-callsla.scm")

(call-with-output-file "slatex"
  (lambda (o)
    (display "echo '" o) (newline o)
    (display "(load " o)
    (write (or *target-file* "slatex.scm") o)
    (display ")" o) (newline o)
    (display ";check pathname above is correct for you" o)
    (newline o)
    (display "(slatex" o)
    (display (if (eqv? *dialect* 'bigloo)
                 "$$"
                 "::") o)
    (display "process-main-tex-file \"'$1'\")" o)
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

(case *dialect*
  ((scsh) (run (chmod "+x" slatex)))
  (else (system "chmod +x slatex")))

