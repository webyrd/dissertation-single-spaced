(call-with-output-file "callsla.scm"
  (lambda (o)
    (write `(define call-slatex
              (let ((already-loaded? #f))
                (lambda (f)
                  (if (not already-loaded?)
                      (begin
                       (load ,(or *target-file*
                                  "slatex.scm"))
                       (set! already-loaded? #t)))
                  (slatex::process-main-tex-file f)
                  (display "Call (La)TeX on ")
                  (display f)
                  (display " now")
                  (newline)
                  )))
o)
(newline o)))

