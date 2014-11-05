(scmxlate-include "make-qualified-names.scm")

(scmxlate-rename
 (flush-output flush-output-port)
 )

(scmxlate-postprocess

(define *scheme-command-name* 
  (if (eqv? *dialect* 'chez) "chez"
      "petite"))

(load "dialects/make-echo-script.scm")
)
