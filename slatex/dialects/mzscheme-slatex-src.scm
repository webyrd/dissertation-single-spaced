(scmxlate-include "make-qualified-names.scm")

(scmxlate-cond
 ((eqv? *operating-system* 'unix)
  (scmxlate-postprocess
   (define *scheme-command-name* "mzscheme")
   (load "dialects/make-echo-script.scm")
   )))
