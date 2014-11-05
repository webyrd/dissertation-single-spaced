(library (pmatch)
         (export pmatch ?)
         (import (rnrs) (only (ikarus) include))
         
(define-syntax ? (identifier-syntax ?))

(include "pmatch-appendix.ss")
)
