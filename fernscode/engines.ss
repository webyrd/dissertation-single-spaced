(library (engines)
  (export engine make-engine timed-lambda expend-tick-to-call nsteps)
  (import (rnrs) (rnrs mutable-pairs) (only (ikarus) include))

  (include "replacebang.ss")
  (include "coaxappendix.ss")
  (include "appendix-extra.ss")

  (define nsteps 1))
