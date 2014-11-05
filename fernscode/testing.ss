(library (testing)
  (export print test print-compiling skipped-tests)
  (import (rnrs) (only (ikarus) printf))

  (define skipped-tests
    (let ((ls '()))
      (case-lambda
        (() ls)
        ((t) (set! ls (cons t ls))))))

  (define-syntax print-compiling
    (syntax-rules ()
      [(_ name) (printf "Compiling ~a tests...\n" name)]))

  (define-syntax print
    (syntax-rules ()
      [(_ e) (printf "~a -> ~a\n" 'e e)]))

  (define-syntax test
    (syntax-rules (skip)
      [(_ title expression expected-result)
      (begin
        (printf "Testing ~a..." title)
        (let ((expected expected-result)
              (computed expression))
          (if (equal? expected computed)
            (printf " done\n")
            (error 'title "fail" 'expression expected computed))))]
      [(_ title skip _ _) (begin (printf "WARNING: SKIPPING ~a\n" title) (skipped-tests title))])))
