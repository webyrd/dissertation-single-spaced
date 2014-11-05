(define fcar
  (timed-lambda (p)
    (letrec ((race-car
               (timed-lambda (q)
                 (cond
                   ((engine-tag-L-car q) (wait nsteps) (race-cdr q))
                   ((engine-tag-U-car q) (coax-a q) (race-cdr q))
                   ((not (pair? q)) (race-car p))
                   (else (promote! p) (car p)))))
             (race-cdr
               (timed-lambda (q)
                 (cond
                   ((engine-tag-L-cdr q) (race-car p))
                   ((engine-tag-U-cdr q) (coax-d q) (race-car p))
                   (else (race-car (cdr q)))))))
      (race-car p))))

