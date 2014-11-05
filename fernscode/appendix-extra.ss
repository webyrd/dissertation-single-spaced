(define state (cons #f 0))

(define expend-tick-to-call
  (lambda (thunk)
    ((call/cc
       (lambda (k)
         (let th  ()
           (cond
             ((not (car state)) (k thunk))
             ((zero? (car state)) ((cdr state) th))
             (else (set-car! state (- (car state) 1)) (k thunk)))))))))

(define make-engine
  (lambda (thunk)
    (lambda (ticks)
      (let* ((gift (if (car state) (min (car state) ticks) ticks))
             (saved-state (cons (and (car state) (- (car state) gift)) (cdr state)))
             (caught (call/cc
                       (lambda (k)
                         (replace! state gift k)
                         (let ((result (thunk)))
                           ((cdr state) (cons (car state) result)))))))
        (replace! state (car saved-state) (cdr saved-state))
        (let ((owed (- ticks gift)))
          (cond
            ((pair? caught)
             (and (car state) (set-car! state (+ (car state) (car caught))))
             (cons (+ (car caught) owed) (cdr caught)))
            (else (let ((e (make-engine caught)))
                    (if (zero? owed) (cons #f e)
                      (let ((th (lambda () (e owed))))
                        ((call/cc (lambda (k^) ((cdr state) (lambda () (k^ th))))))))))))))))

