(define-syntax fern
  (syntax-rules ()
    ((_) '())
    ((_ e e* ...) (frons e (fern e* ...)))))