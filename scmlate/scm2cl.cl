":";exec clisp -norc -q -x '(progn (setq *load-verbose* nil) (defvar scheme-file "'$1'") (load "'$0'") (exit))'
":";exec lisp -e '(defvar scheme-file "'$1'")' -e '(load "'$0'")' -e '(exit)'

;scm2cl
;converts Scheme code to Common Lisp
;resulting code _must_ be spot-checked
;Dorai Sitaram
;April 26, 1997

;last change 2009-04-10

;Shuffle the ":" lines above so that the one relevant to
;your Common Lisp dialect is the first line.  This will let
;you use this file as a Unix shellscript.

;The line containing clisp is for CLISP.
;The other line is for Allegro Common Lisp.

;You can create your own ":" line for your CL dialect
;modeled on the above.  Even if you're using the abovementioned
;dialects, the Lisp executable name may be different in
;your case -- change per need.

(defvar *scm2cl-lisp-extension* "cl")

(setq *print-case* :downcase)
(setq *load-verbose* nil)

;read #t and #f as t and nil resply

(set-dispatch-macro-character #\# #\T
                              #'(lambda (p ig ig2)
                                  (declare (ignore p ig ig2))
                                  t))

(set-dispatch-macro-character #\# #\F
                              #'(lambda (p ig ig2)
                                  (declare (ignore p ig ig2))
                                  nil))

;brackets are like parens

(defun read-bracketed-sexp (i c)
  (declare (ignore c))
  (read-delimited-list #\] i t))

(set-macro-character #\] (get-macro-character #\) nil))
(set-macro-character #\[ #'read-bracketed-sexp nil)

(defvar *in-scmxlate* 
  ;this would already be t if this file is
  ;being loaded by scmxlate
  nil)

(unless *in-scmxlate*
  (defvar *scmxlate-version*
    (with-open-file (i (merge-pathnames
                        "scmxlate"
                        (merge-pathnames
                         (make-pathname :type "scm")
                         *load-pathname*)))
      (loop
       (let ((x (read i nil :eof-object)))
         (when (eql x :eof-object) (return))
         (when (and (consp x)
                    (= (length x) 3)
                    (eql (car x) 'define)
                    (eql (cadr x) '*scmxlate-version*))
           (return (caddr x))))))))

(load (merge-pathnames "clnames" *load-pathname*))

(load (merge-pathnames "clmacros" *load-pathname*))

(load (merge-pathnames "clprocs" *load-pathname*))

(defvar *scm2cl-procs-used* '())

(defvar *scm2cl-bound-vars* '())

(defvar *aliases* *predefined-aliases*) 
(defvar *local-procedures* '())

(defun scheme-to-cl (fi &optional fo)
  ;create cl version of scheme file fi in fo
  (format t "~&Translating ~a ...~%" fi)
  (setq *aliases* *predefined-aliases*)
  (unless fo
    (setq fo (merge-pathnames (make-pathname :type *scm2cl-lisp-extension*)
                              fi)))
  ;
  (with-open-file (i fi :direction :input)
    ;discard first line if it contains #
    (let ((c (peek-char nil i nil :eof-object)))
      (unless (eq c :eof-object)
        (if (char= c #\#) (read-line i nil :eof-object))))
    (with-open-file (o fo :direction :output :if-exists :supersede)
      ;(pprint '(load "clprocs.cl") o)
      (format o "~&;Generated from Scheme source by scm2cl (scmxlate v ~a)~%" *scmxlate-version*)
      (format o ";(c) Dorai Sitaram~%")
      (format o ";    http://www.ccs.neu.edu/~~dorai/scmxlate/scmxlate.html~%")
      (let ((*scm2cl-procs-used* '()))
        (loop
         (let ((x (read i nil :eof-object)))
           (if (eq x :eof-object) (return))
           (let* ((*scm2cl-bound-vars* '())
                  (xm (scm2cl-sexp
                       (nsublis *aliases* x))))
             (cond ((not xm) nil)
                   ((and (consp xm) (eq (car xm) 'progn))
                    (dolist (y (cdr xm))
                      (if y (pprint y o))))
                   (t (pprint xm o)))
             (terpri o))))
        (when  *scm2cl-procs-used*
          (format t "~%~
            The following Scheme procedures in clprocs.cl were used~%")
          (#+abcl print #-abcl pprint *scm2cl-procs-used*)))))
  ;
  (format t "~&Translation written on ~a~%" fo) 
  )

(defun scm2cl-map (f x)
  (if (null x) '()
      (if (consp x)
          (cons (funcall f (car x)) (scm2cl-map f (cdr x)))
          (funcall f x))))

(defun scm2cl-id (e)
  (cond ((consp e)
         (let ((a (car e)))
           (case a
             ((function) (cadr e))
             (t (format t "~&Funny id ~s~%" e)
                e))))
        (t e)))

(defun scm2cl-ids (e)
  (cond ((consp e)
         (let ((a (car e)))
           (case a
             ((function) (cadr e))
             (t (mapcar #'scm2cl-id e)))))
        (t e)))

(defun scm2cl-sexp-insert-funcall (e)
  (scm2cl-sexp e t))

(defun scm2cl-sexp (e &optional insert-funcall-p)
  ;transform scheme sexp e into its cl counterpart
  (cond ((consp e)
         (let ((a (car e)))
           (if (consp a)
               (let ((aa (car a)))
                 (case aa
                   ((function)
                    (let ((f (cadr a)))
                      (if (member f *scm2cl-bound-vars*)
                          (cons 'funcall
                                (cons f
                                      (mapcar #'scm2cl-sexp-insert-funcall
                                              (cdr e))))
                        (let ((xfmr (gethash f *scm2cl-macros*)))
                          (if xfmr
                              (scm2cl-sexp (apply xfmr (cdr e)))
                            (cons f
                                  (mapcar #'scm2cl-sexp-insert-funcall
                                          (cdr e))))))))
                   ((scm2cl-lambda)
                    (cons
                     (scm2cl-sexp
                      (cons 'scm2cl-functionless-lambda (cdr a)))
                     (mapcar #'scm2cl-sexp-insert-funcall (cdr e))))
                   ((case cond if when unless)
                    (cons 'funcall
                          (mapcar #'scm2cl-sexp-insert-funcall
                                  e)))
                   (t
                    (if insert-funcall-p
                        (cons 'funcall
                              (mapcar #'scm2cl-sexp-insert-funcall e))
                      (progn
                       (unless (eq aa 'scm2cl-lambda)
                         (format t "~&Possible funcall at ~s~%" e))
                       (scm2cl-map #'scm2cl-sexp-insert-funcall e))))))
             (let ((xfmr (gethash a *scm2cl-macros*)))
               (if xfmr (scm2cl-sexp (apply xfmr (cdr e)))
                 (cond ((eq a 'case)
                        `(case ,(scm2cl-sexp-insert-funcall (cadr e))
                          ,@(mapcar
                             #'(lambda (c)
                                `(,(scm2cl-ids (car c))
                                  ,@(mapcar #'scm2cl-sexp-insert-funcall
                                            (cdr c))))
                             (cddr e))))
                       ((eq a 'cond)
                        `(cond ,@(mapcar
                                  #'(lambda (c)
                                     (mapcar #'scm2cl-sexp-insert-funcall
                                             c))
                                  (cdr e))))
                       ((member a '(flet multiple-value-setq))
                        ;generated by named let (tr)
                        `(,a ,(cadr e)
                          ,@(mapcar #'scm2cl-sexp-insert-funcall
                                    (cddr e))))
                       ((eq a 'lambda)
                        `(lambda ,(cadr e)
                          ,@(let ((*scm2cl-bound-vars*
                                   (append (cadr e) *scm2cl-bound-vars*)))
                              (mapcar
                               #'scm2cl-sexp-insert-funcall
                               (cddr e)))))
                       ((member a '(if unless when))
                        `(,a ,@(mapcar #'scm2cl-sexp-insert-funcall
                                       (cdr e))))
                       ((eq a 'defun)
                        (let ((bvv (caddr e)))
                          `(defun ,(cadr e) ,bvv
                            ,@(let ((*scm2cl-bound-vars*
                                     (append bvv *scm2cl-bound-vars*)))
                                (mapcar #'scm2cl-sexp-insert-funcall
                                        (cdddr e))))))
                       ((eq a 'setq)
                        (let ((x (cadr e)))
                          (if (and (consp x) (eq (car x) 'function))
                              `(setf (symbol-function ',(cadr x))
                                ,(scm2cl-sexp (caddr e)))
                            `(setq ,x
                              ,(scm2cl-sexp-insert-funcall
                                (caddr e))))))
                       ((and (eq a 'progn) (= (length e) 2))
                        (scm2cl-sexp-insert-funcall (cadr e)))
                       ((eq a 'let) ;(member a '(let let*))
                        `(let ,(mapcar
                                #'(lambda (xv)
                                   `(,(scm2cl-id (car xv))
                                     ,(scm2cl-sexp-insert-funcall (cadr xv))))
                                (cadr e))
                          ,@(let ((*scm2cl-bound-vars*
                                   (append (mapcar
                                            #'(lambda (xv)
                                               (scm2cl-id (car xv)))
                                            (cadr e))
                                           *scm2cl-bound-vars*)))
                              (mapcar #'scm2cl-sexp-insert-funcall
                                      (cddr e)))))
                       ((eq a 'let*)
                        `(let* ,(let ((xvxv (cadr e)) (r '())
                                      (*scm2cl-bound-vars* *scm2cl-bound-vars*))
                                  (loop
                                    (unless xvxv (return (nreverse r)))
                                    (destructuring-bind (x v) (pop xvxv)
                                      (let ((x (scm2cl-id x)))
                                        (push `(,x ,(scm2cl-sexp-insert-funcall v)) r)
                                        (push x *scm2cl-bound-vars*)))))
                          ,@(let ((*scm2cl-bound-vars*
                                   (append (mapcar (lambda (xv) (scm2cl-id (car xv))) (cadr e))
                                           *scm2cl-bound-vars*)))
                              (mapcar #'scm2cl-sexp-insert-funcall (cddr e)))))
                       ((eq a 'labels)
                        (let ((*local-procedures* *local-procedures*))
                          (mapcar #'(lambda (label-init)
                                     (push (car label-init)
                                           *local-procedures*))
                                  (cadr e))
                          `(,a ,(mapcar
                                 #'(lambda (label-init)
                                    (let ((bvv (cadr label-init)))
                                      `(,(scm2cl-id
                                          (scm2cl-sexp (car label-init)))
                                        ,(mapcar #'scm2cl-sexp bvv)
                                        ,@(let ((*scm2cl-bound-vars*
                                                 (append bvv
                                                         *scm2cl-bound-vars*
                                                         )
                                                 ))
                                            (mapcar #'scm2cl-sexp
                                                    (cddr label-init))))))
                                 (cadr e))
                            ,@(mapcar #'scm2cl-sexp-insert-funcall
                                      (cddr e)))))
                       ((eq a 'with-open-file)
                        `(,a ,(mapcar #'scm2cl-sexp (cadr e))
                          ,@(mapcar #'scm2cl-sexp-insert-funcall
                                    (cddr e))))
                       ((eq a 'quote)
                        (let ((quotee (cadr e)))
                          (if (and (consp quotee)
                                   (eql (car quotee) 'function))
                              (list 'quote (cadr quotee))
                            e)))
                       ((member a '(define-macro))
                        (format t "~&Skipping ~a ~a~%" a (cadr e))
                        nil)
                       ((and (struct-setter-p a) (= (length e) 3))
                        `(setf (,(struct-getter a) ,(scm2cl-sexp (cadr e)))
                          ,(scm2cl-sexp (caddr e))))
                       ((struct-maker-p e)
                        (cons a (mapcar #'scm2cl-sexp
                                        (keywordize-fieldnames (cdr e)))))
                       ((member a *scm2cl-bound-vars*)
                        (cons 'funcall (mapcar #'scm2cl-sexp e)))
                       ((member a *local-procedures*)
                        (cons (scm2cl-id (scm2cl-sexp a))
                              (mapcar #'scm2cl-sexp (cdr e))))
                       (t (scm2cl-map #'scm2cl-sexp e))))))))
        ((symbolp e)
         (if (eql (search "SCHEME-" (symbol-name e)) 0)
             (pushnew e *scm2cl-procs-used*))
         (if (member e *local-procedures*)
             `(function ,e)
           e))
        (t e)))

;(trace scm2cl-sexp)

(when (boundp 'scheme-file)
  ;if this file is being used as a shellscript,
  ;convert its first arg, which is a scheme file
  (let ((cl-file
         (merge-pathnames (make-pathname :type *scm2cl-lisp-extension*)
                          scheme-file)))
    (format t "~&This is scm2cl (scmxlate v ~a)~%" *scmxlate-version*)
    (scheme-to-cl scheme-file cl-file)
    ))

;(trace scm2cl-sexp)
