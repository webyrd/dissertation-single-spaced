(cond ((not 'nil)
       ;Common Lisp
       (load
        (merge-pathnames
          (make-pathname :type "cl")
          *load-pathname*))))

;(require (lib "trace.ss"))

'eval-in-cl-also
(define *scmxlate-version* "20120421") ;last change

'eval-in-cl-also
(begin
 (display "This is scmxlate, v ")
 (display *scmxlate-version*)
 (newline))

(define *dialect* #f)
(define *dialect-version* 1) ;?
'eval-in-cl-also
(define *operating-systems-supported* '())
'eval-in-cl-also
(define *operating-system* 'unix)
'eval-in-cl-also
(define *compile?* #f)
'eval-in-cl-also
(define *shell-script?* #f)
'eval-in-cl-also
(define *source-file* #f)
'eval-in-cl-also
(define *source-file-translated?* #f)
'eval-in-cl-also
(define *reading-source-file?* #f)
'eval-in-cl-also
(define *names-defined* '())
'eval-in-cl-also
(define *calls-disallowed* '())
;(define *names-ignored* '())
;(define *names-disabled* '())
'eval-in-cl-also
(define *aliases* '())
;(define *predefined-aliases* '())
;'eval-in-cl-also
;(define *target-file* #f)
'eval-in-cl-also
(define *target-port* #f)
'eval-in-cl-also
(define *postprocessing* #f)

'eval-in-cl-also
(define *cr* #f)
'eval-in-cl-also
(define *lf* #f)

'(define string->lower-case
  (lambda (s)
    (list->string
     (map char-downcase
          (string->list s)))))

;string->list doesn't work in scsh!

'eval-in-cl-also
(define string->lower-case
  (lambda (s)
    (let loop ((i (- (string-length s) 1)) (r '()))
      (if (< i 0) (list->string r)
          (loop (- i 1)
            (cons (char-downcase (string-ref s i)) r))))))

(define read-a-line
  (lambda (i)
    (list->string
     (let loop ()
       (let ((c (read-char i)))
         (if (or (eof-object? c)
                 (char=? c #\newline))
             '()
             (cons c (loop))))))))

(define resolve-aliases
  (lambda (e)
    (cond ((not *reading-source-file?*) e)
          ((pair? e)
           (cons (resolve-aliases (car e))
                 (resolve-aliases (cdr e))))
          ((symbol? e)
           (cond ((assv e *aliases*) => cdr)
                 (else e)))
          (else e))))

'eval-in-cl-also
(define copy-port-to-port
  (lambda (i o)
    (let loop ()
      (let ((c (read-char i)))
        (if (not (eof-object? c))
          (begin
            (if (eqv? *operating-system* 'windows)
                (cond ((char=? c *cr*) 'skip)
                      ((char=? c *lf*) (display *cr* o)
                                       (display *lf* o))
                      (else (display c o)))
                (display c o))
            (loop))
          #f)))))

'eval-in-cl-also
(define copy-file-to-port
  (lambda (f o)
    (call-with-input-file f
      (lambda (i)
        (copy-port-to-port i o)))))

'eval-in-cl-also
(define *files-to-be-ported*
  (call-with-input-file "dialects/files-to-be-ported.scm"
    (lambda (i)
      (let sub ()
        (let ((f (read i)))
          (if (eof-object? f) '()
              (cons (if (string? f) f
                        (string->lower-case (symbol->string f)))
                    (sub))))))))

'eval-in-cl-also
(define *dialects-supported*
  (if (eqv? *dialect* 'cl) '(cl)
      (call-with-input-file "dialects/dialects-supported.scm"
        (lambda (i)
          (let loop ()
            (let ((d (read i)))
              (if (eof-object? d) '()
                  (cons d (loop)))))))))

(define find-dialect
  (lambda ()
   (display "What is your Scheme dialect?")
   (newline) (display "     ") (display "(")
   (let loop ((dd *dialects-supported*) (i 0))
     (cond ((null? dd)
            (display "other)")
            (newline))
           (else
            (if (>= i 5)
                (begin (set! i 0)
                       (newline)
                       (display "     ")
                       (display " "))
                #f)
            (display (car dd)) (display " ")
            (loop (cdr dd) (+ i 1)))))
   (read)))

(set! *dialect* (find-dialect))

(if (eqv? *dialect* 'sxm)
    ;sxm issues warnings for forward-refs,
    ;which can't all be removed anyway
    (warning-handler (lambda zzz #f))
    #f)

(if (eqv? *dialect* 'guile)
    (if (not (defined? 'primitive-load))
        (begin
         (display "Your Scheme dialect is _not_ Guile!")
         (newline))
        #f)
    #f)

(if (eqv? *dialect* 'scheme48)
    (begin
      (display "Structures") (newline)
      (for-each
        (lambda (str)
          (display "  ") (display str) (newline))
        '(c-system-function extended-ports posix-files posix-process-data))
      (display "must be open before you configure or run tex2page.")
      (newline)
      (display "If they aren't open, please open and retry.")
      (newline))
    #f)

;old loc of eval1 defn

(define exists-file?
  (case *dialect*
    ((bigloo chez chicken gambit gauche guile kawa mitscheme mzscheme petite plt
             scm scsh stk stklos sxm umbscheme ypsilon)
     file-exists?)
    ((scheme48)
     (lambda (f)
       (accessible? f (access-mode read))))
    ((pscheme)
     (lambda (f)
       (with-handlers (((lambda (x) #t) (lambda (x) #f)))
         (close-input-port (open-input-file f))
         #t)))
    (else (lambda (f) #t))))

(define obliterate-file
  (case *dialect*
    ((bigloo chez gambit guile kawa mitscheme mzscheme petite plt pscheme
             scsh scm sxm umbscheme ypsilon)
     delete-file)
    ((gauche) sys-remove)
    ((scheme48) unlink)
    ((stk stklos)
     (lambda (f)
       (system (string-append "rm " f))))
;    ((gambit)
;     ;## causes problems with other Scheme dialects
;     (lambda (f)
;       ((eval (call-with-input-string "##shell-command" read))
;        (string-append "rm " f))))
    (else (lambda (f) #t))))

(define ensure-file-deleted
  (lambda (f)
    (if (exists-file? f) (obliterate-file f) #f)))

'eval-in-cl-also
(define copy-file-to-file
  (lambda (fi fo)
    (ensure-file-deleted fo)
    (call-with-output-file fo
      (lambda (o)
        (copy-file-to-port fi o)))))

(if (exists-file? "dialects/operating-systems-supported.scm")
    (set! *operating-systems-supported*
      (call-with-input-file "dialects/operating-systems-supported.scm"
        (lambda (i)
          (let loop ()
            (let ((s (read i)))
              (if (eof-object? s) '()
                  (cons s (loop))))))))
    #f)

(define dialect-getenv
  (case *dialect*
    ((bigloo chez guile mzscheme petite plt
             scm scsh stk stklos sxm ypsilon)
     (lambda (ev) (getenv ev)))
    ((gambit) (lambda (ev)
                (with-exception-handler (lambda (e) #f)
                                        (lambda () (getenv ev)))))
    ((gauche) (lambda (ev) (sys-getenv ev)))
    ((scheme48) (lambda (ev) (lookup-environment-variable ev)))
    ((chicken mitscheme) (lambda (ev) (get-environment-variable ev)))
    (else (lambda (ev) #f))))

(define determine-os
  (lambda ()
    (if (dialect-getenv "COMSPEC")
        (let ((term (dialect-getenv "TERM")))
          (if (and (string? term) (string=? term "cygwin"))
              'unix 'windows))
        'unix)))

(set! *operating-system*
  (case (length *operating-systems-supported*)
    ((0)
     ;if no OSes mentioned, assume unix
     'unix)
    ((1)
     ;if only one OS mentioned, choose it right away
     (car *operating-systems-supported*))
    (else
     (case *dialect*
       ((bigloo chez chicken gambit gauche guile mitscheme mzscheme
                petite plt scheme48 scm scsh stk stklos sxm ypsilon)
        (determine-os))
       ((pscheme) 'windows)
       ((umbscheme) 'unix)
       (else
        (display "What is your operating system? (")
        (let ((first? #t))
          (for-each
           (lambda (os)
             (if first? (set! first? #f)
                 (display " "))
             (display os))
           *operating-systems-supported*))
        (display ")")
        (newline)
        (read))))))

(define integer-to-char
  (lambda (n)
    (integer->char
     (if (memv *dialect* '(scheme48 scsh))
         (+ 1000 n)
         n))))

'eval-in-cl-also
(if (eqv? *operating-system* 'windows)
    (begin
     (set! *cr* (integer-to-char 13))
     (set! *lf* (integer-to-char 10)))
    #f)

;for PLT and Guile, check version

(case *dialect*
  ((mzscheme plt)
   (set! *dialect-version*
     (string->number
      (regexp-replace "^([0-9]+).*"
                      (version) "\\1"))))
  ((guile)
   (set! *dialect-version*
     (string->number
      (string-append
       (major-version) "." (minor-version))))))

(define eval1
  (case *dialect*
    ((guile)
     (if (>= *dialect-version* 1.6)
         (lambda (e) (eval e (interaction-environment)))
         eval))
    ((mitscheme) (lambda (e) (eval e user-initial-environment)))
    ((gauche scheme48 scsh) (lambda (e) (eval e (interaction-environment))))
    (else eval)))

;get "system" for PLT Scheme

(if (memv *dialect* '(mzscheme plt))
    (eval '(require (lib "process.ss")))
  #f)

;get "pretty-print" for PLT Scheme

(if (memv *dialect* '(mzscheme plt))
    (eval '(require (lib "pretty.ss")))
  #f)

;get compiler for MzScheme

(if (memv *dialect* '(mzscheme plt))
    (eval '(require (lib "compile.ss")))
    #f)

;get pretty-print for Guile

(if (eqv? *dialect* 'guile)
    (use-modules (ice-9 pretty-print))
    #f)

'eval-in-cl-also
(define pick-up-shell-magic-lines
  (lambda (f)
    (if (eqv? *dialect* 'mitscheme)
        (begin
         (display "Warning!  Compiled file won't have shell-magic!")
         (newline)
         (list '() f))
        (call-with-input-file f
          (lambda (i)
            (let loop ((r '()))
              (if (memv (peek-char i) '(#\# #\"))
                  (loop (cons (read-a-line i) r))
                  (if (null? r) (list r f)
                      (let ((new-f (string-append f ".temp")))
                        (ensure-file-deleted new-f)
                        (call-with-output-file new-f
                          (lambda (o)
                            (copy-port-to-port i o)))
                        (list (reverse r) new-f))))))))))

(define compile-file-to-file
  (case *dialect*
    ((chez mzscheme petite plt)
     (lambda (fi fo) (compile-file fi fo) fo))
    ((chicken)
     (lambda (fi fo)
       (display "For Chicken, it may be better form to compile from ")
       (display "the command line.") (newline)
       (display "Gamely trying anyway; this will take a while ...") (newline)
       (system (string-append "csc " fi " -o " fo " -b -O2"))
       fo))
    ((mitscheme)
     (lambda (fi fo)
       ;MIT Scheme won't allow source file to
       ;be extensionless, and it forces target
       ;file to have extension .com
       (if (not (pathname-type fi))
         (let ((new-fi
                (merge-pathnames
                 (make-pathname #f #f #f #f "scm" #f) fi)))
           (copy-file-to-file fi new-fi)
           (set! fi new-fi))
         #f)
       (set! fo (merge-pathnames
                 (make-pathname #f #f #f #f "com" #f) fi))
       (newline)
       (cf fi)
       (enough-namestring fo)))
    (else (lambda (fi fo) #f))))

;'eval-in-cl-also
(define compile-possible?
  (lambda ()
    (and (eqv? *operating-system* 'unix)
         (or (memv *dialect* '(chicken cl mzscheme plt))
             (and (memv *dialect* '(chez petite))
                  (eqv? (current-eval) compile))
             (and (eqv? *dialect* 'mitscheme)
                  (environment-bound? user-initial-environment
                                      'cf))))))

'eval-in-cl-also
(define kompile
  (lambda (f)
    (display "Compiling ") (display f)
    (display " ...") (newline)
    (let* ((x (pick-up-shell-magic-lines f))
           (shell-lines (car x))
           (src (cadr x))
           (tgt (string-append src ".so")))
      (set! tgt (compile-file-to-file src tgt))
      (if tgt
          (begin
            (display tgt) (display " created.") (newline)
            (if (memv *dialect* '(chicken mitscheme))
                (if (not (null? shell-lines))
                    (begin
                      (display "Warning: Throwing out shell-magic ")
                      (display "lines for compiled file!")
                      (newline)
                      (if (eqv? *dialect* 'chicken)
                          (begin (display "For Chicken, this may not be a problem, ")
                                 (display "as compiled code can query command args."))
                          (display "This may or may not be right!"))
                      (newline))
                    #f)
                #f)
            (if (not (eqv? *dialect* 'mitscheme))
                (begin
                  (ensure-file-deleted f)
                  (display "Copying ") (display tgt)
                  (display " to ") (display f) (newline)
                  (call-with-output-file f
                    (lambda (o)
                      (for-each
                        (lambda (line)
                          (display line o) (newline o))
                        shell-lines)
                      (copy-file-to-port tgt o)))
                  (set! tgt f))
                #f))
          (begin (display "Compilation failed?")
                 (newline)
                 (set! tgt f)))
      (display "If compilation failed, try without compile option.")
      (newline)
      tgt)))

'eval-in-cl-also
(define *dialect-s*
  (string-append
   (string->lower-case (symbol->string *dialect*)) "-"))

(define write-nicely write)

(define writeln
  (lambda (e o)
    (write-nicely (resolve-aliases e) o)
    (newline o)))

;redefine write-nicely to use pretty-printer
;for dialects that have it

(case *dialect*
  ((chicken gambit guile mzscheme petite plt sxm ypsilon)
   (set! write-nicely pretty-print))
  ((mitscheme)
   (set! write-nicely pp))
  ((stklos)
   (set! write-nicely
     (lambda (e o)
       (pretty-print e :port o)))))

'eval-in-cl-also
(define names-defined
  (lambda (x)
    (let ((r '()))
      (let sub ((x x))
        (if (pair? x)
            (let ((n (length x)))
              (cond ((and (> n 2)
                          (let ((y (car x)))
                            (and (symbol? y)
                                 (or
                                  (memv y
                                        '(define define-macro define-syntax
                                           defmacro defstruct
                                           defconstant defparameter
                                           defun defvar))
                                  (let ((y-s
                                         (symbol->string y)))
                                    (or
                                     (string-ci=? y-s "define-syntax")
                                     (string-ci=? y-s "defstruct")))))))
                     (set! r
                       (cons
                         (let ((name (cadr x)))
                           (if (pair? name) (car name) name))
                         r)))
                    ((and (>= n 2) (memv (car x) '(scmxlate-ignore-define
						   scmxlate-ignore)))
                     (set! r
                       (append (cdr x) r)))
                    ((and (>= n 2) (eqv? (car x) 'scmxlate-rename-define))
                     (set! r
                       (append (map car (cdr x)) r)))
                    ((and (> n 3) (eqv? (car x) 'syntax-table-define))
                     (set! r
                       (cons
                         (cadr (caddr x))
                         r)))
                    ((and (> n 2) (memv (car x) '(begin if when unless)))
                     (for-each sub (cddr x)))))
            #f))
      r)))

(define translate-define-syntax
  (case *dialect*
    ((stk)
     (lambda (e)
       `(define-macro (,(cadr e) . _args)
          (let ((datum->syntax (lambda (x y) y))
                (syntax->datum (lambda (x) x)))
            (,(caddr e) (cons ',(cadr e) _args))))))
    ((mitscheme)
     (if (environment-bound? user-initial-environment 'rsc-macro-transformer)
         (lambda (e)
           `(define-syntax ,(cadr e)
              (rsc-macro-transformer
               (let ((datum->syntax (lambda (x y) y))
                     (syntax->datum (lambda (x) x)))
                 (lambda (__e __r)
                   (,(caddr e) __e))))))
         (lambda (e)
           `(syntax-table-define system-global-syntax-table ',(cadr e)
              (macro _args
                (let ((datum->syntax (lambda (x y) y))
                      (syntax->datum (lambda (x) x)))
                  (,(caddr e) (cons ',(cadr e) _args))))))))
    ((scm kawa umbscheme)
     (lambda (e)
       `(defmacro ,(cadr e) _args
          (let ((datum->syntax (lambda (x y) y))
                (syntax->datum (lambda (x) x)))
            (,(caddr e) (cons ',(cadr e) _args))))))
    ((chicken scheme48 scsh)
     (lambda (e)
       `(define-syntax ,(cadr e)
          (lambda (__form __rename __compare)
            (let ((datum->syntax (lambda (x y) y))
                  (syntax->datum (lambda (x) x)))
              (,(caddr e) __form))))))
    ((gambit gauche guile bigloo pscheme stklos)
     (lambda (e)
       (let ((e-t `(define-macro ,(cadr e)
                     (lambda _args
                       (let ((datum->syntax (lambda (x y) y))
                             (syntax->datum (lambda (x) x)))
                         (,(caddr e) (cons ',(cadr e) _args)))))))
         (if (and #f (eqv? *dialect* 'gambit)) 
           ;disabled; but why was it ever needed?
             `(begin ,e-t
                     (eval ',e-t))
             e-t))))
    ((chez petite sxm)
     ;unlike Mz, these dialects don't allow
     ;a general syntax object as datum->syntax-object's
     ;first arg; it's got to be an identifier
     (lambda (e)
       `(define-syntax ,(cadr e)
          (let* ((old-datum->syntax-object datum->syntax-object)
                 (datum->syntax
                   (lambda (so output)
                     (old-datum->syntax-object
                      (syntax-case so ()
                                   ((k . stuff) (syntax k)))
                      output))))
            ,(caddr e)))))
    ((mzscheme plt)
     (lambda (e)
       (if *compile?* (eval e) #f)
       e)
     )
    (else
     (lambda (e) e))))

(define translate-define-macro
  (case *dialect*
    ((stk stklos)
     (lambda (e)
       `(define-macro (,(cadr e) ,@(cadr (caddr e)))
          ,@(cddr (caddr e)))))
    ((mitscheme)
     (if (environment-bound? user-initial-environment 'rsc-macro-transformer)
         (lambda (e)
           `(define-syntax ,(cadr e)
              (rsc-macro-transformer
               (let ((xfmr ,(caddr e)))
                 (lambda (e r)
                   (apply xfmr (cdr e)))))))
         (lambda (e)
           `(syntax-table-define system-global-syntax-table ',(cadr e)
                                 (macro ,@(cdr (caddr e)))))))
    ((scm kawa umbscheme)
     (lambda (e)
       `(defmacro ,(cadr e) ,(cadr (caddr e))
          ,@(cddr (caddr e)))))
    ((scheme48 scsh)
     (lambda (e)
       `(define-syntax ,(cadr e)
          (lambda (__form __renamee __compare)
            (apply ,(caddr e) (cdr __form))))))
    ((gambit)
     (lambda (e)
       `(begin ,e
          (eval ',e))))
    ((chez petite sxm)
     (lambda (e)
       `(define-syntax ,(cadr e)
          (lambda (x)
            (syntax-case x ()
              ((k . stuff)
               (datum->syntax-object (syntax k)
                 (apply ,(caddr e)
                   (cdr (syntax-object->datum x))))))))))
    ((mzscheme plt)
     (lambda (e)
       (let ((e `(define-syntax ,(cadr e)
                       (lambda (so)
                         (datum->syntax so
                           (let ((so-d (syntax->datum so)))
                             (apply ,(caddr e) (cdr so-d))))))))
         (if *compile?* (eval e) #f)
         e)))
    (else ;guile, bigloo, pscheme, etc
      (lambda (e) e))))

'eval-in-cl-also
(define translate-port-to-port
  (lambda (i o)
    (letrec ((process-top-level-expression
              (lambda (x)
                (let* ((a (if (pair? x) (car x) #f))
                       (names (names-defined x))
                       (name (and (pair? names) (car names))))
                  (cond ((not (pair? x)) 'skip)
                        ((and (not (memv a '(scmxlate-ignore-define
					     scmxlate-ignore
                                             scmxlate-rename-define)))
                              (memv name *names-defined*))
                         'skip)
                        (else
                         (if (pair? names)
                             (set! *names-defined*
                               (append names *names-defined*))
                             #f)
                         (cond (*reading-source-file?*
                                (case a
                                  ((define-macro)
                                   (writeln (translate-define-macro x) o))
                                  ((define-syntax)
                                   (writeln (translate-define-syntax x) o))
                                  (else
                                   (if (not (memv a *calls-disallowed*))
                                       (writeln x o)
                                       #f))))
                               (else
                                (case a
                                  ((scmxlate-rename
                                    scmxlate-rename-define)
                                   (for-each
                                    (lambda (x-y)
                                      (let ((x (car x-y)))
                                        ;(set! *names-defined*
                                        ;  (cons x *names-defined*))
                                        (set! *aliases*
                                          (cons (cons x (cadr x-y))
                                                *aliases*))))
                                    (cdr x)))
                                  ((scmxlate-include)
                                   (translate-file-to-port
                                    (string-append "dialects/"
                                                   (cadr x)) o))
                                  ((scmxlate-insert)
                                   (for-each
                                    (lambda (e) (display (eval1 e) o))
                                    (cdr x)))
                                  ((scmxlate-postamble)
                                   (translate-source-file o))
                                  ((scmxlate-postprocess)
                                   (set! *postprocessing*
                                     (append *postprocessing* (cdr x))))
                                  ((scmxlate-prefix)
                                   (let ((pfx (eval1 (cadr x))))
                                     (for-each
                                      (lambda (id)
                                        (set! *aliases*
                                          (cons (cons
                                                 id
                                                 (string->symbol
                                                  (string-append
                                                   pfx
                                                   (symbol->string id))))
                                                *aliases*)))
                                      (cddr x))))
                                  ;((scmxlate-target-file)
                                  ; (set! *target-file* (cadr x)))
                                  ((scmxlate-compile scmxlate-compile?)
                                   (set! *compile?*
                                     (let ((c (cadr x)))
                                       (cond ((not c) #f)
                                             ((compile-possible?) #t)
                                             (else
                                               (display "Sorry, compile not possible.")
                                               (newline)
                                               #f)))))
                                  ((scmxlate-uncall)
                                   (set! *calls-disallowed*
                                     (append (cdr x) *calls-disallowed*)))
                                  ((scmxlate-ignore-define
				    scmxlate-ignore) #f)
                                  ((scmxlate-eval)
                                   (for-each eval1 (cdr x)))
                                  ((scmxlate-cond)
                                   (let loop ((cc (cdr x)))
                                     (if (null? cc) #f
                                         (let ((c (car cc)) (cc (cdr cc)))
                                           (if (or
                                                (and (null? cc)
                                                     (eqv? (car c) 'else))
                                                (eval1 (car c)))
                                               (for-each
                                                process-top-level-expression
                                                (cdr c))
                                               (loop cc))))))
                                  (else
                                   (writeln x o)))))))))))
      (let loop ()
        (let ((x (read i)))
          (if (not (eof-object? x))
              (begin
               (process-top-level-expression x)
               (loop))
              #f))))))

'eval-in-cl-also
(define translate-file-to-port
  (lambda (f o)
    (call-with-input-file f
      (lambda (i)
        (translate-port-to-port i o)))))

(define guile-load
  (if (eqv? *dialect* 'guile) load #f))

(define chmod+x
  (lambda (f)
    (if (and (eqv? *operating-system* 'unix)
             *shell-script?*
             (not (and (eqv? *dialect* 'mitscheme) *compile?*))
             )
        (let ((chmod-cmd
               (string-append "chmod +x " f)))
          (case *dialect*
            ((bigloo chez chicken guile kawa mzscheme petite plt
                   scheme48
                   scm stk stklos sxm umbscheme ypsilon)
             (system chmod-cmd))
            ((gambit)
             (shell-command chmod-cmd))
            ((gauche)
             (sys-system chmod-cmd))
            ((mitscheme)
             ((if (environment-bound? user-initial-environment 'unix/system)
                 unix/system run-shell-command) chmod-cmd))
            ((scsh)
             (eval (let* ((i (make-string-input-port
                              (string-append
                               "(run ("
                               chmod-cmd "))")))
                          (e (read i)))
                     ;following causes (values) error in scsh
                     ;0.6.2
                     ;(close-input-port i)
                     e)
                   (interaction-environment)))
            (else
             (display "Do") (newline)
             (display "  ") (display chmod-cmd) (newline))))
        #f)))

(define *predefined-aliases*
  (case *dialect*
    ((bigloo)
     '((file-or-directory-modify-seconds . file-modification-time)))
    ((chicken)
     '(
       (file-or-directory-modify-seconds . file-modification-time)
       (getenv . get-environment-variable)
       ))
    ((gauche)
     `(
       (current-seconds . sys-time)
       (delete-file . sys-remove)
       ;(eof . ,(with-input-from-string "" read-char))
       ;(file-or-directory-modify-seconds . file-mtime)
       (flush-output . flush)
       (getenv . sys-getenv)
       (system . sys-system)))
    ((guile)
     '((andmap . and-map)
       (current-seconds . current-time)
       (eof . the-eof-object)
       (flush-output . force-output)
       (load . primitive-load)
       (ormap . or-map)
       ))
    ((kawa)
     '((flush-output . force-output)))
    ((mitscheme)
     `((file-or-directory-modify-seconds . file-modification-time)
       (gensym . generate-uninterned-symbol)
       (getenv . get-environment-variable)
       (open-input-string . string->input-port)
       (system . ,(if (environment-bound? user-initial-environment 'unix/system)
                      'unix/system 'run-shell-command))
       ))
    ((chez petite)
     '((flush-output . flush-output-port)
       ))
    ((gambit)
     `((flush-output . force-output)
       ;(get-output-string . close-output-port)
       (system . shell-command)
       ;(system . ,(call-with-input-string "##shell-command" read))
       ))
    ((scheme48)
     '((delete-file . unlink)
       (getenv . lookup-environment-variable)
       (open-output-string . make-string-output-port)
       (get-output-string . string-output-port-output)
       ))
    ((scsh)
     '((current-seconds . time)
       (flush-output . force-output)
       (get-output-string . string-output-port-output)
       (open-output-string . make-string-output-port)))
    ((scm)
     '((current-seconds . current-time)))
    ((stklos)
     '((current-seconds . current-time)
       (flush-output . flush)))
    ((sxm)
     '((current-seconds . current-time)
       ;(flush-output . flush)
       ))
    (else '())))

'eval-in-cl-also
(define translate-source-file
  (lambda (o)
    (if (not *source-file-translated?*)
        ;scmxlate-postamble may already have translated
        ;source file!
        (begin
         (set! *source-file-translated?* #t)
         (call-with-input-file *source-file*
           (lambda (i)
             (if (char=? (peek-char i) #\#)
                 (begin
                  (set! *shell-script?* #t)
                  (read-a-line i)
                  (display "; ensure shell-magic above" o)
                  (newline o))
                 #f)
             (display ";Configured for " o)
             (case *dialect*
               ((cl)
                (display "Common Lisp " o)
                (display (lisp-implementation-type) o)
                (display #\space o)
                (display (lisp-implementation-version) o))
               (else
                (display "Scheme dialect " o)
                (display *dialect* o)))
             (display " by scmxlate, v " o)
             (display *scmxlate-version* o)
             (display "," o) (newline o)
             (display ";(c) Dorai Sitaram, " o) (newline o)
             (display ";http://www.ccs.neu.edu/~dorai/" o)
             (display "scmxlate/scmxlate.html" o)
             (newline o) (newline o)
             (set! *reading-source-file?* #t)
             (translate-port-to-port i o)
             (set! *reading-source-file?* #f)
             (newline o))))
        #f)))

(if (eqv? *dialect* 'guile)
    (eval1 `(set! load primitive-load))
    #f)

'eval-in-cl-also
(cond ((not (memv *dialect* *dialects-supported*))
       (display "Sorry, dialect ")
       (display *dialect*)
       (display " is not supported. :-<") (newline))
      ((not (or (memv *operating-system* '(unix windows))
                (memv *operating-system* *operating-systems-supported*)))
       (display "Sorry, operating system ")
       (display *operating-system*)
       (display " is not supported. :-<") (newline))
      (else
       (for-each
        (lambda (file-to-be-ported)
          (newline)
          (display "Porting ")
          (display file-to-be-ported)
          (display " ...") (newline)
          (set! *shell-script?* #f)
          (set! *compile?* 'ask)
          (set! *source-file* file-to-be-ported)
          (set! *source-file-translated?* #f)
          (set! *postprocessing* '())
          (set! *names-defined* '())
          (set! *calls-disallowed* '())
          (set! *aliases* *predefined-aliases*)
          (let* ((user-override-file
                  (let ((f (string-append "scmxlate-" file-to-be-ported)))
                    (and (exists-file? f) f)))
                 (dialect-override-file
                  (let ((f (string-append "dialects/"
                                          *dialect-s* file-to-be-ported)))
                    (and (exists-file? f) f)))
                 (target-file
                  (string-append "my-" file-to-be-ported)))
            (ensure-file-deleted target-file)
            (call-with-output-file target-file
              (lambda (o)
                (for-each
                  (lambda (f)
                    (if f (translate-file-to-port f o) #f))
                  (list
                    user-override-file
                    dialect-override-file ))
                (translate-source-file o)))

            ;compile?
            (if (eqv? *compile?* 'ask)
                (set! *compile?*
                  (if (compile-possible?)
                      (begin
                        (display "Compile? [")
                        (display #t) (display " ") (display #f)
                        (display "]")
                        (newline) (read))
                      #f))
                #f)

            (if *compile?* (set! target-file (kompile target-file))
              #f)

            (chmod+x target-file)

            (cond ((null? *postprocessing*)
                   (display "Resulting file is `")
                   (display target-file)
                   (display "'.") (newline)
                   (display "You may want to rename it.") (newline))
                  (else
                   (for-each eval1 *postprocessing*)))
            ))
        *files-to-be-ported*)))

(if (eqv? *dialect* 'guile)
    (eval1 `(set! load guile-load))
    #f)

(if (eqv? *dialect* 'scsh) (force-output)
  #f)

;exit Scheme if possible

(case *dialect*
  ((bigloo chez chicken gambit gauche guile kawa mzscheme petite plt pscheme
	 scsh stk stklos sxm) (exit))
  ((mitscheme) (%exit))
  ((scm) (quit))
  (else (display "You may exit Scheme now!")
        (newline)))
