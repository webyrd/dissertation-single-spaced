;name conversions

;last change 2009-04-10

(defvar *scm2cl-fns-named-same*
  '(
    +
    -
    *
    /
    <
    =
    >
    <=
    >=
    apply
    caaar
    caadr
    caar
    cadar
    caddr
    cadr
    car
    cdaar
    cdadr
    cdar
    cddar
    cdddr
    cddr
    cdr
    cons
    ;eval
    list
    write-char
    ))

(defvar *scm2cl-fns-named-diff*
  '(
    andmap every
    append! nconc
    assoc scheme-assoc
    assq assoc
    assv assoc
    boolean? scheme-boolean?
    call-with-current-continuation scheme-not-quite-call/cc
    call-with-input-file scheme-call-with-input-file
    call-with-input-string scheme-call-with-input-string
    call-with-output-file scheme-call-with-output-file
    call-with-output-string scheme-call-with-output-string
    char->integer char-code
    char? characterp
    char=? char=
    char<? char<
    char>? char>
    char<=? char<=
    char>=? char>=
    char-ci=? char-equal
    char-ci<? char-lessp
    char-ci>? char-greaterp
    char-ci<=? char-not-greaterp
    char-ci>=? char-not-lessp
    char-lower-case? lower-case-p
    char-upper-case? upper-case-p
    char-alphabetic? alpha-char-p
    char-numeric? digit-char-p
    char-whitespace? scheme-char-whitespace?
    close-input-port close
    close-output-port close
    complex? complexp
    current-input-port scheme-current-input-port
    current-output-port scheme-current-output-port
    display princ
    eof-object? scheme-eof-object?
    eq? eq
    equal? equal
    eqv? eql
    eval scheme-eval
    even? evenp
    file-exists? probe-file
    ;file-or-directory-modify-seconds file-write-date
    flush-output force-output
    for-each mapc
    get-output-string get-output-stream-string

    #+allegro getenv 
    #+allegro system::getenv

    #+ecl getenv
    #+ecl si:getenv

    #+clisp getenv
    #+clisp ext:getenv

    #+sbcl getenv
    #+sbcl sb-ext:posix-getenv

    #+clozure getenv
    #+clozure ccl::getenv

    #+abcl getenv
    #+abcl ext:getenv

    inexact->exact identity
    integer? integerp
    integer->char code-char
    length list-length
    list? scheme-list?
    list-ref elt
    list-tail subseq
    list->string scheme-list->string
    list->vector scheme-list->vector
    load-relative scheme-load-relative
    make-string scheme-make-string
    make-vector scheme-make-vector
    map mapcar
    member scheme-member
    memq member
    memv member
    modulo mod
    negative? minusp
    newline terpri
    null? null
    number? numberp
    number->string scheme-number->string
    odd? oddp
    open-input-file scheme-open-input-file
    open-input-string make-string-input-stream
    open-output-file scheme-open-output-file
    open-output-string make-string-output-stream
    ormap some
    pair? consp
    peek-char scheme-peek-char
    positive? plusp
    procedure? functionp;;really?
    quotient floor
    read scheme-read
    read-char scheme-read-char
    read-line scheme-read-line
    real? realp
    reverse! nreverse
    set-car! rplaca
    set-cdr! rplacd
    sort! sort
    string scheme-string
    string? stringp
    string=? string=
    string<? string<
    string>? string>
    string<=? string<=
    string>=? string>=
    string-ci=? string-equal
    string-ci<? string-lessp
    string-ci>? string-greaterp
    string-ci<=? string-not-greaterp
    string-ci>=? string-not-lessp
    string-append scheme-string-append
    string-index scheme-string-index
    string-length length
    string->list scheme-string->list
    string->number scheme-string->number
    string-ref char
    string-reverse-index scheme-string-reverse-index
    string-set! scheme-string-set!
    string->symbol scheme-string->symbol
    substring subseq
    symbol? scheme-symbol?
    symbol->string scheme-symbol->string

    #+(and unix (or allegro clisp)) system 
    #+(and unix (or allegro clisp)) shell

    #+(and unix clozure) system
    #+(and unix clozure) ccl::os-command

    #+(and (or unix darwin) ecl) system
    #+(and (or unix darwin) ecl) si:system

    #+abcl system
    #+abcl ext:run-shell-command

    transcript-on dribble
    transcript-off dribble
    vector? vectorp
    vector->list scheme-vector->list
    vector-ref svref
    vector-set! scheme-vector-set
    void values
    with-input-from-file scheme-with-input-from-file
    with-input-from-port scheme-with-input-from-port
    with-output-to-file scheme-with-output-to-file
    with-output-to-port scheme-with-output-to-port
    write prin1
    zero? zerop
    ))

(defvar *scm2cl-kwds-named-diff*
  '(
    begin progn
    cond scm2cl-cond
    define-syntax scheme-define-syntax
    defstruct scheme-defstruct
    else t
    fluid-let let
    lambda scm2cl-lambda
    let scm2cl-let 
    ;let* scm2cl-let*
    let-syntax scheme-let-syntax
    letrec-syntax scheme-letrec-syntax
    ;loop is a common variable in Scheme.
    ;we don't want it to clash with CL's loop macro
    loop loop! 
    set! setq
    ))

(defvar *predefined-aliases* '())

(dolist (x *scm2cl-fns-named-same*)
  (push (cons x `(function ,x)) *predefined-aliases*))

(do ((s *scm2cl-fns-named-diff* (cddr s)))
    ((null s))
  (push (cons (car s) `(function ,(cadr s))) *predefined-aliases*))

(do ((s *scm2cl-kwds-named-diff* (cddr s)))
    ((null s))
  (push (cons (car s) (cadr s)) *predefined-aliases*))
