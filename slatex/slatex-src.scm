;slatex.scm
;SLaTeX v. 2.4z
;(c) Dorai SItaram, 1991-2002

(define *slatex-version* "20090928") ;last modified

(define *operating-system* 
  (if (getenv "COMSPEC") 'windows 'unix))

(define-syntax defenum
  (lambda (so)
    (datum->syntax so
      (let ((so-d (syntax->datum so)))
        (let loop ((z (cdr so-d)) (i 0) (r '()))
          (if (null? z) `(begin ,@r)
              (loop (cdr z) (+ i 1)
                    (cons `(define ,(car z) (integer->char ,i)) r))))))))

(define-syntax defrecord
  (lambda (so)
    (datum->syntax so
      (let ((so-d (syntax->datum so)))
        (let ((name (cadr so-d))
              (fields (cddr so-d)))
          (let loop ((fields fields) (i 0) (r '()))
            (if (null? fields)
                `(begin (define ,name (lambda () (make-vector ,i)))
                        ,@r)
                (loop (cdr fields) (+ i 1)
                      (cons `(define ,(car fields) ,i) r)))))))))

(define-syntax setf
  (lambda (so)
    (datum->syntax so
      (let ((so-d (syntax->datum so)))
        (let ((l (cadr so-d))
              (r (caddr so-d)))
          (if (symbol? l) `(set! ,l ,r)
              (let ((a (car l)))
                (if (eq? a 'list-ref)
                    `(set-car! (list-tail ,@(cdr l)) ,r)
                    `(,(cond ((eq? a 'string-ref) 'string-set!)
                             ((eq? a 'vector-ref) 'vector-set!)
                             ((eq? a 'of) 'the-setter-for-of)
                             (else
                              (slatex-error "setf is ill-formed" l r)))
                      ,@(cdr l) ,r)))))))))

(define-syntax the-setter-for-of
  (lambda (so)
    (datum->syntax so
      (let ((so-d (syntax->datum so)))
        (let ((r (cadr so-d))
              (i (caddr so-d))
              (j (cadddr so-d))
              (z (cddddr so-d)))
          (cond ((null? z) `(vector-set! ,r ,i ,j))
                ((and (eq? i '/) (= (length z) 1))
                 `(string-set! ,r ,j ,(car z)))
                (else `(the-setter-for-of (vector-ref ,r ,i) ,j ,@z))))))))

(define-syntax of
  (lambda (so)
    (datum->syntax so
      (let ((so-d (syntax->datum so)))
        (let ((r (cadr so-d))
              (i (caddr so-d))
              (z (cdddr so-d)))
          (cond ((null? z) `(vector-ref ,r ,i))
                ((and (eq? i '/) (= (length z) 1))
                 `(string-ref ,r ,(car z)))
                (else `(of (vector-ref ,r ,i) ,@z))))))))

;s4.scm

;seqprocs.scm

(define ormapcdr
  (lambda (f l)
    ;;apply f to successive cdrs of l, returning
    ;;immediately when an application is true.
    ;;only one argument list supported
    (let loop ((l l))
      (if (null? l) #f
          (or (f l) (loop (cdr l)))))))

(define list-prefix?
  (lambda (pfx l)
    ;;tests if list pfx is a prefix of list l
    (cond ((null? pfx) #t)
          ((null? l) #f)
          ((eqv? (car pfx) (car l)) 
           (list-prefix? (cdr pfx) (cdr l)))
          (else #f))))

(define string-suffix?
  (lambda (sfx s)
    ;;tests if string sfx is a suffix of string s
    (let ((sfx-len (string-length sfx)) (s-len (string-length s)))
      (if (> sfx-len s-len) #f
          (let loop ((i (- sfx-len 1)) (j (- s-len 1)))
            (if (< i 0) #t
                (and (char=? (string-ref sfx i) (string-ref s j))
                     (loop (- i 1) (- j 1)))))))))

(define mapcan
  (lambda (f l)
    ;;maps f on l but splices (destructively) the results;
    ;;only one argument list supported
    (let loop ((l l))
      (if (null? l) '()
          (append! (f (car l)) (loop (cdr l)))))))

(define lassoc
  (lambda (x al eq)
    (let loop ((al al))
      (if (null? al) #f
	  (let ((c (car al)))
	    (if (eq (car c) x) c
		(loop (cdr al))))))))

(define lmember
  (lambda (x l eq)
    (let loop ((l l))
      (if (null? l) #f
	  (if (eq (car l) x) l
	      (loop (cdr l)))))))

(define delete
      (lambda (x l eq)
	(let loop ((l l))
	  (cond ((null? l) l)
		((eq (car l) x) (loop (cdr l)))
		(else (set-cdr! l (loop (cdr l)))
		      l)))))

(define adjoin
      (lambda (x l eq)
	(if (lmember x l eq) l
	    (cons x l))))

(define delete-if
  (lambda (p s)
    (let loop ((s s))
      (cond ((null? s) s)
	    ((p (car s)) (loop (cdr s)))
	    (else (set-cdr! s (loop (cdr s)))
		  s)))))

(define string-prefix?
      (lambda (s1 s2 i)
	;Tests if s1 and s2 have the same first i chars.
	;Both s1 and s2 must be at least i long.
	(let loop ((j 0))
	  (if (= j i) #t
	      (and (char=? (string-ref s1 j) (string-ref s2 j))
		   (loop (+ j 1)))))))

(define sublist
  (lambda (l i f)
    ;;finds the sublist of l from index i inclusive to index f exclusive
    (let loop ((l (list-tail l i)) (k i) (r '()))
      (cond ((>= k f) (reverse! r))
	    ((null? l)
	     (slatex-error "sublist: List too small"))
	    (else (loop (cdr l) (+ k 1) (cons (car l) r)))))))

(define position-char
  (lambda (c l)
					;;finds the leftmost index of character-list l where character c occurs
    (let loop ((l l) (i 0))
      (cond ((null? l) #f)
	    ((char=? (car l) c) i)
	    (else (loop (cdr l) (+ i 1)))))))

(define string-position-right
      (lambda (c s)
	;finds the rightmost index of string s where character c occurs
	(let ((n (string-length s)))
	  (let loop ((i (- n 1)))
	    (cond ((< i 0) #f)
		  ((char=? (string-ref s i) c) i)
		  (else (loop (- i 1))))))))

;fileproc.scm


;lerror.scm

(define *return* (integer->char 13))

(define *tab* (integer->char 9))

(define slatex-error
  (lambda (where . what) 
    (display "Error: ")
    (display where) (newline)
    (for-each
     (lambda (v)
       (write v) (newline))
     what)
    (error "slatex-error")))

(define exit-slatex
      (lambda () ;in case it's a macro
        (exit)))

;defaults.scm

(define *slatex-case-sensitive?* #t)

(define keyword-tokens
  (list
    ;;RnRS (plus some additional Scheme) keywords
    "=>"
    "%"
    "abort"
    "and"
    "begin"
    "begin0"
    "case"
    "case-lambda"
    "cond"
    "define"
    "define!"
    "define-macro!"
    "define-syntax"
    "defmacro"
    "defrec!"
    "delay"
    "do"
    "else"
    "extend-syntax"
    "fluid-let"
    "if"
    "lambda"
    "let"
    "let*"
    "letrec"
    "let-syntax"
    "letrec-syntax"
    "or"
    "quasiquote"
    "quote"
    "rec"
    "record-case"
    "record-evcase"
    "recur"
    "set!"
    "sigma"
    "struct"
    "syntax"
    "syntax-rules"
    "trace"
    "trace-lambda"
    "trace-let"
    "trace-recur"
    "unless"
    "unquote"
    "unquote-splicing"
    "untrace"
    "when"
    "with"
    ))

(define variable-tokens '())

(define constant-tokens '())

(define data-tokens '())

(define special-symbols
  (reverse
   (reverse
    '(
      ("." . ".")
      ("..." . "{\\dots}")
      ("-" . "$-$")
      ("1-" . "\\va{1$-$}")
      ("-1+" . "\\va{$-$1$+$}")
      ))))

(define macro-definers
  '("define-syntax" "syntax-rules" "defmacro"
                    "extend-syntax" "define-macro!"))

(define case-and-ilk
  '("case" "record-case"))

(define tex-analog
  (lambda (c)
    ;find a TeX string that corresponds to the character c
    (case c
      ((#\$ #\& #\% #\# #\_) (string #\\ c))
      ;((#\#) "{\\sf\\#}")
      ;((#\\) "{\\ttbackslash}")
      ((#\{ #\}) (string #\$ #\\ c #\$))
      ((#\\) "$\\backslash$")
      ((#\+) "$+$")
      ((#\*) "$\\ast$")
      ((#\=) "$=$")
      ((#\<) "$\\lt$")
      ((#\>) "$\\gt$")
      ((#\^) "\\^{}")
      ((#\|) "$\\vert$")
      ;((#\~) "\\verb-~-")
      ((#\~) "\\~{}")
      ((#\@) "{\\atsign}")
      ((#\") "{\\dq}")
      (else (string c)))))

(define token=?
  (lambda (t1 t2)
    ;tests if t1 and t2 are identical tokens
    ((if *slatex-case-sensitive?* string=?
                 string-ci=?)
             t1 t2)))

(define *slatex-enabled?* #t)
(define *slatex-reenabler* "UNDEFINED")
(define *intext-triggerers* (list "scheme"))
(define *resultintext-triggerers* (list "schemeresult"))
(define *display-triggerers* (list "schemedisplay"))
(define *response-triggerers* (list "schemeresponse"))
(define *respbox-triggerers* (list "schemeresponsebox"))
(define *box-triggerers* (list "schemebox"))
(define *topbox-triggerers* (list "schemetopbox"))
(define *input-triggerers* (list "schemeinput"))
(define *region-triggerers* (list "schemeregion"))
(define *math-triggerers* '())
(define *slatex-in-protected-region?* #f)
(define *protected-files* '())
(define *include-onlys* 'all)
(define *latex?* #t)
(define *slatex-separate-includes?* #f)
(define *tex-calling-directory* "")

;structs.scm

(define *max-line-length* 300)

(defenum
    ;possible values of =space
    &void-space
    &plain-space
    &init-space
    &init-plain-space
    &paren-space
    &bracket-space
    &quote-space
    &inner-space)

(defenum
    ;possible values of =tab
    &void-tab
    &set-tab
    &move-tab
    &tabbed-crg-ret
    &plain-crg-ret)

(defenum
    ;possible values of =notab
    &void-notab
    &begin-comment
    &mid-comment
    &begin-string
    &mid-string
    &end-string
    &begin-math
    &mid-math
    &end-math)

(defrecord make-raw-line
    =rtedge
    =char
    =space
    =tab
    =notab)

(define make-line
    (lambda ()
      ;makes a "line" record
      (let ((l (make-raw-line)))
	(setf (of l =rtedge) 0)
	(setf (of l =char) (make-string *max-line-length* #\space))
	(setf (of l =space) (make-string *max-line-length* &void-space))
	(setf (of l =tab) (make-string *max-line-length* &void-tab))
	(setf (of l =notab) (make-string *max-line-length* &void-notab))
	l)))

(define *line1* (make-line))
(define *line2* (make-line))

(defrecord make-case-frame
  =in-ctag-tkn
  =in-bktd-ctag-exp
  =in-case-exp)

(defrecord make-bq-frame
    =in-comma =in-bq-tkn =in-bktd-bq-exp)

(define *latex-paragraph-mode?* 'fwd1)

(define *intext?* 'fwd2)
(define *code-env-spec* "UNDEFINED")

(define *in* 'fwd3)
(define *out* 'fwd4)

(define *in-qtd-tkn* 'fwd5)
(define *in-bktd-qtd-exp* 'fwd6)

(define *in-mac-tkn* 'fwd7)
(define *in-bktd-mac-exp* 'fwd8)

(define *case-stack* 'fwd9)

(define *bq-stack* 'fwd10)

(define display-space
    (lambda (s p)
      (cond ((eq? s &plain-space) (display #\space p))
	    ((eq? s &init-plain-space) (display #\space p))
	    ((eq? s &init-space) (display "\\HL " p))
	    ((eq? s &paren-space) (display "\\PRN " p))
	    ((eq? s &bracket-space) (display "\\BKT " p))
	    ((eq? s &quote-space) (display "\\QUO " p))
	    ((eq? s &inner-space) (display "\\ " p)))))

(define display-tab
    (lambda (tab p)
      (cond ((eq? tab &set-tab) (display "\\=" p))
	    ((eq? tab &move-tab) (display "\\>" p)))))

(define display-notab
    (lambda (notab p)
      (cond ((eq? notab &begin-string) (display "\\dt{" p))
	    ((eq? notab &end-string) (display "}" p)))))

;helpers.scm



(define prim-data-token?
  (lambda (token)
					;token cannot be empty string!
    (or (char=? (string-ref token 0) #\#)
	(string->number token))))

(define set-keyword
    (lambda (x)
      ;add token x to the keyword database
      (if (not (lmember x keyword-tokens token=?))
	  (begin
            (set! constant-tokens
	      (delete x constant-tokens token=?))
	    (set! variable-tokens
	      (delete x variable-tokens token=?))
	    (set! data-tokens (delete x data-tokens token=?))
	    (set! keyword-tokens (cons x keyword-tokens))))))

(define set-constant
  (lambda (x)
    ;;add token x to the constant database
    (if (not (lmember x constant-tokens token=?))
	(begin
	  (set! keyword-tokens
		(delete x keyword-tokens token=?))
	  (set! variable-tokens
		(delete x variable-tokens token=?))
	  (set! data-tokens (delete x data-tokens token=?))
	  (set! constant-tokens (cons x constant-tokens))))))

(define set-variable
    (lambda (x)
      ;;add token x to the variable database
      (if (not (lmember x variable-tokens token=?))
	  (begin
            (set! keyword-tokens (delete x keyword-tokens token=?))
	    (set! constant-tokens
	      (delete x constant-tokens token=?))
	    (set! data-tokens (delete x data-tokens token=?))
	    (set! variable-tokens (cons x variable-tokens))))))

(define set-data
    (lambda (x)
      ;;add token x to the "data" database
      (if (not (lmember x data-tokens token=?))
	  (begin
            (set! keyword-tokens
	      (delete x keyword-tokens token=?))
	    (set! constant-tokens
	      (delete x constant-tokens token=?))
	    (set! variable-tokens
	      (delete x variable-tokens token=?))
	    (set! data-tokens (cons x data-tokens))))))

(define set-special-symbol
    (lambda (x transl)
      ;;add token x to the special-symbol database with
      ;;the translation transl
      (let ((c (lassoc x special-symbols token=?)))
	(if c (set-cdr! c transl)
	    (set! special-symbols
	      (cons (cons x transl) special-symbols))))))

(define unset-special-symbol
    (lambda (x)
      ;disable token x's special-symbol-hood
      (set! special-symbols
	(delete-if
	    (lambda (c)
	      (token=? (car c) x)) special-symbols))))

(define texify
    (lambda (s)
      ;create a tex-suitable string out of token s
      (list->string (texify-aux s))))

(define texify-data
  (lambda (s)
    ;;create a tex-suitable string out of the data token s
    (let loop ((l (texify-aux s)) (r '()))
      (if (null? l) (list->string (reverse! r))
	  (let ((c (car l)))
	    (loop (cdr l)
		  (if (char=? c #\-) (append! (list #\$ c #\$) r)
		      (cons c r))))))))

(define texify-aux
    (let ((arrow (string->list "-$>$"))
	   (em-dash (string->list "---"))
	   (en-dash (string->list "--"))
	   (arrow2 (string->list "$\\to$"))
	   (em-dash-2 (string->list "${-}{-}{-}$"))
	   (en-dash-2 (string->list "${-}{-}$")))
      (lambda (s)
	;;return the list of tex characters corresponding to token s.
	;;perhaps some extra context-sensitive prettifying
	;;could go in the making of texified-sl below
	(let ((texified-sl (mapcan
			    (lambda (c) (string->list (tex-analog c)))
			    (string->list s))))
	  (let loop ((d texified-sl))
	    ;;cdr down texified-sl
	    ;;to transform any character combinations
	    ;;as desired
	    (cond ((null? d) #f)
		  ((list-prefix? arrow d) ; $->$
		   (let ((d2 (list-tail d 4)))
		     (set-car! d (car arrow2))
		     (set-cdr! d (append (cdr arrow2) d2))
		     (loop d2)))
		  ((list-prefix? em-dash d) ; ---
		   (let ((d2 (list-tail d 3)))
		     (set-car! d (car em-dash-2))
		     (set-cdr! d (append (cdr em-dash-2) d2))
		     (loop d2)))
		  ((list-prefix? en-dash d) ; --
		   (let ((d2 (list-tail d 2)))
		     (set-car! d (car en-dash-2))
		     (set-cdr! d (append (cdr en-dash-2) d2))
		     (loop d2)))
		  (else (loop (cdr d)))))
	  texified-sl))))

(define display-begin-sequence
    (lambda (out)
      (if (or *intext?* (not *latex?*))
	  (begin
            (display "\\" out)
	    (display *code-env-spec* out)
	    (newline out))
	  (begin
            (display "\\begin{" out)
	    (display *code-env-spec* out)
	    (display "}%" out)
	    (newline out)))))

(define display-end-sequence
    (lambda (out)
      (cond (*intext?* ;(or *intext?* (not *latex?*))
	     (display "\\end" out)
	     (display *code-env-spec* out)
	     ;;(display "{}" out)
	     (newline out))
	    (*latex?*
	     (display "\\end{" out)
	     (display *code-env-spec* out)
	     (display "}" out)
	     (newline out))
	    (else
	     (display "\\end" out)
	     (display *code-env-spec* out)
	     (newline out)))))

(define display-tex-char
    (lambda (c p)
      (display (if (char? c) (tex-analog c) c) p)))

(define display-token
  (lambda (s typ p)
    (cond ((eq? typ 'syntax)
           (display "\\sy{" p)
           (display (texify s) p)
           (display "}" p))
          ((eq? typ 'variable)
           (display "\\va{" p)
           (display (texify s) p)
           (display "}" p))
          ((eq? typ 'constant)
           (display "\\cn{" p)
           (display (texify s) p)
           (display "}" p))
          ((eq? typ 'data)
           (display "\\dt{" p)
           (display (texify-data s) p)
           (display "}" p))
          (else (slatex-error 'display-token
                              "Unknown token type" typ)))))


;peephole.scm


(define get-line
  (let ((curr-notab &void-notab))
    (lambda (line)
      ;;read the current tex line into "line";
      ;;returns false on eof
      (let ((graphic-char-seen? #f))
	(let loop ((i 0))
	  (let ((c (read-char *in*)))
	    (cond (graphic-char-seen? (void))
		  ((or (eof-object? c)
		       (char=? c *return*)
		       (char=? c #\newline)
		       (char=? c #\space) (char=? c *tab*))
		   (void))
		  (else (set! graphic-char-seen? #t)))
	    (cond
	     ((eof-object? c)
	      (cond ((eq? curr-notab &mid-string)
		     (if (> i 0)
			 (setf (of line =notab / (- i 1)) &end-string)))
		    ((eq? curr-notab &mid-comment)
		     (set! curr-notab &void-notab))
		    ((eq? curr-notab &mid-math)
		     (slatex-error 'get-line "Found eof inside math")))
	      (setf (of line =char / i) #\newline)
	      (setf (of line =space / i) &void-space)
	      (setf (of line =tab / i) &void-tab)
	      (setf (of line =notab / i) &void-notab)
	      (setf (of line =rtedge) i)
	      (if (eq? (of line =notab / 0) &mid-string)
		  (setf (of line =notab / 0) &begin-string))
	      (if (= i 0) #f #t))
	     ((or (char=? c *return*) (char=? c #\newline))
	      (if (and (memv *operating-system* '(dos windows os2 os2fat))
		       (char=? c *return*))
		  (if (char=? (peek-char *in*) #\newline)
		      (read-char *in*)))
	      (setf (of line =notab / i) &void-notab)
	      (cond ((eq? curr-notab &mid-string)
		     (setf (of line =notab / i) &end-string))
		    ((eq? curr-notab &mid-comment)
		     (set! curr-notab &void-notab))
		    ((eq? curr-notab &mid-math)
		     (slatex-error 'get-line "Sorry, you can't split "
                          "math formulas across lines in Scheme code")))
	      (setf (of line =char / i) #\newline)
	      (setf (of line =space / i) &void-space)
	      (setf (of line =tab / i)
		    (cond ((eof-object? (peek-char *in*)) &plain-crg-ret)
			  (*intext?* &plain-crg-ret)
			  (else &tabbed-crg-ret)))
	      ;;(setf (of line =notab / i) &void-notab)
	      (setf (of line =rtedge) i)
	      (if (eq? (of line =notab / 0) &mid-string)
		  (setf (of line =notab / 0) &begin-string))
	      #t)
	     ((eq? curr-notab &mid-comment)
	      (setf (of line =char / i) c)
	      (setf (of line =space / i)
		    (cond ((char=? c #\space) &plain-space)
			  ((char=? c *tab*) &plain-space)
			  (else &void-space)))
	      (setf (of line =tab / i) &void-tab)
	      (setf (of line =notab / i) &mid-comment)
	      (loop (+ i 1)))
	     ((char=? c #\\)
	      (setf (of line =char / i) c)
	      (setf (of line =space / i) &void-space)
	      (setf (of line =tab / i) &void-tab)
	      (setf (of line =notab / i) curr-notab)
	      (let ((i+1 (+ i 1)) (c+1 (read-char *in*)))
		(if (char=? c+1 *tab*) (set! c+1 #\space))
		(setf (of line =char / i+1) c+1)
		(setf (of line =space / i+1)
		      (if (char=? c+1 #\space) &plain-space
			  &void-space))
		(setf (of line =tab / i+1) &void-tab)
		(setf (of line =notab / i+1) curr-notab)
		(loop (+ i+1 1))))
	     ((eq? curr-notab &mid-math)
	      (if (char=? c *tab*) (set! c #\space))
	      (setf (of line =space / i)
		    (if (char=? c #\space) &plain-space
			&void-space))
	      (setf (of line =tab / i) &void-tab)
	      (cond ((memv c *math-triggerers*)
		     (setf (of line =char / i) #\$)
		     (setf (of line =notab / i) &end-math)
		     (setf curr-notab &void-notab))
		    (else (setf (of line =char / i) c)
			  (setf (of line =notab / i) &mid-math)))
	      (loop (+ i 1)))
	     ((eq? curr-notab &mid-string)
	      (if (char=? c *tab*) (set! c #\space))
	      ;;or should tab and space be treated differently?
	      (setf (of line =char / i) c)
	      (setf (of line =space / i)
		    (if (char=? c #\space) &inner-space &void-space))
	      (setf (of line =tab / i) &void-tab)
	      (setf (of line =notab / i)
		    (cond ((char=? c #\")
			   (set! curr-notab &void-notab)
			   &end-string)
			  (else &mid-string)))
	      (loop (+ i 1)))
	     ;;henceforth curr-notab is &void-notab
	     ((char=? c #\space)
	      (setf (of line =char / i) c)
	      (setf (of line =space / i)
		    (cond (*intext?* &plain-space)
			  (graphic-char-seen? &inner-space)
			  (else &init-space)))
	      (setf (of line =tab / i) &void-tab)
	      (setf (of line =notab / i) &void-notab)
	      (loop (+ i 1)))
	     ((char=? c *tab*)
	      (let loop1 ((i i) (j 0))
		(if (< j 8)
		    (begin
		      (setf (of line =char / i) #\space)
		      (setf (of line =space / i)
			    (cond (*intext?* &plain-space)
				  (graphic-char-seen? &inner-space)
				  (else &init-space)))
		      (setf (of line =tab / i) &void-tab)
		      (setf (of line =notab / i) &void-notab)
		      (loop1 (+ i 1) (+ j 1)))))
	      (loop (+ i 8)))
	     ((char=? c #\")
	      (setf (of line =char / i) c)
	      (setf (of line =space / i) &void-space)
	      (setf (of line =tab / i) &void-tab)
	      (setf (of line =notab / i) &begin-string)
	      (set! curr-notab &mid-string)
	      (loop (+ i 1)))
	     ((char=? c #\;)
	      (setf (of line =char / i) c)
	      (setf (of line =space / i) &void-space)
	      (setf (of line =tab / i) &void-tab)
	      (setf (of line =notab / i) &begin-comment)
	      (set! curr-notab &mid-comment)
	      (loop (+ i 1)))
	     ((memv c *math-triggerers*)
	      (setf (of line =char / i) #\$)
	      (setf (of line =space / i) &void-space)
	      (setf (of line =tab / i) &void-tab)
	      (setf (of line =notab / i) &begin-math)
	      (set! curr-notab &mid-math)
	      (loop (+ i 1)))
	     (else (setf (of line =char / i) c)
		   (setf (of line =space / i) &void-space)
		   (setf (of line =tab / i) &void-tab)
		   (setf (of line =notab / i) &void-notab)
		   (loop (+ i 1))))))))))

(define peephole-adjust
  (lambda (curr prev)
    ;;adjust the tabbing information on the current line curr and
    ;;its previous line prev relative to each other
    (if (or (blank-line? curr)
	    (flush-comment-line? curr))
	(if (not *latex-paragraph-mode?*)
	    (begin
	      (set! *latex-paragraph-mode?* #t)
	      (if (not *intext?*)
		  (begin
		    (remove-some-tabs prev 0)
		    (let ((prev-rtedge (of prev =rtedge)))
		      (if (eq? (of prev =tab / prev-rtedge) &tabbed-crg-ret)
			  (setf (of prev =tab / (of prev =rtedge))
				&plain-crg-ret)))))))
	(begin
	  (if *latex-paragraph-mode?*
	      (set! *latex-paragraph-mode?* #f)
	      (if (not *intext?*)
		  (let ((remove-tabs-from #f))
		    (let loop ((i 0))
		      (cond
		       ((char=? (of curr =char / i) #\newline)
			(set! remove-tabs-from i))
		       ((char=? (of prev =char / i) #\newline)
			(set! remove-tabs-from #f))
		       ((eq? (of curr =space / i) &init-space)
					;eating initial space of curr
			(if (eq? (of prev =notab / i) &void-notab)
			    (begin
			      (cond
			       ((or (char=? (of prev =char / i) #\()
				    (eq? (of prev =space / i) &paren-space))
				(setf (of curr =space / i) &paren-space))
			       ((or (char=? (of prev =char / i) #\[)
				    (eq? (of prev =space / i) &bracket-space))
				(setf (of curr =space / i) &bracket-space))
			       ((or (memv (of prev =char / i) '(#\' #\` #\,))
				    (eq? (of prev =space / i) &quote-space))
				(setf (of curr =space / i) &quote-space)))
			      (if (memq (of prev =tab / i)
					(list &set-tab &move-tab))
				  (setf (of curr =tab / i) &move-tab))))
			(loop (+ i 1)))
		       ;;finished tackling &init-spaces of curr
		       ((= i 0)		;curr starts left-flush
			(set! remove-tabs-from 0))
		       ;;at this stage, curr[notab,i]
		       ;;is either #f or a &begin-comment/string
		       ((not (eq? (of prev =tab / i) &void-tab))
			;;curr starts with nice alignment with prev
			(set! remove-tabs-from (+ i 1))
			(if (memq (of prev =tab / i)
				  (list &set-tab &move-tab))
			    (setf (of curr =tab / i) &move-tab)))
		       ((memq (of prev =space / i)
			      (list &init-space &init-plain-space
				    &paren-space &bracket-space
				    &quote-space))
					;curr starts while prev is still empty
			(set! remove-tabs-from (+ i 1)))
		       ((and (char=? (of prev =char / (- i 1)) #\space)
			     (eq? (of prev =notab / (- i 1)) &void-notab))
					;;curr can induce new alignment straightaway
			(set! remove-tabs-from (+ i 1))
			(setf (of prev =tab / i) &set-tab)
			(setf (of curr =tab / i) &move-tab))
		       (else		;;curr stakes its &move-tab (modulo parens/bkts)
					;;and induces prev to have corresp &set-tab
			(set! remove-tabs-from (+ i 1))
			(let loop1 ((j (- i 1)))
			  (cond ((<= j 0) 'exit-loop1)
				((not (eq? (of curr =tab / j) &void-tab))
				 'exit-loop1)
				((memq (of curr =space / j)
				       (list &paren-space &bracket-space
					     &quote-space))
				 (loop1 (- j 1)))
				((or (not (eq? (of prev =notab / j)
					       &void-notab))
				     (char=? (of prev =char / j) #\space))
				 (let ((k (+ j 1)))
				   (if (not (memq (of prev =notab / k)
						  (list &mid-comment
							&mid-math &end-math
							&mid-string
							&end-string)))
				       (begin
					 (if (eq? (of prev =tab / k)
						  &void-tab)
					     (setf (of prev =tab / k)
						   &set-tab))
					 (setf (of curr =tab / k)
					       &move-tab)))))
				(else 'anything-else?)
				)))))
		    (remove-some-tabs prev remove-tabs-from))))
	  (if (not *intext?*) (add-some-tabs curr))
	  (clean-init-spaces curr)
	  (clean-inner-spaces curr)))))

(define add-some-tabs
    (lambda (line)
      ;;add some tabs in the body of line "line" so the next line
      ;;can exploit them
      (let loop ((i 1) (succ-parens? #f))
	(let ((c (of line =char / i)))
	  (cond ((char=? c #\newline) 'exit-loop)
		((not (eq? (of line =notab / i) &void-notab))
		 (loop (+ i 1) #f))
		((char=? c #\[)
		 (if (eq? (of line =tab / i) &void-tab)
		     (setf (of line =tab / i) &set-tab))
		 (loop (+ i 1) #f))
		((char=? c #\()
		 (if (eq? (of line =tab / i) &void-tab)
		     (if (not succ-parens?)
			 (setf (of line =tab / i) &set-tab)))
		 (loop (+ i 1) #t))
		(else (loop (+ i 1) #f)))))))

(define remove-some-tabs
  (lambda (line i)
    ;;remove useless tabs on line "line" after index i
    (if i
	(let loop ((i i))
	  (cond ((char=? (of line =char / i) #\newline) 'exit)
		((eq? (of line =tab / i) &set-tab)
		 (setf (of line =tab / i) &void-tab)
		 (loop (+ i 1)))
		(else (loop (+ i 1))))))))

(define clean-init-spaces
    (lambda (line)
      ;;remove init-spaces on line "line" because
      ;;tabs make them defunct
      (let loop ((i (of line =rtedge)))
	(cond ((< i 0) 'exit-loop)
	      ((eq? (of line =tab / i) &move-tab)
	       (let loop1 ((i (- i 1)))
		 (cond ((< i 0) 'exit-loop1)
		       ((memq (of line =space / i)
			      (list &init-space &paren-space &bracket-space
				    &quote-space))
			(setf (of line =space / i) &init-plain-space)
			(loop1 (- i 1)))
		       (else (loop1 (- i 1))))))
	      (else (loop (- i 1)))))))

(define clean-inner-spaces
    (lambda (line)
      ;;remove single inner spaces in line "line" since
      ;;paragraph mode takes care of them
      (let loop ((i 0) (succ-inner-spaces? #f))
	(cond ((char=? (of line =char / i) #\newline) 'exit-loop)
	      ((eq? (of line =space / i) &inner-space)
	       (if (not succ-inner-spaces?)
		   (setf (of line =space / i) &plain-space))
	       (loop (+ i 1) #t))
	      (else (loop (+ i 1) #f))))))

(define blank-line?
    (lambda (line)
      ;;check if line "line" is blank
      (let loop ((i 0))
	(let ((c (of line =char / i)))
	  (cond ((char=? c #\space)
		 (if (eq? (of line =notab / i) &void-notab)
		     (loop (+ i 1)) #f))
		((char=? c #\newline)
		 (let loop1 ((j (- i 1)))
		   (if (not (<= j 0))
		       (begin
                         (setf (of line =space / i) &void-space)
			 (loop1 (- j 1)))))
		 #t)
		(else #f))))))

(define flush-comment-line?
  (lambda (line)
    ;;check if line "line" is one with ; in the leftmost column
    (and (char=? (of line =char / 0) #\;)
	 (eq? (of line =notab / 0) &begin-comment)
	 (not (char=? (of line =char / 1) #\;)))))

(define display-tex-line
    (lambda (line)
      (cond;((and (flush-comment-line? line)
       ;;          (char=? (of line =char / 1) #\%))
       ;;     (display "\\ZZZZschemecodebreak" *out*)
       ;;     (newline *out*))
       (else
	(let loop ((i (if (flush-comment-line? line) 1 0)))
	  (let ((c (of line =char / i)))
	    (if (char=? c #\newline)
		(if (not (eq? (of line =tab / i) &void-tab))
		    (newline *out*))
		(begin (write-char c *out*) (loop (+ i 1))))))))))

(define display-scm-line
    (lambda (line)
      (let loop ((i 0))
        (let ((c (of line =char / i)))
          (cond ((char=? c #\newline)
                 (let ((notab (of line =notab / i))
                       (tab (of line =tab / i)))
                   (if (eq? notab &end-string) (display "}" *out*))
                   (cond ((eq? tab &tabbed-crg-ret)
			  (display "\\\\%" *out*)
			  (newline *out*))
			 ((eq? tab &plain-crg-ret) (newline *out*))
			 ((eq? tab &void-tab)
			  (write-char #\% *out*)
			  (newline *out*)))))
		((eq? (of line =notab / i) &begin-comment)
		 (display-tab (of line =tab / i) *out*)
		 (write-char c *out*)
		 (loop (+ i 1)))
		((eq? (of line =notab / i) &mid-comment)
		 (write-char c *out*)
		 (loop (+ i 1)))
		((eq? (of line =notab / i) &begin-string)
		 (display-tab (of line =tab / i) *out*)
		 (display "\\dt{" *out*)
		 (if (char=? c #\space)
		     (display-space (of line =space / i) *out*)
		     (display-tex-char c *out*))
		 (loop (+ i 1)))
		((eq? (of line =notab / i) &mid-string)
		 (if (char=? c #\space)
		     (display-space (of line =space / i) *out*)
		     (display-tex-char c *out*))
		 (loop (+ i 1)))
		((eq? (of line =notab / i) &end-string)
		 (if (char=? c #\space)
		     (display-space (of line =space / i) *out*)
		     (display-tex-char c *out*))
		 (write-char #\} *out*)
		 (if *in-qtd-tkn* (set! *in-qtd-tkn* #f)
		     (if *in-mac-tkn* (set! *in-mac-tkn* #f)))
		 (loop (+ i 1)))
		((eq? (of line =notab / i) &begin-math)
		 (display-tab (of line =tab / i) *out*)
		 (write-char c *out*)
		 (loop (+ i 1)))
		((eq? (of line =notab / i) &mid-math)
		 (write-char c *out*)
		 (loop (+ i 1)))
		((eq? (of line =notab / i) &end-math)
		 (write-char c *out*)
		 (if *in-qtd-tkn* (set! *in-qtd-tkn* #f)
		     (if *in-mac-tkn* (set! *in-mac-tkn* #f)))
		 (loop (+ i 1)))
		;	      ((memq (of line =notab / i) (list &mid-math &end-math))
		;;	       (write-char c *out*)
		;;	       (loop (+ i 1)))
		((char=? c #\space)
		 (display-tab (of line =tab / i) *out*)
		 (display-space (of line =space / i) *out*)
		 (loop (+ i 1)))
		((char=? c #\')
		 (display-tab (of line =tab / i) *out*)
		 (write-char c *out*)
                 (if (or *in-qtd-tkn* 
                         (> *in-bktd-qtd-exp* 0)
                         (and (pair? *bq-stack*)
                              (not (of (car *bq-stack*) =in-comma))))
                     #f
                     (set! *in-qtd-tkn* #t))
		 (loop (+ i 1)))
		((char=? c #\`)
		 (display-tab (of line =tab / i) *out*)
		 (write-char c *out*)
		 (if (or (null? *bq-stack*)
			 (of (car *bq-stack*) =in-comma))
		     (set! *bq-stack*
			   (cons (let ((f (make-bq-frame)))
				   (setf (of f =in-comma) #f)
				   (setf (of f =in-bq-tkn) #t)
				   (setf (of f =in-bktd-bq-exp) 0)
				   f)
				 *bq-stack*)))
		 (loop (+ i 1)))
		((char=? c #\,)
		 (display-tab (of line =tab / i) *out*)
		 (write-char c *out*)
		 (if (not (or (null? *bq-stack*)
			      (of (car *bq-stack*) =in-comma)))
		     (set! *bq-stack*
			   (cons (let ((f (make-bq-frame)))
				   (setf (of f =in-comma) #t)
				   (setf (of f =in-bq-tkn) #t)
				   (setf (of f =in-bktd-bq-exp) 0)
				   f)
				 *bq-stack*)))
		 (if (char=? (of line =char / (+ i 1)) #\@)
		     (begin (display-tex-char #\@ *out*) (loop (+ 2 i)))
		     (loop (+ i 1))))
		((memv c '(#\( #\[))
		 (display-tab (of line =tab / i) *out*)
		 (write-char c *out*)
		 (cond (*in-qtd-tkn* (set! *in-qtd-tkn* #f)
				     (set! *in-bktd-qtd-exp* 1))
		       ((> *in-bktd-qtd-exp* 0)
			(set! *in-bktd-qtd-exp* (+ *in-bktd-qtd-exp* 1))))
		 (cond (*in-mac-tkn* (set! *in-mac-tkn* #f)
				     (set! *in-bktd-mac-exp* 1))
		       ((> *in-bktd-mac-exp* 0) ;is this possible?
			(set! *in-bktd-mac-exp* (+ *in-bktd-mac-exp* 1))))
		 (if (not (null? *bq-stack*))
		     (let ((top (car *bq-stack*)))
		       (cond ((of top =in-bq-tkn)
			      (setf (of top =in-bq-tkn) #f)
			      (setf (of top =in-bktd-bq-exp) 1))
			     ((> (of top =in-bktd-bq-exp) 0)
			      (setf (of top =in-bktd-bq-exp)
				    (+ (of top =in-bktd-bq-exp) 1))))))
		 (if (not (null? *case-stack*))
		     (let ((top (car *case-stack*)))
		       (cond ((of top =in-ctag-tkn)
			      (setf (of top =in-ctag-tkn) #f)
			      (setf (of top =in-bktd-ctag-exp) 1))
			     ((> (of top =in-bktd-ctag-exp) 0)
			      (setf (of top =in-bktd-ctag-exp)
				    (+ (of top =in-bktd-ctag-exp) 1)))
			     ((> (of top =in-case-exp) 0)
			      (setf (of top =in-case-exp)
				    (+ (of top =in-case-exp) 1))
			      (if (= (of top =in-case-exp) 2)
				  (set! *in-qtd-tkn* #t))))))
		 (loop (+ i 1)))
		((memv c '(#\) #\]))
		 (display-tab (of line =tab / i) *out*)
		 (write-char c *out*)
		 (if (> *in-bktd-qtd-exp* 0)
		     (set! *in-bktd-qtd-exp* (- *in-bktd-qtd-exp* 1)))
		 (if (> *in-bktd-mac-exp* 0)
		     (set! *in-bktd-mac-exp* (- *in-bktd-mac-exp* 1)))
		 (if (not (null? *bq-stack*))
		     (let ((top (car *bq-stack*)))
		       (if (> (of top =in-bktd-bq-exp) 0)
			   (begin
                             (setf (of top =in-bktd-bq-exp)
				   (- (of top =in-bktd-bq-exp) 1))
			     (if (= (of top =in-bktd-bq-exp) 0)
				 (set! *bq-stack* (cdr *bq-stack*)))))))
		 (let loop ()
		   (if (not (null? *case-stack*))
		       (let ((top (car *case-stack*)))
			 (cond ((> (of top =in-bktd-ctag-exp) 0)
				(setf (of top =in-bktd-ctag-exp)
				      (- (of top =in-bktd-ctag-exp) 1))
				(if (= (of top =in-bktd-ctag-exp) 0)
				    (setf (of top =in-case-exp) 1)))
			       ((> (of top =in-case-exp) 0)
				(setf (of top =in-case-exp)
				      (- (of top =in-case-exp) 1))
				(if (= (of top =in-case-exp) 0)
				    (begin
                                      (set! *case-stack* (cdr *case-stack*))
				      (loop))))))))
		 (loop (+ i 1)))
		(else (display-tab (of line =tab / i) *out*)
		      (loop (do-token line i))))))))


(define do-all-lines
  (lambda ()
    ;;process all lines, adjusting each adjacent pair
    (let loop ((line1 *line1*) (line2 *line2*))
      (let* ((line2-paragraph? *latex-paragraph-mode?*)
	     (more? (get-line line1)))
	;;
	(peephole-adjust line1 line2)
	;;
	((if line2-paragraph?
	     display-tex-line
	     display-scm-line) line2)
	;;
	(if (not (eq? line2-paragraph? *latex-paragraph-mode?*))
	    ((if *latex-paragraph-mode?*
		 display-end-sequence
		 display-begin-sequence) *out*))
	;;
	(if more? (loop line2 line1))))))

;scheme2tex is the "interface" procedure supplied by this file --
;it takes Scheme code from inport and produces LaTeX source for same
;in outport

(define scheme2tex
    (lambda (inport outport)
      ;create a typeset version of scheme code from inport
      ;;in outport;
      ;;local setting of keywords, etc.?
      (set! *in* inport)
      (set! *out* outport)
      (set! *latex-paragraph-mode?* #t)
      (set! *in-qtd-tkn* #f)
      (set! *in-bktd-qtd-exp* 0)
      (set! *in-mac-tkn* #f)
      (set! *in-bktd-mac-exp* 0)
      (set! *case-stack* '())
      (set! *bq-stack* '())
      (let ((flush-line ;needed anywhere else?
	     (lambda (line)
	       (setf (of line =rtedge) 0)
	       (setf (of line =char / 0) #\newline)
	       (setf (of line =space / 0) &void-space)
	       (setf (of line =tab / 0) &void-tab)
	       (setf (of line =notab / 0) &void-notab))))
	(flush-line *line1*)
	(flush-line *line2*))
      (do-all-lines)))

;codeset.scm


;display-tex-line

;display-scm-line



(define do-token
    (let ((token-delims (list #\( #\) #\[ #\] #\space *return*
                              #\" #\' #\`
			      #\newline #\, #\;)))
      (lambda (line i)
	(let loop ((buf '()) (i i))
	  (let ((c (of line =char / i)))
	    (cond ((char=? c #\\ )
		   (loop (cons (of line =char / (+ i 1)) (cons c buf))
			 (+ i 2)))
		  ((or (memv c token-delims)
		       (memv c *math-triggerers*))
		   (output-token (list->string (reverse! buf)))
		   i)
		  ((char? c) (loop (cons (of line =char / i) buf) (+ i 1)))
		  (else (slatex-error 'do-token "token contains non-char?"
			       c))))))))

(define output-token
    (lambda (token)
      (if (not (null? *case-stack*))
	  (let ((top (car *case-stack*)))
	    (if (of top =in-ctag-tkn)
		(begin
                  (setf (of top =in-ctag-tkn) #f)
		  (setf (of top =in-case-exp) 1)))))
      (if (lassoc token special-symbols token=?)
	  (begin
            (if *in-qtd-tkn* (set! *in-qtd-tkn* #f)
                (if *in-mac-tkn* (set! *in-mac-tkn* #f)))
	    (display (cdr (lassoc token special-symbols token=?))
		     *out*))
	  (display-token
	   token
	   (cond (*in-qtd-tkn*
		  (set! *in-qtd-tkn* #f)
		  (cond ((equal? token "else") 'syntax)
			((lmember token data-tokens token=?) 'data)
			((lmember token constant-tokens token=?)
			 'constant)
			((lmember token variable-tokens token=?)
			 'constant)
			((lmember token keyword-tokens token=?)
			 'constant)
			((prim-data-token? token) 'data)
			(else 'constant)))
		 ((> *in-bktd-qtd-exp* 0) 'constant)
		 ((and (not (null? *bq-stack*))
		       (not (of (car *bq-stack*) =in-comma))) 'constant)
		 (*in-mac-tkn* (set! *in-mac-tkn* #f)
			       (set-keyword token) 'syntax)
		 ((> *in-bktd-mac-exp* 0) (set-keyword token) 'syntax)
		 ((lmember token data-tokens token=?) 'data)
		 ((lmember token constant-tokens token=?) 'constant)
		 ((lmember token variable-tokens token=?) 'variable)
		 ((lmember token keyword-tokens token=?)
		  (cond ((token=? token "quote") (set! *in-qtd-tkn* #t))
			((lmember token macro-definers token=?)
			 (set! *in-mac-tkn* #t))
			((lmember token case-and-ilk token=?)
			 (set! *case-stack*
			   (cons (let ((f (make-case-frame)))
				   (setf (of f =in-ctag-tkn) #t)
				   (setf (of f =in-bktd-ctag-exp) 0)
				   (setf (of f =in-case-exp) 0)
				   f)
				 *case-stack*))))
		  'syntax)
		 ((prim-data-token? token) 'data)
		 (else 'variable))
	   *out*))
      (if (and (not (null? *bq-stack*)) (of (car *bq-stack*) =in-bq-tkn))
	  (set! *bq-stack* (cdr *bq-stack*)))))

;pathproc.scm



(define directory-namestring
      (lambda (f)
	(let ((p (string-position-right *directory-mark* f)))
	  (if p
	      (substring f 0 (+ p 1)) ""))))

(define basename
      (lambda (f)
	(let ((p (string-position-right *directory-mark* f)))
	  (if p
	      (set! f (substring f (+ p 1) (string-length f))))
	  (let ((p (string-position-right #\. f)))
	    (if p
		(substring f 0 p)
		f)))))


(define *texinputs* "")

(define *texinputs-list* #f)

(define *path-separator*
    (cond ((eq? *operating-system* 'unix) #\:)
          ((eq? *operating-system* 'mac-os) (integer->char 0))
	  ((memq *operating-system* '(windows os2 dos os2fat)) #\;)
	  (else (slatex-error "Couldn't determine path separator character."))))

(define *directory-mark*
    (cond ((eq? *operating-system* 'unix) #\/)
          ((eq? *operating-system* 'mac-os) #\:)
	  ((memq *operating-system* '(windows os2 dos os2fat)) #\\)
	  (else (slatex-error "Couldn't determine directory mark."))))

(define *directory-mark-string*
    (list->string (list *directory-mark*)))

(define *file-hider*
    (cond ((memq *operating-system* '(windows os2 unix mac-os)) ".")
	  ((memq *operating-system* '(dos os2fat)) "x") ;no such luck for dos & os2fat
	  (else "."))) ;use any old character

(define path-to-list
    (lambda (p)
      ;convert a unix or dos representation of a path to a list of
      ;directory names (strings)
      (let loop ((p (string->list p)) (r (list "")))
	(let ((separator-pos (position-char *path-separator* p)))
	  (if separator-pos
	      (loop (list-tail p (+ separator-pos 1))
		    (cons (list->string (sublist p 0 separator-pos))
			  r))
	      (reverse! (cons (list->string p) r)))))))

(define find-some-file
    (lambda (path . files)
      ;look through each directory in path till one of files is found
      (let loop ((path path))
	(if (null? path) #f
	    (let ((dir (car path)))
	      (let loop1 ((files
			   (if (or (string=? dir "") (string=? dir "."))
			       files
			       (map (lambda (file)
				      (string-append dir
						     *directory-mark-string*
						     file)) files))))
		(if (null? files) (loop (cdr path))
		    (let ((file (car files)))
		      (if (file-exists? file) file
			  (loop1 (cdr files)))))))))))

(define file-extension
    (lambda (filename)
      ;;find extension of filename
      (let ((i (string-position-right #\. filename)))
	(if i (substring filename i (string-length filename))
	    #f))))

(define full-texfile-name
    (lambda (filename)
      ;;find the full pathname of the .tex/.sty file filename
      (let ((extn (file-extension filename)))
	(if (and extn (or (string=? extn ".sty") (string=? extn ".tex")))
	    (find-some-file *texinputs-list* filename)
	    (find-some-file *texinputs-list*
			    (string-append filename ".tex") filename)))))

(define full-styfile-name
    (lambda (filename)
      ;;find the full pathname of the .sty file filename
      (find-some-file *texinputs-list*
		      (string-append filename ".sty"))))

(define full-clsfile-name
    (lambda (filename)
      ;;find the full pathname of the .cls file filename
      (find-some-file *texinputs-list*
		      (string-append filename ".cls"))))

(define full-scmfile-name
    (lambda (filename)
      ;find the full pathname of the scheme file filename;
      ;acceptable extensions are .scm .ss .s
      (apply find-some-file *texinputs-list*
	     filename
	     (map (lambda (extn) (string-append filename extn))
		  '(".scm" ".ss" ".s")))))

(define subjobname 'fwd)

(define primary-aux-file-count -1)

(define new-primary-aux-file
    (lambda (e)
      ;used by new-aux-file unless in protected region;
      ;this is the default
      (set! primary-aux-file-count
	(+ primary-aux-file-count 1))
      (string-append *tex-calling-directory*
	     *file-hider* "Z"
	     (number->string primary-aux-file-count)
	     subjobname e)))

(define new-secondary-aux-file
    (let ((n -1))
      (lambda (e)
	;used by new-aux-file when in protected region
	(set! n (+ n 1))
	(string-append *tex-calling-directory*
	       *file-hider*
	       "ZZ" (number->string n) subjobname e))))

(define new-aux-file
  (lambda e
    (let ((e (if (pair? e) (car e) "")))
      ;;create a new auxiliary file with provided extension if any
      ((if *slatex-in-protected-region?*
           new-secondary-aux-file
           new-primary-aux-file)
       e))))

;texread.scm




(define eat-till-newline
    (lambda (in)
      ;;skip all characters from port in till newline inclusive or eof
      (let loop ()
	(let ((c (read-char in)))
	  (cond ((eof-object? c) 'done)
		((char=? c #\newline) 'done)
		(else (loop)))))))

(define read-ctrl-seq
  (lambda (in)
    ;;assuming we've just read a backslash, read the remaining
    ;;part of a latex control sequence from port in
    (let ((c (read-char in)))
      (if (eof-object? c)
	  (slatex-error "read-ctrl-exp: \\ followed by eof."))
      (if (char-alphabetic? c)
	  (list->string
	   (reverse!
	    (let loop ((s (list c)))
	      (let ((c (peek-char in)))
		(cond ((eof-object? c) s)
		      ((char-alphabetic? c) (read-char in)
		       (loop (cons c s)))
		      ((char=? c #\%) (eat-till-newline in)
		       (loop s))
		      (else s))))))
	  (string c)))))

(define eat-tabspace
    (lambda (in)
      ;;skip to the next non-space and non-tab character from port in
      (let loop ()
	(let ((c (peek-char in)))
	  (cond ((eof-object? c) 'done)
		((or (char=? c #\space) (char=? c *tab*))
		 (read-char in) (loop))
		(else 'done))))))

(define eat-whitespace
    (lambda (in)
      ;skip to the next whitespace character from port in
      (let loop ()
	(let ((c (peek-char in)))
	  (cond ((eof-object? c) 'done)
		((char-whitespace? c)
		 (read-char in) (loop))
		(else 'done))))))

(define eat-tex-whitespace
    (lambda (in)
      ;;skip to the next whitespace character from port in;
      ;;skips past latex comments too
      (let loop ()
	(let ((c (peek-char in)))
	  (cond ((eof-object? c) 'done)
		((char-whitespace? c) (read-char in) (loop))
		((char=? c #\%) (eat-till-newline in))
		(else 'done))))))

(define chop-off-whitespace
    (lambda (l)
      ;;removes leading whitespace from character-list l
      (ormapcdr (lambda (d) (if (char-whitespace? (car d)) #f d)) l)))

(define read-grouped-latexexp
  (lambda (in)
    ;;reads a latex grouped expression from port in
    ;;(removes the groups)
    (eat-tex-whitespace in)
    (let ((c (read-char in)))
      (if (eof-object? c) (slatex-error "read-grouped-latexexp: ~
Expected { but found eof."))
      (if (not (char=? c #\{))
	  (slatex-error "read-grouped-latexexp: ~
Expected { but found ~a." c))
      (eat-tex-whitespace in)
      (list->string
       (reverse!
	(chop-off-whitespace
	 (let loop ((s '()) (nesting 0) (escape? #f))
	   (let ((c (read-char in)))
	     (if (eof-object? c) (slatex-error "read-groupted-latexexp: ~
Found eof inside {...}."))
	     (cond (escape? (loop (cons c s) nesting #f))
		   ((char=? c #\\)
		    (loop (cons c s) nesting #t))
		   ((char=? c #\%) (eat-till-newline in)
		    (loop s nesting #f))
		   ((char=? c #\{)
		    (loop (cons c s) (+ nesting 1) #f))
		   ((char=? c #\})
		    (if (= nesting 0) s
			(loop (cons c s) (- nesting 1) #f)))
		   (else
		    (loop (cons c s) nesting #f)))))))))))

(define read-filename
    (let ((filename-delims (list #\{ #\} #\[ #\] #\( #\) #\# #\% #\\ #\,
				 #\space *return* #\newline *tab* #\\)))
      (lambda (in)
	;reads a filename as allowed in latex syntax from port in
	(eat-tex-whitespace in)
	(let ((c (peek-char in)))
	  (if (eof-object? c) (slatex-error "read-filename: ~
Expected filename but found eof."))
	  (if (char=? c #\{) (read-grouped-latexexp in)
	      (list->string
	       (reverse!
		(let loop ((s '()) (escape? #f))
		  (let ((c (peek-char in)))
		    (cond ((eof-object? c)
			   (if escape? (slatex-error "read-filename: ~
\\ followed by eof.")
			       s))
			  (escape? (read-char in)
				   (loop (cons c s) #f))
			  ((char=? c #\\) (read-char in)
					  (loop (cons c s) #t))
			  ((memv c filename-delims) s)
			  (else (read-char in)
				(loop (cons c s) #f))))))))))))

(define read-schemeid
    (let ((schemeid-delims (list #\{ #\} #\[ #\] #\( #\)
				 #\space *return* #\newline *tab*)))
      (lambda (in)
	;;reads a scheme identifier from port in
	(eat-whitespace in)
	(list->string
	 (reverse!
	  (let loop ((s '()) (escape? #f))
	    (let ((c (peek-char in)))
	      (cond ((eof-object? c) s)
		    (escape? (read-char in) (loop (cons c s) #f))
		    ((char=? c #\\) (read-char in)
				    (loop (cons c s) #t))
		    ((memv c schemeid-delims) s)
		    (else (read-char in) (loop (cons c s) #f))))))))))

(define read-delimed-commaed-filenames
  (lambda (in lft-delim rt-delim)
    ;;reads a filename from port in, assuming it's delimited by
    ;;lft- and rt-delims
    (eat-tex-whitespace in)
    (let ((c (read-char in)))
      (if (eof-object? c) (slatex-error "read-delimed-commaed-filenames: ~
Expected filename(s) but found eof."))
      (if (not (char=? c lft-delim))
	  (slatex-error "read-delimed-commaed-filenames: ~
Left delimiter ~a not found." lft-delim))
      (let loop ((s '()))
	(eat-tex-whitespace in)
	(let ((c (peek-char in)))
	  (if (eof-object? c) (slatex-error "read-delimed-commaed-filenames: ~
Found eof inside filename(s)."))
	  (if (char=? c rt-delim)
	      (begin (read-char in) (reverse! s))
	      (let ((s (cons (read-filename in) s)))
		(eat-tex-whitespace in)
		(let ((c (peek-char in)))
		  (if (eof-object? c)
		      (slatex-error "read-delimed-commaed-filenames: ~
Found eof inside filename(s)."))
		  (cond
		   ((char=? c #\,) (read-char in))
		   ((char=? c rt-delim) (void))
		   (else (slatex-error "read-delimed-commaed-filenames: ~
Bad filename(s) syntax.")))
		  (loop s)))))))))

(define read-grouped-commaed-filenames
    (lambda (in)
      ;read a filename from port in, assuming it's grouped
      (read-delimed-commaed-filenames in #\{ #\})))

(define read-bktd-commaed-filenames
    (lambda (in)
      ;read a filename from port in, assuming it's bracketed
      (read-delimed-commaed-filenames in #\[ #\])))

(define read-grouped-schemeids
    (lambda (in)
      ;read a list of scheme identifiers from port in,
      ;assuming they're all grouped
      (eat-tex-whitespace in)
      (let ((c (read-char in)))
	(if (eof-object? c) (slatex-error "read-grouped-schemeids: ~
Expected Scheme identifiers but found eof."))
	(if (not (char=? c #\{)) (slatex-error "read-grouped-schemeids: ~
Expected { but found ~a." c))
	(let loop ((s '()))
	  (eat-whitespace in)
	  (let ((c (peek-char in)))
	    (if (eof-object? c) (slatex-error "read-grouped-schemeids:
Found eof inside Scheme identifiers."))
	    (if (char=? c #\})
		(begin (read-char in) (reverse! s))
		(loop (cons (read-schemeid in) s))))))))

(define eat-delimed-text
    (lambda (in lft-delim rt-delim)
      (eat-tex-whitespace in)
      (let ((c (peek-char in)))
	(if (eof-object? c) 'exit
	    (if (char=? c lft-delim)
		(let loop ()
		  (let ((c (read-char in)))
		    (if (eof-object? c) 'exit
			(if (char=? c rt-delim) 'exit
			    (loop))))))))))

(define eat-bktd-text
    (lambda (in)
      (eat-delimed-text in #\[ #\])))

(define eat-grouped-text
    (lambda (in)
      (eat-delimed-text in #\{ #\})))


;proctex.scm


(define ignore2
      (lambda (i ii)
	;ignores its two arguments
	'void))

	

(define disable-slatex-temply
    (lambda (in)
      ;tell slatex that it should not process slatex commands till
      ;the enabling control sequence is called
      (set! *slatex-enabled?* #f)
      (set! *slatex-reenabler* (read-grouped-latexexp in))))

(define enable-slatex-again
    (lambda ()
      ;tell slatex to resume processing slatex commands
      (set! *slatex-enabled?* #t)
      (set! *slatex-reenabler* "UNDEFINED")))

(define add-to-slatex-db
    (lambda (in categ)
      ;;some scheme identifiers to be added to the token category categ
      (if (memq categ '(keyword constant variable))
	  (add-to-slatex-db-basic in categ)
	  (add-to-slatex-db-special in categ))))

(define add-to-slatex-db-basic
    (lambda (in categ)
      ;read the following scheme identifiers and add them to the
      ;token category categ
      (let ((setter (cond ((eq? categ 'keyword) set-keyword)
			  ((eq? categ 'constant) set-constant)
			  ((eq? categ 'variable) set-variable)
			  (else (slatex-error "add-to-slatex-db-basic: ~
Unknown category ~s." categ))))
	    (ids (read-grouped-schemeids in)))
	(for-each setter ids))))

(define add-to-slatex-db-special
    (lambda (in what)
      ;read the following scheme identifier(s) and either
      ;enable/disable its special-symbol status
      (let ((ids (read-grouped-schemeids in)))
	(cond ((eq? what 'unsetspecialsymbol)
	       (for-each unset-special-symbol ids))
	      ((eq? what 'setspecialsymbol)
	       (if (not (= (length ids) 1))
		   (slatex-error "add-to-slatex-db-special: ~
\\setspecialsymbol takes one arg exactly."))
	       (let ((transl (read-grouped-latexexp in)))
		 (set-special-symbol (car ids) transl)))
	      (else (slatex-error "add-to-slatex-db-special: ~
Unknown command ~s." what))))))

(define process-slatex-alias
    (lambda (in what which)
      ;add/remove a slatex control sequence name
      (let ((triggerer (read-grouped-latexexp in)))
	(case which
	  ((intext)
	   (set! *intext-triggerers*
		 (what triggerer *intext-triggerers*
			  string=?)))
	  ((resultintext)
	   (set! *resultintext-triggerers*
		 (what triggerer *resultintext-triggerers*
			  string=?)))
	  ((display)
	   (set! *display-triggerers*
		 (what triggerer *display-triggerers*
			  string=?)))
	  ((response)
	   (set! *response-triggerers*
		 (what triggerer *response-triggerers*
			  string=?)))
	  ((respbox)
	   (set! *respbox-triggerers*
		 (what triggerer *respbox-triggerers*
			  string=?)))
	  ((box)
	   (set! *box-triggerers*
		 (what triggerer *box-triggerers*
			  string=?)))
	  ((input)
	   (set! *input-triggerers*
		 (what triggerer *input-triggerers*
			  string=?)))
	  ((region)
	   (set! *region-triggerers*
		 (what triggerer *region-triggerers*
			  string=?)))
	  ((mathescape)
	   (if (not (= (string-length triggerer) 1))
	       (slatex-error "process-slatex-alias: ~
Math escape should be character."))
	   (set! *math-triggerers*
		 (what (string-ref triggerer 0)
			  *math-triggerers* char=?)))
	  (else (slatex-error "process-slatex-alias:
Unknown command ~s." which))))))

(define decide-latex-or-tex
    (lambda (latex?)
      ;create a junk file if the file is in plain tex rather
      ;than latex; this is used afterward to call the right
      ;command, i.e., latex or tex
      (set! *latex?* latex?)
      (let ((pltexchk.jnk "pltexchk.jnk"))
	(if (file-exists? pltexchk.jnk) (delete-file pltexchk.jnk))
	(if (not *latex?*)
	    (call-with-output-file pltexchk.jnk
	      (lambda (outp)
		(display 'junk outp)
		(newline outp)))))))

(define process-include-only
    (lambda (in)
      ;remember the files mentioned by \includeonly
      (set! *include-onlys* '())
      (for-each
       (lambda (filename)
	 (let ((filename (full-texfile-name filename)))
	   (if filename
	       (set! *include-onlys*
		     (adjoin filename *include-onlys*
			     string=?)))))
       (read-grouped-commaed-filenames in))))

(define process-documentstyle
    (lambda (in)
      ;process the .sty files corresponding to the documentstyle options
      (eat-tex-whitespace in)
      (if (char=? (peek-char in) #\[)
	  (for-each
	   (lambda (filename)
	     (fluid-let ((*slatex-in-protected-region?* #f))
	       (process-tex-file
		(string-append filename ".sty"))))
	   (read-bktd-commaed-filenames in)))))

(define process-documentclass
    (lambda (in)
      (eat-bktd-text in)
      (eat-grouped-text in)))

(define process-case-info
    (lambda (in)
      ;find out and tell slatex if the scheme tokens that differ
      ;only by case should be treated identical or not
      (let ((bool (read-grouped-latexexp in)))
	(set! *slatex-case-sensitive?*
	      (cond ((string-ci=? bool "true") #t)
		    ((string-ci=? bool "false") #f)
		    (else (slatex-error "process-case-info: ~
\\schemecasesensitive's arg should be true or false.")))))))

(define seen-first-command? #f)

(define process-main-tex-file
    (lambda (filename)
      ;kick off slatex on the main .tex file filename
      (display "SLaTeX v. ")
      (display *slatex-version*)
      (newline)
      (set! primary-aux-file-count -1)
      (set! *slatex-separate-includes?* #f)
      (if (or (not *texinputs-list*) (null? *texinputs-list*))
          (set! *texinputs-list*
            (if *texinputs* (path-to-list *texinputs*)
                '(""))))
      (let ((file-hide-file "xZfilhid.tex"))
	(if (file-exists? file-hide-file) (delete-file file-hide-file))
	(if (memq *operating-system* '(dos os2fat))
	    (call-with-output-file file-hide-file
	      (lambda (out)
		(display "\\def\\filehider{x}" out)
		(newline out)))))
      (display "typesetting code")
      (set! *tex-calling-directory* (directory-namestring filename))
      (set! subjobname (basename filename))
      (set! seen-first-command? #f)
      (process-tex-file filename)
      (display "done")
      (newline)))

(define dump-intext
    (lambda (in out)
      (let* ((write-char (if out write-char ignore2))
	     (delim-char (begin (eat-whitespace in) (read-char in)))
	     (delim-char
	      (cond ((char=? delim-char #\{) #\})
		    (else delim-char))))
	(if (eof-object? delim-char)
	    (slatex-error "dump-intext: Expected delimiting character ~
but found eof."))
	(let loop ()
	  (let ((c (read-char in)))
	    (if (eof-object? c)
		(slatex-error "dump-intext: Found eof inside Scheme code."))
	    (if (char=? c delim-char) 'done
		(begin (write-char c out) (loop))))))))

(define dump-display
  (lambda (in out ender)
    (eat-tabspace in)
    (let ((write-char (if out write-char ignore2))
	  (ender-lh (string-length ender)) (c (peek-char in)))
      (if (eof-object? c)
	  (slatex-error "dump-display: Found eof inside displayed code."))
      (if (char=? c #\newline) (read-char in))
      (let loop ((i 0))
	(if (= i ender-lh) 'done
	    (let ((c (read-char in)))
	      (if (eof-object? c)
		  (slatex-error "dump-display: Found eof inside displayed code."))
	      (if (char=? c (string-ref ender i))
		  (loop (+ i 1))
		  (let loop2 ((j 0))
		    (if (< j i)
			(begin
			  (write-char (string-ref ender j) out)
			  (loop2 (+ j 1)))
			(begin
			  (write-char c out)
			  (loop 0)))))))))))

;proctex2.scm


(define debug? #f)

(define process-tex-file
  (lambda (raw-filename)
					;call slatex on the .tex file raw-filename
    (if debug?
	(begin (display "begin ")
	       (display raw-filename)
	       (newline)))
    (let ((filename (full-texfile-name raw-filename)))
      (if (not filename)		;didn't find it
	  (begin (display "[")
		 (display raw-filename)
		 (display "]") (flush-output))
	  (call-with-input-file filename
	    (lambda (in)
	      (let ((done? #f))
		(let loop ()
		  (if done? 'exit-loop
		      (begin
			(let ((c (read-char in)))
			  (cond
			   ((eof-object? c) (set! done? #t))
			   ((char=? c #\%) (eat-till-newline in))
			   ((char=? c #\\)
			    (let ((cs (read-ctrl-seq in)))
			      (if (not seen-first-command?)
				  (begin
				    (set! seen-first-command? #t)
				    (decide-latex-or-tex
				     (or
				      (string=? cs "documentstyle")
				      (string=? cs "documentclass")
				      (string=? cs "NeedsTeXFormat")
				      ))))
			      (cond
			       ((not *slatex-enabled?*)
				(if (string=? cs *slatex-reenabler*)
				    (enable-slatex-again)))
			       ((string=? cs "slatexignorecurrentfile")
				(set! done? #t))
			       ((string=? cs "slatexseparateincludes")
				(if *latex?*
				    (set! *slatex-separate-includes?* #t)))
			       ((string=? cs "slatexdisable")
				(disable-slatex-temply in))
			       ((string=? cs "begin")
				(eat-tex-whitespace in)
				(if (eqv? (peek-char in) #\{)
				    (let ((cs (read-grouped-latexexp in)))
				      (cond
				       ((member cs *display-triggerers*)
					(trigger-scheme2tex
					 'envdisplay in cs))
				       ((member cs *response-triggerers*)
					(trigger-scheme2tex 'envresponse
							    in cs))
				       ((member cs *respbox-triggerers*)
					(trigger-scheme2tex 'envrespbox
							    in cs))
				       ((member cs *box-triggerers*)
					(trigger-scheme2tex 'envbox
							    in cs))
				       ((member cs *topbox-triggerers*)
					(trigger-scheme2tex 'envtopbox
							    in cs))
				       ((member cs *region-triggerers*)
					(trigger-region
					 'envregion in cs))))))
			       ((member cs *intext-triggerers*)
				(trigger-scheme2tex 'intext in #f))
			       ((member cs *resultintext-triggerers*)
				(trigger-scheme2tex 'resultintext in #f))
			       ((member cs *display-triggerers*)
				(trigger-scheme2tex 'plaindisplay
						    in cs))
			       ((member cs *response-triggerers*)
				(trigger-scheme2tex 'plainresponse
						    in cs))
			       ((member cs *respbox-triggerers*)
				(trigger-scheme2tex 'plainrespbox
						    in cs))
			       ((member cs *box-triggerers*)
				(trigger-scheme2tex 'plainbox
						    in cs))
			       ((member cs *topbox-triggerers*)
				(trigger-scheme2tex 'plaintopbox
						    in cs))
			       ((member cs *region-triggerers*)
				(trigger-region 'plainregion
						in cs))
			       ((member cs *input-triggerers*)
				(process-scheme-file
				 (read-filename in)))
			       ((string=? cs "input")
				(let ((f (read-filename in)))
				  (if (not (string=? f ""))
				      (fluid-let
					  ((*slatex-in-protected-region?*
					    #f))
					(process-tex-file f)))))
			       ((string=? cs "usepackage")
				(fluid-let ((*slatex-in-protected-region?*
					     #f))
				  (process-tex-file
				   (string-append (read-filename in)
                                                  ".sty"))))
			       ((string=? cs "include")
				(if *latex?*
				    (let ((f (full-texfile-name
					      (read-filename in))))
				      (if (and f
					       (or (eq? *include-onlys* 'all)
						   (member f
							   *include-onlys*)))
					  (fluid-let
					      ((*slatex-in-protected-region?*
						#f))
					    (if *slatex-separate-includes?*
						(fluid-let
						    ((subjobname
						      (basename f))
						     (primary-aux-file-count
						      -1))
						  (process-tex-file f))
						(process-tex-file f)))))))
			       ((string=? cs "includeonly")
				(if *latex?* (process-include-only in)))
			       ((string=? cs "documentstyle")
				(if *latex?* (process-documentstyle in)))
			       ((string=? cs "documentclass")
				(if *latex?* (process-documentclass in)))
			       ((string=? cs "schemecasesensitive")
				(process-case-info in))
			       ((string=? cs "defschemetoken")
				(process-slatex-alias
				 in adjoin
				 'intext))
			       ((string=? cs "undefschemetoken")
				(process-slatex-alias
				 in delete
				 'intext))
			       ((string=? cs "defschemeresulttoken")
				(process-slatex-alias
				 in adjoin
				 'resultintext))
			       ((string=? cs "undefschemeresulttoken")
				(process-slatex-alias
				 in delete
				 'resultintext))
			       ((string=? cs "defschemeresponsetoken")
				(process-slatex-alias
				 in adjoin
				 'response))
			       ((string=? cs "undefschemeresponsetoken")
				(process-slatex-alias
				 in delete
				 'response))
			       ((string=? cs "defschemeresponseboxtoken")
				(process-slatex-alias
				 in adjoin
				 'respbox))
			       ((string=? cs "undefschemeresponseboxtoken")
				(process-slatex-alias
				 in delete
				 'respbox))
			       ((string=? cs "defschemedisplaytoken")
				(process-slatex-alias
				 in adjoin
				 'display))
			       ((string=? cs "undefschemedisplaytoken")
				(process-slatex-alias
				 in delete
				 'display))
			       ((string=? cs "defschemeboxtoken")
				(process-slatex-alias
				 in adjoin
				 'box))
			       ((string=? cs "undefschemeboxtoken")
				(process-slatex-alias
				 in delete
				 'box))
			       ((string=? cs "defschemetopboxtoken")
				(process-slatex-alias
				 in adjoin
				 'topbox))
			       ((string=? cs "undefschemetopboxtoken")
				(process-slatex-alias
				 in delete
				 'topbox))
			       ((string=? cs "defschemeinputtoken")
				(process-slatex-alias
				 in adjoin
				 'input))
			       ((string=? cs "undefschemeinputtoken")
				(process-slatex-alias
				 in delete
				 'input))
			       ((string=? cs "defschemeregiontoken")
				(process-slatex-alias
				 in adjoin
				 'region))
			       ((string=? cs "undefschemeregiontoken")
				(process-slatex-alias in
						      delete
						      'region))
			       ((string=? cs "defschememathescape")
				(process-slatex-alias in
						      adjoin
						      'mathescape))
			       ((string=? cs "undefschememathescape")
				(process-slatex-alias in
						      delete
						      'mathescape))
			       ((string=? cs "setkeyword")
				(add-to-slatex-db in 'keyword))
			       ((string=? cs "setconstant")
				(add-to-slatex-db in 'constant))
			       ((string=? cs "setvariable")
				(add-to-slatex-db in 'variable))
			       ((string=? cs "setspecialsymbol")
				(add-to-slatex-db in 'setspecialsymbol))
			       ((string=? cs "unsetspecialsymbol")
				(add-to-slatex-db in 'unsetspecialsymbol))
			       )))))
			(loop)))))))))
    (if debug?
	(begin (display "end ")
	       (display raw-filename)
	       (newline)))
    ))

(define process-scheme-file
    (lambda (raw-filename)
      ;typeset the scheme file raw-filename so that it can
      ;be input as a .tex file
      (let ((filename (full-scmfile-name raw-filename)))
	(if (not filename)
	    (begin (display "process-scheme-file: ")
		   (display raw-filename)
		   (display " doesn't exist")
		   (newline))
	    (let ((aux.tex (new-aux-file ".tex")))
	      (display ".") (flush-output)
	      (if (file-exists? aux.tex) (delete-file aux.tex))
	      (call-with-input-file filename
		(lambda (in)
		  (call-with-output-file aux.tex
		    (lambda (out)
		      (fluid-let ((*intext?* #f)
				  (*code-env-spec* "ZZZZschemedisplay"))
			(scheme2tex in out))))))
	      (if *slatex-in-protected-region?*
		  (set! *protected-files* (cons aux.tex *protected-files*)))
	      (process-tex-file filename))))))

(define trigger-scheme2tex
    (lambda (typ in env)
      ;process the slatex command identified by typ;
      ;env is the name of the environment
      (let* ((aux (new-aux-file)) (aux.scm (string-append aux ".scm"))
             (aux.tex (string-append aux ".tex")))
        (if (file-exists? aux.scm) (delete-file aux.scm))
        (if (file-exists? aux.tex) (delete-file aux.tex))
        (display ".") (flush-output)
        (call-with-output-file aux.scm
          (lambda (out)
            (cond ((memq typ '(intext resultintext)) (dump-intext in out))
                  ((memq typ '(envdisplay envresponse envrespbox envbox
                                envtopbox))
                   (dump-display in out (string-append "\\end{" env "}")))
                  ((memq typ '(plaindisplay plainresponse
                                plainrespbox plainbox plaintopbox))
                   (dump-display in out (string-append "\\end" env)))
                  (else (slatex-error "trigger-scheme2tex: ~
                          Unknown triggerer ~s." typ)))))
        (call-with-input-file aux.scm
          (lambda (in)
            (call-with-output-file aux.tex
              (lambda (out)
                (fluid-let
                  ((*intext?* (memq typ '(intext resultintext)))
                   (*code-env-spec*
                     (cond ((eq? typ 'intext) "ZZZZschemecodeintext")
                           ((eq? typ 'resultintext)
                            "ZZZZschemeresultintext")
                           ((memq typ '(envdisplay plaindisplay))
                            "ZZZZschemedisplay")
                           ((memq typ '(envresponse plainresponse))
                            "ZZZZschemeresponse")
                           ((memq typ '(envrespbox plainrespbox))
                            "ZZZZschemeresponsebox")
                           ((memq typ '(envbox plainbox))
                            "ZZZZschemebox")
                           ((memq typ '(envtopbox plaintopbox))
                            "ZZZZschemetopbox")
                           (else (slatex-error "trigger-scheme2tex: ~
                                   Unknown triggerer ~s." typ)))))
                  (scheme2tex in out))))))
        (if *slatex-in-protected-region?*
            (set! *protected-files* (cons aux.tex *protected-files*)))
        (if (memq typ '(envdisplay plaindisplay envbox plainbox
                         envtopbox plaintopbox))
            (process-tex-file aux.tex))
        (delete-file aux.scm)
        )))

(define trigger-region
    (lambda (typ in env)
      ;process a scheme region to create a in-lined file with
      ;slatex output
      (let ((aux.tex (new-primary-aux-file ".tex"))
	    (aux2.tex (new-secondary-aux-file ".tex")))
	(if (file-exists? aux2.tex) (delete-file aux2.tex))
	(if (file-exists? aux.tex) (delete-file aux.tex))
	(display ".") (flush-output)
	(fluid-let ((*slatex-in-protected-region?* #t)
		    (*protected-files* '()))
	  (call-with-output-file aux2.tex
	    (lambda (out)
	      (cond ((eq? typ 'envregion)
		     (dump-display in out (string-append "\\end{" env "}")))
		    ((eq? typ 'plainregion)
		     (dump-display in out (string-append "\\end" env)))
		    (else (slatex-error "trigger-region: ~
Unknown triggerer ~s." typ)))))
	  (process-tex-file aux2.tex)
	  (set! *protected-files* (reverse! *protected-files*))
	  (call-with-input-file aux2.tex
	    (lambda (in)
	      (call-with-output-file aux.tex
		(lambda (out)
		  (inline-protected-files in out)))))
	  (delete-file aux2.tex)
	  ))))

(define inline-protected-files
  (lambda (in out)
    ;;inline all the protected files in port in into port out
    (let ((done? #f))
      (let loop ()
	(if done? 'exit-loop
	    (begin
	      (let ((c (read-char in)))
		(cond ((eof-object? c)
					;(display "{}" out)
		       (set! done? #t))
		      ((or (char=? c *return*) (char=? c #\newline))
		       (let ((c2 (peek-char in)))
			 (if (not (eof-object? c2))
			     (write-char c out))))
		      ((char=? c #\%)
		       (write-char c out) (newline out)
		       (eat-till-newline in))
		      ((char=? c #\\)
		       (let ((cs (read-ctrl-seq in)))
			 (cond
			  ((string=? cs "begin")
			   (let ((cs (read-grouped-latexexp in)))
			     (cond ((member cs *display-triggerers*)
				    (inline-protected
				     'envdisplay in out cs))
				   ((member cs *response-triggerers*)
				    (inline-protected
				     'envresponse in out cs))
				   ((member cs *respbox-triggerers*)
				    (inline-protected
				     'envrespbox in out cs))
				   ((member cs *box-triggerers*)
				    (inline-protected 'envbox in out cs))
				   ((member cs *topbox-triggerers*)
				    (inline-protected 'envtopbox in out cs))
				   ((member cs *region-triggerers*)
				    (inline-protected
				     'envregion in out cs))
				   (else
				    (display "\\begin{" out)
				    (display cs out)
				    (display "}" out)))))
			  ((member cs *intext-triggerers*)
			   (inline-protected 'intext in out #f))
			  ((member cs *resultintext-triggerers*)
			   (inline-protected 'resultintext in out #f))
			  ((member cs *display-triggerers*)
			   (inline-protected 'plaindisplay in out cs))
			  ((member cs *response-triggerers*)
			   (inline-protected 'plainresponse in out cs))
			  ((member cs *respbox-triggerers*)
			   (inline-protected 'plainrespbox in out cs))
			  ((member cs *box-triggerers*)
			   (inline-protected 'plainbox in out cs))
			  ((member cs *topbox-triggerers*)
			   (inline-protected 'plaintopbox in out cs))
			  ((member cs *region-triggerers*)
			   (inline-protected 'plainregion in out cs))
			  ((member cs *input-triggerers*)
			   (inline-protected 'input in out cs))
			  (else
			   (display "\\" out)
			   (display cs out)))))
		      (else (write-char c out))))
	      (loop)))))))

(define inline-protected
    (lambda (typ in out env)
      (cond ((eq? typ 'envregion)
	     (display "\\begin{" out)
	     (display env out)
	     (display "}" out)
	     (dump-display in out (string-append "\\end{" env "}"))
	     (display "\\end{" out)
	     (display env out)
	     (display "}" out))
	    ((eq? typ 'plainregion)
	     (display "\\" out)
	     (display env out)
	     (dump-display in out (string-append "\\end" env))
	     (display "\\end" out)
	     (display env out))
	    (else (let ((f (car *protected-files*)))
		    (set! *protected-files* (cdr *protected-files*))
		    (call-with-input-file f
		      (lambda (in)
			(inline-protected-files in out)))
		    (delete-file f)
		    )
		  (cond ((memq typ '(intext resultintext))
			 (display "{}" out)
			 (dump-intext in #f))
			((memq typ '(envrespbox envbox envtopbox))
			 (if (not *latex?*)
			     (display "{}" out))
			 (dump-display in #f
				       (string-append "\\end{" env "}")))
			((memq typ '(plainrespbox plainbox plaintopbox))
			 (display "{}" out)
			 (dump-display in #f
				       (string-append "\\end" env)))
			((memq typ '(envdisplay envresponse))
			 (dump-display in #f
				       (string-append "\\end{" env "}")))
			((memq typ '(plaindisplay plainresponse))
			 (dump-display in #f (string-append "\\end" env)))
			((eq? typ 'input)
			 (read-filename in)) ;and throw it away
			(else (slatex-error "inline-protected: ~
Unknown triggerer ~s." typ)))))))
