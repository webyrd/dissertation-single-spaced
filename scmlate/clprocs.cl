;This file contains some suggested definitions that you
;may need to manually load into your
;scm2cl-generated CL file. 

(defvar *scm2cl-lisp-extension* "lsp")

;r5rs

(defun scheme-assoc (x l)
  (assoc x l :test #'equal))

(defun scheme-boolean? (o)
  (or (eq o t) (eq o nil)))

(defun scheme-current-input-port ()
  *standard-input*)

(defun scheme-current-output-port ()
  *standard-output*)

(defun scheme-call-with-input-file (f p)
  (with-open-file (i f :direction :input)
    (funcall p i)))

(defun scheme-call-with-output-file (f p)
  (with-open-file (o f :direction :output)
    (funcall p o)))

(defun scheme-call-with-values (producer consumer)
  (multiple-value-call consumer (funcall producer)))

(defun scheme-char-whitespace? (c)
  (or (char= c #\space) (char= c #\tab)
      (not (graphic-char-p c))))

(defun scheme-eof-object? (v)
  (eq v :eof-object))

(defun scheme-eval (exp env)
  (eval exp))

(defun scheme-list? (o)
  (and (listp o) (null (cdr (last o)))))

(defun scheme-list->string (l)
  (concatenate 'string l))

(defun scheme-list->vector (l)
  (concatenate 'vector l))

(defun scheme-make-string (n &optional (c #\space))
  (make-string n :initial-element c))

(defun scheme-make-vector (n &optional x)
  (make-array (list n) :initial-element x))

(defun scheme-member (x l)
  (member x l :test #'equal))

(defun scheme-not-quite-call/cc (p)
  (let ((k (gensym)))
    (catch k
      (funcall p #'(lambda (v) (throw k v))))))

(defun scheme-number->string (n &optional (b 10))
  (write-to-string n :base b))

(defun scheme-open-input-file (f)
  (open f :direction :input))

(defun scheme-open-output-file (f)
  (open f :direction :output))

(defun scheme-peek-char (&optional p)
  (peek-char nil p nil :eof-object))

(defun scheme-read (&optional p)
  (read p nil :eof-object))

(defun scheme-read-char (&optional p)
  (read-char p nil :eof-object))

(defun scheme-string (&rest z)
  (concatenate 'string z))

(defun scheme-string-append (&rest z)
  (apply #'concatenate 'string z))

(defun scheme-string-set! (s i c)
  (setf (char s i) c))

(defun scheme-string->list (s)
  (concatenate 'list s))

(defun scheme-string->number (s &optional (b 10))
  (let ((s1 s))
    (if (position #\: s1 :test #'char=) nil
        (let ((*read-base* b))
          (let ((n (read-from-string s nil)))
            (if (numberp n) n nil))))))

(defun scheme-string->symbol (s)
  (let ((s (map 'string
             #'(lambda (c) (cond ((upper-case-p c) (char-downcase c))
                                 ((lower-case-p c) (char-upcase c))
                                 (t c))) s)))
    (if (or (string= s "") (not (char= (char s 0) #\:)))
        (intern s)
        (intern (subseq s 1) :keyword))))

(defun scheme-symbol? (o)
  (and (symbolp o)
       (not (scheme-boolean? o))))

(defun scheme-symbol->string (sym)
  (string-downcase (symbol-name sym)))

(defun scheme-vector-set (v i x)
  (setf (svref v i) x))

(defun scheme-vector->list (v)
  (concatenate 'list v))

(defun scheme-with-input-from-file (f th)
  (with-open-file (i f :direction :input)
    (let ((*standard-input* i))
      (funcall th))))

(defun scheme-with-output-to-file (f th)
  (with-open-file (o f :direction :output)
    (let ((*standard-output* o))
      (funcall th))))

;mbe

(load (merge-pathnames "mbe-procs" *load-pathname*))

;some slib procs

(defun scheme-read-line (&optional i)
  (read-line i nil :eof-object))

(defun scheme-call-with-input-string (s p)
  (with-input-from-string (i s)
    (funcall p i)))

(defun scheme-call-with-output-string (p)
  (with-output-to-string (o)
    (funcall p o)))

(defun scheme-load-relative (f)
  (load (merge-pathnames (make-pathname :type *scm2cl-lisp-extension*)
          (merge-pathnames f *load-pathname*))))

(defun scheme-string-index (s c)
  (position c s :test #'char=))

(defun scheme-string-reverse-index (s c)
  (position c s :test #'char= :from-end t))
