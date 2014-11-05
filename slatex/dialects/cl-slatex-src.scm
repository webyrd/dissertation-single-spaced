(scmxlate-eval
(unless (find-package :slatex)
  (make-package :slatex))
)

(unless (find-package :slatex)
  (make-package :slatex))

(in-package :slatex)

(defvar *operating-system*
  #+win32 'windows
  #-win32 'unix)
  

(scmxlate-ignore 
 *operating-system*
; adjoin
; delete
 delete-if
 directory-namestring
; exit-scheme
 mapcan
 the-setter-for-of
; position-char
 setf)

(defun scheme-like-delete (x l eq)
  (declare (list l))
  (delete x l :test eq))

(defun scheme-like-adjoin (x l eq)
  (declare (list l))
  (adjoin x l :test eq))

(scmxlate-rename-define
 (adjoin #'scheme-like-adjoin)
 (delete #'scheme-like-delete)
 (exit-slatex #'exit)
 (position-char #'position)
 )

(defmacro defenum (&rest z)
  (let ((z z) (i 0) (r '()))
    (loop
     (when (null z) (return `(progn ,@r)))
     (push `(defvar ,(car z) (code-char ,i)) r)
     (incf i)
     (pop z))))

(defmacro defrecord (name &rest fields)
  (let ((fields fields) (i 0) (r '()))
    (loop
     (when (null fields)
       (return
       `(progn
         (defun ,name () (make-array ,i))
         ,@r)))
     (push `(defvar ,(car fields) ,i) r)
     (pop fields)
     (incf i))))

(defmacro of (r i &rest z)
    (cond ((null z) `(elt ,r ,i))
          ((and (eq i '/) (= (length z) 1))
           `(char ,r ,(car z)))
          (t `(of (elt ,r ,i) ,@z))))


(defun lassoc (x l eq)
  (declare (list l))
  (assoc x l :test eq))

(defun lmember (x l eq)
  (declare (list l))
  (member x l :test eq))

(defun string-prefix? (s1 s2 i)
  (declare (string s1 s2) (integer i))
  (string= s1 s2 :end1 i :end2 i))

(defun string-position-right (c s)
  (declare (character c) (string s))
  (position c s :test #'char= :from-end t))

(defun basename (f)
  (let ((f (file-namestring (merge-pathnames
                             (make-pathname :type "x") f))))
    (subseq f 0 (- (length f) 2))))

(defun ignore2 (i ii)
  (declare (ignore i ii))
  (values))

;(scmxlate-rename
; (ignore2 #'ignore2)
; )

(scmxlate-postprocess
(defvar *cl-command-name* "lisp")


#+clisp
(setq *cl-command-name* "clisp -q")

#+clisp
(defun system (s)
  (shell s))

(if (probe-file "slatex.scm")
    (delete-file "slatex.scm"))

(rename-file "my-slatex-src.scm" "slatex.scm")



#+unix
(with-open-file (o "slatex" :direction :output
                   :if-exists :supersede)
  (format o "echo '~%")
  (format o "(load ~s)~%" (or *target-file* "slatex.scm"))
  (format o "(slatex::process-main-tex-file \"'$1'\")' | ~a~%"
          *cl-command-name*)
  (format o "if test -f pltexchk.jnk~%")
  (format o "then tex $1; rm pltexchk.jnk~%")
  (format o "else latex $1~%")
  (format o "fi~%"))

#+unix
(system "chmod +x slatex")

#+win32
(with-open-file (o "slatex.bat" :direction :output
                   :if-exists :supersede)
  (format o "@echo off~%")
  (format o "echo (load ~s) >> Zslatex.jnk~%"  *target-file*)
  (format o "echo (slatex::process-main-tex-file \"%1\") >> Zslatex.jnk~%")
  (format o "echo (load \"Zslatex.jnk\") | ~a~%" *cl-command-name*)
  (format o "del Zslatex.jnk~%")
  (format o "if exist pltexchk.jnk goto one~%")
  (format o "goto two~%")
  (format o ":one~%")
  (format o "call tex %1~%")
  (format o "del pltexchk.jnk~%")
  (format o "goto end~%")
  (format o ":two~%")
  (format o "call latex %1~%")
  (format o ":end~%"))

(with-open-file 
 (o "callsla.scm" :direction :output)
 (prin1 `(let ((already-loaded-p nil))
          (defun call-slatex (f)
             (unless already-loaded-p
               (load ,(or *target-file* "slatex.scm"))
               (setq already-loaded-p t))
             (slatex::process-main-tex-file f)
             (format t "Call (La)TeX on ~a now~%" f)
             )) o)
 (terpri o))
)
