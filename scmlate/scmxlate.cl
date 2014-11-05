":";exec clisp -q $0
":";exec lisp -Q -L $0

(defvar *dialect* 'cl)

(setq *load-verbose* nil)

#+clisp
(setq *print-pretty* nil)

(defvar *in-scmxlate* t)

(defun compile-possible? () t)

(defmacro define (&rest ee)
  `(defvar ,@ee))

(load (merge-pathnames "scm2cl" *load-pathname*))

(defun eval1 (e)
  (eval e))

(defvar eval1 #'eval1)

(defun exists-file? (f)
  (probe-file f))

(defun ensure-file-deleted (f)
  (if (probe-file f) (delete-file f)))

(defun read-a-line (i)
  (read-line i))

(defun translate-define-syntax (e)
  (format t "~&define-syntax is probably best configured manually: ~s~%" e)
  e)

(defun translate-define-macro (e)
  (format t "~&define-macro is probably best configured manually: ~s~%" e)
  e)

(defun writeln (e o)
  (pprint
   (if *reading-source-file?*
       (let ((*scm2cl-bound-vars* '()))
         (scm2cl-sexp (nsublis *aliases* e)))
       (progn
        (when (and (consp e) (eq (car e) 'defun))
          (let ((name (cadr e)))
            (push (cons name `(function ,name)) *aliases*)))
        (if (and (consp e) (eq (car e) 'define))
            ;silently change define in non-source file
            ;to defvar (useful for user-override file
            ;that user forgot to change for CL)
            `(defvar ,@(cdr e))
            e))) o)
  (terpri o))

(defun scmxlate-system (c)

  #+(or allegro clisp)
  (shell
   #+win32 (concatenate 'string "cmd /c " c)
   #-win32 c)

  #+(and (or cmu sbcl) unix)
  (#+cmu ext:run-program
        #+sbcl run-program
         "/bin/sh" (list "-c" c))

  #+ecl
  (si:system c)

  #+clozure
  (ccl::os-command c)

  #+abcl
  (ext:run-shell-command c)

  )

(defun chmod+x (f)
  #+unix
  (let ((f (namestring f)))
    (when *shell-script?*
      (scmxlate-system
       (concatenate 'string "chmod +x " f)))))

(defun compile-file-to-file (fi fo)
  (when (not (pathname-type fi))
    (let ((new-fi (concatenate 'string fi "-temp.lisp")))
      (copy-file-to-file fi new-fi)
      (setq fi new-fi)))
  (compile-file fi :output-file fo)
  fo)

(with-open-file (i (merge-pathnames
                    (make-pathname :type "scm") *load-pathname*)
                   :direction :input)
  (loop
   (let ((x (read i nil :eof-object)))
     (when (eql x :eof-object) (return))
     (when (equal x ''eval-in-cl-also)
       (eval (scm2cl-sexp (nsublis *predefined-aliases*
                                  (read i))))))))

#+allegro (exit)
#+(or clisp cmu ecl sbcl) (quit)

#-(or allegro clisp cmu ecl sbcl)
(cond ((fboundp 'bye) (bye))
      ((fboundp 'exit) (exit))
      ((fboundp 'quit) (quit))
      (t (format t "~&You may exit CL now!~%")))
