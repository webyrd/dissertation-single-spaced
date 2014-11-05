;The argument of scmxlate-target-file is the full
;pathname you intend to give to the generated
;slatex.scm file.  Supply it as a Scheme (or Common
;Lisp) string.  Note that initially the configuration
;process will produce slatex.scm in the current
;directory.  You will then have to move it (possibly
;after assuming root status) to the path you mention
;here.

;Slashification note for Windows users: In general,
;Scheme (or CL) dialects on these OSes accept
;Unix-style pathnames.  But if they don't, you will
;have to use the backslash (\) where on Unix you'd use
;the forward slash (/).  When rendering such pathnames
;as Scheme strings, you will have to use escapes.

(scmxlate-eval
 ;on denali
 ;(define *target-file* "/home/dorai/.www/slatex/slatex.scm")

 ;on mac
 (define *target-file* "~/latex/slatex/slatex.scm")
 )

;You may define slatex::*texinputs* here.  Eg,


;(define slatex::*texinputs* "c:/bin;c:/lib/emtex/texinput") ;Windows

;(define slatex::*texinputs*
;"/home/dorai/tex:/usr/local/lib/tex/macros") ;in Unix

;(define slatex::*texinputs* (getenv "TEXINPUTS"))

;Unfortunately, that last one is not really as convenient as
;it seems, even if your Scheme has a getenv procedure.
;This is because the TEXINPUTS uses abbreviations that may
;not be readily translatable into a list of directories
;by Scheme.

;Luckily, normal ppl never rely on TEXINPUTS to retrieve
;their document files (as opposed to style files).  Thus,
;not setting slatex::*texinputs* (or setting it to the emptry
;string, as I have done here) is probably quite OK.

;NB: Use slatex$$*texinputs* instead of
;slatex::*texinputs* in Bigloo.

;on Bigloo
;(define slatex$$*texinputs* "")

;on non-Bigloo (including CL)
;(define slatex::*texinputs* "")

;compile?

(scmxlate-compile #f)
