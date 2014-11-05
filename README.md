A very slightly cleaned up version of my dissertation, which (1) is easier to read and (2) typesets under modern XeLaTeX.  The archival version of my dissertation is at http://gradworks.umi.com/3380156.pdf.


Dissertation file: `thesis.pdf`

To typeset, just type `make` (assuming you have a full install of TeX Live, including XeLaTeX).


Changes in this version of the dissertation:

* Changed from double-spacing to single-spacing.  This changed most of the pagebreaks in the document.

* Added/adjusted whitespace and linebreaks, especially around code, to account for single-spacing.

* Let figures float when it improved readability.  This changed the relative position of the text and figures for form figures.

* Removed out-of-date curriculum vitae from end of dissertation.  See http://webyrd.net/ for an up-to-date vitae.

* Changed \shortrightarrow to \rightarrow to avoid use of St. Mary Rd font, which caused typesetting errors.

* Removed hyperlinks, since the hyperref package was causing typesetting errors.  I wish I knew how to fix this...


Known errors/typos (uncorrected):

* End of Section 1.3: "And our desire for termination prevents us from adapting Curry’s residuation."  'adapting' should read 'adopting'.
