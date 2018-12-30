A very slightly cleaned up version of my dissertation, which (1) is easier to read and (2) typesets under modern XeLaTeX.  The archival version of my dissertation is at https://search.proquest.com/docview/304903505.

This document is released under a Creative Commons Attribution 4.0 International (CC BY 4.0) license (http://creativecommons.org/licenses/by/4.0/)

<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/Text" property="dct:title" rel="dct:type">Relational Programming in miniKanren: Techniques, Applications, and Implementations</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="https://github.com/webyrd/dissertation-single-spaced" property="cc:attributionName" rel="cc:attributionURL">William E. Byrd</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.<br />Based on a work at <a xmlns:dct="http://purl.org/dc/terms/" href="https://github.com/webyrd/dissertation-single-spaced" rel="dct:source">https://github.com/webyrd/dissertation-single-spaced</a>.

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
