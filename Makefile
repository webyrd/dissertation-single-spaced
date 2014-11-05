THESIS=thesis

# xelatex vs. slatex

all:
	./xeslatex ${THESIS}.tex
	bibtex ${THESIS}
	./xeslatex ${THESIS}.tex
	bibtex ${THESIS}
	./xeslatex ${THESIS}.tex
	bibtex ${THESIS}
	./xeslatex ${THESIS}.tex

fast:
	./xeslatex ${THESIS}.tex

fastbib:
	bibtex ${THESIS}
	./xeslatex ${THESIS}.tex

clean:
	rm -f .Z*
	rm -f .q*
	rm -f .old*
	rm -f *.aux
	rm -f *.log
	rm -f *.toc
	rm -f ${THESIS}.out
	rm -f ${THESIS}.bbl
	rm -f ${THESIS}.blg
	rm -f *.idx

squeaky:
	make clean
	rm -f ${THESIS}.pdf
	rm -f ${THESIS}.dvi
	rm -f *~
