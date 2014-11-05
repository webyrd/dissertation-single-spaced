submake = make -f ${HOME}/.www/tex2page/makefile.common

TRIGGER_FILES = history manifest makefile version.tex \
		scmxlate.scm scmxlate.cl scm2cl.cl \
		scmxlate.tex 

default:
	@cat README

%.html: %.tex 
	tex2page $(@:%.html=%)
	while grep -i "rerun: tex2page" $(@:%.html=%.hlog); do \
	tex2page $(@:%.html=%); \
	done

scmxlate.html: scmxlate.tex 
	$(submake) $@ 

scmxlate.pdf: scmxlate.tex
	$(submake) $@

dist:
	$(submake) scmxlate.dist

html: scmxlate.html scm2cl.html

pdf: scmxlate.pdf

dvi: scmxlate.dvi

%: FORCE
	$(submake) $@

FORCE: ;
