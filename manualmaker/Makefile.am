
# newly compiled version of yacas
ouryacas = ../src/yacas --rootdir ../scripts/

noinst_PROGRAMS = books.html

ALLBOOKS = in/intro.book in/coding.book in/ref.book in/refprog.book

ALLBOOKSOURCES = in/intro.book.txt in/coding.book.txt \
	in/ref.book.txt in/refprog.book.txt

REFCHAPTERS = in/config.chapt in/arith.chapt in/calc.chapt in/linalg.chapt \
	in/univar.chapt in/lists.chapt in/functional.chapt in/controlflow.chapt \
	in/preds.chapt in/logic.chapt in/const.chapt in/vars.chapt in/io.chapt \
	in/strings.chapt

REFSOURCES = in/config.chapt.txt in/arith.chapt.txt in/calc.chapt.txt in/linalg.chapt.txt \
	in/univar.chapt.txt in/lists.chapt.txt in/functional.chapt.txt in/controlflow.chapt.txt \
	in/preds.chapt.txt in/logic.chapt.txt in/const.chapt.txt in/vars.chapt.txt in/io.chapt.txt \
	in/strings.chapt.txt


EXTRA_DIST = \
	manualmaker styleplain in/all \
	in/intromaker in/codingmaker in/refmaker in/refprogmaker \
	$(REFSOURCES) $(ALLBOOKSOURCES) \
	in/dummies newhelp yacasinit.ys .cvsignore

# Stuff for automatic generation of Yacas books
%.chapt: %.chapt.txt
	perl ../manmake/txt2yacasdoc.pl < $< > $@
%.book: %.book.txt
	perl ../manmake/txt2yacasdoc.pl < $< > $@

%.book.tex: in/%.book
	../manmake/book2TeX.sh $< $@

%.dvi: %.tex
	latex $< > /dev/null
	latex $< > /dev/null

%.ps: %.dvi
	dvips -o $@ $<
	@echo Documentation created: $*.ps

%.pdf: %.dvi
#	dvipdf $<
	pdflatex $* > /dev/null
	@echo Documentation created: $*.pdf

# TeX/PS/PDF top target
texdocs: coding.book.ps coding.book.pdf \
	intro.book.ps intro.book.pdf \
	refprog.book.ps refprog.book.pdf
	echo "done" > texdocs.c

clean:
	rm -f books.html.c *.tex *.dvi *.aux *.toc *.log *.ps *.pdf \
	coding*html intro*html ref*html .o core *~ texdocs.c \
	$(REFCHAPTERS) $(ALLBOOKS)

yacas_helpdir = $(datadir)/yacas/documentation

books.html:  $(ALLBOOKS) $(REFCHAPTERS)
	-$(ouryacas) in/all
	echo "done" > books.html.c

# No separate chapters
intro.html : in/intro.book
	-$(ouryacas) in/intromaker 
	touch books.html

# No separate chapters
coding.html : in/coding.book
	-$(ouryacas) in/codingmaker
	touch books.html

ref.html : in/ref.book $(REFCHAPTERS)
	-$(ouryacas) in/refmaker
	touch books.html

refprog.html : in/refprog.book
	-$(ouryacas) in/refprogmaker
	touch books.html

noindex: intro.html ref.html coding.html

# dist-hook copies the html files into the distributed .tar.gz file.
dist-hook: 
	-cp $(srcdir)/*.html  $(distdir) 

install-data-local: books.html 
	$(mkinstalldirs) $(yacas_helpdir)/
	-for file in $(srcdir)/*.html; do \
	basefile=`basename $$file`; \
	$(INSTALL_DATA) $(srcdir)/$$file $(yacas_helpdir)/$$basefile; \
	done


