
# newly compiled version of yacas
ouryacas = ../src/yacas --rootdir ../scripts/

noinst_PROGRAMS = books.html

ALLBOOKS = intro.book coding.book ref.book refprog.book \
	essays.book

ALLBOOKSOURCES = in/intro.book.txt in/coding.book.txt \
	in/ref.book.txt in/refprog.book.txt in/essays.book.txt

REFCHAPTERS = config.chapt arith.chapt calc.chapt linalg.chapt \
	univar.chapt lists.chapt functional.chapt controlflow.chapt \
	preds.chapt logic.chapt const.chapt vars.chapt io.chapt \
	strings.chapt

REFSOURCES = in/config.chapt.txt in/arith.chapt.txt in/calc.chapt.txt in/linalg.chapt.txt \
	in/univar.chapt.txt in/lists.chapt.txt in/functional.chapt.txt in/controlflow.chapt.txt \
	in/preds.chapt.txt in/logic.chapt.txt in/const.chapt.txt in/vars.chapt.txt in/io.chapt.txt \
	in/strings.chapt.txt

ESSAYCHAPTERS = ABIN-grammar.chapt Plugins-Howto.chapt \
	CVS-Howto.chapt YacasDocs.chapt


ESSAYSOURCES = in/ABIN-grammar.chapt.txt in/Plugins-Howto.chapt.txt \
	in/CVS-Howto.chapt.txt in/YacasDocs.chapt.txt


EXTRA_DIST = \
	manualmaker styleplain in/all \
	in/intromaker in/codingmaker in/refmaker in/refprogmaker \
	$(REFSOURCES) $(ALLBOOKSOURCES) $(ESSAYSOURCES) \
	in/dummies newhelp yacasinit.ys .cvsignore

# Stuff for automatic generation of Yacas books
%.chapt: in/%.chapt.txt
	perl ../manmake/txt2yacasdoc.pl < $< > $@
%.book: in/%.book.txt
	perl ../manmake/txt2yacasdoc.pl < $< > $@

%.book.tex: %.book
	../manmake/book2TeX.sh $< $@

essays.book.tex: essays.book $(ESSAYCHAPTERS)

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
		refprog.book.ps refprog.book.pdf \
		essays.book.ps essays.book.pdf
	echo "done" > texdocs.c

clean:
	rm -f books.html.c *.tex *.dvi *.aux *.toc *.log *.ps *.pdf \
	coding*html intro*html ref*html essays*html .o core *~ texdocs.c \
	$(REFCHAPTERS) $(ESSAYCHAPTERS) $(ALLBOOKS)

yacas_helpdir = $(datadir)/yacas/documentation

books.html:  $(ALLBOOKS) $(REFCHAPTERS) $(ESSAYCHAPTERS)
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

#essays.html : in/essays.book $(ESSAYCHAPTERS)
#	-$(ouryacas) in/essaysmaker
#	touch books.html

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


