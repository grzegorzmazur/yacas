
# newly compiled version of yacas
ouryacas = ../src/yacas --rootdir ../scripts/

noinst_PROGRAMS = books.html

BOOKS = intro.book coding.book ref.book refprog.book essays.book

BOOKSOURCES = in/intro.book.txt in/coding.book.txt \
	in/ref.book.txt in/refprog.book.txt in/essays.book.txt

REFCHAPTERS = config.chapt arith.chapt calc.chapt linalg.chapt \
	univar.chapt lists.chapt functional.chapt controlflow.chapt \
	preds.chapt logic.chapt const.chapt vars.chapt io.chapt \
	strings.chapt addons.chapt

REFSOURCES = in/config.chapt.txt in/arith.chapt.txt in/calc.chapt.txt \
	in/linalg.chapt.txt in/univar.chapt.txt in/lists.chapt.txt \
	in/functional.chapt.txt in/controlflow.chapt.txt in/preds.chapt.txt \
	in/logic.chapt.txt in/const.chapt.txt in/vars.chapt.txt in/io.chapt.txt \
	in/strings.chapt.txt in/addons.chapt.txt 

REFPROGCHAPTERS = yacasapi.chapt
# yacasapi.chapt has no .txt source
REFPROGSOURCES =

CODINGCHAPTERS = YacasDebugger.chapt
CODINGSOURCES = in/YacasDebugger.chapt.txt

ESSAYCHAPTERS = ABIN-grammar.chapt Plugins-Howto.chapt \
	CVS-Howto.chapt YacasDocs.chapt wester-1994.chapt \
	new.chapt paper.chapt Algorithms.chapt wordproblems.chapt \
	algorithms-specfunc.chapt algorithms-elemfunc.chapt \
	algorithms-integration.chapt algorithms-multivar.chapt

ESSAYSOURCES = in/ABIN-grammar.chapt.txt in/Plugins-Howto.chapt.txt \
	in/CVS-Howto.chapt.txt in/YacasDocs.chapt.txt \
        in/new.chapt.txt in/paper.chapt.txt in/wester-1994.chapt.txt \
		in/Algorithms.chapt.txt in/wordproblems.chapt.txt \
	in/algorithms-specfunc.chapt.txt in/algorithms-elemfunc.chapt.txt \
	in/algorithms-integration.chapt.txt in/algorithms-multivar.chapt.txt

# Yacas code extracted from the docs
CODESNIPPETS = wester-1994.ys

EXTRA_DIST = \
	manualmaker styleplain in/all \
	in/intromaker in/codingmaker in/refmaker in/refprogmaker in/essaysmaker\
	$(REFSOURCES) $(BOOKSOURCES) $(ESSAYSOURCES) $(CODINGSOURCES) $(REFPROGSOURCES) $(CODESNIPPETS) \
	in/dummies .cvsignore

# Stuff for automatic generation of Yacas books
%.chapt: in/%.chapt.txt
	perl ../manmake/txt2yacasdoc.pl < $< > $@
%.book: in/%.book.txt
	perl ../manmake/txt2yacasdoc.pl < $< > $@

%.book.tex: %.book
	sh ../manmake/book2TeX.sh $< $@

%.dvi: %.tex
	latex $< > /dev/null
	latex $< > /dev/null
	latex $< > /dev/null

%.ps: %.dvi
	dvips -q -o $@ $<
	@echo Documentation created: $*.ps

%.pdf: %.dvi
#	dvipdf $<
	pdflatex $* > /dev/null
	@echo Documentation created: $*.pdf

# experimental: extract Yacas code from books
%.ys: %.chapt
	sh ../manmake/book2ys.sh $< $@


# specify books that need chapters
essays.book.tex: essays.book $(ESSAYCHAPTERS)
ref.book.tex : ref.book $(REFCHAPTERS)
refprog.book.tex : refprog.book $(REFPROGCHAPTERS)
coding.book.tex : coding.book $(CODINGCHAPTERS)

# special "virtual" chapter built from yacasapi.cpp
in/yacasapi.chapt.txt : ../src/yacasapi.cpp
	perl ../manmake/api2txt.pl < ../src/yacasapi.cpp > in/yacasapi.chapt.txt

# TeX/PS/PDF top target
texdocs: \
		intro.book.tex intro.book.ps intro.book.pdf \
		coding.book.tex coding.book.ps coding.book.pdf \
		essays.book.tex essays.book.ps essays.book.pdf \
		ref.book.tex ref.book.ps ref.book.pdf \
		refprog.book.tex refprog.book.ps refprog.book.pdf
	echo "done" > texdocs.c

clean:
	rm -f books.html.c *.tex *.dvi *.aux *.toc *.log *.ps *.pdf \
	coding*html intro*html ref*html essays*html .o core *~ texdocs.c \
	$(REFCHAPTERS) $(ESSAYCHAPTERS) $(REFPROGCHAPTERS) $(CODINGCHAPTERS) \
	$(CODESNIPPETS) in/yacasapi.chapt.txt $(BOOKS) manicon.html

yacas_helpdir = $(datadir)/yacas/documentation

books.html:  $(BOOKS) $(REFCHAPTERS) $(ESSAYCHAPTERS) \
   $(REFPROGCHAPTERS) $(CODINGCHAPTERS) $(CODESNIPPETS)
	-$(ouryacas) in/all
	echo "done" > books.html.c

# No separate chapters
intro.html : intro.book
	-$(ouryacas) in/intromaker 
	touch books.html

coding.html : coding.book $(CODINGCHAPTERS)
	-$(ouryacas) in/codingmaker
	touch books.html

essays.html : essays.book $(ESSAYCHAPTERS)
	-$(ouryacas) in/essaysmaker
	touch books.html

ref.html : ref.book $(REFCHAPTERS)
	-$(ouryacas) in/refmaker
	touch books.html

refprog.html : refprog.book $(REFPROGCHAPTERS)
	-$(ouryacas) in/refprogmaker
	touch books.html

noindex: intro.html ref.html coding.html essays.html refprog.html

# this dist-hook copies the html files into the distributed .tar.gz file.
dist-hook:
# do not copy html files any more
#	-cp $(srcdir)/*.html  $(distdir) 

distcheck-hook:
	make test

install-data-local: books.html 
	$(mkinstalldirs) $(yacas_helpdir)/
	cp ../docs/yacaslogo.gif $(yacas_helpdir)/
	-for file in $(srcdir)/*.html; do \
	basefile=`basename $$file`; \
	$(INSTALL_DATA) $(srcdir)/$$file $(yacas_helpdir)/$$basefile; \
	done


