# $(srcdir) is the manualmaker/ directory and $(top_srcdir) is the Yacas tree
# so this file should work even if we are building Yacas in a different directory
# (TeX works but HTML docs still fail in that case b/c of dependencies on in/all, manualmaker, styleplain etc.)

# newly compiled version of yacas in ../src (not necessarily under $(srcdir))
ouryacas = ../src/yacas --rootdir $(top_srcdir)/scripts/

noinst_PROGRAMS = books.html

BOOKS = intro.book coding.book ref.book refprog.book essays.book Algo.book

BOOKSOURCES = in/intro.book.txt in/coding.book.txt \
	in/ref.book.txt in/refprog.book.txt in/essays.book.txt in/Algo.book.txt

INTROCHAPTERS = FDL.chapt
INTROSOURCES = in/FDL.chapt.txt

ALGOCHAPTERS = algo-basic.chapt algo-numapprox.chapt algorithms-elemfunc.chapt algorithms-specfunc.chapt algorithms-numtheory.chapt algorithms-integration.chapt algorithms-multivar.chapt SturmSequences.chapt FDL.chapt
ALGOSOURCES = in/algo-basic.chapt.txt in/algo-numapprox.chapt.txt in/algorithms-elemfunc.chapt.txt in/algorithms-specfunc.chapt.txt in/algorithms-numtheory.chapt.txt in/algorithms-integration.chapt.txt in/algorithms-multivar.chapt.txt in/SturmSequences.chapt.txt in/FDL.chapt.txt

REFCHAPTERS = config.chapt arith.chapt calc.chapt linalg.chapt \
	univar.chapt lists.chapt functional.chapt controlflow.chapt \
	preds.chapt logic.chapt const.chapt vars.chapt io.chapt \
	strings.chapt addons.chapt FDL.chapt

REFSOURCES = in/config.chapt.txt in/arith.chapt.txt in/calc.chapt.txt \
	in/linalg.chapt.txt in/univar.chapt.txt in/lists.chapt.txt \
	in/functional.chapt.txt in/controlflow.chapt.txt in/preds.chapt.txt \
	in/logic.chapt.txt in/const.chapt.txt in/vars.chapt.txt in/io.chapt.txt \
	in/strings.chapt.txt in/addons.chapt.txt in/FDL.chapt.txt

REFPROGCHAPTERS = yacasapi.chapt FDL.chapt
# yacasapi.chapt has no .txt source
REFPROGSOURCES =

CODINGCHAPTERS = YacasDebugger.chapt FDL.chapt
CODINGSOURCES = in/YacasDebugger.chapt.txt in/FDL.chapt.txt

ESSAYCHAPTERS = ABIN-grammar.chapt Plugins-Howto.chapt \
	CVS-Howto.chapt YacasDocs.chapt wester-1994.chapt \
	new.chapt paper.chapt wordproblems.chapt FDL.chapt

ESSAYSOURCES = in/ABIN-grammar.chapt.txt in/Plugins-Howto.chapt.txt \
	in/CVS-Howto.chapt.txt in/YacasDocs.chapt.txt \
        in/new.chapt.txt in/paper.chapt.txt in/wester-1994.chapt.txt \
		in/wordproblems.chapt.txt in/FDL.chapt.txt

# Yacas code extracted from the docs
CODESNIPPETS = wester-1994.ys

EXTRA_DIST = \
	manualmaker styleplain in/all \
	in/intromaker in/codingmaker in/refmaker in/refprogmaker in/essaysmaker\
	$(REFSOURCES) $(BOOKSOURCES) $(ESSAYSOURCES) $(CODINGSOURCES) $(INTROSOURCES) $(REFPROGSOURCES) $(ALGOSOURCES) $(CODESNIPPETS) \
	in/dummies .cvsignore

# Stuff for automatic generation of Yacas books
%.chapt: $(srcdir)/in/%.chapt.txt
	perl $(top_srcdir)/manmake/txt2yacasdoc.pl < $< > $@

%.book: $(srcdir)/in/%.book.txt
	perl $(top_srcdir)/manmake/txt2yacasdoc.pl < $< > $@

%.book.tex: %.book
	sh $(top_srcdir)/manmake/book2TeX.sh -run "$(ouryacas)" $< $@

%.dvi: %.tex
	latex $< > /dev/null
	latex $< > /dev/null
# The perl filter is used to help the 'makeindex' command: 1) replace \verb|...| by \verb=...=; 2) escape ()|@! using a preceding quote " .
# Note that @ and ! are not always escaped because they are sometimes part of the index entry and sometimes control chars. Heuristics are used to guess correctly.
	@perl -pe 's/(\\verb)\|(.*)\|/$$1=$$2=/g;s/([()|])/"$$1/g;if(/\@/){s/!/"!/g;};s/\@([^\\])/"\@$$1/g;'<$*.idx>$*.1.idx
	@makeindex $*.1.idx -o $*.ind || echo "***Warning: a problem with 'makeindex'"
	latex $< > /dev/null

%.ps: %.dvi
	dvips -q -o $@ $<
	@echo "***Documentation created:" $*.ps

%.pdf: %.dvi
	pdflatex $* > /dev/null
	@echo Documentation created: $*.pdf
#	dvipdf $<

# experimental: extract Yacas code from books
%.ys: %.chapt
	sh $(top_srcdir)/manmake/book2ys.sh -run "$(ouryacas)" $< $@


# specify books that need chapters
essays.book.tex: essays.book $(ESSAYCHAPTERS)
ref.book.tex : ref.book $(REFCHAPTERS)
refprog.book.tex : refprog.book $(REFPROGCHAPTERS)
coding.book.tex : coding.book $(CODINGCHAPTERS)
intro.book.tex: intro.book $(INTROCHAPTERS)
Algo.book.tex: Algo.book $(ALGOCHAPTERS)

# special "virtual" chapter built from yacasapi.cpp
in/yacasapi.chapt.txt : $(top_srcdir)/src/yacasapi.cpp
	perl $(top_srcdir)/manmake/api2txt.pl < $(top_srcdir)/src/yacasapi.cpp > in/yacasapi.chapt.txt

# TeX/PS/PDF top target
texdocs: \
		intro.book.tex intro.book.ps intro.book.pdf \
		coding.book.tex coding.book.ps coding.book.pdf \
		essays.book.tex essays.book.ps essays.book.pdf \
		ref.book.tex ref.book.ps ref.book.pdf \
		refprog.book.tex refprog.book.ps refprog.book.pdf \
		Algo.book.tex Algo.book.ps Algo.book.pdf
	echo "done" > texdocs.c

clean:
	rm -f books.html.c *.tex *.dvi *.aux *.idx *.ind *.ilg *.toc *.log *.ps *.pdf \
	coding*html intro*html ref*html essays*html .o core *~ texdocs.c \
	$(REFCHAPTERS) $(ESSAYCHAPTERS) $(REFPROGCHAPTERS) $(CODINGCHAPTERS) \
	$(CODESNIPPETS) $(ALGOCHAPTERS) in/yacasapi.chapt.txt $(BOOKS) manicon.html

yacas_helpdir = $(datadir)/yacas/documentation

books.html:  $(BOOKS) $(REFCHAPTERS) $(ESSAYCHAPTERS) \
   $(REFPROGCHAPTERS) $(CODINGCHAPTERS) $(ALGOCHAPTERS) $(CODESNIPPETS)
	-$(ouryacas) $(srcdir)/in/all
	echo "done" > books.html.c

intro.html : intro.book $(INTROCHAPTERS)
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

# not sure if this is very helpful b/c we are overriding distcheck in the top Makefile
distcheck-hook:
	make texdocs
	make test

install-data-local: books.html 
	$(mkinstalldirs) $(yacas_helpdir)/
	cp ../docs/yacaslogo.gif $(yacas_helpdir)/
	-for file in $(srcdir)/*.html; do \
	basefile=`basename $$file`; \
	$(INSTALL_DATA) $(srcdir)/$$file $(yacas_helpdir)/$$basefile; \
	done


