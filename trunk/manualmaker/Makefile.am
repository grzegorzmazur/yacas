## Makefile.am -- Process this file with automake to produce Makefile.in

# $(srcdir) is the manualmaker/ directory and $(top_srcdir) is the Yacas tree
# so this file should work even if we are building Yacas in a different directory
# (TeX works but HTML docs still fail in that case b/c of dependencies on in/all, manualmaker, styleplain etc.)

# newly compiled version of yacas in ../src (not necessarily under $(srcdir))
ouryacas = ../src/yacas --rootdir $(top_srcdir)/scripts/:../scripts/

# top target
all-am: @BOOKS_HTML@ @PS_DOCS@ @PDF_DOCS@

BOOKS = intro.book coding.book ref.book refprog.book essays.book \
	Algo.book LispProgramming.book

BOOKSOURCES = in/intro.book.txt in/coding.book.txt \
	in/ref.book.txt in/refprog.book.txt in/essays.book.txt \
	in/Algo.book.txt LispProgramming.book.txt

INTROCHAPTERS = FDL.chapt
INTROSOURCES = in/FDL.chapt.txt

ALGOCHAPTERS = algo-basic.chapt algo-numapprox.chapt algorithms-elemfunc.chapt algorithms-specfunc.chapt algorithms-numtheory.chapt algorithms-integration.chapt algorithms-multivar.chapt SturmSequences.chapt FDL.chapt
ALGOSOURCES = in/algo-basic.chapt.txt in/algo-numapprox.chapt.txt in/algorithms-elemfunc.chapt.txt in/algorithms-specfunc.chapt.txt in/algorithms-numtheory.chapt.txt in/algorithms-integration.chapt.txt in/algorithms-multivar.chapt.txt in/SturmSequences.chapt.txt in/FDL.chapt.txt

REFCHAPTERS = config.chapt arith.chapt calc.chapt linalg.chapt \
	univar.chapt lists.chapt functional.chapt controlflow.chapt \
	preds.chapt logic.chapt const.chapt vars.chapt io.chapt \
	strings.chapt probability-and-statistics.chapt addons.chapt FDL.chapt

REFSOURCES = in/config.chapt.txt in/arith.chapt.txt in/calc.chapt.txt \
	in/linalg.chapt.txt in/univar.chapt.txt in/lists.chapt.txt \
	in/functional.chapt.txt in/controlflow.chapt.txt in/preds.chapt.txt \
	in/logic.chapt.txt in/const.chapt.txt in/vars.chapt.txt in/io.chapt.txt \
	in/strings.chapt.txt in/probability-and-statistics.chapt.txt in/addons.chapt.txt in/FDL.chapt.txt

REFPROGCHAPTERS = yacasapi.chapt FDL.chapt
# yacasapi.chapt has no .txt source
REFPROGSOURCES =

CODINGCHAPTERS = YacasDebugger.chapt FDL.chapt
CODINGSOURCES = in/YacasDebugger.chapt.txt in/FDL.chapt.txt

ESSAYCHAPTERS = ABIN-grammar.chapt Plugins-Howto.chapt SimpleTools.chapt \
	CVS-Howto.chapt YacasDocs.chapt wester-1994.chapt \
	new.chapt paper.chapt wordproblems.chapt FDL.chapt

ESSAYSOURCES = in/ABIN-grammar.chapt.txt in/Plugins-Howto.chapt.txt in/SimpleTools.chapt.txt \
	in/CVS-Howto.chapt.txt in/YacasDocs.chapt.txt \
        in/new.chapt.txt in/paper.chapt.txt in/wester-1994.chapt.txt \
		in/wordproblems.chapt.txt in/FDL.chapt.txt

LISPPROGRAMMINGCHAPTERS = LispLanguage.chapt FDL.chapt
LISPPROGRAMMINGSOURCES = in/LispLanguage.chapt.txt in/FDL.chapt.txt


# Yacas code extracted from the docs
CODESNIPPETS = wester-1994.ys

## This means: build wester-1994.ys if running tests
check_SCRIPTS = wester-1994.ys

EXTRA_DIST = \
	manualmaker styleplain in/all \
	in/intromaker in/codingmaker in/refmaker in/refprogmaker in/essaysmaker \
	in/LispProgrammingmaker \
	$(REFSOURCES) $(BOOKSOURCES) $(ESSAYSOURCES) $(CODINGSOURCES) $(INTROSOURCES) \
	$(REFPROGSOURCES) $(ALGOSOURCES) $(LISPPROGRAMMINGSOURCES) \
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
LispProgramming.book.tex: LispProgramming.book $(LISPPROGRAMMINGCHAPTERS)

# special "virtual" chapter built from yacasapi.cpp
yacasapi.chapt.txt: $(top_srcdir)/src/yacasapi.cpp
	perl $(top_srcdir)/manmake/api2txt.pl < $(top_srcdir)/src/yacasapi.cpp > yacasapi.chapt.txt
yacasapi.chapt: yacasapi.chapt.txt
	perl $(top_srcdir)/manmake/txt2yacasdoc.pl < $< > $@

PS_DOCS = intro.book.ps coding.book.ps essays.book.ps ref.book.ps refprog.book.ps Algo.book.ps LispProgramming.book.ps

ps-docs: $(PS_DOCS)

pdf-docs: intro.book.pdf coding.book.pdf essays.book.pdf ref.book.pdf refprog.book.pdf Algo.book.pdf LispProgramming.book.pdf

clean:
	rm -f books.html.c *.tex *.dvi *.aux *.idx *.ind *.ilg *.toc *.log *.ps *.pdf \
	coding*html intro*html ref*html essays*html Algo*html .o core *~ texdocs.c \
	$(REFCHAPTERS) $(ESSAYCHAPTERS) $(REFPROGCHAPTERS) $(CODINGCHAPTERS) $(LISPPROGRAMMINGCHAPTERS) \
	$(CODESNIPPETS) $(ALGOCHAPTERS) yacasapi.chapt.txt $(BOOKS) manicon.html

# Newly-built version of yacas with an extended search path
ouryacas2 = ../src/yacas --rootdir $(top_srcdir)/scripts/:../scripts/:$(srcdir)/:$(srcdir)/in/

books.html:  $(BOOKS) $(REFCHAPTERS) $(ESSAYCHAPTERS) \
   $(REFPROGCHAPTERS) $(CODINGCHAPTERS) $(ALGOCHAPTERS) \
   $(LISPPROGRAMMINGCHAPTERS)
	-$(ouryacas2) $(srcdir)/in/all
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

LispProgramming.html : LispProgramming.book $(REFPROGCHAPTERS)
	-$(ouryacas) in/LispProgrammingmaker
	touch books.html

noindex: intro.html ref.html coding.html essays.html refprog.html

## Hook to install documentation
install-data-local: @INSTALL_HTML@ @INSTALL_PS@

install-html: books.html 
	$(mkinstalldirs) $(htmldir)/
	$(INSTALL_DATA) $(top_srcdir)/docs/yacaslogo.gif $(htmldir)/
	-for file in *.html; do \
	   $(INSTALL_DATA) $$file $(htmldir)/$$file; \
	done

install-ps: ps-docs
	$(mkinstalldirs) $(psdir)/
	-for file in $(PS_DOCS); do \
	   $(INSTALL_DATA) $$file $(psdir)/$$file; \
	   gzip $(psdir)/$$file; \
	done

	

