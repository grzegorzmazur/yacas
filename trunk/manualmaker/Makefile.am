## Makefile.am -- Process this file with automake to produce Makefile.in

# $(srcdir) is the manualmaker/ directory and $(top_srcdir) is the Yacas tree
# so this file should work even if we are building Yacas in a different directory
# (TeX works but HTML docs still fail in that case b/c of dependencies on manualmaker, styleplain etc.)

# newly compiled version of yacas in ../src (not necessarily under $(srcdir))
ouryacas = ../src/yacas --rootdir $(top_srcdir)/scripts/:../scripts/

# top target
all-am: @BOOKS_HTML@ @PS_DOCS@ @PDF_DOCS@

BOOKS = intro.book coding.book ref.book refprog.book essays.book \
	Algo.book LispProgramming.book

BOOKSOURCES = in/intro.book.txt in/coding.book.txt \
	in/ref.book.txt in/refprog.book.txt in/essays.book.txt \
	in/Algo.book.txt in/LispProgramming.book.txt

INTROCHAPTERS = FDL.chapt cl-options.chapt
INTROSOURCES = in/FDL.chapt.txt in/cl-options.chapt.txt

ALGOCHAPTERS = algo-basic.chapt algo-numapprox.chapt algorithms-elemfunc.chapt algorithms-specfunc.chapt algorithms-numtheory.chapt algorithms-integration.chapt algorithms-multivar.chapt SturmSequences.chapt FDL.chapt algo-refs.chapt
ALGOSOURCES = in/algo-basic.chapt.txt in/algo-numapprox.chapt.txt in/algorithms-elemfunc.chapt.txt in/algorithms-specfunc.chapt.txt in/algorithms-numtheory.chapt.txt in/algorithms-integration.chapt.txt in/algorithms-multivar.chapt.txt in/SturmSequences.chapt.txt in/FDL.chapt.txt in/algo-refs.chapt.txt

REFCHAPTERS = config.chapt arith.chapt calc.chapt linalg.chapt \
	univar.chapt lists.chapt functional.chapt controlflow.chapt \
	preds.chapt logic.chapt const.chapt vars.chapt io.chapt numtheory.chapt \
	strings.chapt probability-and-statistics.chapt \
	addons.chapt FDL.chapt

REFSOURCES = in/config.chapt.txt in/arith.chapt.txt in/calc.chapt.txt \
	in/linalg.chapt.txt in/univar.chapt.txt in/lists.chapt.txt \
	in/functional.chapt.txt in/controlflow.chapt.txt in/preds.chapt.txt \
	in/logic.chapt.txt in/const.chapt.txt in/vars.chapt.txt in/io.chapt.txt in/numtheory.chapt.txt \
	in/strings.chapt.txt in/probability-and-statistics.chapt.txt \
	in/addons.chapt.txt in/FDL.chapt.txt

REFPROGCHAPTERS = yacasapi.chapt FDL.chapt cl-options.chapt glossary.chapt GPL.chapt numerics.chapt
# yacasapi.chapt and GPL.chapt have no .txt source
REFPROGSOURCES = in/FDL.chapt.txt in/cl-options.chapt.txt in/glossary.chapt.txt in/numerics.chapt.txt

CODINGCHAPTERS = YacasDebugger.chapt FDL.chapt
CODINGSOURCES = in/YacasDebugger.chapt.txt in/FDL.chapt.txt

ESSAYCHAPTERS = ABIN-grammar.chapt Plugins-Howto.chapt SimpleTools.chapt \
	CVS-Howto.chapt YacasDocs.chapt wester-1994.chapt \
	new.chapt paper.chapt wordproblems.chapt FDL.chapt BuildSystem.chapt

ESSAYSOURCES = in/ABIN-grammar.chapt.txt in/Plugins-Howto.chapt.txt in/SimpleTools.chapt.txt \
	in/CVS-Howto.chapt.txt in/YacasDocs.chapt.txt \
        in/new.chapt.txt in/paper.chapt.txt in/wester-1994.chapt.txt \
	in/wordproblems.chapt.txt in/FDL.chapt.txt in/BuildSystem.chapt.txt

LISPPROGRAMMINGCHAPTERS = LispLanguage.chapt FDL.chapt
LISPPROGRAMMINGSOURCES = in/LispLanguage.chapt.txt in/FDL.chapt.txt


# Yacas code extracted from the docs
CODESNIPPETS = wester-1994.ys

## This means: build wester-1994.ys if running tests
check_SCRIPTS = wester-1994.ys

EXTRA_DIST = \
	manualmaker indexmaker styleplain \
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
	perl $(top_srcdir)/manmake/txt2yacasdoc.pl < yacasapi.chapt.txt > $@

# special chapter copied from the GPL (need to replace "<" and ">")
GPL.chapt: $(top_srcdir)/COPYING
	cat $< | sed -e 's,<,{<},g;s,>,{>},g;' | perl $(top_srcdir)/manmake/txt2yacasdoc.pl > $@

PS_DOCS = intro.book.ps coding.book.ps essays.book.ps ref.book.ps refprog.book.ps Algo.book.ps LispProgramming.book.ps

ps-docs: $(PS_DOCS)

pdf-docs: intro.book.pdf coding.book.pdf essays.book.pdf ref.book.pdf refprog.book.pdf Algo.book.pdf LispProgramming.book.pdf

clean:
	rm -f books.html.c *.tex *.dvi *.aux *.idx *.ind *.ilg *.toc *.log *.ps *.pdf \
	coding*.html intro*.html ref*.html essays*.html Algo*.html LispProgramming*.html *.o core *~ texdocs.c \
	$(REFCHAPTERS) $(ESSAYCHAPTERS) $(REFPROGCHAPTERS) $(CODINGCHAPTERS) $(LISPPROGRAMMINGCHAPTERS) \
	$(CODESNIPPETS) $(ALGOCHAPTERS) yacasapi.chapt.txt $(BOOKS) manicon.html

# Newly-built version of yacas with an extended search path
ouryacas2 = ../src/yacas --rootdir $(top_srcdir)/scripts/:../scripts/:$(srcdir)/:$(srcdir)/in/

# refprog.html is not listed because it is generated together with ref.html
HTMLBOOKS = intro.html coding.html essays.html LispProgramming.html Algo.html ref.html

# The top HTML docs target is now "manicon.html" that is built by GenerateIndex().
# The dependence on $(HTMLBOOKS) is only to make sure that all HTML docs
# are built - in fact manicon.html is a constant file and has no dependencies.
manicon.html:  books.html $(HTMLBOOKS)

# books.html only depends on top book files (*.book.txt).
books.html:  $(BOOKS) $(srcdir)/indexmaker $(srcdir)/styleplain
# this is the old command that generated all HTML docs at once:
#	-$(ouryacas2) $(srcdir)/in/all
	-echo '[Use("indexmaker"); GenerateIndex();];' | $(ouryacas2) -fpc
	# do we need this?
	#echo "done" > manicon.html.c

intro.html : intro.book $(INTROCHAPTERS) $(srcdir)/manualmaker $(srcdir)/styleplain
	-@echo '[Use("manualmaker"); GenerateBook("intro");];' | $(ouryacas2) -fpc

coding.html : coding.book $(CODINGCHAPTERS) $(srcdir)/manualmaker $(srcdir)/styleplain
	-@echo '[Use("manualmaker"); GenerateBook("coding");];' | $(ouryacas2) -fpc

essays.html : essays.book $(ESSAYCHAPTERS) $(srcdir)/manualmaker $(srcdir)/styleplain
	-@echo '[Use("manualmaker"); GenerateBook("essays");];' | $(ouryacas2) -fpc

ref.html : ref.book $(REFCHAPTERS) refprog.book $(REFPROGCHAPTERS) $(srcdir)/manualmaker $(srcdir)/styleplain
	-@echo '[Use("manualmaker"); GenerateTwoBooks("ref","refprog");];' | $(ouryacas2) -fpc

LispProgramming.html : LispProgramming.book $(LISPPROGRAMMINGCHAPTERS) $(srcdir)/manualmaker $(srcdir)/styleplain
	-@echo '[Use("manualmaker"); GenerateBook("LispProgramming");];' | $(ouryacas2) -fpc

Algo.html : Algo.book $(ALGOCHAPTERS) $(srcdir)/manualmaker $(srcdir)/styleplain
	-@echo '[Use("manualmaker"); GenerateBook("Algo");];' | $(ouryacas2) -fpc

# do we need this?
#noindex: intro.html ref.html coding.html essays.html refprog.html

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

	

