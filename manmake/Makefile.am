## Makefile.am -- Process this file with automake to produce Makefile.in

# $(srcdir) is the manmake/ directory and $(top_srcdir) is the Yacas tree
# so this file should work even if we are building Yacas in a different directory

# newly compiled version of yacas in ../src (not necessarily under $(srcdir))
ouryacas = ../src/yacas --rootdir $(top_srcdir)/scripts/:../scripts/

# top target
## BOOKS_HTML is either "manicon.html" or empty
## PS_DOCS    is either "ps-docs" or empty
## PDF_DOCS   is either "pdf-docs" or empty
all-am: @BOOKS_HTML@ @PS_DOCS@ @PDF_DOCS@

## ytxt2tex is to be installed in $(bindir). 
bin_SCRIPTS = ytxt2tex
## It needs the following scripts:
ytxt2tex_DEPS = book2TeX.sh book2TeX.ys txt2yacasdoc.pl

BOOKS = intro.book coding.book ref.book refprog.book essays.book \
	Algo.book LispProgramming.book NewDesign.book

BOOKSOURCES = intro.book.txt coding.book.txt ref.book.txt refprog.book.txt \
	essays.book.txt Algo.book.txt LispProgramming.book.txt NewDesign.book.txt

PS_DOCS = intro.book.ps coding.book.ps essays.book.ps ref.book.ps \
	refprog.book.ps Algo.book.ps LispProgramming.book.ps \
	NewDesign.book.ps

PDF_DOCS = intro.book.pdf coding.book.pdf essays.book.pdf ref.book.pdf \
	refprog.book.pdf Algo.book.pdf LispProgramming.book.pdf \
	NewDesign.book.pdf

# refprog.html is not listed because it is generated together with ref.html
HTMLBOOKS = intro.html coding.html essays.html ref.html \
	Algo.html LispProgramming.html NewDesign.html

INTROCHAPTERS = FDL.chapt cl-options.chapt

INTROSOURCES = FDL.chapt.txt cl-options.chapt.txt

NEWDESIGNCHAPTERS = GlobalMatching.chapt NewDesignMisc.chapt FDL.chapt
NEWDESIGNSOURCES = GlobalMatching.chapt.txt NewDesignMisc.chapt.txt \
	FDL.chapt.txt

ALGOCHAPTERS = algo-basic.chapt algo-numapprox.chapt \
	algorithms-elemfunc.chapt algorithms-specfunc.chapt \
	algorithms-numtheory.chapt algorithms-integration.chapt \
	algorithms-multivar.chapt SturmSequences.chapt FDL.chapt \
	algo-refs.chapt algo-contfrac.chapt algorithms-transforms.chapt

ALGOSOURCES = algo-basic.chapt.txt algo-numapprox.chapt.txt \
	algorithms-elemfunc.chapt.txt algorithms-specfunc.chapt.txt \
	algorithms-numtheory.chapt.txt algorithms-integration.chapt.txt \
	algorithms-multivar.chapt.txt SturmSequences.chapt.txt FDL.chapt.txt \
	algo-refs.chapt.txt algo-contfrac.chapt.txt algorithms-transforms.chapt.txt

REFCHAPTERS = config.chapt arith.chapt calc.chapt ode.chapt linalg.chapt \
	univar.chapt lists.chapt functional.chapt controlflow.chapt \
	preds.chapt logic.chapt const.chapt vars.chapt io.chapt \
	numtheory.chapt strings.chapt transforms.chapt cl-options.chapt \
	probability-and-statistics.chapt addons.chapt FDL.chapt \
	plugin-docs.chapt

REFSOURCES = config.chapt.txt arith.chapt.txt calc.chapt.txt ode.chapt.txt \
	linalg.chapt.txt univar.chapt.txt lists.chapt.txt cl-options.chapt.txt \
	functional.chapt.txt controlflow.chapt.txt preds.chapt.txt \
	logic.chapt.txt const.chapt.txt vars.chapt.txt io.chapt.txt \
	numtheory.chapt.txt strings.chapt.txt transforms.chapt.txt \
	probability-and-statistics.chapt.txt addons.chapt.txt FDL.chapt.txt \
	plugin-docs.chapt.txt


REFPROGCHAPTERS = yacasapi.chapt FDL.chapt glossary.chapt \
	GPL.chapt numerics.chapt
# yacasapi.chapt and GPL.chapt have no .txt source
REFPROGSOURCES = FDL.chapt.txt glossary.chapt.txt \
	numerics.chapt.txt

CODINGCHAPTERS = YacasDebugger.chapt FDL.chapt
CODINGSOURCES = YacasDebugger.chapt.txt FDL.chapt.txt

ESSAYCHAPTERS = ABIN-grammar.chapt Plugins-Howto.chapt \
	SimpleTools.chapt CVS-Howto.chapt YacasDocs.chapt \
	wester-1994.chapt new.chapt paper.chapt codecheck.chapt \
	wordproblems.chapt FDL.chapt BuildSystem.chapt 

ESSAYSOURCES = ABIN-grammar.chapt.txt Plugins-Howto.chapt.txt \
	SimpleTools.chapt.txt CVS-Howto.chapt.txt YacasDocs.chapt.txt \
        paper.chapt.txt wester-1994.chapt.txt new.chapt.txt \
	wordproblems.chapt.txt FDL.chapt.txt BuildSystem.chapt.txt \
	codecheck.chapt.txt 

LISPPROGRAMMINGCHAPTERS = LispLanguage.chapt FDL.chapt
LISPPROGRAMMINGSOURCES = LispLanguage.chapt.txt FDL.chapt.txt


# Yacas test code extracted from the docs
TESTCODESNIPPETS = wester-1994.ys ref.book.yts refprog.book.yts

## This means: build wester-1994.ys etc. if running tests
check_SCRIPTS = $(TESTCODESNIPPETS)


EXTRA_DIST = txt2example.pl book2txt.sh book2txt.ys \
	book2xml.sh book2xml.ys \
	book2ys.sh book2ys.ys ys2book.pl \
	ytxt2tex $(ytxt2tex_DEPS) \
	manualmaker indexmaker styleplain \
	$(REFSOURCES) $(BOOKSOURCES) $(ESSAYSOURCES) $(CODINGSOURCES) \
	$(INTROSOURCES) $(NEWDESIGNSOURCES) \
	$(REFPROGSOURCES) $(ALGOSOURCES) $(LISPPROGRAMMINGSOURCES) \
	dummies 

# Stuff for automatic generation of Yacas books

SUFFIXES = .chapt.txt .chapt .book.tex .book.txt .book .tex .dvi .ps .pdf .ys

.chapt.txt.chapt:
	perl $(srcdir)/txt2yacasdoc.pl < $< > $@

.book.txt.book:
	perl $(srcdir)/txt2yacasdoc.pl < $< > $@

.book.book.tex: 
	sh $(srcdir)/book2TeX.sh -run "$(ouryacas)" $< $@

.tex.dvi:
	latex $< > /dev/null
	latex $< > /dev/null
# The perl filter is used to help the 'makeindex' command: 
# 1) replace \verb|...| by \verb=...= 
# 2) escape ()|@! using a preceding quote "
# Note that @ and ! are not always escaped because they are sometimes part of the index entry and sometimes control chars. Heuristics are used to guess correctly.
	@perl -pe 's/(\\verb)\|([^|]*)\|/$$1=$$2=/g;s/([()|])/"$$1/g;if(/\@/){s/!/"!/g;};s/\@([^\\])/"\@$$1/g;'<$*.idx>$*.1.idx
	@makeindex $*.1.idx -o $*.ind || echo "***Warning: a problem with 'makeindex'"
	latex $< > /dev/null

.dvi.ps:
	dvips -q -o $@ $<
	@echo "***Documentation created:" $*.ps

.dvi.pdf:
	pdflatex $* > /dev/null
	@echo Documentation created: $*.pdf

.chapt.ys:
	sh $(srcdir)/book2ys.sh -run "$(ouryacas)" $< $@

# generate test code from book sources

ref.book.yts: $(REFSOURCES) $(srcdir)/txt2example.pl
	@echo "// tests for ref.book" > $@
	for file in $(REFSOURCES); do perl $(srcdir)/txt2example.pl $(srcdir)/$$file >> $@; done

refprog.book.yts: $(REFPROGSOURCES) $(srcdir)/txt2example.pl
	@echo "// tests for refprog.book" > $@
	for file in $(REFPROGSOURCES); do perl $(srcdir)/txt2example.pl $(srcdir)/$$file >> $@; done


# specify books that need chapters
essays.book.tex: essays.book $(ESSAYCHAPTERS)
ref.book.tex : ref.book $(REFCHAPTERS)
refprog.book.tex : refprog.book $(REFPROGCHAPTERS)
coding.book.tex : coding.book $(CODINGCHAPTERS)
intro.book.tex: intro.book $(INTROCHAPTERS)
Algo.book.tex: Algo.book $(ALGOCHAPTERS)
LispProgramming.book.tex: LispProgramming.book $(LISPPROGRAMMINGCHAPTERS)
NewDesign.book.tex: NewDesign.book $(NEWDESIGNCHAPTERS)

# special "virtual" chapter built from corefunctions.h. This is printed by a special program "gencorefunctions", compiled together with yacas.

yacasapi.chapt.txt: ../src/gencorefunctions
	../src/gencorefunctions refman | sort > yacasapi.chapt.txt

# special chapter copied from the GPL (need to replace "<" and ">")
GPL.chapt: $(top_srcdir)/COPYING
	cat $(top_srcdir)/COPYING | sed -e 's,<,{<},g;s,>,{>},g;' | perl $(srcdir)/txt2yacasdoc.pl > $@


# this chapter depends on special chapters
PLUGINDOCSCHAPTERS = yacas_gsl.chapt example.chapt opengl.chapt forth.chapt
plugin-docs.chapt: $(PLUGINDOCSCHAPTERS)

# special chapters built from plugin stubs
yacas_gsl.chapt: ../plugins/yacas_gsl/yacas_gslapi.man.txt
	perl $(srcdir)/txt2yacasdoc.pl < ../plugins/yacas_gsl/yacas_gslapi.man.txt > $@
example.chapt: ../plugins/example/barepluginapi.man.txt
	perl $(srcdir)/txt2yacasdoc.pl < ../plugins/example/barepluginapi.man.txt > $@
opengl.chapt: ../plugins/opengl/openglapi.man.txt
	perl $(srcdir)/txt2yacasdoc.pl < ../plugins/opengl/openglapi.man.txt > $@
forth.chapt: ../plugins/forth/forthpluginapi.man.txt
	perl $(srcdir)/txt2yacasdoc.pl < ../plugins/forth/forthpluginapi.man.txt > $@


ps-docs: $(PS_DOCS)
pdf-docs: $(PDF_DOCS)

CLEANFILES = books.html *.tex *.dvi *.aux *.idx *.ind *.ilg *.toc *.log *.ps *.pdf \
	coding*.html intro*.html ref*.html essays*.html Algo*.html NewDesign*.html LispProgramming*.html *.o core *~ texdocs.c \
	$(REFCHAPTERS) $(ESSAYCHAPTERS) $(REFPROGCHAPTERS) $(CODINGCHAPTERS) $(LISPPROGRAMMINGCHAPTERS) $(NEWDESIGNCHAPTERS) \
	$(TESTCODESNIPPETS) $(ALGOCHAPTERS) yacasapi.chapt.txt $(BOOKS) manicon.html \
	$(PLUGINDOCSCHAPTERS)

# The top HTML docs target is now "manicon.html" that is built by GenerateIndex().
# The dependence on $(HTMLBOOKS) is only to make sure that all HTML docs
# are built - in fact manicon.html is a constant file and has no dependencies.
manicon.html:  books.html $(HTMLBOOKS)

# books.html only depends on top book files (*.book.txt).
books.html:  $(BOOKS) $(srcdir)/indexmaker $(srcdir)/styleplain
	-echo '[DefaultDirectory("$(srcdir)/"); Use("indexmaker"); GenerateIndex();];' | $(ouryacas) -fpc

intro.html : intro.book $(INTROCHAPTERS) $(srcdir)/manualmaker $(srcdir)/styleplain
	-@echo '[DefaultDirectory("$(srcdir)/"); Use("manualmaker"); GenerateBook("intro");];' | $(ouryacas) -fpc

NewDesign.html : NewDesign.book $(NEWDESIGNCHAPTERS) $(srcdir)/manualmaker $(srcdir)/styleplain
	-@echo '[DefaultDirectory("$(srcdir)/"); Use("manualmaker"); GenerateBook("NewDesign");];' | $(ouryacas) -fpc

coding.html : coding.book $(CODINGCHAPTERS) $(srcdir)/manualmaker $(srcdir)/styleplain
	-@echo '[DefaultDirectory("$(srcdir)/"); Use("manualmaker"); GenerateBook("coding");];' | $(ouryacas) -fpc

essays.html : essays.book $(ESSAYCHAPTERS) $(srcdir)/manualmaker $(srcdir)/styleplain
	-@echo '[DefaultDirectory("$(srcdir)/"); Use("manualmaker"); GenerateBook("essays");];' | $(ouryacas) -fpc

ref.html : ref.book $(REFCHAPTERS) refprog.book $(REFPROGCHAPTERS) $(srcdir)/manualmaker $(srcdir)/styleplain
	-@echo '[DefaultDirectory("$(srcdir)/"); Use("manualmaker"); GenerateTwoBooks("ref","refprog");];' | $(ouryacas) -fpc

LispProgramming.html : LispProgramming.book $(LISPPROGRAMMINGCHAPTERS) $(srcdir)/manualmaker $(srcdir)/styleplain
	-@echo '[DefaultDirectory("$(srcdir)/"); Use("manualmaker"); GenerateBook("LispProgramming");];' | $(ouryacas) -fpc

Algo.html : Algo.book $(ALGOCHAPTERS) $(srcdir)/manualmaker $(srcdir)/styleplain
	-@echo '[DefaultDirectory("$(srcdir)/"); Use("manualmaker"); GenerateBook("Algo");];' | $(ouryacas) -fpc

## Hook to install documentation
install-data-local: @INSTALL_HTML@ @INSTALL_PS@ install-scripts

install-scripts: $(ytxt2tex_DEPS)
	$(mkinstalldirs) $(pkgdatadir)/manmake/
	-for file in $(ytxt2tex_DEPS); do \
	   $(INSTALL_DATA) $(srcdir)/$$file $(pkgdatadir)/manmake/$$file; \
	done

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
