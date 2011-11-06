## Makefile.am -- Process this file with automake to produce Makefile.in

# $(srcdir) is the manmake/ directory and $(top_srcdir) is the Yacas tree
# so this file should work even if we are building Yacas in a different directory

# newly compiled version of yacas in ../src (not necessarily under $(srcdir))
ouryacas = ../src/yacas --rootdir $(top_srcdir)/scripts/:../scripts/

# top target
## BOOKS_HTML is either "manicon.html" or empty
## PS_DOCS    is either "ps-docs" or empty
## PDF_DOCS   is either "pdf-docs" or empty
all-am: @BOOKS_HTML@ @PS_DOCS@ @PDF_DOCS@ hints

noinst_PROGRAMS = manripper removeduplicates

manripper_SOURCES = manripper.cpp 

removeduplicates_SOURCES = removeduplicates.cpp

hints: manripper removeduplicates $(REFSOURCES) $(REFPROGSOURCES) ref.book.txt refprog.book.txt
	rm -f hints.unsorted
	for file in ref.book.txt $(REFSOURCES) refprog.book.txt $(REFPROGSOURCES); do \
	./manripper $(srcdir)/"$$file" >> hints.unsorted ; done
	sort -t ":" hints.unsorted > hints.sorted
	./removeduplicates hints.sorted hints.singlesorted
	echo "::::" > tail.txt
	cat hints.singlesorted tail.txt > hints

## ytxt2tex is to be installed in $(bindir). 
bin_SCRIPTS = ytxt2tex
## It needs the following scripts:
ytxt2tex_DEPS = book2TeX.sh book2TeX.ys txt2yacasdoc.pl

BOOKS = intro.book coding.book ref.book refprog.book essays.book \
	Algo.book 

BOOKSOURCES = intro.book.txt coding.book.txt ref.book.txt refprog.book.txt \
	essays.book.txt Algo.book.txt 

PS_DOCS = intro.book.ps coding.book.ps essays.book.ps ref.book.ps \
	refprog.book.ps Algo.book.ps 

PDF_DOCS = intro.book.pdf coding.book.pdf essays.book.pdf ref.book.pdf \
	refprog.book.pdf Algo.book.pdf 

# refprog.html is not listed because it is generated together with ref.html
HTMLBOOKS = intromanual.html codingmanual.html essaysmanual.html refmanual.html refprogmanual.html \
	Algomanual.html 

INTROCHAPTERS = FDL.chapt cl-options.chapt config.chapt addons.chapt  



INTROSOURCES = FDL.chapt.txt cl-options.chapt.txt config.chapt.txt addons.chapt.txt 


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

REFCHAPTERS = arith.chapt calc.chapt complex.chapt solvers.chapt simplify.chapt ode.chapt linalg.chapt \
	univar.chapt lists.chapt functional.chapt controlflow.chapt \
	preds.chapt logic.chapt const.chapt vars.chapt io.chapt \
	numtheory.chapt strings.chapt transforms.chapt cl-options.chapt \
	probability-and-statistics.chapt FDL.chapt 

REFSOURCES = arith.chapt.txt calc.chapt.txt complex.chapt.txt \
	solvers.chapt.txt simplify.chapt.txt ode.chapt.txt \
	linalg.chapt.txt univar.chapt.txt lists.chapt.txt cl-options.chapt.txt \
	functional.chapt.txt controlflow.chapt.txt preds.chapt.txt \
	logic.chapt.txt const.chapt.txt vars.chapt.txt io.chapt.txt \
	numtheory.chapt.txt strings.chapt.txt transforms.chapt.txt \
	probability-and-statistics.chapt.txt FDL.chapt.txt 


REFPROGCHAPTERS = FDL.chapt glossary.chapt \
	GPL.chapt numerics.chapt
# GPL.chapt has no .txt source
REFPROGSOURCES = FDL.chapt.txt glossary.chapt.txt \
	numerics.chapt.txt

CODINGCHAPTERS = YacasDebugger.chapt FDL.chapt
CODINGSOURCES = YacasDebugger.chapt.txt FDL.chapt.txt

ESSAYCHAPTERS = ABIN-grammar.chapt \
	SimpleTools.chapt CVS-Howto.chapt YacasDocs.chapt \
	wester-1994.chapt new.chapt paper.chapt \
	wordproblems.chapt FDL.chapt BuildSystem.chapt 

ESSAYSOURCES = ABIN-grammar.chapt.txt \
	SimpleTools.chapt.txt CVS-Howto.chapt.txt YacasDocs.chapt.txt \
        paper.chapt.txt wester-1994.chapt.txt new.chapt.txt \
	wordproblems.chapt.txt FDL.chapt.txt BuildSystem.chapt.txt 


# Yacas test code extracted from the docs
TESTCODESNIPPETS = wester-1994.yts ref.book.yts refprog.book.yts

## This means: build wester-1994.yts etc. if running tests
check_SCRIPTS = $(TESTCODESNIPPETS)


EXTRA_DIST = txt2example.pl book2txt.sh book2txt.ys \
	book2xml.sh book2xml.ys \
	book2ys.sh book2ys.ys ys2book.pl \
	ytxt2tex $(ytxt2tex_DEPS) \
	manualmaker indexmaker styleplain \
	$(REFSOURCES) $(BOOKSOURCES) $(ESSAYSOURCES) $(CODINGSOURCES) \
	$(INTROSOURCES) \
	$(REFPROGSOURCES) $(ALGOSOURCES)  \
	dummies hints

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
	@echo "***Documentation created:" $*.pdf

.chapt.ys:
	sh $(srcdir)/book2ys.sh -run "$(ouryacas)" $< $@

.chapt.yts:
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

# special "virtual" chapter built from corefunctions.h. This is printed by a special program "gencorefunctions", compiled together with yacas.


# special chapter copied from the GPL (need to replace "<" and ">")
GPL.chapt: $(top_srcdir)/COPYING
	cat $(top_srcdir)/COPYING | sed -e 's,<,{<},g;s,>,{>},g;' | perl $(srcdir)/txt2yacasdoc.pl > $@



ps-docs: $(PS_DOCS)
pdf-docs: $(PDF_DOCS)

CLEANFILES = books.html *.tex *.dvi *.aux *.idx *.ind *.ilg *.toc *.log *.ps *.pdf *.funcs.js \
	coding*.html intro*.html ref*.html essays*.html Algo*.html  *.o core *~ texdocs.c \
	$(REFCHAPTERS) $(ESSAYCHAPTERS) $(REFPROGCHAPTERS) $(CODINGCHAPTERS)  \
	$(TESTCODESNIPPETS) $(ALGOCHAPTERS) $(BOOKS) manicon.html 

# The top HTML docs target is now "manicon.html" that is built by GenerateIndex().
# The dependence on $(HTMLBOOKS) is only to make sure that all HTML docs
# are built - in fact manicon.html is a constant file and has no dependencies.
manicon.html:  books.html $(HTMLBOOKS)

# books.html only depends on top book files (*.book.txt).
books.html:  $(BOOKS) $(srcdir)/indexmaker $(srcdir)/styleplain
	-echo '[DefaultDirectory("$(srcdir)/"); Use("indexmaker"); GenerateIndex();];' | $(ouryacas) -fpc

intromanual.html : intro.book $(INTROCHAPTERS) $(srcdir)/manualmaker $(srcdir)/styleplain
	-@echo '[DefaultDirectory("$(srcdir)/"); Use("manualmaker"); GenerateBook("intro");];' | $(ouryacas) -fpc

codingmanual.html : coding.book $(CODINGCHAPTERS) $(srcdir)/manualmaker $(srcdir)/styleplain
	-@echo '[DefaultDirectory("$(srcdir)/"); Use("manualmaker"); GenerateBook("coding");];' | $(ouryacas) -fpc

essaysmanual.html : essays.book $(ESSAYCHAPTERS) $(srcdir)/manualmaker $(srcdir)/styleplain
	-@echo '[DefaultDirectory("$(srcdir)/"); Use("manualmaker"); GenerateBook("essays");];' | $(ouryacas) -fpc

refmanual.html : ref.book $(REFCHAPTERS) $(srcdir)/manualmaker $(srcdir)/styleplain
	-@echo '[DefaultDirectory("$(srcdir)/"); Use("manualmaker"); GenerateBook("ref");];'    | $(ouryacas) -fpc

refprogmanual.html : ref.book $(REFCHAPTERS) refprog.book $(REFPROGCHAPTERS) $(srcdir)/manualmaker $(srcdir)/styleplain
	-@echo '[DefaultDirectory("$(srcdir)/"); Use("manualmaker"); GenerateBook("refprog");];' | $(ouryacas) -fpc

Algomanual.html : Algo.book $(ALGOCHAPTERS) $(srcdir)/manualmaker $(srcdir)/styleplain
	-@echo '[DefaultDirectory("$(srcdir)/"); Use("manualmaker"); GenerateBook("Algo");];' | $(ouryacas) -fpc

## Hook to install documentation
install-data-local: @INSTALL_HTML@ @INSTALL_PS@ install-scripts install-hints

install-hints: hints
	$(mkinstalldirs) $(DESTDIR)$(pkgdatadir)/documentation
	$(INSTALL_DATA) hints $(DESTDIR)$(pkgdatadir)/documentation/hints

install-scripts: $(ytxt2tex_DEPS)
	$(mkinstalldirs) $(DESTDIR)$(pkgdatadir)/manmake/
	-for file in $(ytxt2tex_DEPS); do \
	   $(INSTALL_DATA) $(srcdir)/$$file $(DESTDIR)$(pkgdatadir)/manmake/$$file; \
	done

install-html: books.html 
	$(mkinstalldirs) $(DESTDIR)$(htmldir)/
	$(INSTALL_DATA) $(top_srcdir)/docs/yacaslogo.gif $(DESTDIR)$(htmldir)/
	-for file in *.html; do \
	   $(INSTALL_DATA) $$file $(DESTDIR)$(htmldir)/$$file; \
	done

install-ps: ps-docs
	$(mkinstalldirs) $(DESTDIR)$(psdir)/
	-for file in $(PS_DOCS); do \
	   $(INSTALL_DATA) $$file $(DESTDIR)$(psdir)/$$file; \
	done

#	   gzip $(DESTDIR)$(psdir)/$$file; 

