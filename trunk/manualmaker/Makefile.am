
noinst_PROGRAMS = books.html 

EXTRA_DIST = \
	manualmaker styleplain \
	in/all in/arith.chapt in/built.chapt in/calc.chapt \
	in/coding.book.txt in/const.chapt in/functional.chapt  \
	in/intro.book.txt in/io.chapt in/linalg.chapt in/dummies  \
	in/lists.chapt in/preds.chapt in/prog.chapt        \
        in/refprog.book in/controlflow.chapt newhelp in/plugins.chapt   \
	in/ref.book in/univar.chapt in/vars.chapt yacasinit.ys in/generic.chapt \
	in/intromaker in/refmaker in/refprogmaker in/codingmaker in/logic.chapt \
        in/config.chapt in/strings.chapt .cvsignore

%.chapt: %.txt
	perl ../manmake/txt2yacasdoc.pl < $< > $@
%.book: %.book.txt
	perl ../manmake/txt2yacasdoc.pl < $< > $@


yacas_helpdir = $(datadir)/yacas/documentation

books.html: in/arith.chapt in/built.chapt in/calc.chapt \
	in/coding.book in/const.chapt in/functional.chapt  \
	in/intro.book in/io.chapt in/linalg.chapt in/plugins.chapt \
	in/lists.chapt in/preds.chapt in/prog.chapt in/controlflow.chapt \
	in/ref.book in/univar.chapt in/vars.chapt in/generic.chapt in/logic.chapt
	-../src/yacas in/all
	echo "done" > books.html.c

texdocs:
	echo "done" > texdocs.c

intro.html : in/intro.book
	-../src/yacas in/intromaker 
	touch books.html

ref.html : in/ref.book \
	in/arith.chapt in/calc.chapt in/linalg.chapt in/univar.chapt in/controlflow.chapt \
	in/lists.chapt in/functional.chapt in/preds.chapt in/const.chapt \
	in/vars.chapt in/io.chapt in/prog.chapt in/built.chapt in/generic.chapt in/logic.chapt
	-../src/yacas in/refmaker
	touch books.html

refprog.html : in/refprog.book 
	-../src/yacas in/refprogmaker
	touch books.html

coding.html : in/coding.book
	-../src/yacas in/codingmaker
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


