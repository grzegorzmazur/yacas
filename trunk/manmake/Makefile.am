## Makefile.am -- Process this file with automake to produce Makefile.in

noinst_PROGRAMS = manmake
manmake_SOURCES = manmake.cc

# ytxt2tex could be installed into /usr/bin. It needs the following scripts:
ytxt2tex_DEPS = book2TeX.sh book2TeX.ys txt2yacasdoc.pl

EXTRA_DIST = README mangen test.txt \
	api2txt.pl book2txt.sh book2txt.ys \
	book2xml.sh book2xml.ys \
	book2ys.sh book2ys.ys ys2book.pl \
	ytxt2tex $(ytxt2tex_DEPS)

bin_SCRIPTS = ytxt2tex

install-data-local: $(ytxt2tex_DEPS)
	$(mkinstalldirs) $(pkgdatadir)/manmake/
	-for file in $(ytxt2tex_DEPS); do \
	   $(INSTALL_DATA) $(srcdir)/$$file $(pkgdatadir)/manmake/$$file; \
	done
	
	
