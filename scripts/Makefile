# Generated automatically from Makefile.in by configure.
# Makefile.in generated automatically by automake 1.4a from Makefile.am

# Copyright (C) 1994, 1995-8, 1999 Free Software Foundation, Inc.
# This Makefile.in is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.


SHELL = /bin/sh

srcdir = .
top_srcdir = ..
prefix = /usr/local
exec_prefix = ${prefix}

bindir = ${exec_prefix}/bin
sbindir = ${exec_prefix}/sbin
libexecdir = ${exec_prefix}/libexec
datadir = ${prefix}/share
sysconfdir = ${prefix}/etc
sharedstatedir = ${prefix}/com
localstatedir = ${prefix}/var
libdir = ${exec_prefix}/lib
infodir = ${prefix}/info
mandir = ${prefix}/man
includedir = ${prefix}/include
oldincludedir = /usr/include

DESTDIR =

pkgdatadir = $(datadir)/yacas
pkglibdir = $(libdir)/yacas
pkgincludedir = $(includedir)/yacas

top_builddir = ..

ACLOCAL = aclocal
AUTOCONF = autoconf
AUTOMAKE = automake
AUTOHEADER = autoheader

INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_SCRIPT = ${INSTALL_PROGRAM}
INSTALL_STRIP_FLAG =
transform = s,x,x,

NORMAL_INSTALL = :
PRE_INSTALL = :
POST_INSTALL = :
NORMAL_UNINSTALL = :
PRE_UNINSTALL = :
POST_UNINSTALL = :
CC = gcc
CPP = gcc -E
CXX = c++
MAKEINFO = makeinfo
NUMBERS_LDFLAGS = -lm
NUMBERS_LIB = libyacasnumbers.a
PACKAGE = yacas
RANLIB = ranlib
RELEASE = 1
VERSION = 1.0.41rev6
scriptdir = ${prefix}/share/yacas/

SCRIPTFILES = complex complex.def constants controlflow controlflow.def 	deffunc deriv deriv.def functional functional.def glview 	integrate integrate.def linalg linalg.def lists lists.def newly 	newly.def numbers numbers.def random random.def simplify 	simplify.def solve solve.def standard stdfuncs stdfuncs.def 	stdopers stubs stubs.def substitute substitute.def sums sums.def 	univar univar.def yacasinit assoc assoc.def html html.def 	fakedb fakedb.def array array.def example example.def 	trigsimp trigsimp.def formula formula.def patterns 	patterns.def predicates predicates.def stats stats.def padic 	padic.def testers testers.def factors factors.def tensor tensor.def 	texform texform.def print c_form logic logic.def cstubgen 


script_DATA = $(SCRIPTFILES)

EXTRA_DIST = $(SCRIPTFILES) 	examples/tests 	examples/findsum 	examples/queens examples/benchmark 	examples/benchmark2 examples/benchbuild   	addons/README addons/editvi addons/editvi.def addons/gnuplot         addons/gnuplot.def

mkinstalldirs = $(SHELL) $(top_srcdir)/mkinstalldirs
CONFIG_HEADER = ../config.h
CONFIG_CLEAN_FILES = 
DATA =  $(script_DATA)

DIST_COMMON =  Makefile.am Makefile.in


DISTFILES = $(DIST_COMMON) $(SOURCES) $(HEADERS) $(TEXINFOS) $(EXTRA_DIST)

TAR = gtar
GZIP_ENV = --best
all: all-redirect
.SUFFIXES:
$(srcdir)/Makefile.in: Makefile.am $(top_srcdir)/configure.in $(ACLOCAL_M4) 
	cd $(top_srcdir) && $(AUTOMAKE) --gnu --include-deps scripts/Makefile

Makefile: $(srcdir)/Makefile.in  $(top_builddir)/config.status
	cd $(top_builddir) \
	  && CONFIG_FILES=$(subdir)/$@ CONFIG_HEADERS= $(SHELL) ./config.status


install-scriptDATA: $(script_DATA)
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(scriptdir)
	@list='$(script_DATA)'; for p in $$list; do \
	  if test -f $(srcdir)/$$p; then \
	    echo " $(INSTALL_DATA) $(srcdir)/$$p $(DESTDIR)$(scriptdir)/$$p"; \
	    $(INSTALL_DATA) $(srcdir)/$$p $(DESTDIR)$(scriptdir)/$$p; \
	  else if test -f $$p; then \
	    echo " $(INSTALL_DATA) $$p $(DESTDIR)$(scriptdir)/$$p"; \
	    $(INSTALL_DATA) $$p $(DESTDIR)$(scriptdir)/$$p; \
	  fi; fi; \
	done

uninstall-scriptDATA:
	@$(NORMAL_UNINSTALL)
	list='$(script_DATA)'; for p in $$list; do \
	  rm -f $(DESTDIR)$(scriptdir)/$$p; \
	done
tags: TAGS
TAGS:


distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)

subdir = scripts

distdir: $(DISTFILES)
	$(mkinstalldirs) $(distdir)/addons $(distdir)/examples
	@for file in $(DISTFILES); do \
	  d=$(srcdir); \
	  if test -d $$d/$$file; then \
	    cp -pr $$d/$$file $(distdir)/$$file; \
	  else \
	    test -f $(distdir)/$$file \
	    || ln $$d/$$file $(distdir)/$$file 2> /dev/null \
	    || cp -p $$d/$$file $(distdir)/$$file || :; \
	  fi; \
	done
info-am:
info: info-am
dvi-am:
dvi: dvi-am
check-am: all-am
check: check-am
installcheck-am:
installcheck: installcheck-am
install-exec-am:
install-exec: install-exec-am

install-data-am: install-scriptDATA install-data-local
install-data: install-data-am

install-am: all-am
	@$(MAKE) $(AM_MAKEFLAGS) install-exec-am install-data-am
install: install-am
uninstall-am: uninstall-scriptDATA
uninstall: uninstall-am
all-am: Makefile $(DATA)
all-redirect: all-am
install-strip:
	$(MAKE) $(AM_MAKEFLAGS) INSTALL_STRIP_FLAG=-s install
installdirs:
	$(mkinstalldirs)  $(DESTDIR)$(scriptdir)


mostlyclean-generic:

clean-generic:

distclean-generic:
	-rm -f Makefile $(CONFIG_CLEAN_FILES)
	-rm -f config.cache config.log stamp-h stamp-h[0-9]*

maintainer-clean-generic:
mostlyclean-am:  mostlyclean-generic

mostlyclean: mostlyclean-am

clean-am:  clean-generic mostlyclean-am

clean: clean-am

distclean-am:  distclean-generic clean-am

distclean: distclean-am

maintainer-clean-am:  maintainer-clean-generic distclean-am
	@echo "This command is intended for maintainers to use;"
	@echo "it deletes files that may require special tools to rebuild."

maintainer-clean: maintainer-clean-am

.PHONY: uninstall-scriptDATA install-scriptDATA tags distdir info-am \
info dvi-am dvi check check-am installcheck-am installcheck \
install-exec-am install-exec install-data-local install-data-am \
install-data install-am install uninstall-am uninstall all-redirect \
all-am all installdirs mostlyclean-generic distclean-generic \
clean-generic maintainer-clean-generic clean mostlyclean distclean \
maintainer-clean


install-data-local:
	-mkdir $(scriptdir)addons/
	-cp addons/README $(scriptdir)addons/
	-cp addons/editvi* $(scriptdir)addons/
	-cp addons/gnuplot* $(scriptdir)addons/

# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
