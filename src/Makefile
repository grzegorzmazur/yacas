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
VERSION = 1.0.45
scriptdir = ${prefix}/share/yacas/

DEFS = -DHAVE_CONFIG_H -DSCRIPT_DIR=\"$(scriptdir)\"
INCLUDES = -I$(top_builddir) -I$(srcdir)/plat/linux32 -I../..

bin_PROGRAMS = yacas
noinst_PROGRAMS = testnum
noinst_LIBRARIES = libyacas.a libyacasnumbers.a
EXTRA_LIBRARIES = libgmpnumbers.a libyacasnumbers.a libyacasplatform.a

noinst_HEADERS = anumber.h choices.h deffile.h grower.h 	infixparser.h lispassert.h lispatom.h lispcleanupstack.h 	lispenvironment.h lisperror.h lispeval.h lispevalhash.h 	lispglobals.h lisphash.h lispio.h lispobject.h lispparser.h 	lispstring.h lispuserfunc.h mathcommands.h mathenvironment.h patcher.h 	mathuserfunc.h numbers.h platmath.h standard.h patterns.h errors.h arggetter.h 	stdfileio.h stringio.h stubs.h tokenizer.h yacas.h ramdisk.h refcount.h 	genericobject.h arrayclass.h evalfunc.h commandline.h unixcommandline.h         patternclass.h log.h substitute.h stdcommandline.h arrayclass.inl mathutil.h         lispplugin.h genericstructs.h

yacas_SOURCES = yacasmain.cpp commandline.cpp unixcommandline.cpp stdcommandline.cpp
yacas_LDADD = libyacas.a libyacasplatform.a libyacasnumbers.a -lm 

#
# some one who understands autoconf/automake: how do I detect if a platform
# supports elf binaries?
#

yacas_LDFLAGS = -ldl -rdynamic

testnum_SOURCES = anumber.cpp grower.cpp lispstring.cpp stdstubs.cpp 	testnum.cpp mathutil.cpp

libyacas_a_SOURCES = deffile.cpp infixparser.cpp lispatom.cpp 	lispcleanupstack.cpp lispenvironment.cpp lispeval.cpp 	lispio.cpp lispobject.cpp lispparser.cpp  	lispuserfunc.cpp mathcommands.cpp mathenvironment.cpp 	mathuserfunc.cpp standard.cpp  stdfileio.cpp arggetter.cpp 	stringio.cpp tokenizer.cpp yacasapi.cpp genericobject.cpp 	arrayclass.cpp lispevalhash.cpp patterns.cpp patternclass.cpp         substitute.cpp mathcommands2.cpp mathcommands3.cpp errors.cpp patcher.cpp         lispplugin.cpp elfdll.cpp genericstructs.cpp

libyacasplatform_a_SOURCES = platmath.cpp grower.cpp stdstubs.cpp lisphash.cpp 	lispstring.cpp mathutil.cpp

libyacasplatform_a_LIBADD = libyacasnumbers.a
libyacasnumbers_a_SOURCES = anumber.cpp yacasnumbers.cpp 
libgmpnumbers_a_SOURCES = gmpnumbers.cpp 

EXTRA_DIST = anumber.inl deffile.inl epocfileio.cpp epocmain.cpp 	epocyacas.hrh epocyacas.rss epocyacas.mmp 	epocstubs.cpp grower.inl lisphash.inl lispobject.inl 	lispstring.inl standard.inl 	win32dll.cpp 	depend.dep makefile.beos 	convertcctocpp.bat 	plat/linux32/lisptype.h 	plat/linux32/stubs.inl 	plat/linux32/platfileio.h 	plat/linux32/platdll.h 	plat/linux64/lisptype.h 	plat/linux64/stubs.inl 	plat/linux64/platfileio.h 	plat/linux64/platdll.h 	plat/epoc32/lisptype.h 	plat/epoc32/stubs.inl 	plat/epoc32/platdll.h 	plat/epoc32/platfileio.h 	plat/beos/lisptype.h 	plat/beos/stubs.inl 	plat/beos/platfileio.h 	plat/beos/platdll.h 	plat/win32/lisptype.h 	plat/win32/platdll.h   	plat/win32/platfileio.h 	plat/win32/stubs.inl     	plat/win32/win32commandline.cpp 	plat/win32/win32commandline.h   	plat/win32/yacas.dsp            	plat/win32/yacas_exe.dsp        	plat/win32/yacasmain-win32.cpp

mkinstalldirs = $(SHELL) $(top_srcdir)/mkinstalldirs
CONFIG_HEADER = ../config.h
CONFIG_CLEAN_FILES = 
LIBRARIES =  $(noinst_LIBRARIES)

CPPFLAGS = 
LDFLAGS = 
LIBS = 
libgmpnumbers_a_LIBADD = 
libgmpnumbers_a_OBJECTS =  gmpnumbers.o
libyacasnumbers_a_LIBADD = 
libyacasnumbers_a_OBJECTS =  anumber.o yacasnumbers.o
libyacasplatform_a_DEPENDENCIES = 
libyacasplatform_a_OBJECTS =  platmath.o grower.o stdstubs.o lisphash.o \
lispstring.o mathutil.o
libyacas_a_LIBADD = 
libyacas_a_OBJECTS =  deffile.o infixparser.o lispatom.o \
lispcleanupstack.o lispenvironment.o lispeval.o lispio.o lispobject.o \
lispparser.o lispuserfunc.o mathcommands.o mathenvironment.o \
mathuserfunc.o standard.o stdfileio.o arggetter.o stringio.o \
tokenizer.o yacasapi.o genericobject.o arrayclass.o lispevalhash.o \
patterns.o patternclass.o substitute.o mathcommands2.o mathcommands3.o \
errors.o patcher.o lispplugin.o elfdll.o genericstructs.o
AR = ar
PROGRAMS =  $(bin_PROGRAMS) $(noinst_PROGRAMS)

yacas_OBJECTS =  yacasmain.o commandline.o unixcommandline.o \
stdcommandline.o
yacas_DEPENDENCIES =  libyacas.a libyacasplatform.a
testnum_OBJECTS =  anumber.o grower.o lispstring.o stdstubs.o testnum.o \
mathutil.o
testnum_LDADD = $(LDADD)
testnum_DEPENDENCIES = 
testnum_LDFLAGS = 
CXXFLAGS = -O2
CXXCOMPILE = $(CXX) $(DEFS) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS)
CXXLD = $(CXX)
CXXLINK = $(CXXLD) $(AM_CXXFLAGS) $(CXXFLAGS) $(LDFLAGS) -o $@
HEADERS =  $(noinst_HEADERS)

DIST_COMMON =  Makefile.am Makefile.in


DISTFILES = $(DIST_COMMON) $(SOURCES) $(HEADERS) $(TEXINFOS) $(EXTRA_DIST)

TAR = gtar
GZIP_ENV = --best
SOURCES = $(libgmpnumbers_a_SOURCES) $(libyacasnumbers_a_SOURCES) $(libyacasplatform_a_SOURCES) $(libyacas_a_SOURCES) $(yacas_SOURCES) $(testnum_SOURCES)
OBJECTS = $(libgmpnumbers_a_OBJECTS) $(libyacasnumbers_a_OBJECTS) $(libyacasplatform_a_OBJECTS) $(libyacas_a_OBJECTS) $(yacas_OBJECTS) $(testnum_OBJECTS)

all: all-redirect
.SUFFIXES:
.SUFFIXES: .S .c .cpp .o .s
$(srcdir)/Makefile.in: Makefile.am $(top_srcdir)/configure.in $(ACLOCAL_M4) $(srcdir)/depend.dep
	cd $(top_srcdir) && $(AUTOMAKE) --gnu --include-deps src/Makefile

Makefile: $(srcdir)/Makefile.in  $(top_builddir)/config.status
	cd $(top_builddir) \
	  && CONFIG_FILES=$(subdir)/$@ CONFIG_HEADERS= $(SHELL) ./config.status


mostlyclean-noinstLIBRARIES:

clean-noinstLIBRARIES:
	-test -z "$(noinst_LIBRARIES)" || rm -f $(noinst_LIBRARIES)

distclean-noinstLIBRARIES:

maintainer-clean-noinstLIBRARIES:

.c.o:
	$(COMPILE) -c $<

.s.o:
	$(COMPILE) -c $<

.S.o:
	$(COMPILE) -c $<

mostlyclean-compile:
	-rm -f *.o core *.core

clean-compile:

distclean-compile:
	-rm -f *.tab.c

maintainer-clean-compile:

libgmpnumbers.a: $(libgmpnumbers_a_OBJECTS) $(libgmpnumbers_a_DEPENDENCIES)
	-rm -f libgmpnumbers.a
	$(AR) cru libgmpnumbers.a $(libgmpnumbers_a_OBJECTS) $(libgmpnumbers_a_LIBADD)
	$(RANLIB) libgmpnumbers.a

libyacasnumbers.a: $(libyacasnumbers_a_OBJECTS) $(libyacasnumbers_a_DEPENDENCIES)
	-rm -f libyacasnumbers.a
	$(AR) cru libyacasnumbers.a $(libyacasnumbers_a_OBJECTS) $(libyacasnumbers_a_LIBADD)
	$(RANLIB) libyacasnumbers.a

libyacasplatform.a: $(libyacasplatform_a_OBJECTS) $(libyacasplatform_a_DEPENDENCIES)
	-rm -f libyacasplatform.a
	$(AR) cru libyacasplatform.a $(libyacasplatform_a_OBJECTS) $(libyacasplatform_a_LIBADD)
	$(RANLIB) libyacasplatform.a

libyacas.a: $(libyacas_a_OBJECTS) $(libyacas_a_DEPENDENCIES)
	-rm -f libyacas.a
	$(AR) cru libyacas.a $(libyacas_a_OBJECTS) $(libyacas_a_LIBADD)
	$(RANLIB) libyacas.a

mostlyclean-binPROGRAMS:

clean-binPROGRAMS:
	-test -z "$(bin_PROGRAMS)" || rm -f $(bin_PROGRAMS)

distclean-binPROGRAMS:

maintainer-clean-binPROGRAMS:

install-binPROGRAMS: $(bin_PROGRAMS)
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(bindir)
	@list='$(bin_PROGRAMS)'; for p in $$list; do \
	  if test -f $$p; then \
	    echo "  $(INSTALL_PROGRAM) $(INSTALL_STRIP_FLAG) $$p $(DESTDIR)$(bindir)/`echo $$p|sed 's/$(EXEEXT)$$//'|sed '$(transform)'|sed 's/$$/$(EXEEXT)/'`"; \
	     $(INSTALL_PROGRAM) $(INSTALL_STRIP_FLAG) $$p $(DESTDIR)$(bindir)/`echo $$p|sed 's/$(EXEEXT)$$//'|sed '$(transform)'|sed 's/$$/$(EXEEXT)/'`; \
	  else :; fi; \
	done

uninstall-binPROGRAMS:
	@$(NORMAL_UNINSTALL)
	list='$(bin_PROGRAMS)'; for p in $$list; do \
	  rm -f $(DESTDIR)$(bindir)/`echo $$p|sed 's/$(EXEEXT)$$//'|sed '$(transform)'|sed 's/$$/$(EXEEXT)/'`; \
	done

mostlyclean-noinstPROGRAMS:

clean-noinstPROGRAMS:
	-test -z "$(noinst_PROGRAMS)" || rm -f $(noinst_PROGRAMS)

distclean-noinstPROGRAMS:

maintainer-clean-noinstPROGRAMS:

yacas: $(yacas_OBJECTS) $(yacas_DEPENDENCIES)
	@rm -f yacas
	$(CXXLINK) $(yacas_LDFLAGS) $(yacas_OBJECTS) $(yacas_LDADD) $(LIBS)

testnum: $(testnum_OBJECTS) $(testnum_DEPENDENCIES)
	@rm -f testnum
	$(CXXLINK) $(testnum_LDFLAGS) $(testnum_OBJECTS) $(testnum_LDADD) $(LIBS)
.cpp.o:
	$(CXXCOMPILE) -c $<

tags: TAGS

ID: $(HEADERS) $(SOURCES) $(LISP)
	list='$(SOURCES) $(HEADERS)'; \
	unique=`for i in $$list; do echo $$i; done | \
	  awk '    { files[$$0] = 1; } \
	       END { for (i in files) print i; }'`; \
	here=`pwd` && cd $(srcdir) \
	  && mkid -f$$here/ID $$unique $(LISP)

TAGS:  $(HEADERS) $(SOURCES)  $(TAGS_DEPENDENCIES) $(LISP)
	tags=; \
	here=`pwd`; \
	list='$(SOURCES) $(HEADERS)'; \
	unique=`for i in $$list; do echo $$i; done | \
	  awk '    { files[$$0] = 1; } \
	       END { for (i in files) print i; }'`; \
	test -z "$(ETAGS_ARGS)$$unique$(LISP)$$tags" \
	  || (cd $(srcdir) && etags $(ETAGS_ARGS) $$tags  $$unique $(LISP) -o $$here/TAGS)

mostlyclean-tags:

clean-tags:

distclean-tags:
	-rm -f TAGS ID

maintainer-clean-tags:

distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)

subdir = src

distdir: $(DISTFILES)
	$(mkinstalldirs) $(distdir)/plat/beos $(distdir)/plat/epoc32 \
	   $(distdir)/plat/linux32 $(distdir)/plat/linux64 \
	   $(distdir)/plat/win32
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
install-exec-am: install-binPROGRAMS
install-exec: install-exec-am

install-data-am:
install-data: install-data-am

install-am: all-am
	@$(MAKE) $(AM_MAKEFLAGS) install-exec-am install-data-am
install: install-am
uninstall-am: uninstall-binPROGRAMS
uninstall: uninstall-am
all-am: Makefile $(LIBRARIES) $(PROGRAMS) $(HEADERS)
all-redirect: all-am
install-strip:
	$(MAKE) $(AM_MAKEFLAGS) INSTALL_STRIP_FLAG=-s install
installdirs:
	$(mkinstalldirs)  $(DESTDIR)$(bindir)


mostlyclean-generic:

clean-generic:

distclean-generic:
	-rm -f Makefile $(CONFIG_CLEAN_FILES)
	-rm -f config.cache config.log stamp-h stamp-h[0-9]*

maintainer-clean-generic:
mostlyclean-am:  mostlyclean-noinstLIBRARIES mostlyclean-compile \
		mostlyclean-binPROGRAMS mostlyclean-noinstPROGRAMS \
		mostlyclean-tags mostlyclean-generic

mostlyclean: mostlyclean-am

clean-am:  clean-noinstLIBRARIES clean-compile clean-binPROGRAMS \
		clean-noinstPROGRAMS clean-tags clean-generic \
		mostlyclean-am

clean: clean-am

distclean-am:  distclean-noinstLIBRARIES distclean-compile \
		distclean-binPROGRAMS distclean-noinstPROGRAMS \
		distclean-tags distclean-generic clean-am

distclean: distclean-am

maintainer-clean-am:  maintainer-clean-noinstLIBRARIES \
		maintainer-clean-compile maintainer-clean-binPROGRAMS \
		maintainer-clean-noinstPROGRAMS maintainer-clean-tags \
		maintainer-clean-generic distclean-am
	@echo "This command is intended for maintainers to use;"
	@echo "it deletes files that may require special tools to rebuild."

maintainer-clean: maintainer-clean-am

.PHONY: mostlyclean-noinstLIBRARIES distclean-noinstLIBRARIES \
clean-noinstLIBRARIES maintainer-clean-noinstLIBRARIES \
mostlyclean-compile distclean-compile clean-compile \
maintainer-clean-compile mostlyclean-binPROGRAMS distclean-binPROGRAMS \
clean-binPROGRAMS maintainer-clean-binPROGRAMS uninstall-binPROGRAMS \
install-binPROGRAMS mostlyclean-noinstPROGRAMS distclean-noinstPROGRAMS \
clean-noinstPROGRAMS maintainer-clean-noinstPROGRAMS tags \
mostlyclean-tags distclean-tags clean-tags maintainer-clean-tags \
distdir info-am info dvi-am dvi check check-am installcheck-am \
installcheck install-exec-am install-exec install-data-am install-data \
install-am install uninstall-am uninstall all-redirect all-am all \
installdirs mostlyclean-generic distclean-generic clean-generic \
maintainer-clean-generic clean mostlyclean distclean maintainer-clean


anumber.o: anumber.cpp anumber.h grower.h stubs.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h \
 plat/linux32/stubs.inl grower.inl lispassert.h choices.h lispstring.h \
 refcount.h lispstring.inl anumber.inl mathutil.h
arggetter.o: arggetter.cpp lispenvironment.h lispobject.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h refcount.h \
 lispassert.h choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h lisphash.h lisphash.inl \
 lispevalhash.h lispcleanupstack.h deffile.h deffile.inl ramdisk.h \
 lispio.h stringio.h lispglobals.h lispplugin.h standard.h lispatom.h \
 numbers.h standard.inl errors.h lispeval.h arggetter.h platmath.h \
 genericstructs.h
arrayclass.o: arrayclass.cpp arrayclass.h plat/linux32/lisptype.h \
 plat/linux32/../../choices.h lispobject.h refcount.h lispassert.h \
 choices.h lispstring.h grower.h stubs.h plat/linux32/stubs.inl \
 grower.inl lispstring.inl genericobject.h evalfunc.h lispobject.inl \
 lisperror.h arrayclass.inl
commandline.o: commandline.cpp commandline.h lispstring.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispassert.h choices.h refcount.h \
 lispstring.inl
deffile.o: deffile.cpp deffile.h lisphash.h grower.h stubs.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h \
 plat/linux32/stubs.inl grower.inl lispassert.h choices.h lispstring.h \
 refcount.h lispstring.inl lisphash.inl deffile.inl lispuserfunc.h \
 lispobject.h genericobject.h evalfunc.h lispobject.inl lisperror.h \
 lispenvironment.h lispevalhash.h lispcleanupstack.h ramdisk.h \
 lispio.h stringio.h lispglobals.h lispplugin.h standard.h lispatom.h \
 numbers.h standard.inl plat/linux32/platfileio.h tokenizer.h
elfdll.o: elfdll.cpp plat/linux32/lisptype.h \
 plat/linux32/../../choices.h lispenvironment.h lispobject.h \
 refcount.h lispassert.h choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h lisphash.h lisphash.inl \
 lispevalhash.h lispcleanupstack.h deffile.h deffile.inl ramdisk.h \
 lispio.h stringio.h lispglobals.h lispplugin.h plat/linux32/platdll.h \
 plat/linux32/../../lispplugin.h
epocfileio.o: epocfileio.cpp stdfileio.h lispio.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h \
 lispenvironment.h lispobject.h refcount.h lispassert.h choices.h \
 lispstring.h grower.h stubs.h plat/linux32/stubs.inl grower.inl \
 lispstring.inl genericobject.h evalfunc.h lispobject.inl lisperror.h \
 lisphash.h lisphash.inl lispevalhash.h lispcleanupstack.h deffile.h \
 deffile.inl ramdisk.h stringio.h lispglobals.h lispplugin.h \
 plat/linux32/platfileio.h
epocmain.o: epocmain.cpp epocyacas.hrh yacas.h lispstring.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispassert.h choices.h refcount.h \
 lispstring.inl stringio.h lispio.h tokenizer.h lisphash.h \
 lisphash.inl lispevalhash.h lispobject.h genericobject.h evalfunc.h \
 lispobject.inl lisperror.h infixparser.h lispparser.h stdfileio.h \
 lispenvironment.h lispcleanupstack.h deffile.h deffile.inl ramdisk.h \
 lispglobals.h lispplugin.h plat/linux32/platfileio.h lispatom.h \
 lispeval.h mathenvironment.h lispuserfunc.h commandline.h standard.h \
 numbers.h standard.inl
epocstubs.o: epocstubs.cpp lisperror.h plat/linux32/lisptype.h \
 plat/linux32/../../choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispassert.h choices.h refcount.h \
 lispstring.inl
errors.o: errors.cpp lispenvironment.h lispobject.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h refcount.h \
 lispassert.h choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h lisphash.h lisphash.inl \
 lispevalhash.h lispcleanupstack.h deffile.h deffile.inl ramdisk.h \
 lispio.h stringio.h lispglobals.h lispplugin.h standard.h lispatom.h \
 numbers.h standard.inl lispeval.h errors.h
genericobject.o: genericobject.cpp genericobject.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h
genericstructs.o: genericstructs.cpp genericstructs.h genericobject.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h
gmpnumbers.o: gmpnumbers.cpp plat/linux32/lisptype.h \
 plat/linux32/../../choices.h numbers.h lispenvironment.h lispobject.h \
 refcount.h lispassert.h choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h lisphash.h lisphash.inl \
 lispevalhash.h lispcleanupstack.h deffile.h deffile.inl ramdisk.h \
 lispio.h stringio.h lispglobals.h lispplugin.h standard.h lispatom.h \
 standard.inl platmath.h
grower.o: grower.cpp grower.h stubs.h plat/linux32/lisptype.h \
 plat/linux32/../../choices.h plat/linux32/stubs.inl grower.inl \
 lispassert.h choices.h
infixparser.o: infixparser.cpp lispassert.h choices.h infixparser.h \
 lispparser.h lispobject.h plat/linux32/lisptype.h \
 plat/linux32/../../choices.h refcount.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h tokenizer.h lispio.h lisphash.h \
 lisphash.inl lispatom.h standard.h lispenvironment.h lispevalhash.h \
 lispcleanupstack.h deffile.h deffile.inl ramdisk.h stringio.h \
 lispglobals.h lispplugin.h numbers.h standard.inl
lispatom.o: lispatom.cpp lispatom.h lispobject.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h refcount.h \
 lispassert.h choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h
lispcleanupstack.o: lispcleanupstack.cpp lispcleanupstack.h grower.h \
 stubs.h plat/linux32/lisptype.h plat/linux32/../../choices.h \
 plat/linux32/stubs.inl grower.inl lispassert.h choices.h
lispenvironment.o: lispenvironment.cpp choices.h lispenvironment.h \
 lispobject.h plat/linux32/lisptype.h plat/linux32/../../choices.h \
 refcount.h lispassert.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h lisphash.h lisphash.inl \
 lispevalhash.h lispcleanupstack.h deffile.h deffile.inl ramdisk.h \
 lispio.h stringio.h lispglobals.h lispplugin.h lispeval.h lispatom.h \
 standard.h numbers.h standard.inl lispuserfunc.h mathuserfunc.h
lispeval.o: lispeval.cpp lispeval.h lispobject.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h refcount.h \
 lispassert.h choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h lispenvironment.h lisphash.h \
 lisphash.inl lispevalhash.h lispcleanupstack.h deffile.h deffile.inl \
 ramdisk.h lispio.h stringio.h lispglobals.h lispplugin.h \
 lispuserfunc.h standard.h lispatom.h numbers.h standard.inl \
 plat/linux32/platfileio.h infixparser.h lispparser.h tokenizer.h \
 errors.h
lispevalhash.o: lispevalhash.cpp lispevalhash.h lispobject.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h refcount.h \
 lispassert.h choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h lisphash.h lisphash.inl
lisphash.o: lisphash.cpp lisperror.h plat/linux32/lisptype.h \
 plat/linux32/../../choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispassert.h choices.h refcount.h \
 lispstring.inl lisphash.h lisphash.inl
lispio.o: lispio.cpp lispio.h plat/linux32/lisptype.h \
 plat/linux32/../../choices.h
lispobject.o: lispobject.cpp lispobject.h plat/linux32/lisptype.h \
 plat/linux32/../../choices.h refcount.h lispassert.h choices.h \
 lispstring.h grower.h stubs.h plat/linux32/stubs.inl grower.inl \
 lispstring.inl genericobject.h evalfunc.h lispobject.inl lisperror.h
lispparser.o: lispparser.cpp lispparser.h lispobject.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h refcount.h \
 lispassert.h choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h tokenizer.h lispio.h lisphash.h \
 lisphash.inl lispatom.h
lispplugin.o: lispplugin.cpp plat/linux32/lisptype.h \
 plat/linux32/../../choices.h lispenvironment.h lispobject.h \
 refcount.h lispassert.h choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h lisphash.h lisphash.inl \
 lispevalhash.h lispcleanupstack.h deffile.h deffile.inl ramdisk.h \
 lispio.h stringio.h lispglobals.h lispplugin.h
lispstring.o: lispstring.cpp lispstring.h plat/linux32/lisptype.h \
 plat/linux32/../../choices.h grower.h stubs.h plat/linux32/stubs.inl \
 grower.inl lispassert.h choices.h refcount.h lispstring.inl
lispuserfunc.o: lispuserfunc.cpp lispuserfunc.h lispobject.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h refcount.h \
 lispassert.h choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h lispenvironment.h lisphash.h \
 lisphash.inl lispevalhash.h lispcleanupstack.h deffile.h deffile.inl \
 ramdisk.h lispio.h stringio.h lispglobals.h lispplugin.h standard.h \
 lispatom.h numbers.h standard.inl
mathcommands.o: mathcommands.cpp lispenvironment.h lispobject.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h refcount.h \
 lispassert.h choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h lisphash.h lisphash.inl \
 lispevalhash.h lispcleanupstack.h deffile.h deffile.inl ramdisk.h \
 lispio.h stringio.h lispglobals.h lispplugin.h standard.h lispatom.h \
 numbers.h standard.inl lispeval.h lispparser.h tokenizer.h \
 stdfileio.h plat/linux32/platfileio.h infixparser.h lispuserfunc.h \
 mathuserfunc.h platmath.h anumber.h anumber.inl arrayclass.h \
 arrayclass.inl patternclass.h patterns.h substitute.h errors.h \
 arggetter.h
mathcommands2.o: mathcommands2.cpp lispenvironment.h lispobject.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h refcount.h \
 lispassert.h choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h lisphash.h lisphash.inl \
 lispevalhash.h lispcleanupstack.h deffile.h deffile.inl ramdisk.h \
 lispio.h stringio.h lispglobals.h lispplugin.h standard.h lispatom.h \
 numbers.h standard.inl lispeval.h lispparser.h tokenizer.h \
 stdfileio.h plat/linux32/platfileio.h infixparser.h lispuserfunc.h \
 mathuserfunc.h platmath.h anumber.h anumber.inl arrayclass.h \
 arrayclass.inl patternclass.h patterns.h substitute.h errors.h
mathcommands3.o: mathcommands3.cpp lispenvironment.h lispobject.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h refcount.h \
 lispassert.h choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h lisphash.h lisphash.inl \
 lispevalhash.h lispcleanupstack.h deffile.h deffile.inl ramdisk.h \
 lispio.h stringio.h lispglobals.h lispplugin.h standard.h lispatom.h \
 numbers.h standard.inl lispeval.h lispparser.h tokenizer.h \
 stdfileio.h plat/linux32/platfileio.h infixparser.h lispuserfunc.h \
 mathuserfunc.h platmath.h anumber.h anumber.inl arrayclass.h \
 arrayclass.inl patternclass.h patterns.h substitute.h errors.h \
 patcher.h plat/linux32/platdll.h plat/linux32/../../lispplugin.h
mathenvironment.o: mathenvironment.cpp mathenvironment.h \
 lispenvironment.h lispobject.h plat/linux32/lisptype.h \
 plat/linux32/../../choices.h refcount.h lispassert.h choices.h \
 lispstring.h grower.h stubs.h plat/linux32/stubs.inl grower.inl \
 lispstring.inl genericobject.h evalfunc.h lispobject.inl lisperror.h \
 lisphash.h lisphash.inl lispevalhash.h lispcleanupstack.h deffile.h \
 deffile.inl ramdisk.h lispio.h stringio.h lispglobals.h lispplugin.h \
 lispatom.h standard.h numbers.h standard.inl lispuserfunc.h \
 mathuserfunc.h
mathuserfunc.o: mathuserfunc.cpp mathuserfunc.h lispuserfunc.h \
 lispobject.h plat/linux32/lisptype.h plat/linux32/../../choices.h \
 refcount.h lispassert.h choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h lispenvironment.h lisphash.h \
 lisphash.inl lispevalhash.h lispcleanupstack.h deffile.h deffile.inl \
 ramdisk.h lispio.h stringio.h lispglobals.h lispplugin.h lispeval.h \
 standard.h lispatom.h numbers.h standard.inl patterns.h \
 patternclass.h
mathutil.o: mathutil.cpp plat/linux32/lisptype.h \
 plat/linux32/../../choices.h
patcher.o: patcher.cpp plat/linux32/lisptype.h \
 plat/linux32/../../choices.h lispio.h standard.h lispobject.h \
 refcount.h lispassert.h choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h lispenvironment.h lisphash.h \
 lisphash.inl lispevalhash.h lispcleanupstack.h deffile.h deffile.inl \
 ramdisk.h stringio.h lispglobals.h lispplugin.h lispatom.h numbers.h \
 standard.inl
patternclass.o: patternclass.cpp patternclass.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h lispobject.h \
 refcount.h lispassert.h choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h patterns.h lispenvironment.h \
 lisphash.h lisphash.inl lispevalhash.h lispcleanupstack.h deffile.h \
 deffile.inl ramdisk.h lispio.h stringio.h lispglobals.h lispplugin.h
patterns.o: patterns.cpp patterns.h plat/linux32/lisptype.h \
 plat/linux32/../../choices.h grower.h stubs.h plat/linux32/stubs.inl \
 grower.inl lispassert.h choices.h lispenvironment.h lispobject.h \
 refcount.h lispstring.h lispstring.inl genericobject.h evalfunc.h \
 lispobject.inl lisperror.h lisphash.h lisphash.inl lispevalhash.h \
 lispcleanupstack.h deffile.h deffile.inl ramdisk.h lispio.h \
 stringio.h lispglobals.h lispplugin.h standard.h lispatom.h numbers.h \
 standard.inl mathuserfunc.h lispuserfunc.h lispeval.h log.h
platmath.o: platmath.cpp plat/linux32/lisptype.h \
 plat/linux32/../../choices.h lispobject.h refcount.h lispassert.h \
 choices.h lispstring.h grower.h stubs.h plat/linux32/stubs.inl \
 grower.inl lispstring.inl genericobject.h evalfunc.h lispobject.inl \
 lisperror.h lispatom.h lispenvironment.h lisphash.h lisphash.inl \
 lispevalhash.h lispcleanupstack.h deffile.h deffile.inl ramdisk.h \
 lispio.h stringio.h lispglobals.h lispplugin.h
standard.o: standard.cpp standard.h lispobject.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h refcount.h \
 lispassert.h choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h lispenvironment.h lisphash.h \
 lisphash.inl lispevalhash.h lispcleanupstack.h deffile.h deffile.inl \
 ramdisk.h lispio.h stringio.h lispglobals.h lispplugin.h lispatom.h \
 numbers.h standard.inl plat/linux32/platfileio.h mathutil.h \
 tokenizer.h infixparser.h lispparser.h lispeval.h
stdcommandline.o: stdcommandline.cpp stdcommandline.h commandline.h \
 lispstring.h plat/linux32/lisptype.h plat/linux32/../../choices.h \
 grower.h stubs.h plat/linux32/stubs.inl grower.inl lispassert.h \
 choices.h refcount.h lispstring.inl
stdfileio.o: stdfileio.cpp stdfileio.h lispio.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h \
 lispenvironment.h lispobject.h refcount.h lispassert.h choices.h \
 lispstring.h grower.h stubs.h plat/linux32/stubs.inl grower.inl \
 lispstring.inl genericobject.h evalfunc.h lispobject.inl lisperror.h \
 lisphash.h lisphash.inl lispevalhash.h lispcleanupstack.h deffile.h \
 deffile.inl ramdisk.h stringio.h lispglobals.h lispplugin.h \
 plat/linux32/platfileio.h
stdstubs.o: stdstubs.cpp lisperror.h plat/linux32/lisptype.h \
 plat/linux32/../../choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispassert.h choices.h refcount.h \
 lispstring.inl
stringio.o: stringio.cpp stringio.h lispio.h plat/linux32/lisptype.h \
 plat/linux32/../../choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispassert.h choices.h refcount.h \
 lispstring.inl
substitute.o: substitute.cpp substitute.h lispobject.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h refcount.h \
 lispassert.h choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h lispatom.h standard.h \
 lispenvironment.h lisphash.h lisphash.inl lispevalhash.h \
 lispcleanupstack.h deffile.h deffile.inl ramdisk.h lispio.h \
 stringio.h lispglobals.h lispplugin.h numbers.h standard.inl
testnum.o: testnum.cpp anumber.h grower.h stubs.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h \
 plat/linux32/stubs.inl grower.inl lispassert.h choices.h lispstring.h \
 refcount.h lispstring.inl anumber.inl
tokenizer.o: tokenizer.cpp tokenizer.h lispstring.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispassert.h choices.h refcount.h \
 lispstring.inl lispio.h lisphash.h lisphash.inl lisperror.h numbers.h \
 lispenvironment.h lispobject.h genericobject.h evalfunc.h \
 lispobject.inl lispevalhash.h lispcleanupstack.h deffile.h \
 deffile.inl ramdisk.h stringio.h lispglobals.h lispplugin.h
unixcommandline.o: unixcommandline.cpp unixcommandline.h commandline.h \
 lispstring.h plat/linux32/lisptype.h plat/linux32/../../choices.h \
 grower.h stubs.h plat/linux32/stubs.inl grower.inl lispassert.h \
 choices.h refcount.h lispstring.inl
win32dll.o: win32dll.cpp plat/linux32/lisptype.h \
 plat/linux32/../../choices.h lispenvironment.h lispobject.h \
 refcount.h lispassert.h choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h lisphash.h lisphash.inl \
 lispevalhash.h lispcleanupstack.h deffile.h deffile.inl ramdisk.h \
 lispio.h stringio.h lispglobals.h lispplugin.h plat/linux32/platdll.h \
 plat/linux32/../../lispplugin.h
yacasapi.o: yacasapi.cpp yacas.h lispstring.h plat/linux32/lisptype.h \
 plat/linux32/../../choices.h grower.h stubs.h plat/linux32/stubs.inl \
 grower.inl lispassert.h choices.h refcount.h lispstring.inl \
 stringio.h lispio.h tokenizer.h lisphash.h lisphash.inl \
 lispevalhash.h lispobject.h genericobject.h evalfunc.h lispobject.inl \
 lisperror.h infixparser.h lispparser.h stdfileio.h lispenvironment.h \
 lispcleanupstack.h deffile.h deffile.inl ramdisk.h lispglobals.h \
 lispplugin.h plat/linux32/platfileio.h lispatom.h lispeval.h \
 mathenvironment.h lispuserfunc.h mathcommands.h standard.h numbers.h \
 standard.inl
yacasmain.o: yacasmain.cpp yacas.h lispstring.h \
 plat/linux32/lisptype.h plat/linux32/../../choices.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispassert.h choices.h refcount.h \
 lispstring.inl stringio.h lispio.h tokenizer.h lisphash.h \
 lisphash.inl lispevalhash.h lispobject.h genericobject.h evalfunc.h \
 lispobject.inl lisperror.h infixparser.h lispparser.h stdfileio.h \
 lispenvironment.h lispcleanupstack.h deffile.h deffile.inl ramdisk.h \
 lispglobals.h lispplugin.h plat/linux32/platfileio.h lispatom.h \
 lispeval.h mathenvironment.h lispuserfunc.h unixcommandline.h \
 commandline.h stdcommandline.h standard.h numbers.h standard.inl \
 arggetter.h
yacasnumbers.o: yacasnumbers.cpp plat/linux32/lisptype.h \
 plat/linux32/../../choices.h numbers.h lispenvironment.h lispobject.h \
 refcount.h lispassert.h choices.h lispstring.h grower.h stubs.h \
 plat/linux32/stubs.inl grower.inl lispstring.inl genericobject.h \
 evalfunc.h lispobject.inl lisperror.h lisphash.h lisphash.inl \
 lispevalhash.h lispcleanupstack.h deffile.h deffile.inl ramdisk.h \
 lispio.h stringio.h lispglobals.h lispplugin.h standard.h lispatom.h \
 standard.inl anumber.h anumber.inl platmath.h

depend:
	gcc -MM *.cpp $(INCLUDES) > depend.dep

# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
