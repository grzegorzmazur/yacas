AM_CXXFLAGS = -DSCRIPT_DIR=\"$(scriptdir)\" $(NUMBERS_CFLAGS)
AM_CPPFLAGS = -I$(srcdir)/plat/linux32 $(NUMBERS_CFLAGS)

bin_PROGRAMS = yacas
noinst_PROGRAMS = testnum
noinst_LTLIBRARIES = libyacas.la libyacasplatform.la libcyacas.la libltdl.la @NUMBERS_LIB@ 
EXTRA_LTLIBRARIES = libgmpnumbers.la libyacasnumbers.la 

noinst_HEADERS = anumber.h choices.h deffile.h grower.h \
	infixparser.h lispassert.h lispatom.h lispcleanupstack.h \
	lispenvironment.h lisperror.h lispeval.h lispevalhash.h \
	lispglobals.h lisphash.h lispio.h lispobject.h lispparser.h \
	lispstring.h lispuserfunc.h mathcommands.h mathenvironment.h patcher.h \
	mathuserfunc.h numbers.h platmath.h standard.h patterns.h errors.h arggetter.h \
	stdfileio.h stringio.h stubs.h tokenizer.h yacas.h ramdisk.h refcount.h \
	genericobject.h arrayclass.h evalfunc.h commandline.h unixcommandline.h \
        patternclass.h log.h substitute.h stdcommandline.h arrayclass.inl mathutil.h \
        lispplugin.h genericstructs.h ctokenizer.h yacasprivate.h yacasbase.h xmltokenizer.h \
        archiver.h elfdll.h cyacas.h ltdl.h
yacas_SOURCES = yacasmain.cpp commandline.cpp unixcommandline.cpp stdcommandline.cpp
yacas_LDADD = libyacas.la libyacasplatform.la libltdl.la @NUMBERS_LIB@ @NUMBERS_LDFLAGS@
yacas_LDFLAGS = -export-dynamic
yacas_DEPENDENCIES = libyacas.la libyacasplatform.la @NUMBERS_LIB@

#
# some one who understands autoconf/automake: how do I detect if a platform
# supports elf binaries?
#

#yacas_LDFLAGS = $(LIBS)

testnum_SOURCES = testnum.cpp 
testnum_LDADD = libyacas.la libyacasplatform.la libltdl.la @NUMBERS_LIB@ @NUMBERS_LDFLAGS@
testnum_LDFLAGS = -export-dynamic
testnum_DEPENDENCIES = libyacas.la libyacasplatform.la @NUMBERS_LIB@


libyacas_la_SOURCES = deffile.cpp infixparser.cpp lispatom.cpp \
	lispcleanupstack.cpp lispenvironment.cpp lispeval.cpp \
	lispio.cpp lispobject.cpp lispparser.cpp  \
	lispuserfunc.cpp mathcommands.cpp mathenvironment.cpp \
	mathuserfunc.cpp standard.cpp  stdfileio.cpp arggetter.cpp \
	stringio.cpp tokenizer.cpp yacasapi.cpp genericobject.cpp \
	arrayclass.cpp lispevalhash.cpp patterns.cpp patternclass.cpp \
        substitute.cpp mathcommands2.cpp mathcommands3.cpp errors.cpp patcher.cpp \
        lispplugin.cpp ltdll.cpp genericstructs.cpp unipoly.cpp ctokenizer.cpp xmltokenizer.cpp \
        archiver.cpp
libyacas_la_LDFLAGS = -export-dynamic

libcyacas_la_SOURCES = cyacas.cpp
libcyacas_la_LDFLAGS = -export-dynamic

libyacasplatform_la_SOURCES = platmath.cpp grower.cpp stdstubs.cpp obmalloc.cpp lisphash.cpp \
	lispstring.cpp mathutil.cpp yacasbase.cpp
libyacasplatform_la_LDFLAGS = -export-dynamic
libyacasnumbers_la_SOURCES = anumber.cpp yacasnumbers.cpp 
libyacasnumbers_la_LDFLAGS = -export-dynamic
libgmpnumbers_la_SOURCES = gmpnumbers.cpp 
libgmpnumbers_la_LDFLAGS = -export-dynamic


libltdl_la_SOURCES = ltdl.c
libltdl_la_LDFLAGS = -export-dynamic -version-info 1:2:1
libltdl_la_LIBADD = 



EXTRA_DIST = anumber.inl deffile.inl epocfileio.cpp epocmain.cpp epocmaincmd.cpp \
	epocyacas.hrh epocyacas.mmp epocyacasconsole.mmp \
	epocstubs.cpp grower.inl lisphash.inl lispobject.inl \
	lispstring.inl standard.inl \
	win32dll.cpp win32dll.h \
	depend.dep makefile.beos \
	version.h \
	convertcctocpp.bat \
	plat/linux32/lisptype.h \
	plat/linux32/stubs.inl \
	plat/linux32/platfileio.h \
	plat/linux32/platdll.h \
	plat/linux64/lisptype.h \
	plat/linux64/stubs.inl \
	plat/linux64/platfileio.h \
	plat/linux64/platdll.h \
	plat/epoc32/lisptype.h \
	plat/epoc32/stubs.inl \
	plat/epoc32/platdll.h \
	plat/epoc32/platfileio.h \
	plat/beos/lisptype.h \
	plat/beos/stubs.inl \
	plat/beos/platfileio.h \
	plat/beos/platdll.h \
	plat/win32/lisptype.h \
	plat/win32/platdll.h   \
	plat/win32/platfileio.h \
	plat/win32/stubs.inl  unipoly.h   \
	plat/win32/win32commandline.cpp \
	plat/win32/win32commandline.h   \
	plat/win32/yacas.dsp            \
	plat/win32/yacas_exe.dsp        \
	plat/win32/yacasmain-win32.cpp makefile.debug Makefile.win32 \
	makefile.agenda yacas.dsp yacas.dsw \
  debugmem.cpp debugmem.h \
	win32dllresource.h win32yacasdll.h win32yacasdll.cpp \
	win32yacasdllresource.h yacasdll.dsp yacasdll.dsw yacasdll.rc \
	setup.py yacasc.i yacas.py \
	epoccli.cpp epoccli.h epoccli.mmp epocclsv.h epocserv.cpp epocserv.h \
	epocserv.mmp yacas.rss \
  elfdll.cpp mkfastprimes.pl

CLEANFILES = fastprimes.c

platmath.cpp: fastprimes.c

fastprimes.c: mkfastprimes.pl
	perl $(srcdir)/mkfastprimes.pl > fastprimes.c
 
yacas_include = $(datadir)/yacas/include

yacas_lib     = $(datadir)/yacas/lib

clean-compile:
	test -z "libyacasplatform.a" || rm -rf libyacasplatform.a
	test -z "libyacasdebug.a" || rm -rf libyacasdebug.a
	test -z "yacasdebug" || rm -rf yacasdebug

## Do we want to install headers or not?

install-data-local: yacas.h
	$(mkinstalldirs) $(yacas_include)/
	$(mkinstalldirs) $(yacas_lib)/
	$(mkinstalldirs) $(yacas_include)/plat/
	$(mkinstalldirs) $(yacas_include)/plat/linux32/
	-for file in $(srcdir)/*.h; do \
	basefile=`basename $$file`; \
	$(INSTALL_DATA) $(srcdir)/$$file $(yacas_include)/$$basefile; \
	done
	-for file in $(srcdir)/*.inl; do \
	basefile=`basename $$file`; \
	$(INSTALL_DATA) $(srcdir)/$$file $(yacas_include)/$$basefile; \
	done
	-for file in $(srcdir)/plat/linux32/*.h; do \
	basefile=`basename $$file`; \
	$(INSTALL_DATA) $(srcdir)/$$file $(yacas_include)/plat/linux32/$$basefile; \
	done
	-for file in $(srcdir)/plat/linux32/*.inl; do \
	basefile=`basename $$file`; \
	$(INSTALL_DATA) $(srcdir)/$$file $(yacas_include)/plat/linux32/$$basefile; \
	done
	-for file in $(srcdir)/*.a; do \
	basefile=`basename $$file`; \
	$(INSTALL_DATA) $(srcdir)/$$file $(yacas_lib)/$$basefile; \
	done

versionstamper:
	echo "#define VERSION \"$(VERSION)\"" > version.h 


# 
# depend.dep contains a list of dependencies. gcc can generate this
# when the flag -MM is given on the command line. It is needed when
# a *.cpp file includes a new header. Also, when changing classes,
# eg. adding or removing properties or virtual methods, the build needs
# the dependency list to determine which files to recompile.
# 

include depend.dep

depend:
	cc -MM *.cpp $(AM_CPPFLAGS) > depend.dep

