## Makefile.am -- Process this file with automake to produce Makefile.in

INCLUDES = @INCLTDL@ -I$(srcdir)/plat/linux32

bin_PROGRAMS =  yacas
noinst_PROGRAMS = mkfastprimes testnum gencorefunctions compressor

lib_LTLIBRARIES = libyacas.la libyacasplatform.la libcyacas.la @NUMBERS_LIB@ 
EXTRA_LTLIBRARIES = libgmpnumbers.la libyacasnumbers.la 

pkglib_LTLIBRARIES = libmath.la

noinst_HEADERS = anumber.h choices.h deffile.h grower.h \
	infixparser.h lispassert.h lispatom.h lispcleanupstack.h \
	lispenvironment.h lisperror.h lispeval.h lispevalhash.h \
	lispglobals.h lisphash.h lispio.h lispobject.h lispparser.h \
	lispstring.h lispuserfunc.h corefunctions.h mathcommands.h mathenvironment.h patcher.h exedll.h \
	mathuserfunc.h numbers.h platmath.h standard.h patterns.h errors.h arggetter.h \
	stdfileio.h stringio.h stubs.h tokenizer.h yacas.h refcount.h \
	genericobject.h arrayclass.h evalfunc.h commandline.h unixcommandline.h \
        patternclass.h log.h substitute.h stdcommandline.h arrayclass.inl mathutil.h \
        lispplugin.h genericstructs.h ctokenizer.h yacasprivate.h yacasbase.h xmltokenizer.h \
        archiver.h elfdll.h cyacas.h GPL_stuff.h

yacas_SOURCES = yacasmain.cpp commandline.cpp unixcommandline.cpp stdcommandline.cpp
yacas_LDADD = @NUMBERS_LIB@ libyacas.la libyacasplatform.la @LIBLTDL@ @NUMBERS_LDFLAGS@
yacas_DEPENDENCIES = @NUMBERS_LIB@ libyacas.la libyacasplatform.la fastprimes.c version.h GPL_stuff.h

# order of libraries is important, @NUMBERS_LIB@ before libyacas.la and that is before libyacasplatform.la

testnum_SOURCES = testnum.cpp 
testnum_LDADD = @NUMBERS_LIB@ libyacas.la libyacasplatform.la @LIBLTDL@ @NUMBERS_LDFLAGS@
testnum_DEPENDENCIES = @NUMBERS_LIB@ libyacas.la libyacasplatform.la

# the program 'gencorefunctions' will prepare a listing of all core functions
gencorefunctions_SOURCES = gencorefunctions.cpp 

# force core_yacasmain.h to be created
$(srcdir)/gencorefunctions.cpp: core_yacasmain.h corefunctions.h

# this file is generated from yacasmain.cpp
core_yacasmain.h: $(srcdir)/yacasmain.cpp
	grep '^CORE_KERNEL_FUNCTION' $(srcdir)/yacasmain.cpp > $@

mkfastprimes_SOURCES = mkfastprimes.cpp 

libyacas_la_SOURCES = deffile.cpp infixparser.cpp lispatom.cpp \
	lispcleanupstack.cpp lispenvironment.cpp lispeval.cpp \
	lispio.cpp lispobject.cpp lispparser.cpp  \
	lispuserfunc.cpp mathcommands.cpp mathenvironment.cpp \
	mathuserfunc.cpp standard.cpp  stdfileio.cpp arggetter.cpp \
	stringio.cpp tokenizer.cpp yacasapi.cpp genericobject.cpp \
	arrayclass.cpp lispevalhash.cpp patterns.cpp patternclass.cpp \
        substitute.cpp mathcommands2.cpp mathcommands3.cpp errors.cpp patcher.cpp \
        lispplugin.cpp ltdll.cpp elfdll.cpp exedll.cpp genericstructs.cpp unipoly.cpp ctokenizer.cpp xmltokenizer.cpp \
        archiver.cpp compressedfiles.cpp 


libcyacas_la_SOURCES = cyacas.cpp

libyacasplatform_la_SOURCES = platmath.cpp grower.cpp stdstubs.cpp obmalloc.cpp lisphash.cpp \
	lispstring.cpp mathutil.cpp yacasbase.cpp debugmem.cpp 
libyacasplatform_la_DEPENDENCIES= fastprimes.c

## The -rpath flag is not added automatically because lib*numbers is listed in EXTRA_LTLIBRARIES,
## so automake does not know in what directory it is installed.
libyacasnumbers_la_SOURCES = anumber.cpp yacasnumbers.cpp numbers.cpp
libyacasnumbers_la_LDFLAGS = -rpath $(libdir)
libgmpnumbers_la_SOURCES = gmpnumbers.cpp numbers.cpp
libgmpnumbers_la_LDFLAGS = -rpath $(libdir)


libmath_la_SOURCES = libmath.cpp
libmath_la_DEPENDENCIES = yacas gencorefunctions

# assuming the directory below does not have plugins!
$(srcdir)/libmath.cpp: yacas $(top_srcdir)/scripts/base.rep/math.ys gencorefunctions core_yacasmain.h 
	(cd ../scripts && make packages.ys corefunctions.ys)
	echo "Set(LoadPlugIns,False);Use(\"yacasinit.ys\");" > compileinit.txt
	echo "CompileCpp(\"base.rep/math.ys\",\"libmath\");" > compile.txt
	./yacas --rootdir  $(srcdir)/../scripts:../scripts -pc --init compileinit.txt compile.txt
	mv libmath.cpp $(srcdir)/libmath.cpp

EXTRA_DIST = anumber.inl deffile.inl epocfileio.cpp epocmain.cpp epocmaincmd.cpp \
	epocyacas.hrh epocyacas.mmp epocyacasconsole.mmp \
	epocstubs.cpp grower.inl lisphash.inl lispobject.inl \
	lispstring.inl standard.inl \
	win32dll.cpp win32dll.h \
	depend.dep makefile.beos makefile.genericstatic \
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
	plat/win32/yacasmain-win32.cpp Makefile.win32 \
	makefile.agenda yacas.dsp yacas.dsw \
  debugmem.h mkfastprimes.dsp \
	win32dllresource.h win32yacasdll.h win32yacasdll.cpp \
	win32yacasdllresource.h yacasdll.dsp yacasdll.dsw yacasdll.rc \
	setup.py yacasc.i yacas.py \
	epoccli.cpp epoccli.h epoccli.mmp epocclsv.h epocserv.cpp epocserv.h \
	epocserv.mmp yacas.rss elfdll.cpp \
	compressedfiles.cpp \
	README.LZO lzoconf.h minilzo.c minilzo.h decompressor.cpp \
	compressor.cpp filescanner.cpp filescanner.h \
	compressedfiles.h compress.bat \
	compressor.dsp compressor.dsw win32rcwriter.cpp 


# list some extra generated files
CLEANFILES = fastprimes.c core_yacasmain.h libmath.cpp 

$(srcdir)/platmath.cpp: fastprimes.c

fastprimes.c: mkfastprimes
	./mkfastprimes > fastprimes.c
# We don't want $(srcdir)/mkfastprimes because this executable
# is not generated inside the $(srcdir). 
 
yacas_include = $(datadir)/yacas/include

clean-compile:
	test -z "libyacasplatform.a" || rm -rf libyacasplatform.a
	test -z "libyacasdebug.a" || rm -rf libyacasdebug.a
	test -z "yacasdebug" || rm -rf yacasdebug

# 'make install' should also install all header files

install-data-local: yacas.h
	$(mkinstalldirs) $(yacas_include)/
	$(mkinstalldirs) $(yacas_lib)/
	$(mkinstalldirs) $(yacas_include)/plat/
	$(mkinstalldirs) $(yacas_include)/plat/linux32/
	-for file in $(srcdir)/*.h; do \
	basefile=`basename $$file`; \
	$(INSTALL_DATA) $$file $(yacas_include)/$$basefile; \
	done
	-for file in $(srcdir)/*.inl; do \
	basefile=`basename $$file`; \
	$(INSTALL_DATA) $$file $(yacas_include)/$$basefile; \
	done
	-for file in $(srcdir)/plat/linux32/*.h; do \
	basefile=`basename $$file`; \
	$(INSTALL_DATA) $$file $(yacas_include)/plat/linux32/$$basefile; \
	done
	-for file in $(srcdir)/plat/linux32/*.inl; do \
	basefile=`basename $$file`; \
	$(INSTALL_DATA) $$file $(yacas_include)/plat/linux32/$$basefile; \
	done

version.h: ../config.h
	echo "#define VERSION \"$(VERSION)\"" > version.h 

noinst_DATA = scripts.dat
checkTARGETS = check-archive
compressor_SOURCES = compressor.cpp filescanner.cpp minilzo.c filescanner.h minilzo.h 

scripts.dat: compressor
	./compressor - $(top_srcdir)/scripts/ scripts.dat 

check: $(checkTARGETS)

check-archive: scripts.dat 
	../src/yacas -pc --archive scripts.dat $(top_srcdir)/tests/arithmetic.yts


## We need a bit of a hack to install the 'scripts.dat' file.
## Ideally, we would like to declare it as lib_LIBRARY, but then automake
## (at least version 1.4) complains that it doesn't know libraries with
## a .dat extension. So we install it by hand in $(libdir)
## Furthermore, we need to install compressedfiles.h because plugins need it.

install-exec-local: scripts.dat
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(libdir)
	$(INSTALL_PROGRAM) scripts.dat $(DESTDIR)$(libdir)/`echo scripts.dat|sed '$(transform)'`


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

