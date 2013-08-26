## Makefile.am -- Process this file with automake to produce Makefile.in

bin_PROGRAMS =  yacas
noinst_PROGRAMS = mkfastprimes gencorefunctions compressor
EXTRA_PROGRAMS = compressor codereviewer

noinst_HEADERS = anumber.h dnumber.h choices.h deffile.h grower.h \
	infixparser.h lispassert.h lispatom.h lispcleanupstack.h \
	lispenvironment.h lisperror.h lispeval.h lispevalhash.h \
	lispglobals.h lisphash.h lispio.h lispobject.h lispparser.h \
	lispstring.h lispuserfunc.h corefunctions.h mathcommands.h mathenvironment.h patcher.h  \
	mathuserfunc.h numbers.h platmath.h standard.h patterns.h errors.h arggetter.h \
	stdfileio.h stringio.h stubs.h tokenizer.h yacas.h refcount.h \
	genericobject.h arrayclass.h evalfunc.h commandline.h unixcommandline.h \
        patternclass.h log.h substitute.h stdcommandline.h arrayclass.inl mathutil.h \
        genericstructs.h yacasprivate.h yacasbase.h xmltokenizer.h \
        archiver.h GPL_stuff.h  

yacas_SOURCES = yacasmain.cpp stdcommandline.cpp deffile.cpp infixparser.cpp lispatom.cpp \
	lispcleanupstack.cpp lispenvironment.cpp lispeval.cpp lisperror.cpp \
	lispio.cpp lispobject.cpp lispparser.cpp \
	lispuserfunc.cpp mathcommands.cpp mathenvironment.cpp \
	mathuserfunc.cpp standard.cpp  stdfileio.cpp arggetter.cpp \
	stringio.cpp tokenizer.cpp yacasapi.cpp genericobject.cpp \
	arrayclass.cpp lispevalhash.cpp patterns.cpp patternclass.cpp \
        substitute.cpp mathcommands2.cpp mathcommands3.cpp errors.cpp patcher.cpp \
        genericstructs.cpp \
	xmltokenizer.cpp \
        archiver.cpp compressedfiles.cpp commandline.cpp \
	anumber.cpp dnumber.cpp yacasnumbers.cpp numbers.cpp \
	platmath.cpp grower.cpp stdstubs.cpp obmalloc.cpp lisphash.cpp \
	lispstring.cpp mathutil.cpp yacasbase.cpp debugmem.cpp fastprimes.c

if LINUX
yacas_SOURCES += unixcommandline.cpp
endif

if WIN32
yacas_SOURCES += win32commandline.cpp
endif

yacas_DEPENDENCIES = fastprimes.c version.h GPL_stuff.h 

mathcommands3.$(OBJEXT): version.h

codereviewer_SOURCES = codereviewer.cpp 
codereviewer_LDADD = 
codereviewer_DEPENDENCIES = 


# the program 'gencorefunctions' will prepare a listing of all core functions
gencorefunctions_SOURCES = gencorefunctions.cpp 

# force core_yacasmain.h to be created
$(srcdir)/gencorefunctions.cpp: core_yacasmain.h corefunctions.h

# this file is generated from yacasmain.cpp
core_yacasmain.h: $(srcdir)/yacasmain.cpp
	grep '^CORE_KERNEL_FUNCTION' $(srcdir)/yacasmain.cpp > $@

mkfastprimes_SOURCES = mkfastprimes.cpp 


EXTRA_DIST =  anumber.inl deffile.inl  \
	grower.inl lisphash.inl lispobject.inl \
	lispstring.inl standard.inl \
	depend.dep makefile.genericstatic \
	version.h \
	lisptype.h \
	platfileio.h \
	debugmem.h \
	compressedfiles.cpp \
	decompressor.cpp \
	compressor.cpp \
	compressedfiles.h \
	fastprimes.c core_yacasmain.h 


# list some extra generated files
CLEANFILES = fastprimes.c core_yacasmain.h 

$(srcdir)/platmath.cpp: fastprimes.c

fastprimes.c: mkfastprimes$(EXEEXT)
	./mkfastprimes$(EXEEXT) > fastprimes.c
# We don't want $(srcdir)/mkfastprimes because this executable
# is not generated inside the $(srcdir). 


yacas_include = $(datadir)/yacas/include

clean-compile:
	test -z "libyacasplatform.a" || rm -rf libyacasplatform.a
	test -z "libyacasdebug.a" || rm -rf libyacasdebug.a
	test -z "yacasdebug" || rm -rf yacasdebug

# 'make install' should also install all header files

install-data-local: yacas.h
	$(mkinstalldirs) $(DESTDIR)$(yacas_include)/
	$(mkinstalldirs) $(DESTDIR)$(yacas_lib)/
	-for file in $(srcdir)/*.h; do \
	basefile=`basename $$file`; \
	$(INSTALL_DATA) $$file $(DESTDIR)$(yacas_include)/$$basefile; \
	done
	-for file in $(srcdir)/*.inl; do \
	basefile=`basename $$file`; \
	$(INSTALL_DATA) $$file $(DESTDIR)$(yacas_include)/$$basefile; \
	done

uninstall-hook:
	rm -rf $(DESTDIR)$(yacas_include)/*.h
	rm -rf $(DESTDIR)$(yacas_include)/*.inl

version.h: ../config.h
	echo "#define VERSION \"$(VERSION)\"" > version.h 

noinst_DATA = @SCRIPTS_DAT@
compressor_SOURCES = compressor.cpp filescanner.cpp filescanner.h

scripts.dat: compressor$(EXEEXT)
	./compressor$(EXEEXT) - $(top_srcdir)/scripts/ scripts.dat 

check: @CHECK_ARCHIVE@ check-codereviewer

check-archive: scripts.dat 
	../src/yacas$(EXEEXT) -pc --archive scripts.dat $(top_srcdir)/tests/arithmetic.yts

check-codereviewer: codereviewer$(EXEEXT)
	./codereviewer$(EXEEXT) c *.h *.inl *.cpp

## We need a bit of a hack to install the 'scripts.dat' file.
## Ideally, we would like to declare it as lib_LIBRARY, but then automake
## (at least version 1.4) complains that it doesn't know libraries with
## a .dat extension. So we install it by hand in $(libdir)
## Furthermore, we need to install compressedfiles.h because plugins need it.

install-exec-local: @INSTALL_ARCHIVE@

install-archive: scripts.dat
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(libdir)
	$(INSTALL_PROGRAM) scripts.dat $(DESTDIR)$(libdir)/`echo scripts.dat|sed '$(transform)'`


distclean-local:
	-rm -f codereviewer$(EXEEXT)
	-rm -f version.h

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

