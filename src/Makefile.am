DEFS = @DEFS@ -DSCRIPT_DIR=\"$(scriptdir)\"
INCLUDES = -I$(top_builddir) -I$(srcdir)/plat/linux32 -I../..



bin_PROGRAMS = yacas
noinst_PROGRAMS = testnum
noinst_LIBRARIES = libyacas.a @NUMBERS_LIB@
EXTRA_LIBRARIES = libgmpnumbers.a libyacasnumbers.a libyacasplatform.a

noinst_HEADERS = anumber.h choices.h deffile.h grower.h \
	infixparser.h lispassert.h lispatom.h lispcleanupstack.h \
	lispenvironment.h lisperror.h lispeval.h lispevalhash.h \
	lispglobals.h lisphash.h lispio.h lispobject.h lispparser.h \
	lispstring.h lispuserfunc.h mathcommands.h mathenvironment.h patcher.h \
	mathuserfunc.h numbers.h platmath.h standard.h patterns.h errors.h arggetter.h \
	stdfileio.h stringio.h stubs.h tokenizer.h yacas.h ramdisk.h refcount.h \
	genericobject.h arrayclass.h evalfunc.h commandline.h unixcommandline.h \
        patternclass.h log.h substitute.h stdcommandline.h arrayclass.inl mathutil.h \
        lispplugin.h genericstructs.h ctokenizer.h
yacas_SOURCES = yacasmain.cpp commandline.cpp unixcommandline.cpp stdcommandline.cpp
yacas_LDADD = libyacas.a libyacasplatform.a @NUMBERS_LIB@ @NUMBERS_LDFLAGS@ 

#
# some one who understands autoconf/automake: how do I detect if a platform
# supports elf binaries?
#

yacas_LDFLAGS = $(LIBS)

testnum_SOURCES = anumber.cpp grower.cpp lispstring.cpp stdstubs.cpp obmalloc.cpp \
	testnum.cpp mathutil.cpp
libyacas_a_SOURCES = deffile.cpp infixparser.cpp lispatom.cpp \
	lispcleanupstack.cpp lispenvironment.cpp lispeval.cpp \
	lispio.cpp lispobject.cpp lispparser.cpp  \
	lispuserfunc.cpp mathcommands.cpp mathenvironment.cpp \
	mathuserfunc.cpp standard.cpp  stdfileio.cpp arggetter.cpp \
	stringio.cpp tokenizer.cpp yacasapi.cpp genericobject.cpp \
	arrayclass.cpp lispevalhash.cpp patterns.cpp patternclass.cpp \
        substitute.cpp mathcommands2.cpp mathcommands3.cpp errors.cpp patcher.cpp \
        lispplugin.cpp elfdll.cpp genericstructs.cpp unipoly.cpp ctokenizer.cpp
        
libyacasplatform_a_SOURCES = platmath.cpp grower.cpp stdstubs.cpp obmalloc.cpp lisphash.cpp \
	lispstring.cpp mathutil.cpp
libyacasplatform_a_LIBADD = @NUMBERS_LIB@
libyacasnumbers_a_SOURCES = anumber.cpp yacasnumbers.cpp 
libgmpnumbers_a_SOURCES = gmpnumbers.cpp 

EXTRA_DIST = anumber.inl deffile.inl epocfileio.cpp epocmain.cpp \
	epocyacas.hrh epocyacas.rss epocyacas.mmp \
	epocstubs.cpp grower.inl lisphash.inl lispobject.inl \
	lispstring.inl standard.inl \
	win32dll.cpp \
	depend.dep makefile.beos \
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
	plat/win32/yacasmain-win32.cpp makefile.debug


include depend.dep

depend:
	gcc -MM *.cpp $(INCLUDES) > depend.dep


