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
        lispplugin.h elfdll.h genericstructs.h
yacas_SOURCES = yacasmain.cc commandline.cc unixcommandline.cc stdcommandline.cc
yacas_LDADD = libyacas.a libyacasplatform.a @NUMBERS_LIB@ @NUMBERS_LDFLAGS@ 

#
# some one who understands autoconf/automake: how do I detect if a platform
# supports elf binaries?
#

yacas_LDFLAGS = -ldl -rdynamic

testnum_SOURCES = anumber.cc grower.cc lispstring.cc stdstubs.cc \
	testnum.cc mathutil.cc
libyacas_a_SOURCES = deffile.cc infixparser.cc lispatom.cc \
	lispcleanupstack.cc lispenvironment.cc lispeval.cc \
	lispio.cc lispobject.cc lispparser.cc  \
	lispuserfunc.cc mathcommands.cc mathenvironment.cc \
	mathuserfunc.cc standard.cc  stdfileio.cc arggetter.cc \
	stringio.cc tokenizer.cc yacasapi.cc genericobject.cc \
	arrayclass.cc lispevalhash.cc patterns.cc patternclass.cc \
        substitute.cc mathcommands2.cc mathcommands3.cc errors.cc patcher.cc \
        lispplugin.cc elfdll.cc genericstructs.cc
libyacasplatform_a_SOURCES = platmath.cc grower.cc stdstubs.cc lisphash.cc \
	lispstring.cc mathutil.cc
libyacasplatform_a_LIBADD = @NUMBERS_LIB@
libyacasnumbers_a_SOURCES = anumber.cc yacasnumbers.cc 
libgmpnumbers_a_SOURCES = gmpnumbers.cc 

EXTRA_DIST = anumber.inl deffile.inl epocfileio.cpp epocmain.cpp \
	epocstubs.cc grower.inl lisphash.inl lispobject.inl \
	lispstring.inl standard.inl \
	yacas.mmp win32dll.h win32dll.cpp \
	depend.dep \
	convertcctocpp.bat \
	plat/linux32/lisptype.h \
	plat/linux32/stubs.inl \
	plat/linux32/platfileio.h \
	plat/linux64/lisptype.h \
	plat/linux64/stubs.inl \
	plat/linux64/platfileio.h \
	plat/epoc32/lisptype.h \
	plat/epoc32/stubs.inl \
	plat/epoc32/platfileio.h \
	plat/win32/platfileio.h \
	plat/win32/stubs.inl \
	plat/win32/List.h \
	plat/win32/List.inl \
	plat/win32/lisptype.h

include depend.dep

depend:
	gcc -MM *.cc $(INCLUDES) > depend.dep


