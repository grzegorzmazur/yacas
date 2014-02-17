## Makefile.am -- Process this file with automake to produce Makefile.in

lib_LTLIBRARIES = libyacas.la
YACAS_LIBRARY_VERSION = 1:0:0
libyacas_la_LDFLAGS = -version-info $(YACAS_LIBRARY_VERSION)

libyacas_la_SOURCES = stdcommandline.cpp deffile.cpp infixparser.cpp lispatom.cpp \
	lispcleanupstack.cpp lispenvironment.cpp lispeval.cpp lisperror.cpp \
	lispio.cpp lispobject.cpp lispparser.cpp \
	lispuserfunc.cpp mathcommands.cpp mathenvironment.cpp \
	mathuserfunc.cpp standard.cpp  stdfileio.cpp arggetter.cpp \
	stringio.cpp tokenizer.cpp yacasapi.cpp genericobject.cpp \
	arrayclass.cpp lispevalhash.cpp patterns.cpp patternclass.cpp \
        substitute.cpp mathcommands2.cpp mathcommands3.cpp errors.cpp patcher.cpp \
        genericstructs.cpp \
	xmltokenizer.cpp \
        commandline.cpp \
	anumber.cpp yacasnumbers.cpp numbers.cpp \
	platmath.cpp grower.cpp stdstubs.cpp obmalloc.cpp lisphash.cpp \
	lispstring.cpp mathutil.cpp yacasbase.cpp debugmem.cpp

bin_PROGRAMS =  yacas
noinst_PROGRAMS = gencorefunctions
EXTRA_PROGRAMS = codereviewer

pkginclude_HEADERS = anumber.h choices.h deffile.h grower.h			\
	infixparser.h lispassert.h lispatom.h lispcleanupstack.h		\
	lispenvironment.h lisperror.h lispeval.h lispevalhash.h			\
	lispglobals.h lisphash.h lispio.h lispobject.h lispparser.h		\
	lispstring.h lispuserfunc.h corefunctions.h core_yacasmain.h	\
	mathcommands.h mathenvironment.h patcher.h mathuserfunc.h		\
	numbers.h platmath.h standard.h patterns.h errors.h arggetter.h	\
	stdfileio.h stringio.h stubs.h tokenizer.h yacas.h refcount.h	\
	genericobject.h arrayclass.h evalfunc.h commandline.h			\
	unixcommandline.h patternclass.h substitute.h stdcommandline.h	\
	arrayclass.inl mathutil.h genericstructs.h yacasprivate.h		\
	yacasbase.h xmltokenizer.h GPL_stuff.h anumber.inl deffile.inl	\
	grower.inl lisphash.inl lispobject.inl lispstring.inl			\
	standard.inl yacas_version.h lisptype.h platfileio.h debugmem.h

yacas_SOURCES = yacasmain.cpp
yacas_LDADD = libyacas.la

if LINUX
yacas_SOURCES += unixcommandline.cpp
endif

if WIN32
yacas_SOURCES += win32commandline.cpp
endif

yacas_DEPENDENCIES = yacas_version.h GPL_stuff.h 

mathcommands3.lo: yacas_version.h

codereviewer_SOURCES = codereviewer.cpp 
codereviewer_LDADD = 
codereviewer_DEPENDENCIES = 


# the program 'gencorefunctions' will prepare a listing of all core functions
gencorefunctions_SOURCES = gencorefunctions.cpp 

# force core_yacasmain.h to be created
$(srcdir)/gencorefunctions.cpp: core_yacasmain.h corefunctions.h

yacas_version.h: ../config.h
	echo "#define YACAS_VERSION \"$(VERSION)\"" > yacas_version.h 

check: check-codereviewer

check-codereviewer: codereviewer$(EXEEXT)
	./codereviewer$(EXEEXT) c *.h *.inl *.cpp

distclean-local:
	-rm -f codereviewer$(EXEEXT)
	-rm -f yacas_version.h
