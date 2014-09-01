## Makefile.am -- Process this file with automake to produce Makefile.in

AM_CPPFLAGS=-I $(builddir)/../include -I $(top_srcdir)/include

lib_LTLIBRARIES = libyacas.la
YACAS_LIBRARY_VERSION = 1:0:0
libyacas_la_LDFLAGS = -version-info $(YACAS_LIBRARY_VERSION)

pkginclude_HEADERS = $(top_srcdir)/include/yacas/anumber.h	\
	$(top_srcdir)/include/yacas/choices.h					\
	$(top_srcdir)/include/yacas/deffile.h					\
	$(top_srcdir)/include/yacas/infixparser.h				\
	$(top_srcdir)/include/yacas/lispatom.h					\
	$(top_srcdir)/include/yacas/lispcleanupstack.h			\
	$(top_srcdir)/include/yacas/lispenvironment.h			\
	$(top_srcdir)/include/yacas/lisperror.h					\
	$(top_srcdir)/include/yacas/lispeval.h					\
	$(top_srcdir)/include/yacas/lispevalhash.h				\
	$(top_srcdir)/include/yacas/lispglobals.h				\
	$(top_srcdir)/include/yacas/lisphash.h					\
	$(top_srcdir)/include/yacas/lispio.h					\
	$(top_srcdir)/include/yacas/lispobject.h				\
	$(top_srcdir)/include/yacas/lispparser.h				\
	$(top_srcdir)/include/yacas/lispstring.h				\
	$(top_srcdir)/include/yacas/lispuserfunc.h				\
	$(top_srcdir)/include/yacas/corefunctions.h				\
	$(top_srcdir)/include/yacas/core_yacasmain.h			\
	$(top_srcdir)/include/yacas/mathcommands.h				\
	$(top_srcdir)/include/yacas/mathenvironment.h			\
	$(top_srcdir)/include/yacas/patcher.h					\
	$(top_srcdir)/include/yacas/mathuserfunc.h				\
	$(top_srcdir)/include/yacas/numbers.h					\
	$(top_srcdir)/include/yacas/platmath.h					\
	$(top_srcdir)/include/yacas/standard.h					\
	$(top_srcdir)/include/yacas/patterns.h					\
	$(top_srcdir)/include/yacas/errors.h					\
	$(top_srcdir)/include/yacas/arggetter.h					\
	$(top_srcdir)/include/yacas/stdfileio.h					\
	$(top_srcdir)/include/yacas/stringio.h					\
	$(top_srcdir)/include/yacas/stubs.h						\
	$(top_srcdir)/include/yacas/tokenizer.h					\
	$(top_srcdir)/include/yacas/yacas.h						\
	$(top_srcdir)/include/yacas/refcount.h					\
	$(top_srcdir)/include/yacas/genericobject.h				\
	$(top_srcdir)/include/yacas/arrayclass.h				\
	$(top_srcdir)/include/yacas/evalfunc.h					\
	$(top_srcdir)/include/yacas/commandline.h				\
	$(top_srcdir)/include/yacas/unixcommandline.h			\
	$(top_srcdir)/include/yacas/patternclass.h				\
	$(top_srcdir)/include/yacas/substitute.h				\
	$(top_srcdir)/include/yacas/stdcommandline.h			\
	$(top_srcdir)/include/yacas/genericstructs.h			\
	$(top_srcdir)/include/yacas/yacasprivate.h				\
	$(top_srcdir)/include/yacas/yacasbase.h					\
	$(top_srcdir)/include/yacas/xmltokenizer.h				\
	$(top_srcdir)/include/yacas/GPL_stuff.h					\
	$(top_srcdir)/include/yacas/anumber.inl					\
	$(top_srcdir)/include/yacas/lispobject.inl				\
	$(top_srcdir)/include/yacas/lispstring.inl				\
	$(top_srcdir)/include/yacas/standard.inl				\
	$(top_srcdir)/include/yacas/yacas_version.h				\
	$(top_srcdir)/include/yacas/lisptype.h					\
	$(top_srcdir)/include/yacas/platfileio.h				\
	$(top_srcdir)/include/yacas/debugmem.h                  \
	$(top_srcdir)/include/yacas/noncopyable.h

libyacas_la_SOURCES = $(libyacas_la_HEADERS) stdcommandline.cpp		 \
	deffile.cpp infixparser.cpp lispatom.cpp lispcleanupstack.cpp	 \
	lispenvironment.cpp lispeval.cpp lisperror.cpp lispio.cpp		 \
	lispobject.cpp lispparser.cpp lispuserfunc.cpp mathcommands.cpp	 \
	mathenvironment.cpp mathuserfunc.cpp standard.cpp stdfileio.cpp	 \
	arggetter.cpp stringio.cpp tokenizer.cpp yacasapi.cpp			 \
	lispevalhash.cpp patterns.cpp patternclass.cpp			 \
	substitute.cpp mathcommands2.cpp mathcommands3.cpp errors.cpp	 \
	patcher.cpp genericstructs.cpp xmltokenizer.cpp commandline.cpp	 \
	anumber.cpp yacasnumbers.cpp numbers.cpp platmath.cpp            \
	stdstubs.cpp obmalloc.cpp lisphash.cpp lispstring.cpp			 \
	yacasbase.cpp debugmem.cpp

bin_PROGRAMS =  yacas
noinst_PROGRAMS = gencorefunctions


yacas_SOURCES = yacasmain.cpp
yacas_LDADD = libyacas.la

if LINUX
yacas_SOURCES += unixcommandline.cpp
endif

if WIN32
yacas_SOURCES += win32commandline.cpp
endif

yacas_DEPENDENCIES = $(builddir)/../include/yacas/yacas_version.h $(top_srcdir)/include/yacas/GPL_stuff.h

mathcommands3.lo: $(builddir)/../include/yacas/yacas_version.h
mathcommands3.o: $(builddir)/../include/yacas/yacas_version.h


# the program 'gencorefunctions' will prepare a listing of all core functions
gencorefunctions_SOURCES = gencorefunctions.cpp

# force core_yacasmain.h to be created
$(srcdir)/gencorefunctions.cpp: $(top_srcdir)/include/yacas/core_yacasmain.h $(top_srcdir)/include/yacas/corefunctions.h

$(builddir)/../include/yacas/yacas_version.h: ../config.h
	mkdir -p $(builddir)/../include/yacas
	echo -e "#ifndef YACAS_VERSION_H\n#define YACAS_VERSION \"$(VERSION)\"\n#endif" > $(builddir)/../include/yacas/yacas_version.h

distclean-local:
	-rm -f $(builddir)/../include/yacas/yacas_version.h
