## Makefile.am -- Process this file with automake to produce Makefile.in

EXTRA_DIST = \
	forthpluginapi.stub                 \
	forth.cc    forthplugin.h     \
	forth.h                             


INCLUDES = -I$(top_srcdir)/src -I$(top_srcdir)/src/plat/linux32/
noinst_LTLIBRARIES = libforth.la
libforth_la_SOURCES = forthplugin.cc forth.cc
libforth_la_LIBADD = forthpluginapi.lo
libforth_la_LDFLAGS = -module -rpath `pwd`

forthpluginapi.cc : forthpluginapi.stub
	../../src/yacas --rootdir ../../scripts/:$(top_srcdir)/scripts/ -pc $(srcdir)/forthpluginapi.stub

libforth.dylib: libforth.la
	c++ -module -dynamic -undefined suppress -flat_namespace -dynamiclib -o $@ forth.lo forthplugin.lo forthpluginapi.lo
