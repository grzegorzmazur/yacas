## Makefile.am -- Process this file with automake to produce Makefile.in

INCLUDES = -I$(top_srcdir)/src -I$(top_srcdir)/src/plat/linux32/
noinst_LTLIBRARIES = libforth.la
libforth_la_SOURCES = forthplugin.cc forthplugin.h forth.cc forth.h forthpluginapi.stub
libforth_la_LIBADD = forthpluginapi.lo
libforth_la_LDFLAGS = -module -rpath `pwd`

CLEANFILES = forthpluginapi.cc forthpluginapi.description

forthpluginapi.cc : forthpluginapi.stub
	../../src/yacas --rootdir ../../scripts/:$(top_srcdir)/scripts/ -pc $(srcdir)/forthpluginapi.stub

libforth.dylib: libforth.la
	c++ -module -dynamic -undefined suppress -flat_namespace -dynamiclib -o $@ forth.lo forthplugin.lo forthpluginapi.lo
