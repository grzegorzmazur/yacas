## Makefile.am -- Process this file with automake to produce Makefile.in

EXTRA_DIST = testload.cc

INCLUDES = -I$(top_srcdir)/src -I$(top_srcdir)/src/plat/linux32/
noinst_LTLIBRARIES = libexample.la
libexample_la_SOURCES = bareplugin.cc bareplugin.h barepluginapi.stub 
libexample_la_LIBADD = barepluginapi.lo
libexample_la_LDFLAGS = -module -rpath `pwd` 

CLEANFILES = barepluginapi.cc barepluginapi.description

barepluginapi.cc : barepluginapi.stub
	../../src/yacas --rootdir ../../scripts/:$(top_srcdir)/scripts/ -pc $(srcdir)/barepluginapi.stub

libexample.dylib: libexample.la
	c++ -module -dynamic -undefined suppress -flat_namespace -dynamiclib -o $@ bareplugin.lo barepluginapi.lo
