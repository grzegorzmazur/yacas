
EXTRA_DIST = \
	bareplugin.h               \
	barepluginapi.stub         \
	testload.cc                



INCLUDES = -I$(top_srcdir)/src -I$(top_srcdir)/src/plat/linux32/
noinst_LTLIBRARIES = libexample.la
libexample_la_SOURCES = barepluginapi.cc bareplugin.cc
libexample_la_LDFLAGS = -module -rpath `pwd`

barepluginapi.cc : barepluginapi.stub
	../../src/yacas --rootdir ../../scripts/:$(top_srcdir)/scripts/ -pc $(srcdir)/barepluginapi.stub

libexample.dylib: libexample.la
	c++ -module -dynamic -undefined suppress -flat_namespace -dynamiclib -o $@ bareplugin.lo barepluginapi.lo
