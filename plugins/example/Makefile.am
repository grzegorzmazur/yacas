
EXTRA_DIST = \
	bareplugin.h               \
	barepluginapi.stub         \
	depend.dep                 \
	makefile.plugin            \
	testload.cc                



INCLUDES = -I../../src -I../../src/plat/linux32/
noinst_LTLIBRARIES = libexample.la
libexample_la_SOURCES = barepluginapi.cc bareplugin.cc
libexample_la_LDFLAGS = -module
libexample_la_DEPENDENCIES = barepluginapi.cc

barepluginapi.cc : barepluginapi.stub
	../../src/yacas --rootdir ../../scripts/ -pc barepluginapi.stub

libexample.dylib: libexample.la
	c++ -module -dynamic -undefined suppress -flat_namespace -dynamiclib -o $@ bareplugin.lo barepluginapi.lo
