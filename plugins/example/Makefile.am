## Makefile.am -- Process this file with automake to produce Makefile.in

EXTRA_DIST = testload.cc

INCLUDES = -I$(top_srcdir)/src -I$(top_srcdir)/src/plat/linux32/
# noinst_ in stead of pkglib_ because it doesn't currently work on MacOSX
noinst_LTLIBRARIES = example.la
example_la_SOURCES = bareplugin.cc bareplugin.h barepluginapi.stub 
example_la_LIBADD = barepluginapi.lo
example_la_LDFLAGS = -module -avoid-version -no-undefined 

CLEANFILES = barepluginapi.cc barepluginapi.description

barepluginapi.cc : barepluginapi.stub
	../../src/yacas --rootdir ../../scripts/:$(top_srcdir)/scripts/ -pc $(srcdir)/barepluginapi.stub

example.dylib: bareplugin.lo barepluginapi.lo
	c++ -module -dynamic -undefined suppress -flat_namespace -dynamiclib -o $@ bareplugin.lo barepluginapi.lo



