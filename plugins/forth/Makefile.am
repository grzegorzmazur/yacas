## Makefile.am -- Process this file with automake to produce Makefile.in

INCLUDES = -I$(top_srcdir)/src -I$(top_srcdir)/src/plat/linux32/

# noinst_ in stead of pkglib_ because it doesn't currently work on MacOSX 
noinst_LTLIBRARIES = forth.la
forth_la_SOURCES = forthplugin.cc forthplugin.h forth.cc forth.h forthpluginapi.stub
forth_la_LIBADD = forthpluginapi.lo
forth_la_LDFLAGS = -module -avoid-version -no-undefined

CLEANFILES = forthpluginapi.cc forthpluginapi.description

forthpluginapi.cc : forthpluginapi.stub
	../../src/yacas --rootdir ../../scripts/:$(top_srcdir)/scripts/ -pc $(srcdir)/forthpluginapi.stub

forth.dylib: forth.la
	c++ -module -dynamic -undefined suppress -flat_namespace -dynamiclib -o $@ forth.lo forthplugin.lo forthpluginapi.lo
