
EXTRA_DIST = \
	forth/depend.dep  forth/forthplugin.cc    \
	forth/forthpluginapi.stub                 \
	forth/forth.cc    forth/forthplugin.h     \
	forth/makefile.plugin                     \
	forth/forth.h                             


INCLUDES = -I../../src -I../../src/plat/linux32/
noinst_LTLIBRARIES = libforth.la
libforth_la_SOURCES = forthpluginapi.cc forthplugin.cc forth.cc
libforth_la_LDFLAGS = -module
libforth_la_DEPENDENCIES = forthpluginapi.cc

forthpluginapi.cc : forthpluginapi.stub
	../../src/yacas --rootdir ../../scripts/ -pc forthpluginapi.stub

libforth.dylib: libforth.la
	c++ -module -dynamic -undefined suppress -flat_namespace -dynamiclib -o $@ forth.lo forthplugin.lo forthpluginapi.lo
