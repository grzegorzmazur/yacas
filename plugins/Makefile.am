
EXTRA_DIST = doc.txt   README   makefile.plugin \
	\
	example/bareplugin.h               \
	example/barepluginapi.stub         \
  example/bareplugin.cc              \
	example/depend.dep                 \
	example/makefile.plugin            \
	example/testload.cc                \
        \
	opengl/glutcode.cc            \
	opengl/glutcode.h             \
	opengl/makefile.plugin        \
	opengl/new                    \
	opengl/openglapi.stub         \
	opengl/testscript             \
        opengl/glututils              \
        \
	forth/depend.dep  forth/forthplugin.cc    \
	forth/forthpluginapi.stub                 \
	forth/forth.cc    forth/forthplugin.h     \
	forth/makefile.plugin                     \
	forth/forth.h                             \
        \
	opengl/depend.dep



#
# Why doesn't this work?
#

INCLUDES = -I../src -I../src/plat/linux32/
noinst_LTLIBRARIES = libexample.la
libexample_la_SOURCES = example/barepluginapi.cc example/bareplugin.cc
libexample_la_LDFLAGS = -module

libexample.dylib: bareplugin.lo barepluginapi.lo
	c++ -module -dynamic -undefined suppress -flat_namespace -dynamiclib -o $@ bareplugin.lo barepluginapi.lo


# -avoid-version -export-dynamic



#mylib:
#	c++ -fno-common -c example/barepluginapi.cc -o barepluginapi.o $(INCLUDES)
#	c++ -fno-common -c example/bareplugin.cc -o bareplugin.o $(INCLUDES)
#	c++ -bundle -flat_namespace -undefined suppress  -o mymodule.so barepluginapi.o bareplugin.o


#
# following lines not necessary, just playing around
#
#barepluginapi.lo: example/barepluginapi.cc
#bareplugin.lo: example/bareplugin.cc 
#libexample_la_LIBADD = # barepluginapi.lo bareplugin.lo
#-dynamic -module -static
# -module 
# -avoid-version -no-undefined -export-dynamic


