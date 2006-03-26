## Makefile.am -- Process this file with automake to produce Makefile.in

EXTRA_DIST = testscript glututils

INCLUDES = -I$(top_srcdir)/src -I$(top_srcdir)/src/plat/linux32/ $(GL_CFLAGS)
# -I$(SYSTEM_LIBRARY_DIR)/Frameworks/Carbon.framework/Libraries/CIncludes -framework AGL -framework OpenGL

pkglib_LTLIBRARIES = $(OPENGL_PLUGIN)
EXTRA_LTLIBRARIES = opengl.la
opengl_la_SOURCES = glutcode.cc openglapi.stub glutcode.h
opengl_la_LIBADD = openglapi.lo
opengl_la_LDFLAGS = $(GL_LIBS) -module -avoid-version -rpath $(pkglibdir)

CLEANFILES = openglapi.cc openglapi.man.txt

## Ensure we always build openglapi.man.txt
all-local: openglapi.man.txt

if BUILD_OPENGL_PLUGIN

openglapi.cc openglapi.man.txt: openglapi.stub
	../../src/yacas --rootdir ../../scripts/:$(top_srcdir)/scripts/ -pc $(srcdir)/openglapi.stub

opengl.dylib: opengl.la
	c++ -module -dynamic -undefined suppress -flat_namespace -dynamiclib -o $@ glutcode.lo openglapi.lo

else

## If we do not build the OpenGL plugin, then an empty .man.txt suffices.
openglapi.man.txt:
	touch openglapi.man.txt

endif
