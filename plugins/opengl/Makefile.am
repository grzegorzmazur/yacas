## Makefile.am -- Process this file with automake to produce Makefile.in

EXTRA_DIST = new testscript glututils

INCLUDES = -I$(top_srcdir)/src -I$(top_srcdir)/src/plat/linux32/ $(GL_CFLAGS)
# -I$(SYSTEM_LIBRARY_DIR)/Frameworks/Carbon.framework/Libraries/CIncludes -framework AGL -framework OpenGL

noinst_LTLIBRARIES = $(OPENGL_PLUGIN)
EXTRA_LTLIBRARIES = libopengl.la
libopengl_la_SOURCES = glutcode.cc openglapi.stub glutcode.h
libopengl_la_LIBADD = openglapi.lo
libopengl_la_LDFLAGS = $(GL_LIBS) -module -rpath `pwd`

CLEANFILES = openglapi.cc openglapi.description

openglapi.cc : openglapi.stub
	../../src/yacas --rootdir ../../scripts/:$(top_srcdir)/scripts/ -pc $(srcdir)/openglapi.stub

libopengl.dylib: libopengl.la
	c++ -module -dynamic -undefined suppress -flat_namespace -dynamiclib -o $@ glutcode.lo openglapi.lo
