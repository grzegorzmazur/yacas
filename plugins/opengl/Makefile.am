
EXTRA_DIST = \
	glutcode.cc            \
	glutcode.h             \
	makefile.plugin        \
	new                    \
	openglapi.stub         \
	testscript             \
  glututils      


INCLUDES = -I../../src -I../../src/plat/linux32/ 
# -I$(SYSTEM_LIBRARY_DIR)/Frameworks/Carbon.framework/Libraries/CIncludes -framework AGL -framework OpenGL

noinst_LTLIBRARIES = libopengl.la
libopengl_la_SOURCES = glutcode.cc openglapi.cc
libopengl_la_LDFLAGS = -module
libopengl_la_DEPENDENCIES = openglapi.cc

openglapi.cc : openglapi.stub
	../../src/yacas --rootdir ../../scripts/ -pc openglapi.stub

libopengl.dylib: libopengl.la
	c++ -module -dynamic -undefined suppress -flat_namespace -dynamiclib -o $@ glutcode.lo openglapi.lo
