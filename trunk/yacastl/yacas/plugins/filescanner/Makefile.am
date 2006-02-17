
EXTRA_DIST = win32_filescanner_plugin.cpp 


INCLUDES = -I$(top_srcdir)/src -I$(top_srcdir)/src/plat/linux32/
pkglib_LTLIBRARIES = filescanner.la
filescanner_la_SOURCES = filescanner.cpp plugin.cc

filescanner_la_LDFLAGS = -module -avoid-version

filescanner.cpp: $(top_srcdir)/src/filescanner.cpp
	cp $(top_srcdir)/src/filescanner.cpp $(top_srcdir)/plugins/filescanner/

CLEANFILES = 

## Ensure we always build barepluginapi.man.txt
# all-local: barepluginapi.man.txt

filescanner.dylib: filescanner.lo 
	c++ -module -dynamic -undefined suppress -flat_namespace -dynamiclib -o $@ filescanner.lo



