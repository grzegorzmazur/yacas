## Makefile.am -- Process this file with automake to produce Makefile.in

INCLUDES = $(GSL_CFLAGS) -I$(top_srcdir)/src -I$(top_srcdir)/src/plat/linux32/ 

pkglib_LTLIBRARIES = $(GSL_PLUGIN)
EXTRA_LTLIBRARIES = yacas_gsl.la

## yacas_gsl_la_SOURCES should only contain files that should be distributed
## The library is built from objects created from C++ files in sources
## Additional objects to be put in the library, are listed in yacas_gsl_la_LIBADD
 
yacas_gsl_la_SOURCES = yacas_gsl.cc yacas_gsl.h yacas_gslapi.stub
yacas_gsl_la_LIBADD = yacas_gslapi.lo
yacas_gsl_la_LDFLAGS = $(GSL_LIBS) -module -avoid-version -no-undefined -rpath $(pkglibdir)

CLEANFILES = yacas_gslapi.cc yacas_gslapi.description gsl_auto.stub

yacas_gslapi.cc : yacas_gslapi.stub gsl_auto.stub
	../../src/yacas --rootdir ../../scripts/:$(top_srcdir)/scripts/ -pc $(srcdir)/yacas_gslapi.stub

gsl_auto.stub : $(srcdir)/../extract_stub.pl
	cat `echo $(GSL_CFLAGS) | sed -e 's/-I//'`/gsl/*.h | perl $(srcdir)/../extract_stub.pl > gsl_auto.stub

yacas_gsl.dylib: yacas_gsl.lo yacas_gslapi.lo
	c++ -module -dynamic -undefined suppress -flat_namespace -dynamiclib -o $@ yacas_gsl.lo yacas_gslapi.lo $(LIBS)
