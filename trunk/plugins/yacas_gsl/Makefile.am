## Makefile.am -- Process this file with automake to produce Makefile.in

EXTRA_DIST = yacas_gslapi.stub


INCLUDES = $(GSL_CFLAGS) -I$(top_srcdir)/src -I$(top_srcdir)/src/plat/linux32/ 
noinst_LTLIBRARIES = $(GSL_PLUGIN)
EXTRA_LTLIBRARIES = libyacas_gsl.la
libyacas_gsl_la_SOURCES = yacas_gsl.cc
libyacas_gsl_la_LIBADD = yacas_gslapi.cc
libyacas_gsl_la_LDFLAGS = $(GSL_LIBS) -module -rpath `pwd`


yacas_gslapi.cc : yacas_gslapi.stub
	../../src/yacas --rootdir ../../scripts/:$(top_srcdir)/scripts/ -pc $(srcdir)/yacas_gslapi.stub

libyacas_gsl.dylib: libyacas_gsl.la
	c++ -module -dynamic -undefined suppress -flat_namespace -dynamiclib -o $@ yacas_gsl.lo yacas_gslapi.lo $(LIBS)
