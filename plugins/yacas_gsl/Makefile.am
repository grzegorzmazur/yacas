
EXTRA_DIST = yacas_gslapi.stub


INCLUDES = -I../../src -I../../src/plat/linux32/ -I/usr/local/include
noinst_LTLIBRARIES = libyacas_gsl.la
libyacas_gsl_la_SOURCES = yacas_gslapi.cc yacas_gsl.cc
#libyacas_gsl_la_LDADD = 
libyacas_gsl_la_LDFLAGS = -module -L/usr/local/lib
libyacas_gsl_la_DEPENDENCIES = yacas_gslapi.cc

yacas_gslapi.cc : yacas_gslapi.stub
	../../src/yacas --rootdir ../../scripts/ -pc yacas_gslapi.stub

libyacas_gsl.dylib: libyacas_gsl.la
	c++ -module -dynamic -undefined suppress -flat_namespace -dynamiclib -o $@ yacas_gsl.lo yacas_gslapi.lo $(LIBS)
