

SUBDIRS =  example forth opengl

EXTRA_DIST = doc.txt   README   makefile.plugin \
        \
	opengl/glutcode.cc            \
	opengl/glutcode.h             \
	opengl/makefile.plugin        \
	opengl/new                    \
	opengl/openglapi.stub         \
	opengl/testscript             \
  opengl/glututils              \
	opengl/depend.dep

macosx:
	cd example ; make libexample.dylib
	cd forth ; make libforth.dylib

#
# Why doesn't this work?
#



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


