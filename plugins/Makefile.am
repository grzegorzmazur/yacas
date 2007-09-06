

SUBDIRS =  example forth pcre filescanner

EXTRA_DIST = doc.txt extract_stub.pl    

macosx:
	cd example ; make example.dylib



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


