EXTRA_DIST = README ramscripts-win32.cpp \
	makefile.compressor depend.dep compressedfiles.cpp \
	README.LZO lzoconf.h minilzo.c minilzo.h decompressor.cpp \
	compressor.cpp filescanner.cpp filescanner.h \
	test.sh README.compressor compressedfiles.h compress.bat \
	compressor.dsp compressor.dsw win32rcwriter.cpp ramscripts.cc


yacas_include = $(datadir)/yacas/include

if GENERATE_ARCHIVE

AM_CPPFLAGS = 
noinst_DATA = scripts.dat
checkTARGETS = check-archive
noinst_PROGRAMS = compressor
compressor_SOURCES = compressor.cpp filescanner.cpp minilzo.c filescanner.h minilzo.h 

scripts.dat: compressor
	./compressor - $(top_srcdir)/scripts/ scripts.dat 

check: $(checkTARGETS)

check-archive: scripts.dat 
	../src/yacas -pc --archive scripts.dat $(top_srcdir)/tests/arithmetic.yts


## We need a bit of a hack to install the 'scripts.dat' file.
## Ideally, we would like to declare it as lib_LIBRARY, but then automake
## (at least version 1.4) complains that it doesn't know libraries with
## a .dat extension. So we install it by hand in $(libdir)
## Furthermore, we need to install compressedfiles.h because plugins need it.

install-exec-local: scripts.dat
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(libdir)
	$(INSTALL_PROGRAM) scripts.dat $(DESTDIR)$(libdir)/`echo scripts.dat|sed '$(transform)'`

endif

install-data-local: compressedfiles.h
	$(INSTALL_DATA) $(srcdir)/compressedfiles.h $(yacas_include)/compressedfiles.h


uninstall-local:
	@$(NORMAL_UNINSTALL)
	rm -f $(DESTDIR)$(libdir)/`echo scripts.dat|sed '$(transform)'`
	rm -f $(yacas_include)/compressedfiles.h







