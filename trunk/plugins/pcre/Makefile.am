
EXTRA_DIST = LICENCE  pcreposix.h \
	chartables.c pcretest.c pcre_config.h internal.h \
	pcre.h printint.c \
	index.html                              pcre_info.html \
	pcre.html                               pcre_maketables.html \
	pcre_compile.html                       pcre_study.html \
	pcre_config.html                        pcre_version.html \
	pcre_copy_named_substring.html          pcreapi.html \
	pcre_copy_substring.html                pcrebuild.html \
	pcre_exec.html                          pcrecallout.html \
	pcre_free_substring.html                pcrecompat.html \
	pcre_free_substring_list.html           pcregrep.html \
	pcre_fullinfo.html                      pcrepattern.html \
	pcre_get_named_substring.html           pcreperform.html \
	pcre_get_stringnumber.html              pcreposix.html \
	pcre_get_substring.html                 pcresample.html \
	pcre_get_substring_list.html            pcretest.html 


noinst_PROGRAMS = dftables

dftables_SOURCES = dftables.c

chartables.c: dftables
	./dftables > chartables.c

INCLUDES = -I$(top_srcdir)/src -I$(top_srcdir)/src/plat/linux32/
pkglib_LTLIBRARIES = pcre.la
pcre_la_SOURCES = study.c pcre.c maketables.c pcreposix.c get.c plugin.cc

pcre_la_LDFLAGS = -module -avoid-version -no-undefined 

CLEANFILES = chartables.c 

## Ensure we always build barepluginapi.man.txt
# all-local: barepluginapi.man.txt

pcre.lo: chartables.c

pcre.dylib: pcre.lo 
	c++ -module -dynamic -undefined suppress -flat_namespace -dynamiclib -o $@ pcre.lo



