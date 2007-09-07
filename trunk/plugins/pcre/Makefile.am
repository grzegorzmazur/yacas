
EXTRA_DIST = LICENCE  pcreposix.h \
	chartables.c pcretest.c pcre_config.h internal.h \
	pcre.h printint.c plugin.cpp \
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
	pcre_get_substring_list.html            pcretest.html \
	study.c pcre.c maketables.c pcreposix.c get.c plugin.cc


noinst_PROGRAMS = dftables

dftables_SOURCES = dftables.c

chartables.c: dftables
	./dftables > chartables.c

CLEANFILES = 





