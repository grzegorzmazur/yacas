## Makefile.am -- Process this file with automake to produce Makefile.in

if BUILD_PROTEUS

INCLUDES = @FLTK_INCS@ -I$(top_srcdir)/src -I$(top_srcdir)/src/plat/linux32/ -DSYSCONFDIR=\"@sysconfdir@\"

bin_PROGRAMS = proteusworksheet

proteusworksheet_LDADD = $(top_srcdir)/src/libyacas.la $(top_srcdir)/src/libyacasplatform.la $(top_srcdir)/src/@NUMBERS_LIB@ @LIBLTDL@ @NUMBERS_LDFLAGS@ @FLTK_LIBS@

proteusworksheet_cpp_sources = tabs.cpp \
	editor.cpp \
	HelpView.cpp \
	FltkConsole.cpp \
	grapher.cpp \
	FltkHintWindow.cpp 

proteusworksheet_h_sources = FltkConsole.h \
	FltkHintWindow.h \
	HelpView.h \
	editor.h \
	grapher.h \
	proteusmenu.h \
	tabs.h \
	yacdefines.h

proteusworksheet_SOURCES = $(proteusworksheet_cpp_sources) \
	$(proteusworksheet_h_sources)

proteusworksheet_datafiles = hints \
	WorksheetBanner \
	flplot.ys \
	flplot.ys.def

proteusworksheet_docfiles = proteusbooks.html

install-data-local: $(proteusworksheet_datafiles) $(proteusworksheet_docfiles)
	$(mkinstalldirs) $(DESTDIR)$(sysconfdir)/
	echo $(DESTDIR)$(pkgdatadir)/ > $(DESTDIR)$(sysconfdir)/proteus.conf
	-for file in $(proteusworksheet_datafiles); do \
		$(INSTALL_DATA) $$file $(DESTDIR)$(pkgdatadir)/$$file; \
	done
	$(mkinstalldirs) $(DESTDIR)$(pkgdatadir)/documentation
	$(INSTALL_DATA) proteusbooks.html $(DESTDIR)$(pkgdatadir)/documentation/proteusbooks.html


# EXTRA_DIST = COPYING.HelpDialog browse.cpp \
# 	books.html.in depend.dep fl_adjustable_file_chooser.H \
# 	fl_adjustable_file_chooser.cpp flplot.ys flplot.ys.def \
# 	fltkgraph.stub fltkgraphstub.cc fltkgraphstub.description \
# 	graphtest hints	makefile.linux makefile.macosx  makeinclude \
# 	mkbackup proteus.conf proteus.dsp proteus.dsw proteusbooks.html \
# 	readme.txt testnotes/graphtest2 testnotes/index \
# 	testnotes/recursetest testnotes/testnotepad \
# 	testnotes/testnotepad2 testnotes/fonts todo.txt yacasconf \
#         WorksheetBanner tracer.cpp tracer.ys makefile.tracer    \
#         debugclass.cpp debugclass.h proteusmanual.book.txt 

else

endif

EXTRA_DIST = COPYING.HelpDialog FltkConsole.cpp FltkConsole.h FltkHintWindow.cpp \
	FltkHintWindow.h HelpView.cpp HelpView.h  browse.cpp                     \
	books.html.in depend.dep editor.cpp editor.h                             \
	fl_adjustable_file_chooser.H fl_adjustable_file_chooser.cpp              \
	fltkgraph.stub fltkgraphstub.cc fltkgraphstub.description         \
	grapher.cpp grapher.h graphtest hints \
	makefile.linux makefile.macosx makefile.tracer makeinclude                  \
	mkbackup proteus.conf proteus.dsp proteus.dsw proteusbooks.html          \
	proteusmenu.h readme.txt tabs.cpp tabs.h testnotes/graphtest2            \
	testnotes/index testnotes/recursetest testnotes/testnotepad              \
	testnotes/testnotepad2 todo.txt yacasconf yacdefines.h                   \
        WorksheetBanner tracer.cpp tracer.ys \
        debugclass.cpp debugclass.h flplot.ys flplot.ys.def





