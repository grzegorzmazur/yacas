## Makefile.am -- Process this file with automake to produce Makefile.in

# Yacas test suite
# ----------------
# Two tests are supported:
# 1. 'make check' tests the version of yacas in the build tree
# 2. 'make installcheck' tests the installed version of yacas

# The test scripts to be run, are listed in TESTFILES

# This variable can be overriden; eg. to only run complex.yts,
# do 'make TESTFILES=complex.yts check'.

include includetestfiles


# To run the version of yacas in the build tree
BUILT_YACAS = ../src/yacas -pc --rootdir $(top_srcdir)/scripts/:../scripts/ 

# To run the installed version of yacas
INSTALLED_YACAS = $(bindir)/yacas -pc   

EXTRA_DIST =  test-yacas $(NORMAL_TESTFILES) includetestfiles

.PHONY : quicktest

quicktest:
	@$(srcdir)/test-yacas "$(BUILT_YACAS)" $(srcdir) $(QUICKTESTFILES)

check-am: 
	@$(srcdir)/test-yacas "$(BUILT_YACAS)" $(srcdir) $(TESTFILES)

installcheck-am: 
	@$(srcdir)/test-yacas "$(INSTALLED_YACAS)" $(srcdir) $(TESTFILES)
