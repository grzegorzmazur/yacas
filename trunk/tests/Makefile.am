## Makefile.am -- Process this file with automake to produce Makefile.in

# Yacas test suite
# ----------------
# Two tests are supported:
# 1. 'make check' tests the version of yacas in the build tree
# 2. 'make installcheck' tests the installed version of yacas

# The test scripts to be run, are listed in TESTFILES

# This variable can be overriden; eg. to only run complex.yts,
# do 'make TESTFILES=complex.yts check'.
  
NORMAL_TESTFILES = \
	openmath.yts macro.yts arithmetic.yts c_tex_form.yts calculus.yts canprove.yts \
	comments.yts complex.yts deriv.yts dimensions.yts dot.yts \
	integrate.yts lists.yts logic_simplify_test.yts matrixpower.yts \
	nthroot.yts outer.yts predicates.yts ode.yts \
	tensors.yts trace.yts tr.yts multivar.yts numbers.yts io.yts \
	programming.yts regress.yts simplify.yts solve.yts sums.yts \
	transforms.yts radsimp.yts linalg.yts orthopoly.yts poly.yts numerics.yts \
	scopestack.yts plots.yts plugin.yts GaussianIntegers.yts nummethods.yts sturm.yts \
        cyclotomic.yts 

SPECIAL_TESTFILES = ../manmake/wester-1994.ys

TESTFILES = $(NORMAL_TESTFILES) $(SPECIAL_TESTFILES)

# To run the version of yacas in the build tree
BUILT_YACAS = ../src/yacas -pc --rootdir $(top_srcdir)/scripts/:../scripts/ --dlldir ../plugins/example:../src/ 

# To run the installed version of yacas
INSTALLED_YACAS = $(bindir)/yacas -pc   

EXTRA_DIST = test.bat test-yacas $(NORMAL_TESTFILES)

check-am: 
	@$(srcdir)/test-yacas $(BUILT_YACAS) \; $(srcdir) $(TESTFILES)

installcheck-am: 
	@$(srcdir)/test-yacas $(INSTALLED_YACAS) \; $(srcdir) $(TESTFILES)
