SCRIPTFILES = \
        complex.rep/code.ys complex.rep/code.ys.def constants.rep/code.ys constants.rep/code.ys.def controlflow.rep/code.ys controlflow.rep/code.ys.def \
        deffunc.rep/code.ys deffunc.rep/code.ys.def deriv.rep/code.ys deriv.rep/code.ys.def functional.rep/code.ys functional.rep/code.ys.def glview.ys \
        integrate.rep/code.ys integrate.rep/code.ys.def linalg.rep/code.ys linalg.rep/code.ys.def lists.rep/code.ys lists.rep/code.ys.def newly.rep/code.ys \
        newly.rep/code.ys.def numbers.rep/code.ys numbers.rep/code.ys.def random.rep/code.ys random.rep/code.ys.def simplify.rep/code.ys \
        simplify.rep/code.ys.def solve.rep/code.ys solve.rep/code.ys.def standard.ys stdfuncs.rep/code.ys stdfuncs.rep/code.ys.def \
        stdopers.ys stubs.rep/code.ys stubs.rep/code.ys.def substitute.rep/code.ys substitute.rep/code.ys.def sums.rep/code.ys sums.rep/code.ys.def \
        univar.rep/code.ys univar.rep/code.ys.def yacasinit.ys assoc.rep/code.ys assoc.rep/code.ys.def html.rep/code.ys html.rep/code.ys.def \
        fakedb.rep/code.ys fakedb.rep/code.ys.def array.rep/code.ys array.rep/code.ys.def example.rep/code.ys example.rep/code.ys.def \
        trigsimp.rep/code.ys trigsimp.rep/code.ys.def formula.rep/code.ys formula.rep/code.ys.def patterns.rep/code.ys \
        patterns.rep/code.ys.def predicates.rep/code.ys predicates.rep/code.ys.def stats.rep/code.ys stats.rep/code.ys.def padic.rep/code.ys \
        padic.rep/code.ys.def testers.rep/code.ys testers.rep/code.ys.def factors.rep/code.ys factors.rep/code.ys.def tensor.rep/code.ys tensor.rep/code.ys.def \
        texform.rep/code.ys texform.rep/code.ys.def print.ys c_form.rep/code.ys c_form.rep/code.ys.def logic.rep/code.ys logic.rep/code.ys.def cstubgen.rep/code.ys cstubgen.rep/code.ys.def \
        radsimp.rep/code.ys radsimp.rep/code.ys.def pslq.rep/code.ys pslq.rep/code.ys.def odesolver.rep/code.ys odesolver.rep/code.ys.def limit.rep/code.ys limit.rep/code.ys.def \
        localrules.rep/code.ys localrules.rep/code.ys.def rabinmiller.rep/code.ys rabinmiller.rep/code.ys.def \
        orthopoly.rep/code.ys orthopoly.rep/code.ys.def openmath.rep/code.ys openmath.rep/code.ys.def \
        multivar.rep/code.ys multivar.rep/code.ys.def 

script_DATA = # $(SCRIPTFILES)

install-data-local:
	-cp -r * $(scriptdir)
#	-mkdir $(scriptdir)addons/
#	-cp addons/README $(scriptdir)addons/
#	-cp addons/editvi* $(scriptdir)addons/
#	-cp addons/gnuplot* $(scriptdir)addons/

EXTRA_DIST = $(SCRIPTFILES) \
        examples/tests.ys \
        examples/findsum.ys \
        examples/queens.ys examples/benchmark.ys \
        examples/benchmark2.ys examples/benchbuild.ys  examples/goldbach.ys examples/series.ys \
        addons/README addons/editvi.ys addons/editvi.ys.def addons/gnuplot.ys \
        addons/gnuplot.ys.def

testscriptdir = ../scripts/
yacastest = ../src/yacas -pc --rootdir $(testscriptdir)

test:
	$(yacastest) ../tests/arithmetic.yts
	$(yacastest) ../tests/calculus.yts
	$(yacastest) ../tests/canprove.yts
	$(yacastest) ../tests/comments.yts
	$(yacastest) ../tests/linalg.yts
	$(yacastest) ../tests/lists.yts
	$(yacastest) ../tests/logic_simplify_test.yts
	$(yacastest) ../tests/poly.yts
	$(yacastest) ../tests/predicates.yts
	$(yacastest) ../tests/programming.yts
	$(yacastest) ../tests/radsimp.yts
	$(yacastest) ../tests/regress.yts
	$(yacastest) ../tests/simplify.yts
	$(yacastest) ../tests/solve.yts
	$(yacastest) ../tests/sums.yts
	$(yacastest) ../tests/tensors.yts
	$(yacastest) ../tests/c_tex_form.yts
	$(yacastest) ../tests/numerics.yts
	$(yacastest) ../tests/orthopoly.yts
