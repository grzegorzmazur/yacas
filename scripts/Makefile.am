SCRIPTFILES = \
        complex/code.ys complex/code.ys.def constants/code.ys constants/code.ys.def controlflow/code.ys controlflow/code.ys.def \
        deffunc/code.ys deffunc/code.ys.def deriv/code.ys deriv/code.ys.def functional/code.ys functional/code.ys.def glview.ys \
        integrate/code.ys integrate/code.ys.def linalg/code.ys linalg/code.ys.def lists/code.ys lists/code.ys.def newly/code.ys \
        newly/code.ys.def numbers/code.ys numbers/code.ys.def random/code.ys random/code.ys.def simplify/code.ys \
        simplify/code.ys.def solve/code.ys solve/code.ys.def standard.ys stdfuncs/code.ys stdfuncs/code.ys.def \
        stdopers.ys stubs/code.ys stubs/code.ys.def substitute/code.ys substitute/code.ys.def sums/code.ys sums/code.ys.def \
        univar/code.ys univar/code.ys.def yacasinit.ys assoc/code.ys assoc/code.ys.def html/code.ys html/code.ys.def \
        fakedb/code.ys fakedb/code.ys.def array/code.ys array/code.ys.def example/code.ys example/code.ys.def \
        trigsimp/code.ys trigsimp/code.ys.def formula/code.ys formula/code.ys.def patterns/code.ys \
        patterns/code.ys.def predicates/code.ys predicates/code.ys.def stats/code.ys stats/code.ys.def padic/code.ys \
        padic/code.ys.def testers/code.ys testers/code.ys.def factors/code.ys factors/code.ys.def tensor/code.ys tensor/code.ys.def \
        texform/code.ys texform/code.ys.def print.ys c_form/code.ys c_form/code.ys.def logic/code.ys logic/code.ys.def cstubgen/code.ys cstubgen/code.ys.def \
        radsimp/code.ys radsimp/code.ys.def pslq/code.ys pslq/code.ys.def odesolver/code.ys odesolver/code.ys.def limit/code.ys limit/code.ys.def \
        localrules/code.ys localrules/code.ys.def rabinmiller/code.ys rabinmiller/code.ys.def \
        orthopoly/code.ys orthopoly/code.ys.def

script_DATA = $(SCRIPTFILES)

install-data-local:
	-mkdir $(scriptdir)addons/
	-cp addons/README $(scriptdir)addons/
	-cp addons/editvi* $(scriptdir)addons/
	-cp addons/gnuplot* $(scriptdir)addons/

EXTRA_DIST = $(SCRIPTFILES) \
        examples/tests.ys \
        examples/findsum.ys \
        examples/queens.ys examples/benchmark.ys \
        examples/benchmark2.ys examples/benchbuild.ys  examples/goldbach.ys examples/series.ys \
        addons/README addons/editvi.ys addons/editvi.ys.def addons/gnuplot.ys \
        addons/gnuplot.ys.def

test:
	../src/yacas -pc ../tests/arithmetic.yts
	../src/yacas -pc ../tests/calculus.yts
	../src/yacas -pc ../tests/canprove.yts
	../src/yacas -pc ../tests/comments.yts
	../src/yacas -pc ../tests/linalg.yts
	../src/yacas -pc ../tests/lists.yts
	../src/yacas -pc ../tests/logic_simplify_test.yts
	../src/yacas -pc ../tests/poly.yts
	../src/yacas -pc ../tests/predicates.yts
	../src/yacas -pc ../tests/programming.yts
	../src/yacas -pc ../tests/radsimp.yts
	../src/yacas -pc ../tests/regress.yts
	../src/yacas -pc ../tests/simplify.yts
	../src/yacas -pc ../tests/solve.yts
	../src/yacas -pc ../tests/sums.yts
	../src/yacas -pc ../tests/tensors.yts
	../src/yacas -pc ../tests/orthopoly.yts
