SCRIPTFILES = \
        complex.ys complex.ys.def constants.ys controlflow.ys controlflow.ys.def \
        deffunc.ys deriv.ys deriv.ys.def functional.ys functional.ys.def glview.ys \
        integrate.ys integrate.ys.def linalg.ys linalg.ys.def lists.ys lists.ys.def newly.ys \
        newly.ys.def numbers.ys numbers.ys.def random.ys random.ys.def simplify.ys \
        simplify.ys.def solve.ys solve.ys.def standard.ys stdfuncs stdfuncs.ys.def \
        stdopers.ys stubs stubs.ys.def substitute.ys substitute.ys.def sums.ys sums.ys.def \
        univar.ys univar.ys.def yacasinit.ys assoc.ys assoc.ys.def html.ys html.ys.def \
        fakedb.ys fakedb.ys.def array.ys array.ys.def example.ys example.ys.def \
        trigsimp.ys trigsimp.ys.def formula.ys formula.ys.def patterns.ys \
        patterns.ys.def predicates.ys predicates.ys.def stats.ys stats.ys.def padic.ys \
        padic.ys.def testers.ys testers.ys.def factors.ys factors.ys.def tensor.ys tensor.ys.def \
        texform.ys texform.ys.def print.ys c_form.ys c_form.ys.def logic.ys logic.ys.def cstubgen.ys \
        radsimp.ys radsimp.ys.def pslq.ys pslq.ys.def odesolver.ys odesolver.ys.def limit.ys limit.ys.def \
        localrules.ys localrules.def

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
