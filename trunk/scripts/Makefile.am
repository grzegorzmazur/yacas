SCRIPTFILES = complex complex.def constants controlflow controlflow.def \
	deffunc deriv deriv.def functional functional.def glview \
	integrate integrate.def linalg linalg.def lists lists.def newly \
	newly.def numbers numbers.def random random.def simplify \
	simplify.def solve solve.def standard stdfuncs stdfuncs.def \
	stdopers stubs stubs.def substitute substitute.def sums sums.def \
	univar univar.def yacasinit assoc assoc.def html html.def \
	fakedb fakedb.def array array.def example example.def \
	trigsimp trigsimp.def formula formula.def patterns \
	patterns.def predicates predicates.def stats stats.def padic \
	padic.def testers testers.def factors factors.def tensor tensor.def \
	texform texform.def print c_form c_form.def logic logic.def cstubgen \
        radsimp radsimp.def pslq pslq.def odesolver odesolver.def limit

script_DATA = $(SCRIPTFILES)

install-data-local:
	-mkdir $(scriptdir)addons/
	-cp addons/README $(scriptdir)addons/
	-cp addons/editvi* $(scriptdir)addons/
	-cp addons/gnuplot* $(scriptdir)addons/

EXTRA_DIST = $(SCRIPTFILES) \
	examples/tests \
	examples/findsum \
	examples/queens examples/benchmark \
	examples/benchmark2 examples/benchbuild  examples/goldbach examples/series \
	addons/README addons/editvi addons/editvi.def addons/gnuplot \
        addons/gnuplot.def
 
