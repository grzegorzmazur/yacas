cmake_minimum_required (VERSION 3.0)

cmake_policy(SET CMP0048 NEW)

option (ENABLE_CYACAS_CONSOLE "build the C++ yacas engine" ON)
option (ENABLE_CYACAS_GUI "build the C++ yacas engine" ON)
option (ENABLE_CYACAS_KERNEL "build the C++ yacas engine" ON)
option (ENABLE_JYACAS "build the Java yacas engine" OFF)
option (ENABLE_DOCS "generate documentation" OFF)

set (ENABLE_CYACAS ${ENABLE_CYACAS_CONSOLE} OR ${ENABLE_CYACAS_GUI} OR ${ENABLE_CYACAS_KERNEL})

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

set (LANGUAGES CXX C)

if (ENABLE_JYACAS)
  find_package (Java)
  include (UseJava)
  set (LANGUAGES ${LANGUAGES} Java)
endif ()

project (YACAS VERSION 1.5.99 LANGUAGES ${LANGUAGES})

#if (${CMAKE_SYSTEM_NAME} STREQUAL "Emscripten")
#  set (YACAS_SOURCES ${YACAS_SOURCES} ${YACAS_JS_INTERFACE_SOURCES})
#endif ()

set (YACAS_SCRIPTS
  scripts/array.rep/code.ys
  scripts/array.rep/code.ys.def
  scripts/assoc.rep/code.ys
  scripts/assoc.rep/code.ys.def
  scripts/base.rep/math.ys
  scripts/base.rep/math.ys.def
  scripts/c_form.rep/code.ys
  scripts/c_form.rep/code.ys.def
  scripts/calendar.rep/code.ys
  scripts/calendar.rep/code.ys.def
  scripts/complex.rep/code.ys
  scripts/complex.rep/code.ys.def
  scripts/complex.rep/om.ys
  scripts/constants.rep/code.ys
  scripts/constants.rep/code.ys.def
  scripts/constants.rep/om.ys
  scripts/controlflow.rep/code.ys
  scripts/controlflow.rep/code.ys.def
  scripts/debug.rep/code.ys
  scripts/debug.rep/code.ys.def
  scripts/deffunc.rep/code.ys
  scripts/deffunc.rep/code.ys.def
  scripts/deriv.rep/code.ys
  scripts/deriv.rep/code.ys.def
  scripts/example.rep/code.ys
  scripts/example.rep/code.ys.def
  scripts/factors.rep/binaryfactors.ys
  scripts/factors.rep/binaryfactors.ys.def
  scripts/factors.rep/code.ys
  scripts/factors.rep/code.ys.def
  scripts/functional.rep/code.ys
  scripts/functional.rep/code.ys.def
  scripts/functional.rep/om.ys
  scripts/graph.rep/code.ys
  scripts/graph.rep/code.ys.def
  scripts/html.rep/code.ys
  scripts/html.rep/code.ys.def
  scripts/integrate.rep/code.ys
  scripts/integrate.rep/code.ys.def
  scripts/integrate.rep/om.ys
  scripts/io.rep/code.ys
  scripts/io.rep/code.ys.def
  scripts/io.rep/defaultprint.ys
  scripts/io.rep/defaultprint.ys.def
  scripts/io.rep/errors.ys
  scripts/io.rep/formula.ys
  scripts/io.rep/print.ys
  scripts/limit.rep/code.ys
  scripts/limit.rep/code.ys.def
  scripts/limit.rep/om.ys
  scripts/linalg.rep/code.ys
  scripts/linalg.rep/code.ys.def
  scripts/lists.rep/code.ys
  scripts/lists.rep/code.ys.def
  scripts/lists.rep/scopestack.ys
  scripts/lists.rep/scopestack.ys.def
  scripts/localrules.rep/code.ys
  scripts/localrules.rep/code.ys.def
  scripts/logic.rep/code.ys
  scripts/logic.rep/code.ys.def
  scripts/logic.rep/om.ys
  scripts/multivar.rep/code.ys
  scripts/multivar.rep/code.ys.def
  scripts/multivar.rep/makemulti.ys
  scripts/multivar.rep/sparsenomial.ys
  scripts/multivar.rep/sparsetree.ys
  scripts/multivar.rep/sparsetree.ys.def
  scripts/newly.rep/code.ys
  scripts/newly.rep/code.ys.def
  scripts/nintegrate.rep/code.ys
  scripts/nintegrate.rep/code.ys.def
  scripts/numbers.rep/GaussianIntegers.ys
  scripts/numbers.rep/GaussianIntegers.ys.def
  scripts/numbers.rep/NumberTheory.ys
  scripts/numbers.rep/NumberTheory.ys.def
  scripts/numbers.rep/code.ys
  scripts/numbers.rep/code.ys.def
  scripts/numbers.rep/nthroot.ys
  scripts/numbers.rep/nthroot.ys.def
  scripts/numbers.rep/om.ys
  scripts/odesolver.rep/code.ys
  scripts/odesolver.rep/code.ys.def
  scripts/openmath.rep/code.ys
  scripts/openmath.rep/code.ys.def
  scripts/orthopoly.rep/code.ys
  scripts/orthopoly.rep/code.ys.def
  scripts/packages.ys
  scripts/padic.rep/code.ys
  scripts/padic.rep/code.ys.def
  scripts/patterns.rep/code.ys
  scripts/patterns.rep/code.ys.def
  scripts/plots.rep/backends-2d.ys
  scripts/plots.rep/backends-3d.ys
  scripts/plots.rep/code.ys
  scripts/plots.rep/code.ys.def
  scripts/plots.rep/plot2d.ys
  scripts/plots.rep/plot2d.ys.def
  scripts/plots.rep/plot3d.ys
  scripts/plots.rep/plot3d.ys.def
  scripts/predicates.rep/code.ys
  scripts/predicates.rep/code.ys.def
  scripts/probability.rep/code.ys
  scripts/probability.rep/code.ys.def
  scripts/pslq.rep/code.ys
  scripts/pslq.rep/code.ys.def
  scripts/rabinmiller.rep/code.ys
  scripts/rabinmiller.rep/code.ys.def
  scripts/radsimp.rep/code.ys
  scripts/radsimp.rep/code.ys.def
  scripts/random.rep/code.ys
  scripts/random.rep/code.ys.def
  scripts/rational.rep/code.ys
  scripts/rational.rep/code.ys.def
  scripts/simplify.rep/code.ys
  scripts/simplify.rep/code.ys.def
  scripts/simplify.rep/factorial.ys
  scripts/simplify.rep/factorial.ys.def
  scripts/solve.rep/code.ys
  scripts/solve.rep/code.ys.def
  scripts/specfunc.rep/bernou.ys
  scripts/specfunc.rep/bernou.ys.def
  scripts/specfunc.rep/bessel.ys
  scripts/specfunc.rep/bessel.ys.def
  scripts/specfunc.rep/code.ys
  scripts/specfunc.rep/code.ys.def
  scripts/specfunc.rep/gamma.ys
  scripts/specfunc.rep/gamma.ys.def
  scripts/specfunc.rep/gammaconst.ys
  scripts/specfunc.rep/gammaconst.ys.def
  scripts/specfunc.rep/om.ys
  scripts/specfunc.rep/zeta.ys
  scripts/specfunc.rep/zeta.ys.def
  scripts/standard.ys
  scripts/standard.ys.def
  scripts/statistics.rep/distributions.ys
  scripts/statistics.rep/distributions.ys.def
  scripts/statistics.rep/hypothesystest.ys
  scripts/statistics.rep/hypothesystest.ys.def
  scripts/statistics.rep/incompletegamma.ys
  scripts/statistics.rep/incompletegamma.ys.def
  scripts/statistics.rep/randomtest.ys
  scripts/statistics.rep/regression.ys
  scripts/statistics.rep/regression.ys.def
  scripts/statistics.rep/statistics.ys
  scripts/statistics.rep/statistics.ys.def
  scripts/stats.rep/code.ys
  scripts/stats.rep/code.ys.def
  scripts/stdarith.ys
  scripts/stdarith.ys.def
  scripts/stdfuncs.rep/code.ys
  scripts/stdfuncs.rep/code.ys.def
  scripts/stdfuncs.rep/elemfuncs.ys
  scripts/stdfuncs.rep/elemfuncs.ys.def
  scripts/stdfuncs.rep/numerical.ys
  scripts/stdfuncs.rep/numerical.ys.def
  scripts/stdfuncs.rep/nummethods.ys
  scripts/stdfuncs.rep/nummethods.ys.def
  scripts/stdfuncs.rep/om.ys
  scripts/stdopers.ys
  scripts/stubs.rep/code.ys
  scripts/stubs.rep/code.ys.def
  scripts/stubs.rep/om.ys
  scripts/substitute.rep/code.ys
  scripts/substitute.rep/code.ys.def
  scripts/sums.rep/code.ys
  scripts/sums.rep/code.ys.def
  scripts/sums.rep/om.ys
  scripts/trigsimp.rep/code.ys.def
  scripts/univar.rep/Cyclotomic.ys
  scripts/univar.rep/Cyclotomic.ys.def
  scripts/univar.rep/code.ys
  scripts/trigsimp.rep/code.ys.def
  scripts/univar.rep/Cyclotomic.ys
  scripts/univar.rep/Cyclotomic.ys.def
  scripts/univar.rep/code.ys
  scripts/sums.rep/taylor.ys
  scripts/sums.rep/taylor.ys.def
  scripts/sums.rep/taylor3.ys
  scripts/sums.rep/taylor3.ys.def
  scripts/tensor.rep/code.ys
  scripts/tensor.rep/code.ys.def
  scripts/testers.rep/code.ys
  scripts/testers.rep/code.ys.def
  scripts/texform.rep/code.ys
  scripts/texform.rep/code.ys.def
  scripts/transforms.rep/code.ys
  scripts/trigsimp.rep/code.ys.def
  scripts/univar.rep/Cyclotomic.ys
  scripts/univar.rep/Cyclotomic.ys.def
  scripts/univar.rep/code.ys
  scripts/transforms.rep/code.ys.def
  scripts/trigsimp.rep/code.ys
  scripts/trigsimp.rep/code.ys.def
  scripts/univar.rep/Cyclotomic.ys
  scripts/univar.rep/Cyclotomic.ys.def
  scripts/univar.rep/code.ys
  scripts/univar.rep/code.ys.def
  scripts/univar.rep/sparse.ys
  scripts/univar.rep/sparse.ys.def
  scripts/univar.rep/sturm.ys
  scripts/univar.rep/sturm.ys.def
  scripts/yacasinit.ys)

if (${ENABLE_CYACAS})
    install (DIRECTORY scripts/ DESTINATION share/yacas/scripts COMPONENT app)
endif ()

if (ENABLE_DOCS)
  add_subdirectory (docs)
endif()

if (ENABLE_JYACAS)
    add_subdirectory (jyacas)
endif ()

if (ENABLE_CYACAS)
    add_subdirectory (cyacas)
endif ()

if (${ENABLE_CYACAS} OR ${ENABLE_JYACAS})
    include (CTest)
    add_subdirectory (tests)
endif ()
