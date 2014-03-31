cmake_minimum_required (VERSION 2.8.7)

option (ENABLE_DOCS "generate documentation" OFF)
option (ENABLE_JYACAS "build the Java yacas engine" OFF)

if (ENABLE_JYACAS)
  find_package (Java)
  include (${CMAKE_ROOT}/Modules/UseJava.cmake)
  project (YACAS CXX C Java)
else ()
  project (YACAS CXX C)
endif ()

set (YACAS_VERSION_MAJOR 1)
set (YACAS_VERSION_MINOR 3)
set (YACAS_VERSION_MICRO 5)

set (YACAS_VERSION ${YACAS_VERSION_MAJOR}.${YACAS_VERSION_MINOR}.${YACAS_VERSION_MICRO})

include (CTest)
set(CTEST_PROJECT_NAME "yacas")


include (CheckIncludeFiles)

check_include_files (unistd.h HAVE_UNISTD_H)
check_include_files (sys/ioctl.h HAVE_SYS_IOCTL_H)
check_include_files (sys/socket.h HAVE_SYS_SOCKET_H)
check_include_files (sys/time.h HAVE_SYS_TIME_H)
check_include_files (sys/types.h HAVE_SYS_TYPES_H)
check_include_files (sys/sysinfo.h HAVE_SYS_SYSINFO_H)
check_include_files (malloc.h HAVE_MALLOC_H)
check_include_files (memory.h HAVE_MEMORY_H)
check_include_files (stdlib.h HAVE_STDLIB_H)
check_include_files (stdio.h HAVE_STDIO_H)
check_include_files (ctype.h HAVE_CTYPE_H)
check_include_files (dirent.h HAVE_DIRENT_H)
check_include_files (math.h HAVE_MATH_H)
check_include_files (stdarg.h HAVE_STDARG_H)
check_include_files (signal.h HAVE_SIGNAL_H)
check_include_files (string.h HAVE_STRING_H)
check_include_files (strings.h HAVE_STRINGS_H)


include (${CMAKE_ROOT}/Modules/CheckFunctionExists.cmake)

check_function_exists (select HAVE_SELECT)
check_function_exists (socket HAVE_SOCKET)
check_function_exists (strtod HAVE_STRTOD)
check_function_exists (strchr HAVE_STRCHR)
check_function_exists (vsnprintf HAVE_VSNPRINTF)

configure_file (
  "${PROJECT_SOURCE_DIR}/cmake_config.h.in"
  "${PROJECT_BINARY_DIR}/config.h"
  )

configure_file (
  "${PROJECT_SOURCE_DIR}/yacas_version.h.in"
  "${PROJECT_BINARY_DIR}/include/yacas/yacas_version.h"
  )

add_definitions(-DHAVE_CONFIG_H)

include_directories(
  "${PROJECT_SOURCE_DIR}/include"
  "${PROJECT_BINARY_DIR}/include"
  "${PROJECT_BINARY_DIR}"
  )

set (YACAS_LIB_SOURCES src/stdcommandline.cpp src/deffile.cpp src/infixparser.cpp src/lispatom.cpp src/lispcleanupstack.cpp src/lispenvironment.cpp src/lispeval.cpp src/lisperror.cpp src/lispio.cpp src/lispobject.cpp src/lispparser.cpp src/lispuserfunc.cpp src/mathcommands.cpp src/mathenvironment.cpp src/mathuserfunc.cpp src/standard.cpp src/stdfileio.cpp src/arggetter.cpp src/stringio.cpp src/tokenizer.cpp src/yacasapi.cpp src/genericobject.cpp src/arrayclass.cpp src/lispevalhash.cpp src/patterns.cpp src/patternclass.cpp src/substitute.cpp src/mathcommands2.cpp src/mathcommands3.cpp src/errors.cpp src/patcher.cpp src/genericstructs.cpp src/xmltokenizer.cpp src/commandline.cpp src/anumber.cpp src/yacasnumbers.cpp src/numbers.cpp src/platmath.cpp src/grower.cpp src/stdstubs.cpp src/obmalloc.cpp src/lisphash.cpp src/lispstring.cpp src/mathutil.cpp src/yacasbase.cpp src/debugmem.cpp)

set (YACAS_COMMON_SOURCES src/yacasmain.cpp)

set (YACAS_UNIX_SOURCES src/unixcommandline.cpp)
set (YACAS_WIN32_SOURCES src/win32commandline.cpp res/yacas.rc)

if (UNIX)
  set (YACAS_SOURCES ${YACAS_COMMON_SOURCES} ${YACAS_UNIX_SOURCES})
else ()
  set (YACAS_SOURCES ${YACAS_COMMON_SOURCES} ${YACAS_WIN32_SOURCES})
endif ()

set (YACAS_SCRIPTS scripts/corefunctions.ys scripts/packages.ys scripts/standard.ys scripts/stdarith.ys scripts/stdopers.ys scripts/yacasinit.ys scripts/standard.ys.def scripts/stdarith.ys.def scripts/array.rep/code.ys scripts/assoc.rep/code.ys scripts/base.rep/math.ys scripts/c_form.rep/code.ys scripts/calendar.rep/code.ys scripts/complex.rep/code.ys scripts/complex.rep/om.ys scripts/constants.rep/code.ys scripts/constants.rep/om.ys scripts/controlflow.rep/code.ys scripts/debug.rep/code.ys scripts/deffunc.rep/code.ys scripts/deriv.rep/code.ys scripts/example.rep/code.ys scripts/factors.rep/binaryfactors.ys scripts/factors.rep/code.ys scripts/functional.rep/code.ys scripts/functional.rep/om.ys scripts/graph.rep/code.ys scripts/html.rep/code.ys scripts/integrate.rep/code.ys scripts/integrate.rep/om.ys scripts/io.rep/code.ys scripts/io.rep/defaultprint.ys scripts/io.rep/errors.ys scripts/io.rep/formula.ys scripts/io.rep/print.ys scripts/limit.rep/code.ys scripts/limit.rep/om.ys scripts/linalg.rep/code.ys scripts/lists.rep/code.ys scripts/lists.rep/scopestack.ys scripts/localrules.rep/code.ys scripts/logic.rep/code.ys scripts/logic.rep/om.ys scripts/multivar.rep/code.ys scripts/multivar.rep/makemulti.ys scripts/multivar.rep/sparsenomial.ys scripts/multivar.rep/sparsetree.ys scripts/newly.rep/code.ys scripts/numbers.rep/GaussianIntegers.ys scripts/numbers.rep/NumberTheory.ys scripts/numbers.rep/code.ys scripts/numbers.rep/nthroot.ys scripts/numbers.rep/om.ys scripts/odesolver.rep/code.ys scripts/openmath.rep/code.ys scripts/orthopoly.rep/code.ys scripts/padic.rep/code.ys scripts/patterns.rep/code.ys scripts/plots.rep/backends-2d.ys scripts/plots.rep/backends-3d.ys scripts/plots.rep/code.ys scripts/plots.rep/plot2d.ys scripts/plots.rep/plot3d.ys scripts/predicates.rep/code.ys scripts/probability.rep/code.ys scripts/pslq.rep/code.ys scripts/rabinmiller.rep/code.ys scripts/radsimp.rep/code.ys scripts/random.rep/code.ys scripts/simplify.rep/code.ys scripts/simplify.rep/factorial.ys scripts/solve.rep/code.ys scripts/specfunc.rep/bernou.ys scripts/specfunc.rep/bessel.ys scripts/specfunc.rep/code.ys scripts/specfunc.rep/gamma.ys scripts/specfunc.rep/gammaconst.ys scripts/specfunc.rep/om.ys scripts/specfunc.rep/zeta.ys scripts/statistics.rep/distributions.ys scripts/statistics.rep/hypothesystest.ys scripts/statistics.rep/incompletegamma.ys scripts/statistics.rep/randomtest.ys scripts/statistics.rep/regression.ys scripts/statistics.rep/statistics.ys scripts/stats.rep/code.ys scripts/stdfuncs.rep/code.ys scripts/stdfuncs.rep/elemfuncs.ys scripts/stdfuncs.rep/numerical.ys scripts/stdfuncs.rep/nummethods.ys scripts/stdfuncs.rep/om.ys scripts/stubs.rep/code.ys scripts/stubs.rep/om.ys scripts/substitute.rep/code.ys scripts/sums.rep/code.ys scripts/sums.rep/om.ys scripts/sums.rep/taylor.ys scripts/sums.rep/taylor3.ys scripts/tensor.rep/code.ys scripts/testers.rep/code.ys scripts/texform.rep/code.ys scripts/transforms.rep/code.ys scripts/trigsimp.rep/code.ys scripts/univar.rep/Cyclotomic.ys scripts/univar.rep/code.ys scripts/univar.rep/sparse.ys scripts/univar.rep/sturm.ys scripts/array.rep/code.ys.def scripts/assoc.rep/code.ys.def scripts/base.rep/math.ys.def scripts/c_form.rep/code.ys.def scripts/calendar.rep/code.ys.def scripts/complex.rep/code.ys.def scripts/constants.rep/code.ys.def scripts/controlflow.rep/code.ys.def scripts/debug.rep/code.ys.def scripts/deffunc.rep/code.ys.def scripts/deriv.rep/code.ys.def scripts/example.rep/code.ys.def scripts/factors.rep/binaryfactors.ys.def scripts/factors.rep/code.ys.def scripts/functional.rep/code.ys.def scripts/graph.rep/code.ys.def scripts/html.rep/code.ys.def scripts/integrate.rep/code.ys.def scripts/io.rep/code.ys.def scripts/io.rep/defaultprint.ys.def scripts/limit.rep/code.ys.def scripts/linalg.rep/code.ys.def scripts/lists.rep/code.ys.def scripts/lists.rep/scopestack.ys.def scripts/localrules.rep/code.ys.def scripts/logic.rep/code.ys.def scripts/multivar.rep/code.ys.def scripts/multivar.rep/sparsetree.ys.def scripts/newly.rep/code.ys.def scripts/numbers.rep/GaussianIntegers.ys.def scripts/numbers.rep/NumberTheory.ys.def scripts/numbers.rep/code.ys.def scripts/numbers.rep/nthroot.ys.def scripts/odesolver.rep/code.ys.def scripts/openmath.rep/code.ys.def scripts/orthopoly.rep/code.ys.def scripts/padic.rep/code.ys.def scripts/patterns.rep/code.ys.def scripts/plots.rep/code.ys.def scripts/plots.rep/plot2d.ys.def scripts/plots.rep/plot3d.ys.def scripts/predicates.rep/code.ys.def scripts/probability.rep/code.ys.def scripts/pslq.rep/code.ys.def scripts/rabinmiller.rep/code.ys.def scripts/radsimp.rep/code.ys.def scripts/random.rep/code.ys.def scripts/simplify.rep/code.ys.def scripts/simplify.rep/factorial.ys.def scripts/solve.rep/code.ys.def scripts/specfunc.rep/bernou.ys.def scripts/specfunc.rep/bessel.ys.def scripts/specfunc.rep/code.ys.def scripts/specfunc.rep/gamma.ys.def scripts/specfunc.rep/gammaconst.ys.def scripts/specfunc.rep/zeta.ys.def scripts/statistics.rep/distributions.ys.def scripts/statistics.rep/hypothesystest.ys.def scripts/statistics.rep/incompletegamma.ys.def scripts/statistics.rep/regression.ys.def scripts/statistics.rep/statistics.ys.def scripts/stats.rep/code.ys.def scripts/stdfuncs.rep/code.ys.def scripts/stdfuncs.rep/elemfuncs.ys.def scripts/stdfuncs.rep/numerical.ys.def scripts/stdfuncs.rep/nummethods.ys.def scripts/stubs.rep/code.ys.def scripts/substitute.rep/code.ys.def scripts/sums.rep/code.ys.def scripts/sums.rep/taylor.ys.def scripts/sums.rep/taylor3.ys.def scripts/tensor.rep/code.ys.def scripts/testers.rep/code.ys.def scripts/texform.rep/code.ys.def scripts/transforms.rep/code.ys.def scripts/trigsimp.rep/code.ys.def scripts/univar.rep/Cyclotomic.ys.def scripts/univar.rep/code.ys.def scripts/univar.rep/sparse.ys.def scripts/univar.rep/sturm.ys.def )

add_library (libyacas_static STATIC ${YACAS_LIB_SOURCES})
add_library (libyacas_shared SHARED ${YACAS_LIB_SOURCES})

set_target_properties(libyacas_static PROPERTIES OUTPUT_NAME "yacas")
set_target_properties(libyacas_shared PROPERTIES OUTPUT_NAME "yacas" SOVERSION 1.0.0) 

install (TARGETS libyacas_static ARCHIVE DESTINATION lib COMPONENT dev)
install (TARGETS libyacas_shared LIBRARY DESTINATION lib ARCHIVE DESTINATION lib RUNTIME DESTINATION bin COMPONENT app)

add_executable (yacas ${YACAS_SOURCES})

target_link_libraries (yacas libyacas_shared)

if (WIN32)
  target_link_libraries (yacas wsock32 ws2_32 shlwapi)
endif()

add_executable (gencorefunctions src/gencorefunctions.cpp)

add_executable (codereviewer src/codereviewer.cpp)

add_executable (autocompleter docs/autocompleter.cpp)
add_executable (riptestfromyart docs/riptestfromyart.cpp)

add_executable (manripper manmake/manripper.cpp)
add_executable (removeduplicates manmake/removeduplicates.cpp)

if (CMAKE_CROSSCOMPILING)
  set (YACAS ${YACAS_NATIVE_BUILD}/yacas)
else ()
  set (YACAS ${PROJECT_BINARY_DIR}/yacas)
endif ()

if (ENABLE_DOCS)

  find_program (PERL perl)
  find_package (LATEX)

  set (CHAPTERS FDL.chapt GPL.chapt cl-options.chapt config.chapt addons.chapt YacasDebugger.chapt ABIN-grammar.chapt SimpleTools.chapt CVS-Howto.chapt YacasDocs.chapt wester-1994.chapt new.chapt paper.chapt wordproblems.chapt BuildSystem.chapt arith.chapt calc.chapt complex.chapt solvers.chapt simplify.chapt ode.chapt linalg.chapt univar.chapt lists.chapt functional.chapt controlflow.chapt preds.chapt logic.chapt const.chapt vars.chapt io.chapt numtheory.chapt strings.chapt transforms.chapt probability-and-statistics.chapt glossary.chapt glossary.chapt algo-basic.chapt algo-numapprox.chapt algorithms-elemfunc.chapt algorithms-specfunc.chapt algorithms-numtheory.chapt algorithms-integration.chapt algorithms-multivar.chapt SturmSequences.chapt algo-refs.chapt algo-contfrac.chapt algorithms-transforms.chapt numerics.chapt)
  foreach (_chapter ${CHAPTERS})
    add_custom_command (
      OUTPUT ${_chapter}
      COMMAND ${PERL} ${PROJECT_SOURCE_DIR}/manmake/txt2yacasdoc.pl
      ARGS < ${PROJECT_SOURCE_DIR}/manmake/${_chapter}.txt > ${_chapter}
      MAIN_DEPENDENCY manmake/${_chapter}.txt
      DEPENDS manmake/txt2yacasdoc.pl)
  endforeach()

  set (BOOKS intro.book coding.book ref.book refprog.book essays.book Algo.book)
  foreach (_book ${BOOKS})
    add_custom_command (
      OUTPUT ${_book}
      COMMAND ${PERL} ${PROJECT_SOURCE_DIR}/manmake/txt2yacasdoc.pl
      ARGS < ${PROJECT_SOURCE_DIR}/manmake/${_book}.txt > ${_book}
      MAIN_DEPENDENCY manmake/${_book}.txt
      DEPENDS manmake/txt2yacasdoc.pl)
  endforeach()

  add_custom_target (docs DEPENDS ${BOOKS} ${CHAPTERS})

  foreach (_book ${BOOKS})
    add_custom_command (
      OUTPUT ${_book}.tex
      COMMAND ${YACAS} --rootdir ${PROJECT_SOURCE_DIR}/scripts:${PROJECT_BINARY_DIR} -i "[ Use(\"${PROJECT_SOURCE_DIR}/manmake/book2TeX.ys\"); Load(\"${_book}\"); TeXFinishUp(); ];" > ${_book}.tex
      MAIN_DEPENDENCY ${_book}
      DEPENDS docs yacas
      VERBATIM)
  endforeach()

  set (TEX_BOOKS)
  foreach (_book ${BOOKS})
    list (APPEND TEX_BOOKS ${_book}.tex)
  endforeach()

  add_custom_target (tex_docs DEPENDS ${TEX_BOOKS})

  foreach (_book ${BOOKS})
    add_custom_command ( 
      OUTPUT    ${PROJECT_BINARY_DIR}/${_book}.pdf
      COMMAND   ${PDFLATEX_COMPILER}
      ARGS      ${PROJECT_BINARY_DIR}/${_book}.tex
      DEPENDS   ${PROJECT_BINARY_DIR}/${_book}.tex
      COMMENT   "LaTeX"
  )
  endforeach()

  set (PDF_BOOKS)
  foreach (_book ${BOOKS})
    list (APPEND PDF_BOOKS ${_book}.pdf)
  endforeach()

  add_custom_target (pdf_docs ALL DEPENDS ${PDF_BOOKS})

  set (PDF_BOOK_PATHS)
  foreach (_book ${PDF_BOOKS})
    list (APPEND PDF_BOOK_PATHS ${PROJECT_BINARY_DIR}/${_book})
  endforeach()

  install (FILES ${PDF_BOOK_PATHS} DESTINATION share/yacas/documentation COMPONENT doc)

endif()

foreach (_script ${YACAS_SCRIPTS})
  if (${_script} MATCHES ".+def$")
    string (REPLACE scripts/ "" _script ${_script})
    string (REPLACE .def "" _script ${_script})
    list (APPEND YACAS_PACKAGES ${_script})
  endif ()
endforeach ()

list (SORT YACAS_PACKAGES)
list (REMOVE_DUPLICATES YACAS_PACKAGES)

file (REMOVE ${PROJECT_BINARY_DIR}/scripts/packages.ys)

file (APPEND ${PROJECT_BINARY_DIR}/scripts/packages.ys "Defun(DefFileList,{}) {\n")

foreach (_package ${YACAS_PACKAGES})
  file (APPEND ${PROJECT_BINARY_DIR}/scripts/packages.ys "    \"${_package}\",\n")
endforeach ()

file (APPEND ${PROJECT_BINARY_DIR}/scripts/packages.ys "};\n")

add_dependencies (yacas ${PROJECT_BINARY_DIR}/scripts/packages.ys)

install (TARGETS yacas RUNTIME DESTINATION bin COMPONENT app)
install (DIRECTORY ${PROJECT_BINARY_DIR}/scripts/ DESTINATION share/yacas/scripts COMPONENT app)
install (DIRECTORY scripts/ DESTINATION share/yacas/scripts COMPONENT app)
install (DIRECTORY include/ DESTINATION include COMPONENT dev)
install (FILES ${PROJECT_BINARY_DIR}/include/yacas/yacas_version.h DESTINATION include/yacas COMPONENT dev)

set (YACAS_TESTS openmath.yts macro.yts arithmetic.yts c_tex_form.yts calculus.yts canprove.yts comments.yts complex.yts deriv.yts dimensions.yts dot.yts journal.yts integrate.yts lists.yts logic_simplify_test.yts matrixpower.yts nthroot.yts outer.yts predicates.yts ode.yts tensors.yts trace.yts tr.yts multivar.yts numbers.yts io.yts programming.yts simplify.yts solve.yts sums.yts transforms.yts radsimp.yts linalg.yts orthopoly.yts poly.yts numerics.yts scopestack.yts plots.yts GaussianIntegers.yts nummethods.yts sturm.yts cyclotomic.yts binaryfactors.yts padic.yts calendar.yts newly.yts regress.yts)

foreach (_test ${YACAS_TESTS})
  add_test(NAME ${_test} WORKING_DIRECTORY ${PROJECT_SOURCE_DIR} COMMAND ${PROJECT_SOURCE_DIR}/tests/test-yacas "${PROJECT_BINARY_DIR}/yacas -pc --rootdir ${PROJECT_SOURCE_DIR}/scripts:${PROJECT_BINARY_DIR}/scripts" ${PROJECT_SOURCE_DIR}/tests ${_test})
endforeach()

if (ENABLE_JYACAS)

  configure_file (
    "${PROJECT_SOURCE_DIR}/JavaYacas/CVersion.java.in"
    "${CMAKE_CURRENT_BINARY_DIR}/CVersion.java"
    )

  file (COPY ${PROJECT_SOURCE_DIR}/scripts DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/jyacas.dir)

  set (CMAKE_JAVA_INCLUDE_PATH ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/jyacas.dir/)

  set (CMAKE_JAVA_JAR_ENTRY_POINT net/sf/yacas/YacasConsole)

  add_jar (jyacas ${CMAKE_CURRENT_BINARY_DIR}/CVersion.java JavaYacas/net/sf/yacas/GraphicsPrimitives.java JavaYacas/net/sf/yacas/SBox.java JavaYacas/net/sf/yacas/SBoxBuilder.java JavaYacas/net/sf/yacas/TeXParser.java JavaYacas/net/sf/yacas/FormulaViewApplet.java JavaYacas/net/sf/yacas/Grapher.java JavaYacas/net/sf/yacas/GrapherApplet.java JavaYacas/net/sf/yacas/StdFileOutput.java JavaYacas/net/sf/yacas/StdFileInput.java JavaYacas/net/sf/yacas/JarInputFile.java JavaYacas/net/sf/yacas/CachedStdFileInput.java JavaYacas/net/sf/yacas/InputStatus.java JavaYacas/net/sf/yacas/LispInput.java JavaYacas/net/sf/yacas/LispDefFile.java JavaYacas/net/sf/yacas/LispDefFiles.java JavaYacas/net/sf/yacas/InputDirectories.java JavaYacas/net/sf/yacas/LispOutput.java JavaYacas/net/sf/yacas/LispHashTable.java JavaYacas/net/sf/yacas/LispAssociatedHash.java JavaYacas/net/sf/yacas/LispGlobal.java JavaYacas/net/sf/yacas/LispGlobalVariable.java JavaYacas/net/sf/yacas/LispArgList.java JavaYacas/net/sf/yacas/GenericClass.java JavaYacas/net/sf/yacas/LispGenericClass.java JavaYacas/net/sf/yacas/ArrayClass.java JavaYacas/net/sf/yacas/LispInfixOperator.java JavaYacas/net/sf/yacas/LispPtr.java JavaYacas/net/sf/yacas/LispObject.java JavaYacas/net/sf/yacas/LispPrinter.java JavaYacas/net/sf/yacas/LispError.java JavaYacas/net/sf/yacas/LispTokenizer.java JavaYacas/net/sf/yacas/LispParser.java JavaYacas/net/sf/yacas/LispAtom.java JavaYacas/net/sf/yacas/LispNumber.java JavaYacas/net/sf/yacas/LispSubList.java JavaYacas/net/sf/yacas/LispStandard.java JavaYacas/net/sf/yacas/LispUserFunctions.java JavaYacas/net/sf/yacas/LispArityUserFunction.java JavaYacas/net/sf/yacas/BranchingUserFunction.java JavaYacas/net/sf/yacas/MacroUserFunction.java JavaYacas/net/sf/yacas/ListedMacroUserFunction.java JavaYacas/net/sf/yacas/ListedBranchingUserFunction.java JavaYacas/net/sf/yacas/LispMultiUserFunction.java JavaYacas/net/sf/yacas/StringInput.java JavaYacas/net/sf/yacas/StringOutput.java JavaYacas/net/sf/yacas/InfixParser.java JavaYacas/net/sf/yacas/ParsedObject.java JavaYacas/net/sf/yacas/LispEvaluatorBase.java JavaYacas/net/sf/yacas/BigNumber.java JavaYacas/net/sf/yacas/YacasEvaluator.java JavaYacas/net/sf/yacas/YacasEvalCaller.java JavaYacas/net/sf/yacas/YacasCoreCommands.java JavaYacas/net/sf/yacas/UserStackInformation.java JavaYacas/net/sf/yacas/BasicEvaluator.java JavaYacas/net/sf/yacas/EvalFuncBase.java JavaYacas/net/sf/yacas/LispOperators.java JavaYacas/net/sf/yacas/LispUserFunction.java JavaYacas/net/sf/yacas/LispPtrArray.java JavaYacas/net/sf/yacas/LispLocalFrame.java JavaYacas/net/sf/yacas/LispIterator.java JavaYacas/net/sf/yacas/InfixPrinter.java JavaYacas/net/sf/yacas/LispEnvironment.java JavaYacas/net/sf/yacas/MathCommands.java JavaYacas/net/sf/yacas/YacasPatternPredicateBase.java JavaYacas/net/sf/yacas/PatternClass.java JavaYacas/net/sf/yacas/CYacas.java JavaYacas/net/sf/yacas/YacasConsole.java JavaYacas/net/sf/yacas/ConsoleApplet.java JavaYacas/net/sf/yacas/CDataReader.java JavaYacas/net/sf/yacas/HintItem.java JavaYacas/net/sf/yacas/HintWindow.java JavaYacas/net/sf/yacas/Hints.java JavaYacas/net/sf/yacas/SelectSymbol.java JavaYacas/net/sf/yacas/TResult.java JavaYacas/net/sf/yacas/YacasInterpreter.java JavaYacas/net/sf/yacas/YacasParamMatcherBase.java JavaYacas/net/sf/yacas/MatchSubList.java JavaYacas/net/sf/yacas/MatchNumber.java JavaYacas/net/sf/yacas/MatchAtom.java JavaYacas/net/sf/yacas/MatchVariable.java JavaYacas/net/sf/yacas/TWord.java JavaYacas/net/sf/yacas/YacasGraphicsContext.java JavaYacas/net/sf/yacas/Article.java JavaYacas/net/sf/yacas/DatahubApplet.java JavaYacas/net/sf/yacas/XmlTokenizer.java JavaYacas/net/sf/yacas/SubstBehaviourBase.java JavaYacas/net/sf/yacas/SubstBehaviour.java JavaYacas/net/sf/yacas/LocalSymbolBehaviour.java JavaYacas/net/sf/yacas/BackQuoteBehaviour.java JavaYacas/net/sf/yacas/YacasException.java ${YACAS_SCRIPTS})
endif ()

if (WIN32)
  install (FILES /usr/lib/gcc/i686-w64-mingw32/4.6/libgcc_s_sjlj-1.dll DESTINATION bin COMPONENT app)
endif ()

set (CPACK_DEB_COMPONENT_INSTALL on)
set (CPACK_COMPONENTS_IGNORE_GROUPS 1)
set (CPACK_COMPONENTS_ALL app doc dev)
set (CPACK_COMPONENTS_GROUPING ONE_PER_GROUP)

if (UNIX)
  set (CPACK_GENERATOR DEB)
  set (CPACK_DEBIAN_PACKAGE_ARCHITECTURE "amd64")
else ()
  set (CPACK_GENERATOR NSIS)
  set (CPACK_NSIS_MODIFY_PATH "ON")
  set (CPACK_NSIS_ENABLE_UNINSTALL_BEFORE_INSTALL "ON")
  set (CPACK_NSIS_HELP_LINK "http://yacas.sourceforge.net/")
  set (CPACK_NSIS_URL_INFO_ABOUT "http://yacas.sourceforge.net/")
  set (CPACK_NSIS_MUI_ICON "${PROJECT_SOURCE_DIR}/res/yacas.ico")
  set (CPACK_NSIS_INSTALLED_ICON_NAME "bin\\\\yacas.exe")
  set (CPACK_CREATE_DESKTOP_LINKS "yacas.exe" "yacas")
endif ()

set (CPACK_PACKAGE_NAME "yacas")
set (CPACK_PACKAGE_VENDOR "yacas")
set (CPACK_PACKAGE_CONTACT "yacas-devel@lists.sourceforge.net")
set (CPACK_PACKAGE_DESCRIPTION_SUMMARY "YACAS is an easy to use, general purpose computer algebra system.")
set (CPACK_PACKAGE_DESCRIPTION_FILE "${PROJECT_SOURCE_DIR}/README")
set (CPACK_RESOURCE_FILE_LICENSE "${PROJECT_SOURCE_DIR}/COPYING")
set (CPACK_RESOURCE_FILE_README "${PROJECT_SOURCE_DIR}/README")
set (CPACK_PACKAGE_VERSION ${YACAS_VERSION})
set (CPACK_PACKAGE_VERSION_MAJOR ${YACAS_VERSION_MAJOR})
set (CPACK_PACKAGE_VERSION_MINOR ${YACAS_VERSION_MINOR})
set (CPACK_PACKAGE_VERSION_PATCH ${YACAS_VERSION_MICRO})
set (CPACK_PACKAGE_INSTALL_DIRECTORY "yacas")
set (CPACK_PACKAGE_EXECUTABLES yacas;Yacas)

set(CPACK_COMPONENT_APP_DISPLAY_NAME "Yacas")
set(CPACK_COMPONENT_DOC_DISPLAY_NAME "Documentation")
set(CPACK_COMPONENT_DEV_DISPLAY_NAME "Development")

include (CPack)
