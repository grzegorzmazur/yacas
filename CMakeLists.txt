cmake_minimum_required (VERSION 3.0)

cmake_policy(SET CMP0048 NEW)

option (ENABLE_DOCS "generate documentation" OFF)
option (ENABLE_JYACAS "build the Java yacas engine" OFF)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

if (APPLE)
  set(CMAKE_MACOSX_RPATH 1)
  set(CMAKE_FRAMEWORK_INSTALL_DIR "/Library/Frameworks" CACHE STRING "Directory to install frameworks to.")
endif()

set (LANGUAGES CXX C)

if (ENABLE_JYACAS)
  find_package (Java)
  include (UseJava)
  set (LANGUAGES ${LANGUAGES} Java)
endif ()

project (YACAS VERSION 1.5.0 LANGUAGES ${LANGUAGES})

if (${CMAKE_CXX_COMPILER_ID} STREQUAL "GNU" OR ${CMAKE_CXX_COMPILER_ID} STREQUAL "Clang" OR ${CMAKE_CXX_COMPILER_ID} STREQUAL "AppleClang")
   set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pedantic -Wall -std=c++11")
elseif (${CMAKE_CXX_COMPILER_ID} STREQUAL "MSVC")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd4800")
    add_definitions(-DYACAS_NO_CONSTEXPR -DYACAS_NO_ATOMIC_TYPES -DYACAS_UINT32_T_IN_GLOBAL_NAMESPACE)
endif ()

if (${CMAKE_SYSTEM_NAME} STREQUAL "Emscripten")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -s DISABLE_EXCEPTION_CATCHING=0 -s ASSERTIONS=1")
    set (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} --embed-file ${PROJECT_SOURCE_DIR}/scripts@/share/yacas/scripts")
endif ()

add_subdirectory (config)

if (APPLE OR WIN32)
    add_definitions(-DNO_GLOBALS)
endif ()

include_directories(
  "${PROJECT_SOURCE_DIR}/include"
  "${PROJECT_BINARY_DIR}/include"
  "${PROJECT_BINARY_DIR}/include/yacas"
  )

set (YACAS_LIB_SOURCES
  src/associationclass.cpp
  src/stdcommandline.cpp
  src/deffile.cpp
  src/infixparser.cpp
  src/lispatom.cpp
  src/lispenvironment.cpp
  src/lispeval.cpp
  src/lisperror.cpp
  src/lispio.cpp
  src/lispobject.cpp
  src/lispparser.cpp
  src/lispuserfunc.cpp
  src/mathcommands.cpp
  src/mathuserfunc.cpp
  src/standard.cpp
  src/stdfileio.cpp
  src/arggetter.cpp
  src/stringio.cpp
  src/tokenizer.cpp
  src/yacasapi.cpp
  src/lispevalhash.cpp
  src/patterns.cpp
  src/patternclass.cpp
  src/substitute.cpp
  src/mathcommands2.cpp
  src/mathcommands3.cpp
  src/errors.cpp
  src/patcher.cpp
  src/xmltokenizer.cpp
  src/commandline.cpp
  src/anumber.cpp
  src/yacasnumbers.cpp
  src/numbers.cpp
  src/platmath.cpp
  src/stdstubs.cpp
  src/obmalloc.cpp
  src/lisphash.cpp)

set (YACAS_LIB_HEADERS
  include/yacas/anumber.h
  include/yacas/anumber.inl
  include/yacas/arggetter.h
  include/yacas/arrayclass.h
  include/yacas/associationclass.h
  include/yacas/commandline.h
  include/yacas/corefunctions.h
  include/yacas/core_yacasmain.h
  include/yacas/deffile.h
  include/yacas/errors.h
  include/yacas/evalfunc.h
  include/yacas/genericobject.h
  include/yacas/GPL_stuff.h
  include/yacas/infixparser.h
  include/yacas/lispatom.h
  include/yacas/lispenvironment.h
  include/yacas/lisperror.h
  include/yacas/lispeval.h
  include/yacas/lispevalhash.h
  include/yacas/lispglobals.h
  include/yacas/lisphash.h
  include/yacas/lispio.h
  include/yacas/lispobject.h
  include/yacas/lispoperator.h
  include/yacas/lispparser.h
  include/yacas/lispstring.h
  include/yacas/lisptype.h
  include/yacas/lispuserfunc.h
  include/yacas/mathcommands.h
  include/yacas/mathuserfunc.h
  include/yacas/noncopyable.h
  include/yacas/numbers.h
  include/yacas/patcher.h
  include/yacas/patternclass.h
  include/yacas/patterns.h
  include/yacas/platfileio.h
  include/yacas/platmath.h
  include/yacas/refcount.h
  include/yacas/standard.h
  include/yacas/standard.inl
  include/yacas/stdcommandline.h
  include/yacas/stringio.h
  include/yacas/string_utils.h
  include/yacas/stubs.h
  include/yacas/substitute.h
  include/yacas/tokenizer.h
  include/yacas/utf8/core.h
  include/yacas/utf8/checked.h
  include/yacas/utf8/unchecked.h
  include/yacas/utf8.h
  include/yacas/xmltokenizer.h
  include/yacas/yacas.h
  include/yacas/yacasprivate.h)

set (YACAS_COMMON_SOURCES src/yacasmain.cpp)

set (YACAS_UNIX_SOURCES src/unixcommandline.cpp)
set (YACAS_WIN32_SOURCES src/win32commandline.cpp res/yacas.rc)

set (YACAS_UNIX_HEADERS include/yacas/unixcommandline.h)
set (YACAS_WIN32_HEADERS include/yacas/win32commandline.h)

set (YACAS_JS_INTERFACE_SOURCES src/js_interface.cpp)

if (UNIX)
  set (YACAS_SOURCES ${YACAS_COMMON_SOURCES} ${YACAS_UNIX_SOURCES})
  set (YACAS_HEADERS ${YACAS_UNIX_HEADERS})
else ()
  set (YACAS_SOURCES ${YACAS_COMMON_SOURCES} ${YACAS_WIN32_SOURCES})
  set (YACAS_HEADERS ${YACAS_WIN32_HEADERS})
endif ()

if (${CMAKE_SYSTEM_NAME} STREQUAL "Emscripten")
  set (YACAS_SOURCES ${YACAS_SOURCES} ${YACAS_JS_INTERFACE_SOURCES})
endif ()

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

add_library (libyacas ${YACAS_LIB_SOURCES} ${YACAS_LIB_HEADERS})
set_target_properties (libyacas PROPERTIES OUTPUT_NAME "yacas")

#to generate yacas framework on macos:
if (APPLE)
  add_library (libyacas_framework SHARED ${YACAS_LIB_SOURCES})
  set_target_properties(libyacas_framework PROPERTIES OUTPUT_NAME "yacas" VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION} FRAMEWORK ON)
  add_custom_command(TARGET libyacas_framework POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${PROJECT_SOURCE_DIR}/scripts $<TARGET_FILE_DIR:libyacas_framework>/Resources/scripts)
  add_custom_command(TARGET libyacas_framework POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${PROJECT_BINARY_DIR}/scripts $<TARGET_FILE_DIR:libyacas_framework>/Resources/scripts)
  add_custom_command(TARGET libyacas_framework POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${PROJECT_SOURCE_DIR}/include $<TARGET_FILE_DIR:libyacas_framework>/Headers)
  add_custom_command(TARGET libyacas_framework POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${PROJECT_BINARY_DIR}/include $<TARGET_FILE_DIR:libyacas_framework>/Headers)
  add_custom_command(TARGET libyacas_framework POST_BUILD COMMAND ln -sf  $<TARGET_FILE_DIR:libyacas_framework>/Headers $<TARGET_FILE_DIR:libyacas_framework>/../../Headers)

  install (TARGETS libyacas_framework FRAMEWORK DESTINATION ${CMAKE_FRAMEWORK_INSTALL_DIR} COMPONENT framework)
endif()

install (TARGETS libyacas ARCHIVE DESTINATION lib RUNTIME DESTINATION bin COMPONENT app)

add_executable (yacas ${YACAS_SOURCES} ${YACAS_HEADERS})

if (APPLE)
    set_target_properties(yacas PROPERTIES INSTALL_RPATH "@loader_path/../lib")
endif()

target_link_libraries (yacas libyacas)

if (WIN32)
  target_link_libraries (yacas wsock32 ws2_32 shlwapi)
endif()

if (ENABLE_DOCS)
  add_subdirectory (docs)
endif()

install (TARGETS yacas RUNTIME DESTINATION bin COMPONENT app)
install (DIRECTORY scripts/ DESTINATION share/yacas/scripts COMPONENT app)
install (DIRECTORY include/ DESTINATION include COMPONENT dev)
install (FILES ${PROJECT_BINARY_DIR}/include/yacas/yacas_version.h DESTINATION include/yacas COMPONENT dev)
install (FILES ${PROJECT_BINARY_DIR}/include/yacas/yacas_config.h DESTINATION include/yacas COMPONENT dev)

if (ENABLE_JYACAS)
    add_subdirectory (JavaYacas)
endif ()

include (CTest)

add_subdirectory (tests)

add_subdirectory (packaging)
