cmake_minimum_required (VERSION 2.8.7)

option (ENABLE_DOCS "generate documentation" OFF)
option (ENABLE_JYACAS "build the Java yacas engine" OFF)

if (APPLE)
#  if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    SET(CMAKE_INSTALL_PREFIX "/usr/" CACHE PATH "Yacas install prefix" FORCE)
#  endif(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)

  set(CMAKE_MACOSX_RPATH 1)
  set(CMAKE_FRAMEWORK_INSTALL_DIR "/Library/Frameworks" CACHE STRING "Directory to install frameworks to.")
endif()

if (ENABLE_JYACAS)
  find_package (Java)
  include (${CMAKE_ROOT}/Modules/UseJava.cmake)
  project (YACAS CXX Java)
else ()
  project (YACAS CXX)
endif ()


if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pedantic -Wall -std=c++11")
elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "MSVC")
    add_definitions(-DYACAS_NO_CONSTEXPR)
endif ()

set (YACAS_VERSION_MAJOR 1)
set (YACAS_VERSION_MINOR 3)
set (YACAS_VERSION_MICRO 5)

set (YACAS_VERSION ${YACAS_VERSION_MAJOR}.${YACAS_VERSION_MINOR}.${YACAS_VERSION_MICRO})

include (CMakeLists-conf.txt)

include_directories(
  "${PROJECT_SOURCE_DIR}/include"
  "${PROJECT_BINARY_DIR}/include"
  "${PROJECT_BINARY_DIR}"
  )

set (YACAS_LIB_SOURCES src/stdcommandline.cpp src/deffile.cpp src/infixparser.cpp src/lispatom.cpp src/lispcleanupstack.cpp src/lispenvironment.cpp src/lispeval.cpp src/lisperror.cpp src/lispio.cpp src/lispobject.cpp src/lispparser.cpp src/lispuserfunc.cpp src/mathcommands.cpp src/mathuserfunc.cpp src/standard.cpp src/stdfileio.cpp src/arggetter.cpp src/stringio.cpp src/tokenizer.cpp src/yacasapi.cpp src/lispevalhash.cpp src/patterns.cpp src/patternclass.cpp src/substitute.cpp src/mathcommands2.cpp src/mathcommands3.cpp src/errors.cpp src/patcher.cpp src/genericstructs.cpp src/xmltokenizer.cpp src/commandline.cpp src/anumber.cpp src/yacasnumbers.cpp src/numbers.cpp src/platmath.cpp src/stdstubs.cpp src/obmalloc.cpp src/lisphash.cpp src/yacasbase.cpp src/debugmem.cpp)

set (YACAS_COMMON_SOURCES src/yacasmain.cpp)

set (YACAS_UNIX_SOURCES src/unixcommandline.cpp)
set (YACAS_WIN32_SOURCES src/win32commandline.cpp res/yacas.rc)

if (UNIX)
  set (YACAS_SOURCES ${YACAS_COMMON_SOURCES} ${YACAS_UNIX_SOURCES})
else ()
  set (YACAS_SOURCES ${YACAS_COMMON_SOURCES} ${YACAS_WIN32_SOURCES})
endif ()

set (YACAS_STATIC_SCRIPTS scripts/standard.ys scripts/stdarith.ys scripts/stdopers.ys scripts/yacasinit.ys scripts/standard.ys.def scripts/stdarith.ys.def scripts/array.rep/code.ys scripts/assoc.rep/code.ys scripts/base.rep/math.ys scripts/c_form.rep/code.ys scripts/calendar.rep/code.ys scripts/complex.rep/code.ys scripts/complex.rep/om.ys scripts/constants.rep/code.ys scripts/constants.rep/om.ys scripts/controlflow.rep/code.ys scripts/debug.rep/code.ys scripts/deffunc.rep/code.ys scripts/deriv.rep/code.ys scripts/example.rep/code.ys scripts/factors.rep/binaryfactors.ys scripts/factors.rep/code.ys scripts/functional.rep/code.ys scripts/functional.rep/om.ys scripts/graph.rep/code.ys scripts/html.rep/code.ys scripts/integrate.rep/code.ys scripts/integrate.rep/om.ys scripts/io.rep/code.ys scripts/io.rep/defaultprint.ys scripts/io.rep/errors.ys scripts/io.rep/formula.ys scripts/io.rep/print.ys scripts/limit.rep/code.ys scripts/limit.rep/om.ys scripts/linalg.rep/code.ys scripts/lists.rep/code.ys scripts/lists.rep/scopestack.ys scripts/localrules.rep/code.ys scripts/logic.rep/code.ys scripts/logic.rep/om.ys scripts/multivar.rep/code.ys scripts/multivar.rep/makemulti.ys scripts/multivar.rep/sparsenomial.ys scripts/multivar.rep/sparsetree.ys scripts/newly.rep/code.ys scripts/numbers.rep/GaussianIntegers.ys scripts/numbers.rep/NumberTheory.ys scripts/numbers.rep/code.ys scripts/numbers.rep/nthroot.ys scripts/numbers.rep/om.ys scripts/odesolver.rep/code.ys scripts/openmath.rep/code.ys scripts/orthopoly.rep/code.ys scripts/padic.rep/code.ys scripts/patterns.rep/code.ys scripts/plots.rep/backends-2d.ys scripts/plots.rep/backends-3d.ys scripts/plots.rep/code.ys scripts/plots.rep/plot2d.ys scripts/plots.rep/plot3d.ys scripts/predicates.rep/code.ys scripts/probability.rep/code.ys scripts/pslq.rep/code.ys scripts/rabinmiller.rep/code.ys scripts/radsimp.rep/code.ys scripts/random.rep/code.ys scripts/simplify.rep/code.ys scripts/simplify.rep/factorial.ys scripts/solve.rep/code.ys scripts/specfunc.rep/bernou.ys scripts/specfunc.rep/bessel.ys scripts/specfunc.rep/code.ys scripts/specfunc.rep/gamma.ys scripts/specfunc.rep/gammaconst.ys scripts/specfunc.rep/om.ys scripts/specfunc.rep/zeta.ys scripts/statistics.rep/distributions.ys scripts/statistics.rep/hypothesystest.ys scripts/statistics.rep/incompletegamma.ys scripts/statistics.rep/randomtest.ys scripts/statistics.rep/regression.ys scripts/statistics.rep/statistics.ys scripts/stats.rep/code.ys scripts/stdfuncs.rep/code.ys scripts/stdfuncs.rep/elemfuncs.ys scripts/stdfuncs.rep/numerical.ys scripts/stdfuncs.rep/nummethods.ys scripts/stdfuncs.rep/om.ys scripts/stubs.rep/code.ys scripts/stubs.rep/om.ys scripts/substitute.rep/code.ys scripts/sums.rep/code.ys scripts/sums.rep/om.ys scripts/sums.rep/taylor.ys scripts/sums.rep/taylor3.ys scripts/tensor.rep/code.ys scripts/testers.rep/code.ys scripts/texform.rep/code.ys scripts/transforms.rep/code.ys scripts/trigsimp.rep/code.ys scripts/univar.rep/Cyclotomic.ys scripts/univar.rep/code.ys scripts/univar.rep/sparse.ys scripts/univar.rep/sturm.ys scripts/array.rep/code.ys.def scripts/assoc.rep/code.ys.def scripts/base.rep/math.ys.def scripts/c_form.rep/code.ys.def scripts/calendar.rep/code.ys.def scripts/complex.rep/code.ys.def scripts/constants.rep/code.ys.def scripts/controlflow.rep/code.ys.def scripts/debug.rep/code.ys.def scripts/deffunc.rep/code.ys.def scripts/deriv.rep/code.ys.def scripts/example.rep/code.ys.def scripts/factors.rep/binaryfactors.ys.def scripts/factors.rep/code.ys.def scripts/functional.rep/code.ys.def scripts/graph.rep/code.ys.def scripts/html.rep/code.ys.def scripts/integrate.rep/code.ys.def scripts/io.rep/code.ys.def scripts/io.rep/defaultprint.ys.def scripts/limit.rep/code.ys.def scripts/linalg.rep/code.ys.def scripts/lists.rep/code.ys.def scripts/lists.rep/scopestack.ys.def scripts/localrules.rep/code.ys.def scripts/logic.rep/code.ys.def scripts/multivar.rep/code.ys.def scripts/multivar.rep/sparsetree.ys.def scripts/newly.rep/code.ys.def scripts/numbers.rep/GaussianIntegers.ys.def scripts/numbers.rep/NumberTheory.ys.def scripts/numbers.rep/code.ys.def scripts/numbers.rep/nthroot.ys.def scripts/odesolver.rep/code.ys.def scripts/openmath.rep/code.ys.def scripts/orthopoly.rep/code.ys.def scripts/padic.rep/code.ys.def scripts/patterns.rep/code.ys.def scripts/plots.rep/code.ys.def scripts/plots.rep/plot2d.ys.def scripts/plots.rep/plot3d.ys.def scripts/predicates.rep/code.ys.def scripts/probability.rep/code.ys.def scripts/pslq.rep/code.ys.def scripts/rabinmiller.rep/code.ys.def scripts/radsimp.rep/code.ys.def scripts/random.rep/code.ys.def scripts/simplify.rep/code.ys.def scripts/simplify.rep/factorial.ys.def scripts/solve.rep/code.ys.def scripts/specfunc.rep/bernou.ys.def scripts/specfunc.rep/bessel.ys.def scripts/specfunc.rep/code.ys.def scripts/specfunc.rep/gamma.ys.def scripts/specfunc.rep/gammaconst.ys.def scripts/specfunc.rep/zeta.ys.def scripts/statistics.rep/distributions.ys.def scripts/statistics.rep/hypothesystest.ys.def scripts/statistics.rep/incompletegamma.ys.def scripts/statistics.rep/regression.ys.def scripts/statistics.rep/statistics.ys.def scripts/stats.rep/code.ys.def scripts/stdfuncs.rep/code.ys.def scripts/stdfuncs.rep/elemfuncs.ys.def scripts/stdfuncs.rep/numerical.ys.def scripts/stdfuncs.rep/nummethods.ys.def scripts/stubs.rep/code.ys.def scripts/substitute.rep/code.ys.def scripts/sums.rep/code.ys.def scripts/sums.rep/taylor.ys.def scripts/sums.rep/taylor3.ys.def scripts/tensor.rep/code.ys.def scripts/testers.rep/code.ys.def scripts/texform.rep/code.ys.def scripts/transforms.rep/code.ys.def scripts/trigsimp.rep/code.ys.def scripts/univar.rep/Cyclotomic.ys.def scripts/univar.rep/code.ys.def scripts/univar.rep/sparse.ys.def scripts/univar.rep/sturm.ys.def )
set (YACAS_GENERATED_SCRIPTS ${PROJECT_BINARY_DIR}/scripts/corefunctions.ys ${PROJECT_BINARY_DIR}/scripts/packages.ys )

set (YACAS_SCRIPTS ${YACAS_STATIC_SCRIPTS} ${YACAS_GENERATED_SCRIPTS})

add_library (libyacas_static STATIC ${YACAS_LIB_SOURCES})
add_library (libyacas_shared SHARED ${YACAS_LIB_SOURCES})

if (WIN32)
  target_link_libraries (libyacas_shared pthread)
endif()

set_target_properties(libyacas_static PROPERTIES OUTPUT_NAME "yacas")
set_target_properties(libyacas_shared PROPERTIES OUTPUT_NAME "yacas" SOVERSION 1.0.0) 

#to generate yacas framework on macos:
if (APPLE)
  add_library (libyacas_framework SHARED ${YACAS_LIB_SOURCES})
  set_target_properties(libyacas_framework PROPERTIES OUTPUT_NAME "yacas" VERSION ${YACAS_VERSION} SOVERSION 1.0.0 FRAMEWORK ON)
  add_custom_command(TARGET libyacas_framework POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${PROJECT_SOURCE_DIR}/scripts $<TARGET_FILE_DIR:libyacas_framework>/Resources/scripts)
  add_custom_command(TARGET libyacas_framework POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${PROJECT_BINARY_DIR}/scripts $<TARGET_FILE_DIR:libyacas_framework>/Resources/scripts)
  add_custom_command(TARGET libyacas_framework POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${PROJECT_SOURCE_DIR}/include $<TARGET_FILE_DIR:libyacas_framework>/Headers)
  add_custom_command(TARGET libyacas_framework POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${PROJECT_BINARY_DIR}/include $<TARGET_FILE_DIR:libyacas_framework>/Headers)
  add_custom_command(TARGET libyacas_framework POST_BUILD COMMAND ln -sf  $<TARGET_FILE_DIR:libyacas_framework>/Headers $<TARGET_FILE_DIR:libyacas_framework>/../../Headers)

  install (TARGETS libyacas_framework FRAMEWORK DESTINATION ${CMAKE_FRAMEWORK_INSTALL_DIR} COMPONENT dev)
endif()

install (TARGETS libyacas_static ARCHIVE DESTINATION lib COMPONENT dev)

if (APPLE)
  install (TARGETS libyacas_shared LIBRARY DESTINATION lib COMPONENT app)
else()
  install (TARGETS libyacas_shared LIBRARY DESTINATION lib ARCHIVE DESTINATION lib RUNTIME DESTINATION bin COMPONENT app)
endif()


add_executable (yacas ${YACAS_SOURCES})

if (APPLE)
    set_target_properties(yacas PROPERTIES INSTALL_RPATH "@loader_path/../lib")
endif()

target_link_libraries (yacas libyacas_shared)

if (WIN32)
  target_link_libraries (yacas wsock32 ws2_32 shlwapi)
endif()

add_executable (gencorefunctions src/gencorefunctions.cpp)

add_executable (autocompleter docs/autocompleter.cpp)
add_executable (riptestfromyart docs/riptestfromyart.cpp)

add_executable (manripper manmake/manripper.cpp)
add_executable (removeduplicates manmake/removeduplicates.cpp)

if (CMAKE_CROSSCOMPILING)
  set (YACAS ${YACAS_NATIVE_BUILD}/yacas)

  add_custom_command (
    OUTPUT ${PROJECT_BINARY_DIR}/scripts/corefunctions.ys
    COMMAND ${YACAS_NATIVE_BUILD}/gencorefunctions
    ARGS > ${PROJECT_BINARY_DIR}/scripts/corefunctions.ys
    DEPENDS ${YACAS_NATIVE_BUILD}/gencorefunctions)
else ()
  set (YACAS ${PROJECT_BINARY_DIR}/yacas)

  add_custom_command (
    OUTPUT ${PROJECT_BINARY_DIR}/scripts/corefunctions.ys
    COMMAND ${PROJECT_BINARY_DIR}/gencorefunctions
    ARGS > ${PROJECT_BINARY_DIR}/scripts/corefunctions.ys
    DEPENDS ${PROJECT_BINARY_DIR}/gencorefunctions)
endif ()

if (ENABLE_DOCS)
    include (CMakeLists-docs.txt)
endif()

foreach (_script ${YACAS_SCRIPTS})
  if (${_script} MATCHES ".+def$")
    string (REPLACE scripts/ "" _script ${_script})
    string (REPLACE .def "" _script ${_script})
    list (APPEND YACAS_PACKAGES_LIST ${_script})
  endif ()
endforeach ()

list (SORT YACAS_PACKAGES_LIST)
list (REMOVE_DUPLICATES YACAS_PACKAGES_LIST)

set (YACAS_PACKAGES "Defun(DefFileList,{}) {\n")
foreach (_package ${YACAS_PACKAGES_LIST})
  set (YACAS_PACKAGES "${YACAS_PACKAGES}    \"${_package}\",\n")
endforeach ()
set (YACAS_PACKAGES "${YACAS_PACKAGES}};\n")

configure_file(
  "${PROJECT_SOURCE_DIR}/packages.ys.in"
  "${PROJECT_BINARY_DIR}/scripts/packages.ys")

install (TARGETS yacas RUNTIME DESTINATION bin COMPONENT app)
install (DIRECTORY ${PROJECT_BINARY_DIR}/scripts/ DESTINATION share/yacas/scripts COMPONENT app)
install (DIRECTORY scripts/ DESTINATION share/yacas/scripts COMPONENT app)
install (DIRECTORY include/ DESTINATION include COMPONENT dev)
install (FILES ${PROJECT_BINARY_DIR}/include/yacas/yacas_version.h DESTINATION include/yacas COMPONENT dev)

include (CMakeLists-tests.txt)

if (ENABLE_JYACAS)
    include (CMakeLists-jyacas.txt)
endif ()

include (CMakeLists-packages.txt)
