cmake_minimum_required (VERSION 2.6)

project (YACAS)

set (YACAS_VERSION_MAJOR 1)
set (YACAS_VERSION_MINOR 3)
set (YACAS_VERSION_MICRO 3)

set (YACAS_VERSION ${YACAS_VERSION_MAJOR}.${YACAS_VERSION_MINOR}.${YACAS_VERSION_MICRO})

include (${CMAKE_ROOT}/Modules/CheckIncludeFiles.cmake)

check_include_files (unistd.h HAVE_UNISTD_H)
check_include_files (sys/ioctl.h HAVE_SYS_IOCTL_H)
check_include_files (sys/socket.h HAVE_SYS_SOCKET_H)
check_include_files (sys/time.h HAVE_SYS_TIME_H)
check_include_files (sys/types.h HAVE_SYS_TYPES_H)
check_include_files (malloc.h HAVE_MALLOC_H)
check_include_files (memory.h HAVE_MEMORY_H)
check_include_files (stdlib.h HAVE_STDLIB_H)
check_include_files (stdio.h HAVE_STDIO_H)
check_include_files (ctype.h HAVE_CTYPE_H)
check_include_files (dlfcn.h HAVE_DLFCN_H)
check_include_files (dl.h HAVE_DL_H)
check_include_files (dld.h HAVE_DLD_H)
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
  "${PROJECT_SOURCE_DIR}/version.h.in"
  "${PROJECT_BINARY_DIR}/version.h"
  )

add_definitions(-DHAVE_CONFIG_H)

include_directories(
  "${PROJECT_SOURCE_DIR}"
  "${PROJECT_BINARY_DIR}"
  )

add_executable (mkfastprimes src/mkfastprimes.cpp)

add_custom_command(OUTPUT fastprimes.c
                   COMMAND mkfastprimes
                   ARGS > fastprimes.c
                   DEPENDS mkfastprimes
                   COMMENT "generating prime numbers"
                   VERBATIM)

set (YACAS_COMMON_SOURCES src/yacasmain.cpp src/stdcommandline.cpp src/deffile.cpp src/infixparser.cpp src/lispatom.cpp src/lispcleanupstack.cpp src/lispenvironment.cpp src/lispeval.cpp src/lisperror.cpp src/lispio.cpp src/lispobject.cpp src/lispparser.cpp src/lispuserfunc.cpp src/mathcommands.cpp src/mathenvironment.cpp src/mathuserfunc.cpp src/standard.cpp src/ src/stdfileio.cpp src/arggetter.cpp src/stringio.cpp src/tokenizer.cpp src/yacasapi.cpp src/genericobject.cpp src/arrayclass.cpp src/lispevalhash.cpp src/patterns.cpp src/patternclass.cpp src/substitute.cpp src/mathcommands2.cpp src/mathcommands3.cpp src/errors.cpp src/patcher.cpp src/genericstructs.cpp src/xmltokenizer.cpp src/archiver.cpp src/compressedfiles.cpp src/commandline.cpp src/anumber.cpp src/yacasnumbers.cpp src/numbers.cpp src/platmath.cpp src/grower.cpp src/stdstubs.cpp src/obmalloc.cpp src/lisphash.cpp src/lispstring.cpp src/mathutil.cpp src/yacasbase.cpp src/debugmem.cpp fastprimes.c)

set (YACAS_UNIX_SOURCES src/unixcommandline.cpp)
set (YACAS_WIN32_SOURCES src/win32commandline.cpp)

if (UNIX)
  set (YACAS_SOURCES ${YACAS_COMMON_SOURCES} ${YACAS_UNIX_SOURCES})
else ()
  set (YACAS_SOURCES ${YACAS_COMMON_SOURCES} ${YACAS_WIN32_SOURCES})
endif ()

add_executable (yacas ${YACAS_SOURCES})
target_link_libraries(yacas lzo2)

add_custom_command(OUTPUT core_yacasmain.h
                   COMMAND grep 
                   ARGS ^CORE_KERNEL_FUNCTION ${PROJECT_SOURCE_DIR}/src/yacasmain.cpp > core_yacasmain.h
                   DEPENDS yacas
                   COMMENT "generating core functions list"
                   VERBATIM)

set_property(SOURCE src/gencorefunctions.cpp APPEND PROPERTY OBJECT_DEPENDS core_yacasmain.h)
add_executable (gencorefunctions src/gencorefunctions.cpp)

add_executable (codereviewer src/codereviewer.cpp)

add_executable (compressor src/compressor.cpp src/filescanner.cpp)
target_link_libraries(compressor lzo2)

add_executable (autocompleter docs/autocompleter.cpp)
add_executable (riptestfromyart docs/riptestfromyart.cpp)

add_custom_command(OUTPUT scripts.dat
                   COMMAND compressor 
                   ARGS - ${PROJECT_SOURCE_DIR}/scripts/ scripts.dat
                   DEPENDS compressor
                   COMMENT "generating scripts archive"
                   VERBATIM)

add_custom_target(archive ALL DEPENDS ${PROJECT_BINARY_DIR}/scripts.dat)

add_executable (manripper manmake/manripper.cpp)
add_executable (removeduplicates manmake/removeduplicates.cpp)

install (TARGETS yacas DESTINATION bin)
install (FILES ${PROJECT_BINARY_DIR}/scripts.dat DESTINATION lib)
install (DIRECTORY scripts/ DESTINATION share/yacas)
install (DIRECTORY docs/ DESTINATION share/yacas/documentation FILES_MATCHING PATTERN "*.html")
install (DIRECTORY docs/ DESTINATION share/yacas/documentation FILES_MATCHING PATTERN "*.gif")
install (DIRECTORY docs/ DESTINATION share/yacas/documentation FILES_MATCHING PATTERN "*.css")
install (DIRECTORY docs/ DESTINATION share/yacas/documentation FILES_MATCHING PATTERN "*.js")
