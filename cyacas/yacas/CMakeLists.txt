include_directories(
  "include"
  )


set (YACAS_COMMON_SOURCES src/yacasmain.cpp src/commandline.cpp src/stdcommandline.cpp)
set (YACAS_COMMON_HEADERS include/commandline.h include/stdcommandline.h)

set (YACAS_UNIX_SOURCES src/unixcommandline.cpp)
set (YACAS_WIN32_SOURCES src/win32commandline.cpp res/yacas.rc)

set (YACAS_UNIX_HEADERS include/unixcommandline.h)
set (YACAS_WIN32_HEADERS include/win32commandline.h)

if (UNIX)
  set (YACAS_SOURCES ${YACAS_COMMON_SOURCES} ${YACAS_UNIX_SOURCES})
  set (YACAS_HEADERS ${YACAS_COMMON_HEADERS} ${YACAS_UNIX_HEADERS})
else ()
  set (YACAS_SOURCES ${YACAS_COMMON_SOURCES} ${YACAS_WIN32_SOURCES})
  set (YACAS_HEADERS ${YACAS_COMMON_HEADERS} ${YACAS_WIN32_HEADERS})
endif ()

add_executable (yacas ${YACAS_SOURCES} ${YACAS_HEADERS})

if (APPLE)
    set_target_properties(yacas PROPERTIES INSTALL_RPATH "@loader_path/../lib")
endif()

target_link_libraries (yacas libyacas)

if (WIN32)
  target_link_libraries (yacas wsock32 ws2_32 shlwapi)
endif()

install (TARGETS yacas RUNTIME DESTINATION bin COMPONENT app)

#include (CTest)
#
#add_subdirectory (tests)
#
#add_subdirectory (packaging)
