
if (APPLE)
  set(CMAKE_MACOSX_RPATH 1)
  set(CMAKE_INSTALL_FRAMEWORK_PREFIX "/Library/Frameworks" CACHE STRING "Directory to install frameworks to.")
  set(CMAKE_INSTALL_BUNDLE_PREFIX "/Applications" CACHE STRING "Directory to install application bundles to.")
endif()

if (CMAKE_CXX_COMPILER_ID STREQUAL GNU OR CMAKE_CXX_COMPILER_ID STREQUAL Clang OR CMAKE_CXX_COMPILER_ID STREQUAL AppleClang)
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pedantic -Wall -fPIC")
elseif (MSVC)
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd4800")
    add_definitions(-DYACAS_NO_CONSTEXPR -DYACAS_NO_ATOMIC_TYPES -DYACAS_UINT32_T_IN_GLOBAL_NAMESPACE)
endif ()

if (CMAKE_SYSTEM_NAME STREQUAL Emscripten)
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -s DISABLE_EXCEPTION_CATCHING=0 -s ASSERTIONS=1 -s EXTRA_EXPORTED_RUNTIME_METHODS=['cwrap'] -s ALLOW_MEMORY_GROWTH=1")
    set (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} --embed-file ${PROJECT_SOURCE_DIR}/scripts@/share/yacas/scripts")
endif ()

include(CheckIPOSupported)
check_ipo_supported(RESULT IPO_SUPPORTED)

add_library(coverage_config INTERFACE)

if (ENABLE_CODE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    # Add required flags (GCC & LLVM/Clang)
    target_compile_options(coverage_config INTERFACE
      -g         # generate debug info
      --coverage # sets all required flags
    )
    if (CMAKE_VERSION VERSION_GREATER_EQUAL 3.13)
        target_link_options(coverage_config INTERFACE --coverage)
    else ()
        target_link_libraries(coverage_config INTERFACE --coverage)
    endif ()
endif ()

add_subdirectory (libyacas_mp)
add_subdirectory (libyacas)

if (ENABLE_CYACAS_CONSOLE)
    add_subdirectory (yacas)
endif ()

if (ENABLE_CYACAS_GUI)
    add_subdirectory (yacas-gui)
endif ()

if (ENABLE_CYACAS_KERNEL)
    add_subdirectory (yacas-kernel)
endif ()

if (ENABLE_CYACAS_XEUS)
    add_subdirectory (xeus-yacas)
endif ()
