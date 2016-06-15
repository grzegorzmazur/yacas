set (CPACK_PACKAGE_NAME "yacas")
set (CPACK_PACKAGE_VERSION ${YACAS_VERSION})
set (CPACK_PACKAGE_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set (CPACK_PACKAGE_VERSION_MINOR ${PROJECT_VERSION_MINOR})
set (CPACK_PACKAGE_VERSION_PATCH ${PROJECT_VERSION_PATCH})
set (CPACK_PACKAGE_VENDOR "Yacas Team")
set (CPACK_PACKAGE_CONTACT "Yacas Team <yacas@googlegroups.com>")
set (CPACK_PACKAGE_DESCRIPTION_SUMMARY "Yacas is an easy to use, general purpose computer algebra system.")
set (CPACK_PACKAGE_DESCRIPTION_FILE "${PROJECT_SOURCE_DIR}/README.rst")
set (CPACK_PACKAGE_DESCRIPTION "Yet Another Computer Algebra System
 Yacas is an easy to use, general purpose Computer Algebra System, a program
 for symbolic manipulation of mathematical expressions. It uses its own
 programming language designed for symbolic as well as arbitrary-precision
 numerical computations. The system has a library of scripts that implement
 many of the symbolic algebra operations; new algorithms can be easily added
 to the library. Yacas comes with extensive documentation (hundreds of pages)
 covering the scripting language, the functionality that is already implemented
 in the system, and the algorithms we used.")
set (CPACK_PACKAGE_INSTALL_DIRECTORY "yacas")
set (CPACK_PACKAGE_EXECUTABLES yacas;Yacas)
set (CPACK_STRIP_FILES TRUE)

set (CPACK_SOURCE_GENERATOR "TGZ")
set (CPACK_SOURCE_IGNORE_FILES "/build/;/.svn/;/.git/;/install/;/nbproject/;~$;${CPACK_SOURCE_IGNORE_FILES}")
set (CPACK_SOURCE_PACKAGE_FILE_NAME "yacas_${PROJECT_VERSION}")

set (CPACK_COMPONENTS_IGNORE_GROUPS 1)
set (CPACK_COMPONENTS_GROUPING ONE_PER_GROUP)

set (CPACK_COMPONENTS_ALL app doc dev)
if (APPLE)
    set (CPACK_COMPONENTS_ALL ${CPACK_COMPONENTS_ALL} framework)
    set (CPACK_SET_DESTDIR TRUE) #framework goes to /Library/Frameworks without Access Denied during compilation, 
endif()

set(CPACK_COMPONENT_APP_DISPLAY_NAME "Yacas")
set(CPACK_COMPONENT_DOC_DISPLAY_NAME "Documentation")
set(CPACK_COMPONENT_DEV_DISPLAY_NAME "Development")
set(CPACK_COMPONENT_FRAMEWORK_DISPLAY_NAME "Framework")
set(CPACK_COMPONENT_FRAMEWORK_DISABLED 1)
set(CPACK_COMPONENT_FRAMEWORK_DESCRIPTION "Yacas.framework wil be installed into /Library/Frameworks")


if(APPLE)
    set (CPACK_GENERATOR "PackageMaker")
    configure_file(
        "${PROJECT_SOURCE_DIR}/COPYING"
        "${PROJECT_BINARY_DIR}/COPYING.txt"
        @ONLY)
    configure_file(
        "${PROJECT_SOURCE_DIR}/README.rst"
        "${PROJECT_BINARY_DIR}/README.txt"
        @ONLY)
    set (CPACK_RESOURCE_FILE_LICENSE "${PROJECT_BINARY_DIR}/COPYING.txt")
    set (CPACK_RESOURCE_FILE_README "${PROJECT_BINARY_DIR}/README.txt")
else ()
    set (CPACK_RESOURCE_FILE_LICENSE "${PROJECT_SOURCE_DIR}/COPYING")
    set (CPACK_RESOURCE_FILE_README "${PROJECT_SOURCE_DIR}/README.rst")
endif ()

if (WIN32 AND CMAKE_CROSSCOMPILING)
    if (CMAKE_SIZEOF_VOID_P EQUAL 8)
        exec_program (${CMAKE_CXX_COMPILER} ARGS -print-file-name=libgcc_s_seh-1.dll OUTPUT_VARIABLE SJLJ_PATH)
    else ()
        exec_program (${CMAKE_CXX_COMPILER} ARGS -print-file-name=libgcc_s_sjlj-1.dll OUTPUT_VARIABLE SJLJ_PATH)
    endif ()
    exec_program (${CMAKE_CXX_COMPILER} ARGS -print-file-name=libwinpthread-1.dll OUTPUT_VARIABLE WINPTHREAD_PATH)
    install (FILES ${SJLJ_PATH} ${WINPTHREAD_PATH} DESTINATION bin COMPONENT app)
endif ()

if (WIN32 AND NOT UNIX)
    set (CPACK_GENERATOR NSIS)
    set (CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_SKIP 1)
    include (InstallRequiredSystemLibraries)
    install (PROGRAMS ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS} DESTINATION bin COMPONENT app)
    if(CMAKE_CL_64)
        set (CPACK_NSIS_EXTRA_PREINSTALL_COMMANDS  "SetRegView 64")
        set (CPACK_NSIS_INSTALL_ROOT "$PROGRAMFILES64")
    endif ()
    set (CPACK_NSIS_MODIFY_PATH "ON")
    set (CPACK_NSIS_ENABLE_UNINSTALL_BEFORE_INSTALL "ON")
    set (CPACK_NSIS_HELP_LINK "http://www.yacas.org/")
    set (CPACK_NSIS_URL_INFO_ABOUT "http://www.yacas.org/")
    set (CPACK_NSIS_MUI_ICON "${PROJECT_SOURCE_DIR}/cyacas/yacas/res/yacas.ico")
    set (CPACK_NSIS_INSTALLED_ICON_NAME "bin\\\\yacas.exe")
    set (CPACK_CREATE_DESKTOP_LINKS "yacas.exe" "yacas")
endif ()

if (WIN32 OR APPLE)
    include (CPack)
endif ()
