set (CMAKE_INCLUDE_CURRENT_DIR ON)
set (CMAKE_AUTOMOC ON)


if (APPLE)
    set(OS_BUNDLE MACOSX_BUNDLE)
elseif(WIN32)
    set(OS_BUNDLE WIN32)
endif()

find_package (Qt5Core REQUIRED)
find_package (Qt5Widgets REQUIRED)
find_package (Qt5WebEngine REQUIRED)
find_package (Qt5WebEngineWidgets REQUIRED)
find_package (Qt5Svg REQUIRED)
find_package (Qt5PrintSupport REQUIRED)

if (Qt5_POSITION_INDEPENDENT_CODE)
  set(CMAKE_POSITION_INDEPENDENT_CODE ON)
endif()

include_directories (include)

qt5_wrap_ui (ui_mainwindow.h ui/mainwindow.ui)
qt5_wrap_ui (ui_preferences_dialog.h ui/preferences_dialog.ui)

qt5_add_resources (qrc_img.cpp img.qrc OPTIONS --compress 9)

set (QRESOURCES qrc_img.cpp)

set (SOURCES
    src/main.cpp
    src/mainwindow.cpp
    src/cellproxy.cpp
    src/yacasrequest.cpp
    src/yacasengine.cpp
    src/yacasserver.cpp
    src/preferences_dialog.cpp
    src/preferences.cpp
)

set (HEADERS
    include/cellproxy.h
    include/preferences_dialog.h
    include/yacasengine.h
    include/yacasserver.h
    include/mainwindow.h
    include/preferences.h
    include/yacasrequest.h
    ui_mainwindow.h
    ui_preferences_dialog.h
)

if (WIN32)
    set (SOURCES ${SOURCES} winres/yacas_gui.rc)
endif ()

if (APPLE)
    set(ICON_NAME yacas)

    set(OSX_ICON_FILES ${ICON_NAME}.icns)
    set_source_files_properties(${OSX_ICON_FILES} PROPERTIES MACOSX_PACKAGE_LOCATION Resources)
    set(SOURCES ${SOURCES} ${OSX_ICON_FILES})

    set(MACOSX_BUNDLE_ICON_FILE ${ICON_NAME})
endif ()

add_executable (yacas-gui ${OS_BUNDLE} ${SOURCES} ${HEADERS} ${QRESOURCES})

target_link_libraries(yacas-gui Qt5::Core Qt5::Widgets Qt5::WebEngine Qt5::WebEngineWidgets Qt5::Svg Qt5::PrintSupport libyacas)

if (APPLE)
    add_dependencies (yacas-gui yacas-gui_codemirror yacas-gui_flot yacas-gui_vis yacas-gui_mathjax)
    # add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E remove_directory $<TARGET_FILE_DIR:yacas-gui>/../Frameworks/yacas.framework)
    # add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory $<TARGET_FILE_DIR:libyacas_framework>/../../.. $<TARGET_FILE_DIR:yacas-gui>/../Frameworks)
    # add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E remove_directory $<TARGET_FILE_DIR:yacas-gui>/../Frameworks/yacas_mp.framework)
    # add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory $<TARGET_FILE_DIR:libyacas_mp_framework>/../../.. $<TARGET_FILE_DIR:yacas-gui>/../Frameworks)
    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${PROJECT_SOURCE_DIR}/scripts $<TARGET_FILE_DIR:yacas-gui>/../Resources/scripts)
    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${_qt5Core_install_prefix}/bin/macdeployqt $<TARGET_FILE_DIR:yacas-gui>/../.. -always-overwrite)

    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/resources/webchannel $<TARGET_FILE_DIR:yacas-gui>/../Resources/webchannel)

    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/resources/jquery $<TARGET_FILE_DIR:yacas-gui>/../Resources/jquery)
    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/resources/mathbar $<TARGET_FILE_DIR:yacas-gui>/../Resources/mathbar)
    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/resources/yacas_gui $<TARGET_FILE_DIR:yacas-gui>/../Resources/yacas_gui)
    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/resources/yacas_gui.html $<TARGET_FILE_DIR:yacas-gui>/../Resources)

    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_BINARY_DIR}/resources/external_packages/src/yacas-gui_codemirror/lib $<TARGET_FILE_DIR:yacas-gui>/../Resources/codemirror/lib)
    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_BINARY_DIR}/resources/external_packages/src/yacas-gui_codemirror/mode/yacas $<TARGET_FILE_DIR:yacas-gui>/../Resources/codemirror/mode/yacas)
    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_BINARY_DIR}/resources/external_packages/src/yacas-gui_codemirror/addon/edit $<TARGET_FILE_DIR:yacas-gui>/../Resources/codemirror/addon/edit)
    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_BINARY_DIR}/resources/external_packages/src/yacas-gui_codemirror/addon/hint $<TARGET_FILE_DIR:yacas-gui>/../Resources/codemirror/addon/hint)

    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/resources/external_packages/src/yacas-gui_flot/jquery.flot.js $<TARGET_FILE_DIR:yacas-gui>/../Resources/flot/jquery.flot.js)
    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/resources/external_packages/src/yacas-gui_flot/jquery.flot.resize.js $<TARGET_FILE_DIR:yacas-gui>/../Resources/flot/jquery.flot.resize.js)

    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/third_party/plotly-1.49.0.min.js DESTINATION $<TARGET_FILE_DIR:yacas-gui>/../Resources/plotly-1.49.0.min.js)

    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/resources/external_packages/src/yacas-gui_vis/dist/vis.min.css $<TARGET_FILE_DIR:yacas-gui>/../Resources/vis/vis.min.css)
    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/resources/external_packages/src/yacas-gui_vis/dist/vis.min.js $<TARGET_FILE_DIR:yacas-gui>/../Resources/vis/vis.min.js)

    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_BINARY_DIR}/resources/external_packages/src/yacas-gui_mathjax/fonts/HTML-CSS/STIX-Web/otf $<TARGET_FILE_DIR:yacas-gui>/../Resources/mathjax/fonts/HTML-CSS/STIX-Web/otf)
    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_BINARY_DIR}/resources/external_packages/src/yacas-gui_mathjax/fonts/HTML-CSS/STIX-Web/woff $<TARGET_FILE_DIR:yacas-gui>/../Resources/mathjax/fonts/HTML-CSS/STIX-Web/woff)
    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_BINARY_DIR}/resources/external_packages/src/yacas-gui_mathjax/fonts/HTML-CSS/TeX/otf $<TARGET_FILE_DIR:yacas-gui>/../Resources/mathjax/fonts/HTML-CSS/TeX/otf)
    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_BINARY_DIR}/resources/external_packages/src/yacas-gui_mathjax/unpacked $<TARGET_FILE_DIR:yacas-gui>/../Resources/mathjax/unpacked)

    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/resources/external_packages/src/yacas-gui_delanuay/delaunay.js $<TARGET_FILE_DIR:yacas-gui>/../Resources/plot3d)
endif ()

if (WIN32)
    target_link_libraries (yacas-gui shlwapi)
endif ()

add_subdirectory (resources)

if (APPLE)
    install (TARGETS yacas-gui BUNDLE DESTINATION ${CMAKE_INSTALL_BUNDLE_PREFIX})
else ()
    install (TARGETS yacas-gui DESTINATION ${CMAKE_INSTALL_BINDIR})
endif ()

if (${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
    install (PROGRAMS yacas-gui.desktop PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/applications)
endif ()

if (WIN32)
    include(DeployQt)
    windeployqt(yacas-gui)
endif ()
