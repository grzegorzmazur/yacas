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

qt5_add_resources (qrc_img.cpp img.qrc --compress 9)

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
    set (SOURCES ${SOURCES} winres/yagy.rc)
endif ()

if (APPLE)
    set(ICON_NAME yacas)

    set(OSX_ICON_FILES ${ICON_NAME}.icns)
    set_source_files_properties(${OSX_ICON_FILES} PROPERTIES MACOSX_PACKAGE_LOCATION Resources)
    set(SOURCES ${SOURCES} ${OSX_ICON_FILES})

    set(MACOSX_BUNDLE_ICON_FILE ${ICON_NAME})
endif ()

add_executable (yacas-gui ${OS_BUNDLE} ${SOURCES} ${HEADERS} ${QRESOURCES})

target_link_libraries(yacas-gui Qt5::Core Qt5::Widgets Qt5::WebEngine Qt5::WebEngineWidgets Qt5::Svg Qt5::PrintSupport)

if (APPLE)
    target_link_libraries(yacas-gui libyacas_framework)
else ()
    target_link_libraries(yacas-gui libyacas)
endif ()

if (APPLE)
    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E remove_directory $<TARGET_FILE_DIR:yacas-gui>/../Frameworks/yacas.framework)
    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory $<TARGET_FILE_DIR:libyacas_framework>/../../.. $<TARGET_FILE_DIR:yacas-gui>/../Frameworks)
    add_custom_command (TARGET yacas-gui POST_BUILD COMMAND ${_qt5Core_install_prefix}/bin/macdeployqt $<TARGET_FILE_DIR:yacas-gui>/../..)
endif ()

if (WIN32)
    target_link_libraries (yacas-gui shlwapi)
endif ()

set_property (TARGET yacas-gui APPEND PROPERTY
    COMPILE_DEFINITIONS $<$<CONFIG:Debug>:YAGY_ENABLE_INSPECTOR>
)

add_subdirectory (resources)

if (APPLE)
    install (TARGETS yacas-gui BUNDLE DESTINATION ${CMAKE_INSTALL_BUNDLE_PREFIX})
else ()
    install (TARGETS yacas-gui DESTINATION bin)
endif ()

if (${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
    install (PROGRAMS yacas-gui.desktop PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ DESTINATION share/applications)
endif ()

if (WIN32)
    # FIXME: this is plain ugly, there must be a better way
    # install all the dlls we use
	
    get_target_property(QtCore_location Qt5::Core LOCATION)
    get_filename_component(QT_DLL_DIR ${QtCore_location} DIRECTORY)
	
    set (QT_DLLS Qt5WebKit.dll Qt5WebKitWidgets.dll Qt5WebChannel.dll icuin54.dll icuuc54.dll icudt54.dll Qt5Core.dll  Qt5Gui.dll Qt5Multimedia.dll Qt5MultimediaWidgets.dll Qt5Network.dll Qt5OpenGL.dll Qt5Positioning.dll Qt5PrintSupport.dll Qt5Qml.dll Qt5Quick.dll Qt5Sensors.dll Qt5Sql.dll Qt5Svg.dll Qt5Widgets.dll)
    
    foreach (_dll ${QT_DLLS})
        install (FILES "${QT_DLL_DIR}/${_dll}" DESTINATION bin)
    endforeach ()
    
    install (DIRECTORY "${QT_DLL_DIR}/../plugins/platforms" DESTINATION bin)
    install (DIRECTORY "${QT_DLL_DIR}/../plugins/imageformats" DESTINATION bin)
    install (DIRECTORY "${QT_DLL_DIR}/../plugins/iconengines" DESTINATION bin)
endif ()
