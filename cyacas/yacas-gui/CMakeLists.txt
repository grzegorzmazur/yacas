set (CMAKE_INCLUDE_CURRENT_DIR ON)
set (CMAKE_AUTOMOC ON)

find_package (Qt5Core REQUIRED)
find_package (Qt5Widgets REQUIRED)
find_package (Qt5WebKit REQUIRED)
find_package (Qt5WebKitWidgets REQUIRED)
find_package (Qt5PrintSupport REQUIRED)

if (Qt5_POSITION_INDEPENDENT_CODE)
  set(CMAKE_POSITION_INDEPENDENT_CODE ON)
endif()

include_directories (include)

# seems to be required for older qt5 cmake modules
include_directories (
    ${Qt5Core_INCLUDE_DIRS}
    ${Qt5Widgets_INCLUDE_DIRS}
    ${Qt5WebKit_INCLUDE_DIRS}
    ${Qt5WebKitWidgets_INCLUDE_DIRS}
    ${Qt5PrintSupport_INCLUDE_DIRS}
)

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

add_executable (yacas-gui ${SOURCES} ${HEADERS} ${QRESOURCES})

target_link_libraries(yacas-gui Qt5::Core Qt5::Widgets Qt5::WebKit Qt5::WebKitWidgets Qt5::PrintSupport libyacas)

if (WIN32)
    target_link_libraries (yacas-gui shlwapi)
    set_target_properties(yacas-gui PROPERTIES WIN32_EXECUTABLE YES)
endif ()

set_property (TARGET yacas-gui APPEND PROPERTY
    COMPILE_DEFINITIONS $<$<CONFIG:Debug>:YAGY_ENABLE_INSPECTOR>
)

add_subdirectory (resources)

install (TARGETS yacas-gui DESTINATION bin)

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
