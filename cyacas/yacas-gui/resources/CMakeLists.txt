include (${CMAKE_ROOT}/Modules/ExternalProject.cmake)

ExternalProject_Add (yacas-gui_codemirror
    PREFIX external_packages
    URL https://github.com/codemirror/CodeMirror/archive/5.23.0.zip
    CONFIGURE_COMMAND ""
    BUILD_IN_SOURCE 1
    BUILD_COMMAND npm install --no-progress
    INSTALL_COMMAND "")

ExternalProject_Add (yacas-gui_flot
    PREFIX external_packages
    URL https://github.com/flot/flot/archive/v0.8.3.zip
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND "")

ExternalProject_Add (yacas-gui_three
    PREFIX external_packages
    URL https://github.com/mrdoob/three.js/archive/r84.zip
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND "")

ExternalProject_Add (yacas-gui_vis
    PREFIX external_packages
    URL https://github.com/almende/vis/archive/v4.18.1.zip
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND "")

ExternalProject_Add (yacas-gui_mathjax
    PREFIX external_packages
    URL https://github.com/mathjax/MathJax/archive/2.7.0.zip
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND "")

ExternalProject_Add (yacas-gui_delanuay
    PREFIX external_packages
    URL https://github.com/ironwallaby/delaunay/archive/1.0.1.zip
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND "")

install (FILES ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_delanuay/delaunay.js DESTINATION share/yacas/resources/plot3d)

if (NOT APPLE)
    install (FILES plot3d/plot3d.js DESTINATION share/yacas/resources/plot3d)
    install (DIRECTORY jquery DESTINATION share/yacas/resources)
    install (DIRECTORY webchannel DESTINATION share/yacas/resources)
    install (DIRECTORY mathbar DESTINATION share/yacas/resources)
    install (DIRECTORY yacas_gui DESTINATION share/yacas/resources)
    install (FILES yacas_gui.html DESTINATION share/yacas/resources)
    install (DIRECTORY pixmaps DESTINATION share)
    install (DIRECTORY icons DESTINATION share)

    install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_codemirror/lib DESTINATION share/yacas/resources/codemirror)
    install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_codemirror/mode/yacas DESTINATION share/yacas/resources/codemirror/mode)
    install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_codemirror/addon/edit DESTINATION share/yacas/resources/codemirror/addon)
    install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_codemirror/addon/hint DESTINATION share/yacas/resources/codemirror/addon)

    install (FILES ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_flot/jquery.flot.js ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_flot/jquery.flot.resize.js DESTINATION share/yacas/resources/flot)

    install (FILES ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_three/build/three.min.js ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_three/examples/js/Detector.js ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_three/examples/js/controls/TrackballControls.js ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_three/examples/js/renderers/CanvasRenderer.js ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_three/examples/js/renderers/Projector.js DESTINATION share/yacas/resources/three)

    install (FILES ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_vis/dist/vis.min.css ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_vis/dist/vis.min.js DESTINATION share/yacas/resources/vis)

    install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_mathjax/fonts/HTML-CSS/STIX-Web/otf DESTINATION share/yacas/resources/mathjax/fonts/HTML-CSS/STIX-Web)
    install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_mathjax/fonts/HTML-CSS/STIX-Web/woff DESTINATION share/yacas/resources/mathjax/fonts/HTML-CSS/STIX-Web)
    install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_mathjax/fonts/HTML-CSS/TeX/otf DESTINATION share/yacas/resources/mathjax/fonts/HTML-CSS/TeX)
    install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_mathjax/unpacked DESTINATION share/yacas/resources/mathjax)
endif ()
