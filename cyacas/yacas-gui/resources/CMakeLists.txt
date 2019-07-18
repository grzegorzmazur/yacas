include (ExternalProject)

ExternalProject_Add (yacas-gui_codemirror
    PREFIX external_packages
    URL https://github.com/Dominator008/CodeMirror-minified/archive/5.47.0.zip
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND "")

ExternalProject_Add (yacas-gui_flot
    PREFIX external_packages
    URL https://github.com/flot/flot/archive/v0.8.3.zip
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND "")

ExternalProject_Add (yacas-gui_three
    PREFIX external_packages
    URL https://github.com/mrdoob/three.js/archive/r90.zip
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND "")

ExternalProject_Add (yacas-gui_vis
    PREFIX external_packages
    URL https://github.com/almende/vis/archive/v4.21.0.zip
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND "")

ExternalProject_Add (yacas-gui_mathjax
    PREFIX external_packages
    URL https://github.com/mathjax/MathJax/archive/2.7.3.zip
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND "")

ExternalProject_Add (yacas-gui_delanuay
    PREFIX external_packages
    URL https://github.com/ironwallaby/delaunay/archive/1.0.1.zip
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND "")

install (FILES ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_delanuay/delaunay.js DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/plot3d)

if (NOT APPLE)
    install (FILES plot3d/plot3d.js DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/plot3d)
    install (DIRECTORY jquery DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources)
    install (DIRECTORY webchannel DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources)
    install (DIRECTORY mathbar DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources)
    install (DIRECTORY yacas_gui DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources)
    install (FILES yacas_gui.html DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources)
    install (DIRECTORY pixmaps DESTINATION ${CMAKE_INSTALL_DATAROOTDIR})
    install (DIRECTORY icons DESTINATION ${CMAKE_INSTALL_DATAROOTDIR})

    install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_codemirror/lib DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/codemirror)
    install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_codemirror/mode/yacas DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/codemirror/mode)
    install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_codemirror/addon/edit DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/codemirror/addon)
    install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_codemirror/addon/hint DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/codemirror/addon)

    install (FILES ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_flot/jquery.flot.js ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_flot/jquery.flot.resize.js DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/flot)

    install (FILES
            ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_three/build/three.min.js
            ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_three/examples/js/Detector.js
            ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_three/examples/js/controls/TrackballControls.js
            ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_three/examples/js/renderers/CanvasRenderer.js
            ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_three/examples/js/renderers/Projector.js
            ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_three/examples/js/utils/SceneUtils.js
            DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/three)

    install (FILES ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_vis/dist/vis.min.css ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_vis/dist/vis.min.js DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/vis)

    install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_mathjax/fonts/HTML-CSS/STIX-Web/otf DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/mathjax/fonts/HTML-CSS/STIX-Web)
    install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_mathjax/fonts/HTML-CSS/STIX-Web/woff DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/mathjax/fonts/HTML-CSS/STIX-Web)
    install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_mathjax/fonts/HTML-CSS/TeX/otf DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/mathjax/fonts/HTML-CSS/TeX)
    install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_mathjax/unpacked DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/mathjax)
endif ()
