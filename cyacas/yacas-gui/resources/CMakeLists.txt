include (ExternalProject)

ExternalProject_Add (yacas-gui_vis
    PREFIX external_packages
    URL file://${CMAKE_SOURCE_DIR}/third_party/vis-4.21.0.zip
    URL_HASH MD5=413939fa8b8c150e373ec5818b6db126
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND "")

if (NOT APPLE)
    install (FILES ${CMAKE_SOURCE_DIR}/third_party/plotly-1.49.0.min.js DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources)
    install (DIRECTORY jquery DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources)
    install (DIRECTORY webchannel DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources)
    install (DIRECTORY mathbar DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources)
    install (DIRECTORY yacas_gui DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources)
    install (FILES yacas_gui.html DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources)
    install (DIRECTORY pixmaps DESTINATION ${CMAKE_INSTALL_DATAROOTDIR})
    install (DIRECTORY icons DESTINATION ${CMAKE_INSTALL_DATAROOTDIR})

    install (FILES ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_vis/dist/vis.min.css ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_vis/dist/vis.min.js DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/vis)

    if (ENABLE_CYACAS_GUI_PRIVATE_CODEMIRROR)
        ExternalProject_Add (yacas-gui_codemirror
            PREFIX external_packages
            URL file://${CMAKE_SOURCE_DIR}/third_party/CodeMirror-minified-5.47.0.zip
            URL_HASH MD5=2e15622d1f5d8f82884484b95cf631f5
            CONFIGURE_COMMAND ""
            BUILD_COMMAND ""
            INSTALL_COMMAND "")

        install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_codemirror/lib DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/codemirror)
        install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_codemirror/mode/yacas DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/codemirror/mode)
        install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_codemirror/addon/edit DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/codemirror/addon)
        install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_codemirror/addon/hint DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/codemirror/addon)
    else ()
        find_file (CODEMIRROR_PATH codemirror.js PATHS /usr/share/javascript/codemirror ENV CODEMIRROR_PATH)
        if (CODEMIRROR_PATH STREQUAL CODEMIRROR_PATH-NOTFOUND)
            message (SEND_ERROR "CodeMirror not found")
        else ()
            get_filename_component (CODEMIRROR_DIR ${CODEMIRROR_PATH} DIRECTORY)
            install (CODE "execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink ${CODEMIRROR_DIR} \"${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/codemirror\")")
        endif ()
    endif ()
    if (ENABLE_CYACAS_GUI_PRIVATE_MATHJAX)
        ExternalProject_Add (yacas-gui_mathjax
            PREFIX external_packages
            URL file://${CMAKE_SOURCE_DIR}/third_party/MathJax-2.7.3.zip
            URL_HASH MD5=3d1a968f6af90097ef4453fe2b90b373
            CONFIGURE_COMMAND ""
            BUILD_COMMAND ""
            INSTALL_COMMAND "")

        install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_mathjax/fonts/HTML-CSS/STIX-Web/otf DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/mathjax/fonts/HTML-CSS/STIX-Web)
        install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_mathjax/fonts/HTML-CSS/STIX-Web/woff DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/mathjax/fonts/HTML-CSS/STIX-Web)
        install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_mathjax/fonts/HTML-CSS/TeX/otf DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/mathjax/fonts/HTML-CSS/TeX)
        install (DIRECTORY ${CMAKE_BINARY_DIR}/cyacas/yacas-gui/resources/external_packages/src/yacas-gui_mathjax/unpacked DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/mathjax)
    else ()
        find_file (MATHJAX_PATH MathJax.js PATHS /usr/share/javascript/mathjax ENV MATHJAX_PATH)
        if (MATHJAX_PATH STREQUAL MATHJAX_PATH-NOTFOUND)
            message (SEND_ERROR "MathJax not found")
        else ()
            get_filename_component (MATHJAX_DIR ${MATHJAX_PATH} DIRECTORY)
            install (CODE "execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink ${MATHJAX_DIR} \"${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATAROOTDIR}/yacas/resources/mathjax\")")
        endif ()
    endif ()
else ()
    ExternalProject_Add (yacas-gui_codemirror
        PREFIX external_packages
        URL file://${CMAKE_SOURCE_DIR}/third_party/CodeMirror-minified-5.47.0.zip
        URL_HASH MD5=2e15622d1f5d8f82884484b95cf631f5
        CONFIGURE_COMMAND ""
        BUILD_COMMAND ""
        INSTALL_COMMAND "")

    ExternalProject_Add (yacas-gui_mathjax
        PREFIX external_packages
        URL file://${CMAKE_SOURCE_DIR}/third_party/MathJax-2.7.3.zip
        CONFIGURE_COMMAND ""
        BUILD_COMMAND ""
        INSTALL_COMMAND "")
endif ()
