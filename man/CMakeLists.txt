if (${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
    find_package (UnixCommands REQUIRED)
    find_program (RST2MAN rst2man)

    if (RST2MAN)
        add_custom_target (man ALL COMMAND ${RST2MAN} ${PROJECT_SOURCE_DIR}/man/yacas.1.rst | ${GZIP} -9 > ${PROJECT_BINARY_DIR}/yacas.1.gz)
        install (FILES ${PROJECT_BINARY_DIR}/yacas.1.gz DESTINATION share/man/man1 COMPONENT app)
    endif ()
endif ()