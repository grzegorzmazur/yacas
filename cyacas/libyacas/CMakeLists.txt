configure_file (
  "config/yacas/yacas_version.h.in"
  "${CMAKE_CURRENT_BINARY_DIR}/config/yacas/yacas_version.h"
  )

set (SOURCES
  src/associationclass.cpp
  src/deffile.cpp
  src/infixparser.cpp
  src/lispatom.cpp
  src/lispenvironment.cpp
  src/lispeval.cpp
  src/lisperror.cpp
  src/lispio.cpp
  src/lispobject.cpp
  src/lispparser.cpp
  src/lispuserfunc.cpp
  src/mathcommands.cpp
  src/mathuserfunc.cpp
  src/standard.cpp
  src/stdfileio.cpp
  src/arggetter.cpp
  src/stringio.cpp
  src/tokenizer.cpp
  src/yacasapi.cpp
  src/lispevalhash.cpp
  src/patterns.cpp
  src/patternclass.cpp
  src/substitute.cpp
  src/mathcommands2.cpp
  src/mathcommands3.cpp
  src/mempool.cpp
  src/errors.cpp
  src/patcher.cpp
  src/xmltokenizer.cpp
  src/anumber.cpp
  src/yacasnumbers.cpp
  src/numbers.cpp
  src/platmath.cpp
  src/lisphash.cpp)

set (HEADERS
  include/yacas/anumber.h
  include/yacas/anumber.inl
  include/yacas/arggetter.h
  include/yacas/arrayclass.h
  include/yacas/associationclass.h
  include/yacas/corefunctions.h
  include/yacas/deffile.h
  include/yacas/errors.h
  include/yacas/evalfunc.h
  include/yacas/genericobject.h
  include/yacas/GPL_stuff.h
  include/yacas/infixparser.h
  include/yacas/lispatom.h
  include/yacas/lispenvironment.h
  include/yacas/lisperror.h
  include/yacas/lispeval.h
  include/yacas/lispevalhash.h
  include/yacas/lispglobals.h
  include/yacas/lisphash.h
  include/yacas/lispio.h
  include/yacas/lispobject.h
  include/yacas/lispoperator.h
  include/yacas/lispparser.h
  include/yacas/lispstring.h
  include/yacas/lispuserfunc.h
  include/yacas/mathcommands.h
  include/yacas/mathuserfunc.h
  include/yacas/mempool.h
  include/yacas/noncopyable.h
  include/yacas/numbers.h
  include/yacas/patcher.h
  include/yacas/patternclass.h
  include/yacas/patterns.h
  include/yacas/platfileio.h
  include/yacas/platmath.h
  include/yacas/refcount.h
  include/yacas/standard.h
  include/yacas/standard.inl
  include/yacas/stringio.h
  include/yacas/string_utils.h
  include/yacas/substitute.h
  include/yacas/tokenizer.h
  include/yacas/utf8/core.h
  include/yacas/utf8/checked.h
  include/yacas/utf8/unchecked.h
  include/yacas/utf8.h
  include/yacas/xmltokenizer.h
  include/yacas/yacas.h)

add_library (libyacas ${SOURCES} ${HEADERS})
set_target_properties (libyacas PROPERTIES OUTPUT_NAME "yacas")
target_include_directories (libyacas PUBLIC include "${CMAKE_CURRENT_BINARY_DIR}/config")
target_link_libraries (libyacas libyacas_mp)

install (TARGETS libyacas LIBRARY DESTINATION lib ARCHIVE DESTINATION lib RUNTIME DESTINATION bin COMPONENT app)
install (DIRECTORY include/ DESTINATION include COMPONENT dev)
install (FILES "${CMAKE_CURRENT_BINARY_DIR}/config/yacas/yacas_version.h" DESTINATION include/yacas COMPONENT dev)

if (APPLE)
  add_library (libyacas_framework SHARED ${SOURCES} ${HEADERS})
  set_target_properties(libyacas_framework PROPERTIES OUTPUT_NAME "yacas" VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION} FRAMEWORK ON)
  target_link_libraries(libyacas_framework libyacas_mp)
  target_include_directories (libyacas_framework PUBLIC include "${CMAKE_CURRENT_BINARY_DIR}/config")
  add_custom_command(TARGET libyacas_framework POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${PROJECT_SOURCE_DIR}/scripts $<TARGET_FILE_DIR:libyacas_framework>/Resources/scripts)
  add_custom_command(TARGET libyacas_framework POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/include $<TARGET_FILE_DIR:libyacas_framework>/Headers)
  add_custom_command(TARGET libyacas_framework POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_BINARY_DIR}/config $<TARGET_FILE_DIR:libyacas_framework>/Headers)
  add_custom_command(TARGET libyacas_framework POST_BUILD COMMAND cd "$<TARGET_FILE_DIR:libyacas_framework>/../.." && rm -f Headers && ln -s Versions/Current/Headers Headers)
  install (TARGETS libyacas_framework FRAMEWORK DESTINATION ${CMAKE_INSTALL_FRAMEWORK_PREFIX} COMPONENT framework)
endif()
