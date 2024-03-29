#
#
# This file is part of yacas.
# Yacas is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesset General Public License as
# published by the Free Software Foundation, either version 2.1
# of the License, or (at your option) any later version.
#
# Yacas is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with yacas.  If not, see <http://www.gnu.org/licenses/>.
#
#

set (SOURCES
  src/limbs_vector.cpp
  src/nn.cpp
  src/zz.cpp
  src/rr.cpp)

set (HEADERS
  include/yacas/mp/limbs_vector.hpp
  include/yacas/mp/nn.hpp
  include/yacas/mp/zz.hpp
  include/yacas/mp/rr.hpp)


add_library (libyacas_mp ${SOURCES} ${HEADERS})
set_target_properties (libyacas_mp PROPERTIES OUTPUT_NAME "yacas_mp"  INTERPROCEDURAL_OPTIMIZATION ${IPO_SUPPORTED})
target_include_directories (libyacas_mp PUBLIC include)
target_link_libraries(libyacas_mp PUBLIC coverage_config)

install (TARGETS libyacas_mp LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
                             ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
                             RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT app)
install (DIRECTORY include/ DESTINATION ${CMAKE_INSTALL_INCLUDEDIR} COMPONENT dev)

# if (APPLE)
#   add_library (libyacas_mp_framework SHARED ${SOURCES} ${HEADERS})
#   set_target_properties(libyacas_mp_framework PROPERTIES OUTPUT_NAME "yacas_mp" VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION} FRAMEWORK ON)
#   target_include_directories (libyacas_mp_framework PUBLIC include)
#   add_custom_command(TARGET libyacas_mp_framework POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/include $<TARGET_FILE_DIR:libyacas_mp_framework>/Headers)
#   add_custom_command(TARGET libyacas_mp_framework POST_BUILD COMMAND cd "$<TARGET_FILE_DIR:libyacas_mp_framework>/../.." && rm -f Headers && ln -s Versions/Current/Headers Headers)
#   install (TARGETS libyacas_mp_framework FRAMEWORK DESTINATION ${CMAKE_INSTALL_FRAMEWORK_PREFIX} COMPONENT framework)
# endif()


if (ENABLE_CYACAS_BENCHMARKS)
    add_subdirectory (benchmark)
endif ()

if (ENABLE_CYACAS_UNIT_TESTS)
    add_subdirectory (test)
endif ()
