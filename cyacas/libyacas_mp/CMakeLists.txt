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
  src/nn.cpp
  src/zz.cpp)

set (HEADERS
  include/yacas/mp/nn.hpp
  include/yacas/mp/zz.hpp)


add_library (libyacas_mp ${SOURCES} ${HEADERS})
set_target_properties (libyacas_mp PROPERTIES OUTPUT_NAME "yacas_mp")
target_include_directories (libyacas_mp PUBLIC include)

install (TARGETS libyacas_mp LIBRARY DESTINATION lib ARCHIVE DESTINATION lib RUNTIME DESTINATION bin COMPONENT app)
install (DIRECTORY include/ DESTINATION include COMPONENT dev)

if (APPLE)
  add_library (libyacas_mp_framework SHARED ${SOURCES} ${HEADERS})
  set_target_properties(libyacas_mp_framework PROPERTIES OUTPUT_NAME "yacas_mp" VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION} FRAMEWORK ON)
  add_custom_command(TARGET libyacas_mp_framework POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/include $<TARGET_FILE_DIR:libyacas_mp_framework>/Headers)
  add_custom_command(TARGET libyacas_mp_framework POST_BUILD COMMAND cd "$<TARGET_FILE_DIR:libyacas_mp_framework>/../.." && rm -f Headers && ln -s Versions/Current/Headers Headers)
  install (TARGETS libyacas_mp_framework FRAMEWORK DESTINATION ${CMAKE_INSTALL_FRAMEWORK_PREFIX} COMPONENT framework)
endif()


if (ENABLE_CYACAS_BENCHMARKS)
    add_subdirectory (benchmark)
endif ()

if (ENABLE_CYACAS_UNIT_TESTS)
    add_subdirectory (test)
endif ()