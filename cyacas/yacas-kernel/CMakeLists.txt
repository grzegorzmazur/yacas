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
# License along with yacas. If not, see <http://www.gnu.org/licenses/>.
#
#

if (UNIX)
    find_path (ZEROMQ_INCLUDE_DIRS zmq.hpp)
    find_library (ZEROMQ_LIBRARY NAMES zmq)
else ()
    find_package(ZeroMQ REQUIRED)
    find_package(cppzmq CONFIG REQUIRED)
endif ()

# https://github.com/open-source-parsers/jsoncpp/wiki/Building#another-approach-for-cmake
find_package(jsoncpp CONFIG REQUIRED)
get_target_property(JSON_INC_PATH jsoncpp_lib INTERFACE_INCLUDE_DIRECTORIES)
find_package (OpenSSL)
find_package (Boost REQUIRED date_time filesystem)

add_executable (yacas-kernel src/main.cpp src/yacas_kernel.cpp src/yacas_engine.cpp src/hmac_sha256.cpp src/base64.cpp)

set_target_properties (yacas-kernel PROPERTIES  INTERPROCEDURAL_OPTIMIZATION ${IPO_SUPPORTED})

target_include_directories (yacas-kernel PRIVATE include)
target_include_directories (yacas-kernel PRIVATE ${ZeroMQ_INCLUDE_DIRS})
target_include_directories (yacas-kernel PRIVATE ${JSON_INC_PATH})

if (UNIX)
    target_link_libraries (yacas-kernel PRIVATE libyacas ${ZEROMQ_LIBRARY} jsoncpp_lib OpenSSL::SSL OpenSSL::Crypto Boost::boost  Boost::date_time Boost::filesystem pthread ${CMAKE_DL_LIBS})
else ()
    target_link_libraries (yacas-kernel PRIVATE libyacas libzmq cppzmq cppzmq-static jsoncpp_lib jsoncpp_object OpenSSL::SSL OpenSSL::Crypto Boost::boost  Boost::date_time Boost::filesystem bcrypt)
endif ()


install (TARGETS yacas-kernel DESTINATION ${CMAKE_INSTALL_BINDIR})
