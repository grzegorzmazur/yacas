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

find_path (ZEROMQ_INCLUDE_DIR zmq.hpp)
find_library (ZEROMQ_LIBRARY NAMES zmq)

find_path (ZMQPP_INCLUDE_DIR zmqpp.hpp)
find_library (ZMQPP_LIBRARY NAMES zmqpp)

find_path (JSONCPP_INCLUDE_DIR json.h)
find_library (JSONCPP_LIBRARY NAMES jsoncpp)

find_package (OpenSSL)
find_package (Boost REQUIRED date_time)

include_directories (include)

add_executable (yacas-kernel src/main.cpp src/yacas_kernel.cpp src/yacas_engine.cpp src/hmac_sha256.cpp src/base64.cpp)
target_link_libraries (yacas-kernel libyacas ${ZMQPP_LIBRARY} ${ZEROMQ_LIBRARY} ${JSONCPP_LIBRARY} ${OPENSSL_CRYPTO_LIBRARY} ${Boost_LIBRARIES} pthread)

install (TARGETS yacas-kernel DESTINATION ${CMAKE_INSTALL_BINDIR})
