find_path (ZEROMQ_INCLUDE_DIR zmq.hpp)
find_library (ZEROMQ_LIBRARY NAMES zmq)

find_path (ZMQPP_INCLUDE_DIR zmqpp.hpp)
find_library (ZMQPP_LIBRARY NAMES zmqpp)

find_path (JSONCPP_INCLUDE_DIR json.h)
find_library (JSONCPP_LIBRARY NAMES jsoncpp)

find_package (OpenSSL)
find_package (Boost REQUIRED date_time)

include_directories (include)

add_executable (yacas-kernel src/main.cpp src/yacas_kernel.cpp src/yacas_engine.cpp src/hmac_sha256.cpp)
target_link_libraries (yacas-kernel libyacas ${ZMQPP_LIBRARY} ${ZEROMQ_LIBRARY} ${JSONCPP_LIBRARY} ${OPENSSL_CRYPTO_LIBRARY} ${Boost_LIBRARIES} pthread)

install (TARGETS yacas-kernel DESTINATION bin)