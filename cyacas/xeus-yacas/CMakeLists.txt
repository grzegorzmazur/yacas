find_package(xeus REQUIRED)
find_package(xeus-zmq REQUIRED)
find_package(Threads)

find_package (Boost REQUIRED filesystem)

add_executable(xeus-yacas src/main.cpp src/interpreter.cpp)
target_include_directories(xeus-yacas PRIVATE include)
target_link_libraries(xeus-yacas PRIVATE xeus-zmq-static Threads::Threads Boost::filesystem libyacas)

install (TARGETS xeus-yacas RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT app)
