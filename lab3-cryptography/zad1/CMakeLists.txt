cmake_minimum_required(VERSION 3.27)
project(gf_cpp)

set(CMAKE_CXX_STANDARD 20)
set(Boost_USE_MULTITHREADED ON)

find_package(Boost COMPONENTS multiprecision)
include_directories(${Boost_INCLUDE_DIRS})

find_library(GMP_LIBRARY gmp)
find_path(GMP_INCLUDE_DIR gmp.h)
include_directories(${GMP_INCLUDE_DIR})

add_executable(gf_cpp main.cpp gf/tools.hpp
        gf/gf.hpp
        DHSetup.hpp
        User.hpp
)
target_link_libraries(gf_cpp ${Boost_LIBRARIES} ${GMP_LIBRARY})