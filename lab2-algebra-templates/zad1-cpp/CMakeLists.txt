cmake_minimum_required(VERSION 3.27)
project(zad1)

set(CMAKE_CXX_STANDARD 20)

add_executable(zad1 main.cpp
        GaloisField.hpp
        NumberTools.cpp
        NumberTools.hpp
)
