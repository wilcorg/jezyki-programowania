cmake_minimum_required(VERSION 3.27)
project(1z C)

set(CMAKE_C_STANDARD 17)

add_executable(1z main.c
        lib/iter.h
        lib/iter.c
        utils/common.h
        lib/rec.h
        lib/rec.c
        utils/hidden.h
        lib/iter.c
        utils/optional.h
)
