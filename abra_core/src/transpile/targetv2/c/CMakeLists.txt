cmake_minimum_required(VERSION 3.24)
project(abra_lang C)

set(CMAKE_C_STANDARD 11)

include_directories(abra/include)

add_executable(abra_lang
        abra/include/prelude.h
        abra/src/prelude.c
        abra/include/hashmap.h
        abra/src/hashmap.c)
