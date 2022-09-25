cmake_minimum_required(VERSION 3.18)
project(lcc)

set(CMAKE_C_STANDARD 99)
set(CMAKE_CXX_STANDARD 20)

add_subdirectory(tests/ir)

add_executable(lcc main.cpp)