cmake_minimum_required(VERSION 3.20)
project(auto_test)
file(GLOB test_src "*.cc")
add_executable(${PROJECT_NAME} ${test_src} catch2/catch_amalgamated.cpp)