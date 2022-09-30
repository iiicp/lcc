cmake_minimum_required(VERSION 3.18)
project(CompilerTest)

set(CMAKE_C_STANDARD 99)
set(CMAKE_CXX_STANDARD 20)

set(EXECUTABLE_OUTPUT_PATH ../../bin)

set(GTest_DIR "" CACHE PATH "path to GTest Cmake Dir")
message(STATUS ${GTest_DIR})

#set(GTest_DIR "/opt/homebrew/Cellar/googletest/1.12.1/lib/cmake/GTest")
find_package(GTest REQUIRED)
if (GTest_FOUND)
    message(STATUS "found GTest")
    message(STATUS ${GTest_INCLUDE_DIRS})
    message(STATUS ${GTest_INCLUDES})
    message(STATUS ${GTest_LIBRARY})
    message(STATUS ${GTest_LIBRARIES})
endif(GTest_FOUND)
add_executable(${PROJECT_NAME} lexer.cpp)
target_link_libraries(${PROJECT_NAME} GTest::Main)