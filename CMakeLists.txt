cmake_minimum_required(VERSION 3.23)
project(lcc)

set(CMAKE_C_STANDARD 99)
set(CMAKE_CXX_STANDARD 20)

set(LLVM_DIR "" CACHE PATH "path to llvm cmake directory")
message(STATUS "LLVM_DIR: ${LLVM_DIR}")

find_package(LLVM REQUIRED)

message(STATUS "LLVM_INCLUDE_DIRS: ${LLVM_INCLUDE_DIRS}")
message(STATUS "LLVM_TARGETS_TO_BUILD: ${LLVM_TARGETS_TO_BUILD}")

llvm_map_components_to_libnames(llvm_all ${LLVM_TARGETS_TO_BUILD} Passes)
#message(STATUS "llvm_all: ${llvm_all}")

include_directories(. ${LLVM_INCLUDE_DIRS})

add_executable(lcc main.cpp)
target_link_libraries(${PROJECT_NAME} ${llvm_all})