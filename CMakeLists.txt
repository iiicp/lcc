cmake_minimum_required(VERSION 3.18)

# set compiler version
set(CMAKE_C_STANDARD 99)
set(CMAKE_CXX_STANDARD 20)

set(CMAKE_COMPILE_WARNING_AS_ERROR OFF)

if (CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
    project(lcc)

    # find llvm lib
#    set(LLVM_DIR "" CACHE PATH "path to llvm cmake dir")
    find_package(LLVM REQUIRED HINTS "${LLVM_CMAKE_PATH}")
    list(APPEND CMAKE_MODULE_PATH ${LLVM_DIR})
    include(ChooseMSVCCRT)

    include(AddLLVM)
    include(HandleLLVMOptions)

    include_directories("${LLVM_BINARY_DIR}/include" "${LLVM_INCLUDE_DIR}")
    link_directories("${LLVM_LIBRARY_DIR}")

    set(LCC_BUILT_STANDALONE 1)
endif ()

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules")

if (CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR AND NOT MSVC_IDE)
    message(FATAL_ERROR "In-source builds are not allowed.")
endif ()

set(LCC_VERSION_STRING "0.4")
configure_file(
        ${CMAKE_CURRENT_SOURCE_DIR}/include/lcc/Basic/Version.inc.in
        ${CMAKE_CURRENT_BINARY_DIR}/include/lcc/Basic/Version.inc)

include(AddLcc)

include_directories(BEFORE
        ${CMAKE_CURRENT_BINARY_DIR}/include
        ${CMAKE_CURRENT_SOURCE_DIR}/include)

add_subdirectory(lib)
add_subdirectory(tools)
