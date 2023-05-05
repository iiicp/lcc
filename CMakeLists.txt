cmake_minimum_required(VERSION 3.18)
project(lcc)

# set compiler version
set(CMAKE_C_STANDARD 99)
set(CMAKE_CXX_STANDARD 20)

# find llvm lib
set(LLVM_DIR "" CACHE PATH "path to llvm cmake dir")
find_package(LLVM REQUIRED 14)
llvm_map_components_to_libnames(llvm_all ${LLVM_TARGETS_TO_BUILD} Passes ExecutionEngine MCJIT)

include_directories(src/Lex src/Parse src/Sema src/CodeGen src/Support)

# find source file
file(GLOB lcc_src "src/Lex/*.cc" "src/Parse/*.cc" "src/Sema/*.cc" "src/CodeGen/*.cc" "src/Support/*.cc")

set(LCC_VERSION_STRING "0.3")
configure_file(
        ${CMAKE_CURRENT_SOURCE_DIR}/src/Support/Version.inc.in
        ${CMAKE_CURRENT_BINARY_DIR}/src/Support/Version.inc)

# compiler and set search include and link llvm lib
add_executable(${PROJECT_NAME} main.cpp ${lcc_src})
target_include_directories(${PROJECT_NAME}
        PRIVATE
        src
        ${CMAKE_CURRENT_BINARY_DIR}/src/Support
        ${LLVM_INCLUDE_DIRS})
target_link_libraries(${PROJECT_NAME} PUBLIC ${llvm_all})

#add_subdirectory(tests/ir)
#add_subdirectory(tests/auto)
