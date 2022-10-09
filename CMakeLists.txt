cmake_minimum_required(VERSION 3.18)
project(lcc)

set(CMAKE_C_STANDARD 99)
set(CMAKE_CXX_STANDARD 20)

set(LLVM_DIR "" CACHE PATH "path to llvm cmake dir")
find_package(LLVM REQUIRED 14)
message(STATUS "LLVM_FOUND: ${LLVM_FOUND}")
message(STATUS "LLVM_INCLUDE_DIRS ${LLVM_INCLUDE_DIRS}")

llvm_map_components_to_libnames(llvm_all ${LLVM_TARGETS_TO_BUILD} Passes ExecutionEngine MCJIT)

#add_subdirectory(tests/ir)
#add_subdirectory(tests/Compiler)
#add_subdirectory(tests/misc)

file(GLOB lex_src "src/Compiler/Lex/*.cc")
add_executable(lcc main.cpp ${lex_src})
target_include_directories(${PROJECT_NAME}
        PRIVATE
        src/Compiler
        src/Compiler/Lex
        ${LLVM_INCLUDE_DIRS})
target_link_libraries(${PROJECT_NAME} PUBLIC ${llvm_all})