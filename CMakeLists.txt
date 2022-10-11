cmake_minimum_required(VERSION 3.18)
project(lcc)

# set compiler version
set(CMAKE_C_STANDARD 99)
set(CMAKE_CXX_STANDARD 20)

# find llvm lib
set(LLVM_DIR "" CACHE PATH "path to llvm cmake dir")
find_package(LLVM REQUIRED 14)
llvm_map_components_to_libnames(llvm_all ${LLVM_TARGETS_TO_BUILD} Passes ExecutionEngine MCJIT)

# find source file
file(GLOB lcc_src "src/*.cc")

# compiler and set search include and link llvm lib
add_executable(${PROJECT_NAME} main.cpp ${lcc_src} src/Token.cc src/Token.cc src/Token.h src/Lexer.cc src/Lexer.h)
target_include_directories(${PROJECT_NAME}
        PRIVATE
        src
        ${LLVM_INCLUDE_DIRS})
target_link_libraries(${PROJECT_NAME} PUBLIC ${llvm_all})