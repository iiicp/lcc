cmake_minimum_required(VERSION 3.20)
project(ir_gen)

set(CMAKE_C_STANDARD 99)
set(CMAKE_CXX_STANDARD 20)

set(EXECUTABLE_OUTPUT_PATH ../../bin)

set(LLVM_DIR "" CACHE PATH "path to llvm cmake directory")
message(STATUS "LLVM_DIR: ${LLVM_DIR}")

find_package(LLVM REQUIRED 14)

message(STATUS "LLVM_INCLUDE_DIRS: ${LLVM_INCLUDE_DIRS}")
message(STATUS "LLVM_TARGETS_TO_BUILD: ${LLVM_TARGETS_TO_BUILD}")

llvm_map_components_to_libnames(llvm_all ${LLVM_TARGETS_TO_BUILD} Passes ExecutionEngine MCJIT)
#message(STATUS "llvm_all: ${llvm_all}")

include_directories(. ${LLVM_INCLUDE_DIRS})

add_executable(c_basic_type_global c_basic_type_global.cpp)
target_link_libraries(c_basic_type_global ${llvm_all})

add_executable(c_pointer_global c_pointer_global.cpp)
target_link_libraries(c_pointer_global ${llvm_all})

add_executable(c_array_global c_array_global.cpp)
target_link_libraries(c_array_global ${llvm_all})

add_executable(c_struct_global c_struct_global.cpp)
target_link_libraries(c_struct_global ${llvm_all})

add_executable(c_basic_type_local c_basic_type_local.cpp)
target_link_libraries(c_basic_type_local ${llvm_all})

add_executable(c_if_else c_if_else.cpp)
target_link_libraries(c_if_else ${llvm_all})

add_executable(c_for c_for.cpp)
target_link_libraries(c_for ${llvm_all})

add_executable(c_func_call c_func_call.cpp)
target_link_libraries(c_func_call ${llvm_all})

add_executable(c_global_show c_global_show.cpp)
target_link_libraries(c_global_show ${llvm_all})


add_executable(c_fun_if_else c_fun_if_else.cpp)
target_link_libraries(c_fun_if_else ${llvm_all})

add_executable(c_fun_for c_fun_for.cpp)
target_link_libraries(c_fun_for ${llvm_all})

add_executable(c_fun_fib c_fun_fib.cpp)
target_link_libraries(c_fun_fib ${llvm_all})

add_executable(c_while c_while.cpp)
target_link_libraries(c_while ${llvm_all})

add_executable(use_mgr use_sourcemgr.cc)
target_link_libraries(use_mgr ${llvm_all})

add_executable(c_switch c_switch.cc)
target_link_libraries(c_switch ${llvm_all})
