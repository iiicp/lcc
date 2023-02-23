cmake_minimum_required(VERSION 3.20)
project(auto_test)
file(GLOB test_src "*.cc")

Include(FetchContent)

FetchContent_Declare(
        Catch2
        GIT_REPOSITORY https://github.com/catchorg/Catch2.git
        GIT_TAG        v3.3.1 # or a later release
)

FetchContent_MakeAvailable(Catch2)

add_executable(${PROJECT_NAME} ${test_src})
target_link_libraries(${PROJECT_NAME} PRIVATE Catch2::Catch2)