cmake_minimum_required (VERSION 3.10)

project(biocpp_clang_tidy NONE)

find_program(CLANG_TIDY "clang-tidy-16")

if (CLANG_TIDY STREQUAL "CLANG_TIDY-NOTFOUND")
    find_program(CLANG_TIDY "clang-tidy")
endif()

if (CLANG_TIDY STREQUAL "CLANG_TIDY-NOTFOUND")
    message(FATAL_ERROR "clang-tidy could not be found.")
else ()
    set(CMAKE_CXX_CLANG_TIDY ${CLANG_TIDY};--config-file=${CMAKE_CURRENT_SOURCE_DIR}/../../.clang-tidy)
    add_subdirectory(../unit unit)
    #TODO shall we add snippet and integration as well?
endif ()
