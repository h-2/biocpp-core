# -----------------------------------------------------------------------------------------------------
# Copyright (c) 2022 deCODE Genetics
# Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
# Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
# This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
# shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md
# -----------------------------------------------------------------------------------------------------

# This file adds version support for `find_package(biocpp_core 3.1)`.
# See https://cmake.org/cmake/help/v3.16/manual/cmake-packages.7.html#package-version-file for more information.
#
# This file was partially generated by
# https://cmake.org/cmake/help/v3.16/module/CMakePackageConfigHelpers.html#command:write_basic_package_version_file

# Note that biocpp-config.cmake can be standalone and thus BIOCPP_CORE_CLONE_DIR might be empty.
find_path (BIOCPP_CORE_CLONE_DIR NAMES build_system/biocpp_core-config.cmake HINTS "${CMAKE_CURRENT_LIST_DIR}/..")
find_path (BIOCPP_CORE_INCLUDE_DIR NAMES bio/core.hpp HINTS "${BIOCPP_CORE_CLONE_DIR}/include")

# extract version from bio/core.hpp header
file(STRINGS "${BIOCPP_CORE_INCLUDE_DIR}/bio/core.hpp" BIOCPP_CORE_VERSION_HPP REGEX "#define BIOCPP_CORE_VERSION_(MAJOR|MINOR|PATCH)")
string(REGEX REPLACE "#define BIOCPP_CORE_VERSION_(MAJOR|MINOR|PATCH) " "" PACKAGE_VERSION "${BIOCPP_CORE_VERSION_HPP}")
string(REGEX REPLACE ";" "." PACKAGE_VERSION "${PACKAGE_VERSION}")

if(PACKAGE_VERSION VERSION_LESS PACKAGE_FIND_VERSION)
    set(PACKAGE_VERSION_COMPATIBLE FALSE)
else()

    if(PACKAGE_VERSION MATCHES "^([0-9]+)\\.")
        set(_PACKAGE_VERSION_MAJOR "${CMAKE_MATCH_1}")
    endif()

    if(PACKAGE_FIND_VERSION_MAJOR STREQUAL _PACKAGE_VERSION_MAJOR)
        set(PACKAGE_VERSION_COMPATIBLE TRUE)
    else()
        set(PACKAGE_VERSION_COMPATIBLE FALSE)
    endif()

    if(PACKAGE_FIND_VERSION STREQUAL PACKAGE_VERSION)
        set(PACKAGE_VERSION_EXACT TRUE)
    endif()
endif()

# if the installed or the using project don't have CMAKE_SIZEOF_VOID_P set, ignore it:
if("${CMAKE_SIZEOF_VOID_P}" STREQUAL "")
    return()
endif()

# check that the installed version has the same 32/64bit-ness as the one which is currently searching:
if(NOT "${CMAKE_SIZEOF_VOID_P}" STREQUAL "8")
    math(EXPR installedBits "8 * 8")
    # For some time we set these variables to show that we do not support 32 bit architectures,
    # but it seems to hard to actively forbid them. Most of the library does build, but some parts
    # might not.
    # set(PACKAGE_VERSION "${PACKAGE_VERSION} (${installedBits}bit)")
    # set(PACKAGE_VERSION_UNSUITABLE TRUE)
    message (AUTHOR_WARNING "BioC++ does not support 32bit architectures; No guarantees; Patches are welcome.")
endif()