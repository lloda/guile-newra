# -*- coding: utf-8; mode: cmake -*-
# test CMakeLists.txt

# (c) lloda@sarc.name 2019
# This library is free software; you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by the Free
# Software Foundation; either version 3 of the License, or (at your option) any
# later version.

cmake_minimum_required (VERSION 3.5)
project (newra-test)

include_directories (.)
set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/../config)

# -------------------
# bindings
# -------------------

find_package (Guile REQUIRED)

foreach (target everything format)
    add_test (${target} ${GUILE_EXECUTABLE} -q -L "${CMAKE_SOURCE_DIR}/../mod" "${CMAKE_SOURCE_DIR}/${target}.scm")
endforeach ()

enable_testing ()
