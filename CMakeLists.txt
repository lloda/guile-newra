# -*- coding: utf-8; mode: cmake -*-
# main CMakeLists.txt

# (c) lloda@sarc.name 2023
# This library is free software; you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by the Free
# Software Foundation; either version 3 of the License, or (at your option) any
# later version.

cmake_minimum_required (VERSION 3.5)
project (newra)

set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/config)

foreach (dir test docs)
  add_subdirectory (${dir})
endforeach ()

enable_testing ()
