# Copyright (c) 2008, 2014 OpenCog.org (http://opencog.org)
#
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.

# - Try to find Guile; Once done this will define
#
# GUILE_FOUND            - system has the GUILE library
# GUILE_INCLUDE_DIRS     - the GUILE include directory
# GUILE_LIBRARIES        - The libraries needed to use GUILE
# GUILE_VERSION_STRING   - Version
# GUILE_SITE_DIR         - site dir
# GUILE_EXTENSION_DIR    - extension dir
# GUILE_ROOT_DIR         - prefix dir

# (lloda) grabbed from https://github.com/stevengj/nlopt/blob/master/cmake/FindGuile.cmake
# added 3.0 to version list
# $GUILE in environment overrides the other searches.

if (NOT "$ENV{GUILE}" STREQUAL "")
  set(GUILE "$ENV{GUILE}" CACHE INTERNAL "Copied from environment variable")
  get_filename_component(GUILE_ROOT "$ENV{GUILE}" DIRECTORY)
  set(GUILE_INCLUDE_DIR_HINT "${GUILE_ROOT}/../include")
  set(GUILE_LIB_DIR_HINT "${GUILE_ROOT}/../lib")
endif()

# Look for guile-3.0 first, then 2.2, 2.0, 1.8
# Macports for OSX puts things in /opt/local

# Look for the header file
find_path (GUILE_INCLUDE_DIR libguile.h
  PATH_SUFFIXES guile/3.0 guile/2.2 guile/2.0 guile/1.8 libguile guile
  HINTS /opt/local/include ${GUILE_INCLUDE_DIR_HINT})

# Look for the library
find_library (GUILE_LIBRARY
  NAMES guile-3.0 guile-2.2 guile-2.0 guile
  HINTS /opt/local/lib ${GUILE_LIB_DIR_HINT})

set (GUILE_INCLUDE_DIRS ${GUILE_INCLUDE_DIR})
set (GUILE_LIBRARIES ${GUILE_LIBRARY})

# check guile's version if we're using cmake >= 2.6
if (GUILE_INCLUDE_DIR)
  SET(GUILE_VERSION_MAJOR 0)
  SET(GUILE_VERSION_MINOR 0)
  SET(GUILE_VERSION_PATCH 0)

  IF(NOT EXISTS "${GUILE_INCLUDE_DIR}/libguile.h")
    message("OMG")
  ENDIF(NOT EXISTS "${GUILE_INCLUDE_DIR}/libguile.h")

  IF(NOT EXISTS "${GUILE_INCLUDE_DIR}/libguile/version.h")
    MESSAGE(FATAL_ERROR "Found ${GUILE_INCLUDE_DIR}/libguile.h but not version.h; check your guile installation!")
  ENDIF(NOT EXISTS "${GUILE_INCLUDE_DIR}/libguile/version.h")

  # Extract the libguile version from the 'version.h' file
  SET(GUILE_MAJOR_VERSION 0)
  FILE(READ "${GUILE_INCLUDE_DIR}/libguile/version.h" _GUILE_VERSION_H_CONTENTS)

  STRING(REGEX MATCH "#define SCM_MAJOR_VERSION[	 ]+([0-9])" _MATCH "${_GUILE_VERSION_H_CONTENTS}")
  SET(GUILE_VERSION_MAJOR ${CMAKE_MATCH_1})
  STRING(REGEX MATCH "#define SCM_MINOR_VERSION[	 ]+([0-9]+)" _MATCH "${_GUILE_VERSION_H_CONTENTS}")
  SET(GUILE_VERSION_MINOR ${CMAKE_MATCH_1})
  STRING(REGEX MATCH "#define SCM_MICRO_VERSION[	 ]+([0-9]+)" _MATCH "${_GUILE_VERSION_H_CONTENTS}")
  SET(GUILE_VERSION_PATCH ${CMAKE_MATCH_1})

  SET(GUILE_VERSION_STRING "${GUILE_VERSION_MAJOR}.${GUILE_VERSION_MINOR}.${GUILE_VERSION_PATCH}")

endif ()

find_program(GUILE_EXECUTABLE
  NAMES guile
  HINTS ${GUILE_ROOT})
find_program(GUILE_CONFIG_EXECUTABLE
  NAMES guile-config
  HINTS ${GUILE_ROOT})

if (GUILE_CONFIG_EXECUTABLE)
  execute_process (COMMAND ${GUILE_CONFIG_EXECUTABLE} info prefix
    OUTPUT_VARIABLE GUILE_ROOT_DIR
    OUTPUT_STRIP_TRAILING_WHITESPACE)

  execute_process (COMMAND ${GUILE_CONFIG_EXECUTABLE} info sitedir
    OUTPUT_VARIABLE GUILE_SITE_DIR
    OUTPUT_STRIP_TRAILING_WHITESPACE)

  execute_process (COMMAND ${GUILE_CONFIG_EXECUTABLE} info extensiondir
    OUTPUT_VARIABLE GUILE_EXTENSION_DIR
    OUTPUT_STRIP_TRAILING_WHITESPACE)
endif ()

message ("* GUILE_EXECUTABLE: ${GUILE_EXECUTABLE}")
message ("* GUILE_ROOT_DIR: ${GUILE_ROOT_DIR}")
message ("* GUILE_INCLUDE_DIRS: ${GUILE_INCLUDE_DIRS}")
message ("* GUILE_LIBRARIES: ${GUILE_LIBRARIES}")
message ("* GUILE_VERSION_STRING: ${GUILE_VERSION_STRING}")

# handle REQUIRED and QUIET options
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (Guile
  REQUIRED_VARS GUILE_EXECUTABLE GUILE_ROOT_DIR GUILE_INCLUDE_DIRS GUILE_LIBRARIES
  VERSION_VAR GUILE_VERSION_STRING)

mark_as_advanced (GUILE_INCLUDE_DIR GUILE_LIBRARY)
