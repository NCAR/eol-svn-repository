# Install script for directory: /opt/local/bufr/eccodes-2.16.0-Source/definitions

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/opt/local/bufr")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "RelWithDebInfo")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "0")
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/eccodes/definitions" TYPE FILE PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ FILES
    "/opt/local/bufr/eccodes-2.16.0-Source/definitions/boot.def"
    "/opt/local/bufr/eccodes-2.16.0-Source/definitions/empty_template.def"
    "/opt/local/bufr/eccodes-2.16.0-Source/definitions/param_limits.def"
    "/opt/local/bufr/eccodes-2.16.0-Source/definitions/parameters_version.def"
    "/opt/local/bufr/eccodes-2.16.0-Source/definitions/mars_param.table"
    "/opt/local/bufr/eccodes-2.16.0-Source/definitions/param_id.table"
    "/opt/local/bufr/eccodes-2.16.0-Source/definitions/stepUnits.table"
    "/opt/local/bufr/eccodes-2.16.0-Source/definitions/CMakeLists.txt"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/eccodes/definitions" TYPE FILE FILES "/opt/local/bufr/eccodes-2.16.0-Source/definitions/installDefinitions.sh")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/eccodes/definitions" TYPE DIRECTORY FILES
    "/opt/local/bufr/eccodes-2.16.0-Source/definitions/budg"
    "/opt/local/bufr/eccodes-2.16.0-Source/definitions/bufr"
    "/opt/local/bufr/eccodes-2.16.0-Source/definitions/cdf"
    "/opt/local/bufr/eccodes-2.16.0-Source/definitions/common"
    "/opt/local/bufr/eccodes-2.16.0-Source/definitions/grib1"
    "/opt/local/bufr/eccodes-2.16.0-Source/definitions/grib2"
    "/opt/local/bufr/eccodes-2.16.0-Source/definitions/grib3"
    "/opt/local/bufr/eccodes-2.16.0-Source/definitions/gts"
    "/opt/local/bufr/eccodes-2.16.0-Source/definitions/mars"
    "/opt/local/bufr/eccodes-2.16.0-Source/definitions/metar"
    "/opt/local/bufr/eccodes-2.16.0-Source/definitions/tide"
    "/opt/local/bufr/eccodes-2.16.0-Source/definitions/hdf5"
    "/opt/local/bufr/eccodes-2.16.0-Source/definitions/wrap"
    FILES_MATCHING REGEX "/[^/]*\\.def$" REGEX "/[^/]*\\.txt$" REGEX "/[^/]*\\.list$" REGEX "/[^/]*\\.table$" REGEX "/4\\.2\\.192\\.[^/]*\\.table$" EXCLUDE PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)
endif()

