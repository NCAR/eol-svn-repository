# Install script for directory: /opt/local/bufr/eccodes-2.16.0-Source/python

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
  message("Building Python extension modules:
/bin/python setup.py build_ext --rpath /opt/local/bufr/lib")
                  execute_process(COMMAND /bin/python setup.py build_ext --rpath /opt/local/bufr/lib
                                  WORKING_DIRECTORY /opt/local/bufr/build/python)
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  if( NOT $ENV{DESTDIR} STREQUAL "" )
                    set( __root "--root=$ENV{DESTDIR}" )
                  endif()
                  message("Installing Python modules:
/bin/python setup.py install ${__root}
                                      
                                      --prefix=/opt/local/bufr
                                      --record=/opt/local/bufr/build/extra_install.txt")
                  execute_process(COMMAND /bin/python setup.py install
                                            ${__root}
                                            --prefix=/opt/local/bufr
                                            
                                            --record=/opt/local/bufr/build/extra_install.txt
                                  WORKING_DIRECTORY /opt/local/bufr/build/python)
endif()

