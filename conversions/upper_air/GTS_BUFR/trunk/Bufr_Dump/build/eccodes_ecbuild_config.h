/*
 * (C) Copyright 2011- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef eccodes_ecbuild_config_h
#define eccodes_ecbuild_config_h

/* ecbuild info */

#ifndef ECBUILD_VERSION_STR
#define ECBUILD_VERSION_STR "3.2.0"
#endif
#ifndef ECBUILD_VERSION
#define ECBUILD_VERSION "3.2.0"
#endif
#ifndef ECBUILD_MACROS_DIR
#define ECBUILD_MACROS_DIR  "/opt/local/bufr/eccodes-2.16.0-Source/cmake"
#endif

/* config info */

#define ECCODES_OS_NAME          "Linux-3.10.0-1062.1.2.el7.x86_64"
#define ECCODES_OS_BITS          64
#define ECCODES_OS_BITS_STR      "64"
#define ECCODES_OS_STR           "linux.64"
#define ECCODES_OS_VERSION       "3.10.0-1062.1.2.el7.x86_64"
#define ECCODES_SYS_PROCESSOR    "x86_64"

#define ECCODES_BUILD_TIMESTAMP  "20200212124945"
#define ECCODES_BUILD_TYPE       "RelWithDebInfo"

#define ECCODES_C_COMPILER_ID      "GNU"
#define ECCODES_C_COMPILER_VERSION "4.8.5"

#define ECCODES_CXX_COMPILER_ID      ""
#define ECCODES_CXX_COMPILER_VERSION ""

#define ECCODES_C_COMPILER       "/bin/cc"
#define ECCODES_C_FLAGS          " -pipe -O2 -g -DNDEBUG"

#define ECCODES_CXX_COMPILER     ""
#define ECCODES_CXX_FLAGS        ""

/* Needed for finding per package config files */

#define ECCODES_INSTALL_DIR       "/opt/local/bufr"
#define ECCODES_INSTALL_BIN_DIR   "/opt/local/bufr/bin"
#define ECCODES_INSTALL_LIB_DIR   "/opt/local/bufr/lib"
#define ECCODES_INSTALL_DATA_DIR  "/opt/local/bufr/share/eccodes"

#define ECCODES_DEVELOPER_SRC_DIR "/opt/local/bufr/eccodes-2.16.0-Source"
#define ECCODES_DEVELOPER_BIN_DIR "/opt/local/bufr/build"

#define EC_HAVE_FORTRAN

#ifdef EC_HAVE_FORTRAN

#define ECCODES_Fortran_COMPILER_ID      "GNU"
#define ECCODES_Fortran_COMPILER_VERSION "4.8.5"

#define ECCODES_Fortran_COMPILER "/bin/gfortran"
#define ECCODES_Fortran_FLAGS    " -O2 -g -DNDEBUG"

#endif

#endif /* eccodes_ecbuild_config_h */
