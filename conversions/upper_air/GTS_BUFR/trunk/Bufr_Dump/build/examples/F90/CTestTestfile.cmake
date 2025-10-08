# CMake generated Testfile for 
# Source directory: /opt/local/bufr/eccodes-2.16.0-Source/examples/F90
# Build directory: /opt/local/bufr/build/examples/F90
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(eccodes_f_grib_set_pv "/opt/local/bufr/eccodes-2.16.0-Source/examples/F90/grib_set_pv.sh")
set_tests_properties(eccodes_f_grib_set_pv PROPERTIES  ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script" _BACKTRACE_TRIPLES "/opt/local/bufr/eccodes-2.16.0-Source/cmake/ecbuild_add_test.cmake;431;add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/F90/CMakeLists.txt;59;ecbuild_add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/F90/CMakeLists.txt;0;")
add_test(eccodes_f_grib_set_data "/opt/local/bufr/eccodes-2.16.0-Source/examples/F90/grib_set_data.sh")
set_tests_properties(eccodes_f_grib_set_data PROPERTIES  ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script" _BACKTRACE_TRIPLES "/opt/local/bufr/eccodes-2.16.0-Source/cmake/ecbuild_add_test.cmake;431;add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/F90/CMakeLists.txt;59;ecbuild_add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/F90/CMakeLists.txt;0;")
add_test(eccodes_f_grib_ecc-671 "/opt/local/bufr/eccodes-2.16.0-Source/examples/F90/grib_ecc-671.sh")
set_tests_properties(eccodes_f_grib_ecc-671 PROPERTIES  ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script" _BACKTRACE_TRIPLES "/opt/local/bufr/eccodes-2.16.0-Source/cmake/ecbuild_add_test.cmake;431;add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/F90/CMakeLists.txt;59;ecbuild_add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/F90/CMakeLists.txt;0;")
