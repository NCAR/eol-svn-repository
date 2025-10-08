# CMake generated Testfile for 
# Source directory: /opt/local/bufr/eccodes-2.16.0-Source/examples/python
# Build directory: /opt/local/bufr/build/examples/python
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(eccodes_p_grib_set_pv_test "/opt/local/bufr/eccodes-2.16.0-Source/examples/python/grib_set_pv.sh")
set_tests_properties(eccodes_p_grib_set_pv_test PROPERTIES  ENVIRONMENT "PYTHON=/bin/python;OMP_NUM_THREADS=1" LABELS "eccodes;script" _BACKTRACE_TRIPLES "/opt/local/bufr/eccodes-2.16.0-Source/cmake/ecbuild_add_test.cmake;431;add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/python/CMakeLists.txt;79;ecbuild_add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/python/CMakeLists.txt;0;")
add_test(eccodes_p_grib_read_sample_test "/opt/local/bufr/eccodes-2.16.0-Source/examples/python/grib_read_sample.sh")
set_tests_properties(eccodes_p_grib_read_sample_test PROPERTIES  ENVIRONMENT "PYTHON=/bin/python;OMP_NUM_THREADS=1" LABELS "eccodes;script" _BACKTRACE_TRIPLES "/opt/local/bufr/eccodes-2.16.0-Source/cmake/ecbuild_add_test.cmake;431;add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/python/CMakeLists.txt;79;ecbuild_add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/python/CMakeLists.txt;0;")
add_test(eccodes_p_bufr_read_sample_test "/opt/local/bufr/eccodes-2.16.0-Source/examples/python/bufr_read_sample.sh")
set_tests_properties(eccodes_p_bufr_read_sample_test PROPERTIES  ENVIRONMENT "PYTHON=/bin/python;OMP_NUM_THREADS=1" LABELS "eccodes;script" _BACKTRACE_TRIPLES "/opt/local/bufr/eccodes-2.16.0-Source/cmake/ecbuild_add_test.cmake;431;add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/python/CMakeLists.txt;79;ecbuild_add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/python/CMakeLists.txt;0;")
add_test(eccodes_p_bufr_ecc-869_test "/opt/local/bufr/eccodes-2.16.0-Source/examples/python/bufr_ecc-869.sh")
set_tests_properties(eccodes_p_bufr_ecc-869_test PROPERTIES  ENVIRONMENT "PYTHON=/bin/python;OMP_NUM_THREADS=1" LABELS "eccodes;script" _BACKTRACE_TRIPLES "/opt/local/bufr/eccodes-2.16.0-Source/cmake/ecbuild_add_test.cmake;431;add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/python/CMakeLists.txt;79;ecbuild_add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/python/CMakeLists.txt;0;")
