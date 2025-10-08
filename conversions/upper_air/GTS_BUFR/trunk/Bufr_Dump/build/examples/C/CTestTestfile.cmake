# CMake generated Testfile for 
# Source directory: /opt/local/bufr/eccodes-2.16.0-Source/examples/C
# Build directory: /opt/local/bufr/build/examples/C
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(eccodes_c_grib_multi "/opt/local/bufr/eccodes-2.16.0-Source/examples/C/grib_multi.sh")
set_tests_properties(eccodes_c_grib_multi PROPERTIES  ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script" _BACKTRACE_TRIPLES "/opt/local/bufr/eccodes-2.16.0-Source/cmake/ecbuild_add_test.cmake;431;add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/C/CMakeLists.txt;106;ecbuild_add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/C/CMakeLists.txt;0;")
add_test(eccodes_c_grib_set_data "/opt/local/bufr/eccodes-2.16.0-Source/examples/C/grib_set_data.sh")
set_tests_properties(eccodes_c_grib_set_data PROPERTIES  ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script" _BACKTRACE_TRIPLES "/opt/local/bufr/eccodes-2.16.0-Source/cmake/ecbuild_add_test.cmake;431;add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/C/CMakeLists.txt;106;ecbuild_add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/C/CMakeLists.txt;0;")
add_test(eccodes_c_large_grib1 "/opt/local/bufr/eccodes-2.16.0-Source/examples/C/large_grib1.sh")
set_tests_properties(eccodes_c_large_grib1 PROPERTIES  ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script" _BACKTRACE_TRIPLES "/opt/local/bufr/eccodes-2.16.0-Source/cmake/ecbuild_add_test.cmake;431;add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/C/CMakeLists.txt;106;ecbuild_add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/C/CMakeLists.txt;0;")
add_test(eccodes_c_grib_sections_copy "/opt/local/bufr/eccodes-2.16.0-Source/examples/C/grib_sections_copy.sh")
set_tests_properties(eccodes_c_grib_sections_copy PROPERTIES  ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script" _BACKTRACE_TRIPLES "/opt/local/bufr/eccodes-2.16.0-Source/cmake/ecbuild_add_test.cmake;431;add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/C/CMakeLists.txt;106;ecbuild_add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/C/CMakeLists.txt;0;")
add_test(eccodes_c_get_product_kind_samples "/opt/local/bufr/eccodes-2.16.0-Source/examples/C/get_product_kind_samples.sh")
set_tests_properties(eccodes_c_get_product_kind_samples PROPERTIES  ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script" _BACKTRACE_TRIPLES "/opt/local/bufr/eccodes-2.16.0-Source/cmake/ecbuild_add_test.cmake;431;add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/C/CMakeLists.txt;106;ecbuild_add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/C/CMakeLists.txt;0;")
add_test(eccodes_c_new_sample "/opt/local/bufr/build/examples/C/eccodes_c_new_sample" "out.grib")
set_tests_properties(eccodes_c_new_sample PROPERTIES  ENVIRONMENT "ECCODES_SAMPLES_PATH=/opt/local/bufr/eccodes-2.16.0-Source/samples;ECCODES_DEFINITION_PATH=/opt/local/bufr/eccodes-2.16.0-Source/definitions;OMP_NUM_THREADS=1" LABELS "eccodes;executable" _BACKTRACE_TRIPLES "/opt/local/bufr/eccodes-2.16.0-Source/cmake/ecbuild_add_test.cmake;433;add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/C/CMakeLists.txt;142;ecbuild_add_test;/opt/local/bufr/eccodes-2.16.0-Source/examples/C/CMakeLists.txt;0;")
