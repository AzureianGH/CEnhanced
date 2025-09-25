# CMake generated Testfile for 
# Source directory: /mnt/i/CEnhanced/tests
# Build directory: /mnt/i/CEnhanced/build/tests
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test([=[ret0]=] "/mnt/i/CEnhanced/build/chancec" "-o" "/mnt/i/CEnhanced/build/tests/ret0.exe" "/mnt/i/CEnhanced/tests/examples/ret0.ce")
set_tests_properties([=[ret0]=] PROPERTIES  FIXTURES_SETUP "ret0_build" _BACKTRACE_TRIPLES "/mnt/i/CEnhanced/tests/CMakeLists.txt;5;add_test;/mnt/i/CEnhanced/tests/CMakeLists.txt;20;add_ce_test;/mnt/i/CEnhanced/tests/CMakeLists.txt;0;")
add_test([=[ret0_check]=] "/usr/bin/cmake" "-P" "/mnt/i/CEnhanced/build/tests/ret0_check.cmake")
set_tests_properties([=[ret0_check]=] PROPERTIES  FIXTURES_REQUIRED "ret0_build" _BACKTRACE_TRIPLES "/mnt/i/CEnhanced/tests/CMakeLists.txt;16;add_test;/mnt/i/CEnhanced/tests/CMakeLists.txt;20;add_ce_test;/mnt/i/CEnhanced/tests/CMakeLists.txt;0;")
add_test([=[ret42]=] "/mnt/i/CEnhanced/build/chancec" "-o" "/mnt/i/CEnhanced/build/tests/ret42.exe" "/mnt/i/CEnhanced/tests/examples/ret42.ce")
set_tests_properties([=[ret42]=] PROPERTIES  FIXTURES_SETUP "ret42_build" _BACKTRACE_TRIPLES "/mnt/i/CEnhanced/tests/CMakeLists.txt;5;add_test;/mnt/i/CEnhanced/tests/CMakeLists.txt;21;add_ce_test;/mnt/i/CEnhanced/tests/CMakeLists.txt;0;")
add_test([=[ret42_check]=] "/usr/bin/cmake" "-P" "/mnt/i/CEnhanced/build/tests/ret42_check.cmake")
set_tests_properties([=[ret42_check]=] PROPERTIES  FIXTURES_REQUIRED "ret42_build" _BACKTRACE_TRIPLES "/mnt/i/CEnhanced/tests/CMakeLists.txt;16;add_test;/mnt/i/CEnhanced/tests/CMakeLists.txt;21;add_ce_test;/mnt/i/CEnhanced/tests/CMakeLists.txt;0;")
add_test([=[ret_sum]=] "/mnt/i/CEnhanced/build/chancec" "-o" "/mnt/i/CEnhanced/build/tests/ret_sum.exe" "/mnt/i/CEnhanced/tests/examples/ret_sum.ce")
set_tests_properties([=[ret_sum]=] PROPERTIES  FIXTURES_SETUP "ret_sum_build" _BACKTRACE_TRIPLES "/mnt/i/CEnhanced/tests/CMakeLists.txt;5;add_test;/mnt/i/CEnhanced/tests/CMakeLists.txt;22;add_ce_test;/mnt/i/CEnhanced/tests/CMakeLists.txt;0;")
add_test([=[ret_sum_check]=] "/usr/bin/cmake" "-P" "/mnt/i/CEnhanced/build/tests/ret_sum_check.cmake")
set_tests_properties([=[ret_sum_check]=] PROPERTIES  FIXTURES_REQUIRED "ret_sum_build" _BACKTRACE_TRIPLES "/mnt/i/CEnhanced/tests/CMakeLists.txt;16;add_test;/mnt/i/CEnhanced/tests/CMakeLists.txt;22;add_ce_test;/mnt/i/CEnhanced/tests/CMakeLists.txt;0;")
add_test([=[call_printf]=] "/mnt/i/CEnhanced/build/chancec" "-o" "/mnt/i/CEnhanced/build/tests/call_printf.exe" "/mnt/i/CEnhanced/tests/examples/call_printf.ce")
set_tests_properties([=[call_printf]=] PROPERTIES  FIXTURES_SETUP "call_printf_build" _BACKTRACE_TRIPLES "/mnt/i/CEnhanced/tests/CMakeLists.txt;5;add_test;/mnt/i/CEnhanced/tests/CMakeLists.txt;23;add_ce_test;/mnt/i/CEnhanced/tests/CMakeLists.txt;0;")
add_test([=[call_printf_check]=] "/usr/bin/cmake" "-P" "/mnt/i/CEnhanced/build/tests/call_printf_check.cmake")
set_tests_properties([=[call_printf_check]=] PROPERTIES  FIXTURES_REQUIRED "call_printf_build" _BACKTRACE_TRIPLES "/mnt/i/CEnhanced/tests/CMakeLists.txt;16;add_test;/mnt/i/CEnhanced/tests/CMakeLists.txt;23;add_ce_test;/mnt/i/CEnhanced/tests/CMakeLists.txt;0;")
add_test([=[ret0_fs]=] "/mnt/i/CEnhanced/build/chancec" "--freestanding" "-o" "/mnt/i/CEnhanced/build/tests/ret0.exe" "/mnt/i/CEnhanced/tests/examples/ret0.ce")
set_tests_properties([=[ret0_fs]=] PROPERTIES  FIXTURES_SETUP "ret0_fs_build" _BACKTRACE_TRIPLES "/mnt/i/CEnhanced/tests/CMakeLists.txt;28;add_test;/mnt/i/CEnhanced/tests/CMakeLists.txt;42;add_ce_test_fs;/mnt/i/CEnhanced/tests/CMakeLists.txt;0;")
add_test([=[ret0_fs_check]=] "/usr/bin/cmake" "-P" "/mnt/i/CEnhanced/build/tests/ret0_fs_check.cmake")
set_tests_properties([=[ret0_fs_check]=] PROPERTIES  FIXTURES_REQUIRED "ret0_fs_build" _BACKTRACE_TRIPLES "/mnt/i/CEnhanced/tests/CMakeLists.txt;38;add_test;/mnt/i/CEnhanced/tests/CMakeLists.txt;42;add_ce_test_fs;/mnt/i/CEnhanced/tests/CMakeLists.txt;0;")
add_test([=[ret42_fs]=] "/mnt/i/CEnhanced/build/chancec" "--freestanding" "-o" "/mnt/i/CEnhanced/build/tests/ret42.exe" "/mnt/i/CEnhanced/tests/examples/ret42.ce")
set_tests_properties([=[ret42_fs]=] PROPERTIES  FIXTURES_SETUP "ret42_fs_build" _BACKTRACE_TRIPLES "/mnt/i/CEnhanced/tests/CMakeLists.txt;28;add_test;/mnt/i/CEnhanced/tests/CMakeLists.txt;43;add_ce_test_fs;/mnt/i/CEnhanced/tests/CMakeLists.txt;0;")
add_test([=[ret42_fs_check]=] "/usr/bin/cmake" "-P" "/mnt/i/CEnhanced/build/tests/ret42_fs_check.cmake")
set_tests_properties([=[ret42_fs_check]=] PROPERTIES  FIXTURES_REQUIRED "ret42_fs_build" _BACKTRACE_TRIPLES "/mnt/i/CEnhanced/tests/CMakeLists.txt;38;add_test;/mnt/i/CEnhanced/tests/CMakeLists.txt;43;add_ce_test_fs;/mnt/i/CEnhanced/tests/CMakeLists.txt;0;")
