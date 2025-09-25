execute_process(COMMAND "I:/CEnhanced/build/mingw-release/tests/ret42.exe" RESULT_VARIABLE rc)
if(NOT rc EQUAL 42)
 message(FATAL_ERROR "Expected rc 42 got ${rc}")
endif()
