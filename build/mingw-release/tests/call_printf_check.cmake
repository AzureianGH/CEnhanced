execute_process(COMMAND "I:/CEnhanced/build/mingw-release/tests/call_printf.exe" RESULT_VARIABLE rc)
if(NOT rc EQUAL 16)
 message(FATAL_ERROR "Expected rc 16 got ${rc}")
endif()
