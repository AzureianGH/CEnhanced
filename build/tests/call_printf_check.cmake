execute_process(COMMAND "/mnt/i/CEnhanced/build/tests/call_printf.exe" RESULT_VARIABLE rc)
if(NOT rc EQUAL 16)
 message(FATAL_ERROR "Expected rc 16 got ${rc}")
endif()
