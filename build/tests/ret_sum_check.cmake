execute_process(COMMAND "/mnt/i/CEnhanced/build/tests/ret_sum.exe" RESULT_VARIABLE rc)
if(NOT rc EQUAL 7)
 message(FATAL_ERROR "Expected rc 7 got ${rc}")
endif()
