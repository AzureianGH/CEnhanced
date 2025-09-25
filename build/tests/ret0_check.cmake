execute_process(COMMAND "/mnt/i/CEnhanced/build/tests/ret0.exe" RESULT_VARIABLE rc)
if(NOT rc EQUAL 0)
 message(FATAL_ERROR "Expected rc 0 got ${rc}")
endif()
