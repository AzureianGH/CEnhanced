execute_process(COMMAND "/mnt/i/CEnhanced/build/tests/ret42.exe" RESULT_VARIABLE rc)
if(NOT rc EQUAL 42)
 message(FATAL_ERROR "Expected rc 42 got ${rc}")
endif()
