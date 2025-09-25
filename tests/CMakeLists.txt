set(CHANCEC $<TARGET_FILE:chancec>)

function(add_ce_test name src expected_rc)
    set(out_exe ${CMAKE_CURRENT_BINARY_DIR}/${name}.exe)
    add_test(NAME ${name} COMMAND ${CHANCEC} -o ${out_exe} ${CMAKE_CURRENT_SOURCE_DIR}/examples/${src})
    set_tests_properties(${name} PROPERTIES FIXTURES_SETUP ${name}_build)
    # Check exact exit code in a follow-up script
    set(check ${CMAKE_CURRENT_BINARY_DIR}/${name}_check.cmake)
    file(WRITE ${check}
        "execute_process(COMMAND \"${out_exe}\" RESULT_VARIABLE rc)\n"
        "if(NOT rc EQUAL ${expected_rc})\n"
        " message(FATAL_ERROR \"Expected rc ${expected_rc} got $"
        "{rc}\")\n"
        "endif()\n"
    )
    add_test(NAME ${name}_check COMMAND ${CMAKE_COMMAND} -P ${check})
    set_tests_properties(${name}_check PROPERTIES FIXTURES_REQUIRED ${name}_build)
endfunction()

add_ce_test(ret0 ret0.ce 0)
add_ce_test(ret42 ret42.ce 42)
add_ce_test(ret_sum ret_sum.ce 7)
add_ce_test(call_printf call_printf.ce 16)

# Freestanding mode tests
function(add_ce_test_fs name src expected_rc)
    set(out_exe ${CMAKE_CURRENT_BINARY_DIR}/${name}.exe)
    add_test(NAME ${name}_fs COMMAND ${CHANCEC} --freestanding -o ${out_exe} ${CMAKE_CURRENT_SOURCE_DIR}/examples/${src})
    set_tests_properties(${name}_fs PROPERTIES FIXTURES_SETUP ${name}_fs_build)
    set(check ${CMAKE_CURRENT_BINARY_DIR}/${name}_fs_check.cmake)
    file(WRITE ${check}
        "execute_process(COMMAND \"${out_exe}\" RESULT_VARIABLE rc)\n"
        "if(NOT rc EQUAL ${expected_rc})\n"
        " message(FATAL_ERROR \"Expected rc ${expected_rc} got $"
        "{rc}\")\n"
        "endif()\n"
    )
    add_test(NAME ${name}_fs_check COMMAND ${CMAKE_COMMAND} -P ${check})
    set_tests_properties(${name}_fs_check PROPERTIES FIXTURES_REQUIRED ${name}_fs_build)
endfunction()

add_ce_test_fs(ret0 ret0.ce 0)
add_ce_test_fs(ret42 ret42.ce 42)
