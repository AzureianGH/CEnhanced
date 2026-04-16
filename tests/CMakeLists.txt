set(CHANCEC $<TARGET_FILE:chancec>)

add_test(NAME c_frontend_diagnostics
    COMMAND ${CHANCEC} --diagnostics-only ${CMAKE_CURRENT_SOURCE_DIR}/examples/test.c)

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
add_ce_test(managed_string_switch_match managed_string_switch_match.ce 0)
add_ce_test(managed_split_extension_length managed_split_extension_length.ce 0)
add_ce_test(managed_empty_array_literal managed_empty_array_literal.ce 0)
add_ce_test(address_of address_of.ce 0)
add_ce_test(shift_left shift_left.ce 42)
add_ce_test(shift_right shift_right.ce 42)
add_ce_test(logical_not logical_not.ce 0)
add_ce_test(global_vars global_vars.ce 12)
add_ce_test(global_struct global_struct.ce 16)
add_ce_test(preproc_diagnostics preproc_diagnostics.ce 0)
add_ce_test(template_generics template_generics.ce 0)
add_ce_test(compound_assign compound_assign.ce 0)
add_ce_test(bundle_global_init bundle_global_init.ce 0)
add_ce_test(bundle_lifecycle bundle_lifecycle.ce 0)

add_test(NAME preproc_static_assert_fail COMMAND ${CHANCEC} ${CMAKE_CURRENT_SOURCE_DIR}/examples/preproc_static_assert_fail.ce)
set_tests_properties(preproc_static_assert_fail PROPERTIES WILL_FAIL TRUE)

add_test(NAME preproc_error COMMAND ${CHANCEC} ${CMAKE_CURRENT_SOURCE_DIR}/examples/preproc_error.ce)
set_tests_properties(preproc_error PROPERTIES WILL_FAIL TRUE)

add_test(NAME bundle_invalid_ctor_args COMMAND ${CHANCEC} ${CMAKE_CURRENT_SOURCE_DIR}/examples/bundle_invalid_ctor_args.ce)
set_tests_properties(bundle_invalid_ctor_args PROPERTIES WILL_FAIL TRUE)

add_test(NAME bundle_invalid_destructor COMMAND ${CHANCEC} ${CMAKE_CURRENT_SOURCE_DIR}/examples/bundle_invalid_destructor.ce)
set_tests_properties(bundle_invalid_destructor PROPERTIES WILL_FAIL TRUE)

add_test(NAME split_alias_import_fail COMMAND ${CHANCEC} ${CMAKE_CURRENT_SOURCE_DIR}/examples/split_alias_import_fail.ce)
set_tests_properties(split_alias_import_fail PROPERTIES WILL_FAIL TRUE)

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
add_ce_test_fs(shift_left shift_left.ce 42)
add_ce_test_fs(shift_right shift_right.ce 42)
add_ce_test_fs(logical_not logical_not.ce 0)
add_ce_test_fs(global_vars global_vars.ce 12)
add_ce_test_fs(global_struct global_struct.ce 16)
add_ce_test_fs(compound_assign compound_assign.ce 0)
