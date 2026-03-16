cmake_minimum_required(VERSION 3.20)

project(CHance LANGUAGES C)

# Options
option(CHANCE_BUILD_TESTS "Build tests via CTest" ON)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Avoid Ninja: do not set a generator here; users can configure with VS or NMake.

set(CHANCE_CORE_SOURCES
    ${CMAKE_CURRENT_SOURCE_DIR}/src/lexer.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/parser.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/sema.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/mangle.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/includes.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/preproc.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/codegen_ccb.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/ccsim.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/module_registry.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/cclib.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/driver_cli.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/driver_overrides.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/driver_paths.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/driver_project.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/driver_options.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/driver_link.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/driver_runtime.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/driver_verbose.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/driver_toolchain.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/driver_validate.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/util.c
)

add_library(chance_core ${CHANCE_CORE_SOURCES})

target_include_directories(chance_core PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${CMAKE_CURRENT_SOURCE_DIR}/../ChanceCode/include
)

add_executable(chancec
    ${CMAKE_CURRENT_SOURCE_DIR}/src/main.c
)

target_link_libraries(chancec PRIVATE chance_core)

set(CHANCE_DEFAULT_RUNTIME "" CACHE STRING "Default runtime.cclib path")
set(CHANCE_DEFAULT_STDLIB "" CACHE STRING "Default stdlib.cclib path")
if(CHANCE_DEFAULT_RUNTIME)
    target_compile_definitions(chancec PRIVATE DEFAULT_RUNTIME=\"${CHANCE_DEFAULT_RUNTIME}\")
endif()
if(CHANCE_DEFAULT_STDLIB)
    target_compile_definitions(chancec PRIVATE DEFAULT_STDLIB=\"${CHANCE_DEFAULT_STDLIB}\")
endif()

include(GNUInstallDirs)
install(TARGETS chancec RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

if(CHANCE_BUILD_TESTS)
    include(CTest)
    add_subdirectory(tests)
endif()
